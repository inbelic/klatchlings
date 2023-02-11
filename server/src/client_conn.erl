-module(client_conn).

-include("../include/command.hrl").
-include("../include/client_conn.hrl").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([start_link/1]).

-record(state,
        { socket = undefined
        , game = menu
        }).

-record(game,
        { gid
        , harness
        , req_state
        }).

start_link(#client_config{} = Config) ->
    gen_server:start_link(?MODULE, Config, []).

init(#client_config{listen_socket = ListenSocket}) ->
    gen_server:cast(self(), accept),
    {ok, #state{socket = ListenSocket}}.

%% Error catch all
handle_call(_Request, _From, State) ->
    {stop, unknown_request, State}.

handle_cast(accept, State) ->
    do_accept(State);
handle_cast(greet, State) ->
    do_greet(State);
handle_cast({input, Cmd, Args}, State) ->
    do_input(Cmd, Args, State);
handle_cast({game_req, Req}, State) ->
    do_game_req(Req, State);
handle_cast({start_game, GID, Harness}, State) ->
    Game = #game{gid = GID, harness = Harness, req_state = awaiting},
    {noreply, State#state{game = Game}};

handle_cast({send_output, Output}, State) when is_list(Output) ->
    handle_output(list_to_binary(Output), State);
handle_cast({send_output, Output}, State) when is_binary(Output) ->
    handle_output(Output, State);

handle_cast({send_output, Cmd, Args}, State) when is_binary(Cmd) ->
    {noreply, _State} = handle_output(Cmd, State),
    case true of
        _ when is_list(Args) ->
            handle_output(list_to_binary(Args), State);
        _ when is_binary(Args) ->
            handle_output(Args, State);
        _ ->
            {noreply, State}
    end;

handle_cast({send_confirm, Status}, #state{socket = Socket} = State)
  when is_binary(Status) andalso size(Status) == 1 ->
    gen_tcp:send(Socket, Status),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_cast({mgr_stop, Reason}, #state{socket = Socket} = State) ->
    gen_tcp:close(Socket),
    {stop, {mgr, Reason}, State};

%% Error catch all
handle_cast(_Requst, State) ->
    {stop, unknown_request, State}.

handle_info({tcp, Socket, Bin}, #state{socket = Socket} = State) ->
    do_tcp(Bin, State);
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    {stop, normal, State};
handle_info({tcp_error, Socket}, #state{socket = Socket} = State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, unknown_info, State}.

terminate(_Reason, #state{socket = Socket} = _State) ->
    gen_tcp:close(Socket),
    ok.

do_accept(#state{socket = ListenSocket} = State) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    gen_server:cast(manager, create),
    gen_server:cast(self(), greet),
    {noreply, State#state{socket = AcceptSocket}}.

do_greet(State) ->
    gen_server:cast(self(), {send_output, "hello client!"}),
    {noreply, State}.

%% This chunk deals with processing a validated input
do_input(order, Args, #state{game = Game} = State) ->
    #game{gid = GID, harness = Harness} = Game,
    %% We will then forward to the game harness with the GID prepended
    gen_server:cast(Harness, {player_input, self(), GID, {order, Args}}),
    {noreply, State};
do_input(target, Args, #state{game = Game} = State) ->
    #game{gid = GID, harness = Harness} = Game,
    %% We will then forward to the game harness with the GID prepended
    gen_server:cast(Harness, {player_input, self(), GID, {target, Args}}),
    {noreply, State};
do_input(play, Args, State) ->
    gen_server:cast(gqueue, {queue, self(), 100, 100, Args}),
    {noreply, State};
do_input(_Cmd, _Args, State) ->
    {noreply, State}.

do_game_req({view, PlayerView}, State) ->
    gen_server:cast(self(), {send_output, <<?VIEW>>, PlayerView}),
    {noreply, State};
do_game_req({ordr, MyOrder, OpOrder, SysOrder},
            #state{game = Game} = State) ->
    TtlOrder = encode:encode_order(MyOrder)
             ++ encode:encode_order(OpOrder)
             ++ encode:encode_order(SysOrder),
    gen_server:cast(self(), {send_output, <<?ORDR>>, TtlOrder}),
    Game1 = Game#game{req_state = {ordr, length(MyOrder)}},
    {noreply, State#state{game = Game1}};
do_game_req({trgt, Hdr, Range},
            #state{game = Game} = State) ->
    Output = encode:encode_header(Hdr) ++ encode:encode_range(fun integer_to_list/1, Range),
    gen_server:cast(self(), {send_output, <<?TRGT>>, Output}),
    Game1 = Game#game{req_state = {trgt, Range}},
    {noreply, State#state{game = Game1}};
do_game_req(_, State) ->
    {noreply, State}.

%% This chunk deals with validating an incoming tcp input
do_tcp(Bin, #state{socket = Socket} = State) ->
    case load_input(Socket, Bin) of
        bad_hdr ->
            gen_server:cast(self(), {send_confirm, <<?BAD_HEADER>>}),
            {noreply, State};
        InputStr ->
            io:format("handling ~p~n", [InputStr]),
            ok = handle_input(encode:decode_input(InputStr), State#state.game),
            {noreply, State}
    end.

%% We are required to deal with the fact that the sends could come
%% all at once, or in fragments throughout
load_input(Socket, <<SizeBin:1/binary>>) ->
    Expected = hd(binary_to_list(SizeBin)),
    case {Expected, gen_tcp:recv(Socket, Expected)} of
        {_, {error, closed}} ->
            self() ! {tcp_error, Socket},
            bad_hdr;
        {255, {ok, Bin}} when size(Bin) == 255 ->
            SizeBin = gen_tcp:recv(Socket, 1),
            binary_to_list(Bin) ++ load_input(Socket, SizeBin);
        {_, {ok, Bin}} when size(Bin) == Expected ->
            binary_to_list(Bin);
        _ ->
            bad_hdr
    end;
load_input(Socket, <<SizeBin:1/binary, Bin/binary>>) ->
    Expected = hd(binary_to_list(SizeBin)),
    case size(Bin) of
        0 ->
            "";
        255 when Expected == 255 ->
            case gen_tcp:recv(Socket, 1) of
                {error, closed} ->
                    self() ! {tcp_error, Socket},
                    bad_hdr;
                {ok, NextBin} ->
                    binary_to_list(Bin) ++ load_input(Socket, NextBin)
            end;
        X when Expected == 255 andalso X > 255 ->
            <<CurBin:255/binary, RestBin/binary>> = Bin,
            binary_to_list(CurBin) ++ load_input(Socket, RestBin);
        X when X == Expected ->
            binary_to_list(Bin);
        _ ->
            bad_hdr
    end.

handle_input(bad_cmd, _) ->
    gen_server:cast(self(), {send_confirm, <<?BAD_COMMAND>>}),
    ok;
handle_input({Cmd, Args}, Game) ->
    case validate_args(Cmd, Args, Game) of
        valid ->
            gen_server:cast(self(), {send_confirm, <<?OKAY>>}),
            gen_server:cast(self(), {input, Cmd, Args}),
            ok;
        invalid ->
            gen_server:cast(self(), {send_confirm, <<?BAD_ARGS>>}),
            ok
    end.

validate_args(quit, _Args, _Game) ->
    valid;
validate_args(play, _Args, menu) ->
    valid;
validate_args(order, OrderStr, #game{req_state = {ordr, Length}} = _Game) ->
    try
        {Order, ""} = encode:decode_range(fun encode:extract_number/1, OrderStr),
        true = lists:sort(Order) == lists:seq(1, Length),
        valid
    catch
        _:_ ->
            invalid
    end;
validate_args(target, TrgtStr, #game{req_state = {trgt, Range}} = _Game) ->
    try
        {Trgt, ""} = encode:extract_number(TrgtStr),
        true = lists:member(Trgt, Range),
        valid
    catch
        _:_ ->
            invalid
    end;
validate_args(_Cmd, _Args, _Game) ->
    invalid.

handle_output(Bin, #state{socket = Socket} = State) ->
    Size = size(Bin),
    io:format("sending ~p bytes: ~p~n", [Size, Bin]),
    case min(Size, 255) of
        255 when 255 < Size ->
            <<ToSend:255/binary, Rest/binary>> = Bin,
            gen_tcp:send(Socket, <<255>>),
            gen_tcp:send(Socket, ToSend),
            handle_output(Rest, State);
        Expected ->
            gen_tcp:send(Socket, <<Expected>>),
            gen_tcp:send(Socket, Bin),
            inet:setopts(Socket, [{active, once}]),
            {noreply, State}
    end.
