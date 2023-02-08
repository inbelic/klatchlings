-module(client_conn).

-include("../include/command.hrl").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([start_link/1]).

-record(state,
        { server = undefined
        , socket = undefined
        , status = menu         :: menu | game | queued
        }).

start_link({Server, ListenSocket}) ->
    gen_server:start_link(?MODULE, {Server, ListenSocket}, []).

init({Server, ListenSocket}) ->
    gen_server:cast(self(), accept),
    {ok, #state{server = Server, socket = ListenSocket}}.

%% Error catch all
handle_call(_Request, _From, State) ->
    {stop, unknown_request, State}.

handle_cast(accept, #state{socket = ListenSocket} = State) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    gen_server:cast(manager, create),
    gen_server:cast(self(), greet),
    {noreply, State#state{socket = AcceptSocket}};

handle_cast(greet, #state{socket = Socket} = State) ->
    gen_tcp:send(Socket, "hello client!"),
    {noreply, State};
handle_cast({mgr_stop, Reason}, #state{socket = Socket} = State) ->
    gen_tcp:close(Socket),
    {stop, {mgr, Reason}, State};

%% Error catch all
handle_cast(_Requst, State) ->
    {stop, unknown_request, State}.

handle_info({tcp, Socket, Size}, #state{socket = Socket} = State) ->
    case load_request(Socket, Size) of
        bad_hdr ->
            gen_tcp:send(Socket, <<1>>),
            inet:setopts(Socket, [{active, once}]),
            {noreply, State};
        Request ->
            io:format("<req> ~p~n", [Request]),
            gen_tcp:send(Socket, <<0>>),
            inet:setopts(Socket, [{active, once}]),
            {noreply, State}
    end;
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    {stop, normal, State};
handle_info({tcp_error, Socket}, #state{socket = Socket} = State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, unknown_info, State}.

terminate(_Reason, _State) ->
    ok.

%% We are required to deal with the fact that the sends could come
%% all at once, or in fragments throughout
load_request(Socket, <<SizeBin:1/binary>>) ->
    Expected = hd(binary_to_list(SizeBin)),
    case {Expected, gen_tcp:recv(Socket, Expected)} of
        {_, {error, closed}} ->
            self() ! {tcp_error, Socket},
            bad_hdr;
        {255, {ok, Bin}} when size(Bin) == 255 ->
            SizeBin = gen_tcp:recv(Socket, 1),
            binary_to_list(Bin) ++ load_request(Socket, SizeBin);
        {_, {ok, Bin}} when size(Bin) == Expected ->
            binary_to_list(Bin);
        _ ->
            bad_hdr
    end;
load_request(Socket, <<SizeBin:1/binary, Bin/binary>>) ->
    Expected = hd(binary_to_list(SizeBin)),
    case size(Bin) of
        255 when Expected == 255 ->
            case gen_tcp:recv(Socket, 1) of
                {error, closed} ->
                    self() ! {tcp_error, Socket},
                    bad_hdr;
                {ok, NextBin} ->
                    binary_to_list(Bin) ++ load_request(Socket, NextBin)
            end;
        X when Expected == 255 andalso X > 255 ->
            <<CurBin:255/binary, RestBin/binary>> = Bin,
            binary_to_list(CurBin) ++ load_request(Socket, RestBin);
        X when X == Expected ->
            binary_to_list(Bin);
        _ ->
            bad_hdr
    end.
