-module(harness).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([start_link/0]).

-define(GAME_PORT, 3000).

-record(game,
        { p1 
        , p2 
        , status
        , seed
        }).

-record(game_config,
        { p1 
        , p2 
        , start_config
        }).

-record(state,
        { socket = undefined
        , games = dict:new()
        , next_gid = 0
        , listen_socket = undefined
        }).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, LSock}
        = gen_tcp:listen(?GAME_PORT, [binary, {packet, 0}, {reuseaddr, true},
                                      {active, true}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, #state{socket = Sock, listen_socket = LSock}}.

handle_info({tcp, Socket, Bin}, #state{socket = Socket} = State) ->
    do_tcp(Bin, State);
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    {stop, haskell_closed, State};
handle_info({tcp_error, Socket}, #state{socket = Socket} = State) ->
    {stop, haskell_closed, State};
handle_info(_Info, State) ->
    {stop, unknown_info, State}.

%% Error catch all
handle_call({start_game, #game_config{} = GameConfig}, _From, State) ->
    do_startup(GameConfig, State);
handle_call(_Request, _From, State) ->
    {stop, unknown_request, State}.

%% Error catch all

handle_cast({player_input, From, GID, Input}, State) ->
    do_player_input(From, Input, GID, State);

handle_cast({send_input, GID, Input}, #state{socket = Socket} = State) ->
    ToInput = integer_to_list(GID) ++ ":" ++ Input,
    io:format("sending: ~p~n", [ToInput]),
    gen_tcp:send(Socket, list_to_binary(ToInput)),
    {noreply, State};

handle_cast({dbg_input, Input}, #state{socket = Socket} = State)
  when is_list(Input) ->
    gen_tcp:send(Socket, list_to_binary(Input)),
    {noreply, State};
handle_cast({dbg_input, Input}, #state{socket = Socket} = State)
  when is_binary(Input) ->
    gen_tcp:send(Socket, Input),
    {noreply, State};
handle_cast(_Requst, State) ->
    {stop, unknown_request, State}.

terminate(_Reason, #state{socket = Sock, listen_socket = LSock} = _State) ->
    try
        gen_tcp:close(Sock),
        gen_tcp:close(LSock),
        ok
    catch
        _:_ ->
            ok
    end.

do_startup(#game_config{p1 = P1, p2 = P2, start_config = Config},
           #state{next_gid = GID, games = Games} = State) ->
    Seed = rand:seed_s(exsss),
    Game = #game{p1 = P1, p2 = P2, status = awaiting, seed = Seed},
    gen_server:cast(self(), {send_input, GID, Config}),
    {reply,
     {GID},
     State#state{games = dict:store(GID, Game, Games), next_gid = GID + 1}}.

do_player_input(From, Input, GID,
                #state{games = Games} = State) ->
    try
        {ok, Game} = dict:find(GID, Games),
        case validate_input(From, Game, Input) of
            {valid, forward, Game1, ValidInput} ->
                gen_server:cast(self(), {send_input, GID, ValidInput}),
                {noreply, State#state{games = dict:store(GID, Game1, Games)}};
            {valid, await, Game1} ->
                {noreply, State#state{games = dict:store(GID, Game1, Games)}};
            invalid ->
                {noreply, State}
        end
    catch
        _:_ ->
            {noreply, State}
    end.

validate_input(From, #game{p1 = From} = Game, {order, OrderStr}) ->
    Order = encode:decode_range(fun encode:extract_num/1, OrderStr),
    case Game#game.status of
        {ordr, System, empty, empty} ->
            {valid, await, Game#game{status = {ordr, System, Order, empty}}};
        {ordr, System, empty, OpOrder} ->
            %% FIXME: need a deterministic way to order the orderings
            Order = System ++ Order ++ OpOrder,
            Input = encode:encode_range(Order),
            {valid, forward, Game#game{status = awaiting}, Input};
        _ ->
            invalid
    end;
validate_input(From, #game{p2 = From} = Game, {order, OrderStr}) ->
    Order = encode:decode_range(fun encode:extract_num/1, OrderStr),
    case Game#game.status of
        {ordr, System, empty, empty} ->
            {valid, await, Game#game{status = {ordr, System, empty, Order}}};
        {ordr, System, OpOrder, empty} ->
            %% FIXME: need a deterministic way to order the orderings
            TtlOrder = System ++ OpOrder ++ Order,
            Input = encode:encode_range(TtlOrder),
            {valid, forward, Game#game{status = awaiting}, Input};
        _ ->
            invalid
    end;
validate_input(From, #game{} = Game, {target, Trgt}) ->
    case Game#game.status of
        {trgt, Player} when Player == either orelse Player == From ->
            Input = integer_to_list(Trgt),
            {valid, forward, Game#game{status = awaiting}, Input};
        _ ->
            invalid
    end;
validate_input(_, _, _) ->
    invalid.

do_tcp(<<"connected">>, State) ->
    io:format("~p~n", [harness_connected]),
    {noreply, State};
do_tcp(GameReq, #state{games = Games} = State) ->
    {GID, ":" ++ Request} = encode:extract_num(binary_to_list(GameReq)),
    io:format("~p~n", [Request]),
    case dict:find(GID, Games) of
        error ->
            %% We can assume this won't happen but it is code so...
            {noreply, State};
        {ok, Game} ->
            case encode:decode_request(Request) of
                {view, Args} ->
                    do_view(Args, GID, Game, State);
                {rand, Args} ->
                    do_random(Args, GID, Game, State);
                {ordr, Args} ->
                    do_order(Args, GID, Game, State);
                {trgt, Args} ->
                    do_target(Args, GID, Game, State);
                bad_req ->
                    {noreply, State}
            end
    end.

do_view(ViewStr, GID, Game, #state{games = Games} = State) ->
    case lists:splitwith(fun(Char) -> Char /= $~ end, ViewStr) of
        {P1View, "~" ++ P2View} ->
            %% Forward to appropriate player
            gen_server:cast(Game#game.p1, {game_req, {view, P1View}}),
            gen_server:cast(Game#game.p2, {game_req, {view, P2View}}),

            %% Respond that we received the views okay
            gen_server:cast(self(), {send_input, GID, "ok"}),

            %% Update state
            Game1 = Game#game{status = view},
            Games1 = dict:store(GID, Game1, Games),
            {noreply, State#state{games = Games1}};
        _ ->
            %% Should probably send back something to notify bad recv
            {noreply, State}
    end.

do_random(RangeStr, GID, Game,
          #state{games = Games} = State) ->
    %% Decode string
    case encode:decode_range(fun encode:extract_num/1, RangeStr) of
        none ->
            %% Should probably send back something to notify bad recv
            {noreply, State};
        {Range, _Token}  ->
            %% Generate random target
            {RandIdx, Seed} = rand:uniform_s(length(Range), Game#game.seed),
            Trgt = lists:nth(RandIdx, Range),

            %% Forward the random number
            gen_server:cast(self(), {send_input, GID, integer_to_list(Trgt)}),

            %% Update the state accordingly
            Game1 = Game#game{status = rand, seed = Seed},
            Games1 = dict:store(GID, Game1, Games),
            {noreply, State#state{games = Games1}}
    end.

do_order(RangeStr, GID, Game, #state{games = Games} = State) ->
    %% Decode string
    Fun = encode:extract_tuple(fun encode:extract_owner/1, fun encode:extract_header/1),
    case encode:decode_range(Fun, RangeStr) of
        none ->
            %% Should probably send back something to notify bad recv
            {noreply, State};
        {Range, _Token} ->
            IsP1 = fun({Owner, _Hdr}) -> Owner == p1 end,
            IsP2 = fun({Owner, _Hdr}) -> Owner == p2 end,
            {P1WithOwner, Rest} = lists:partition(IsP1, Range),
            {P2WithOwner, SystemWithOwner} = lists:partition(IsP2, Rest),

            %% Filter out the owners since it will be implicit in the order
            MapFun = fun({_Owner, Hdr}) -> Hdr end,
            P1 = lists:map(MapFun, P1WithOwner),
            P2 = lists:map(MapFun, P2WithOwner),
            System = lists:map(MapFun, SystemWithOwner),

            %% Forward target request to players
            gen_server:cast(Game#game.p1, {game_req, {ordr, P1, P2, System}}),
            gen_server:cast(Game#game.p2, {game_req, {ordr, P2, P1, System}}),

            %% Update the state accordingly
            Game1 = Game#game{status = {ordr, System, empty, empty}},
            Games1 = dict:store(GID, Game1, Games),
            {noreply, State#state{games = Games1}}
    end.

do_target(TrgtStr, GID, Game, #state{games = Games} = State) ->
    try
        {Owner, HdrStr} = encode:extract_owner(TrgtStr),
        {Hdr, " " ++ RangeStr} = encode:extract_header(HdrStr),
        {Range, _} = encode:decode_range(fun encode:extract_num/1, RangeStr),
        TargetPlayer = case Owner of
                           p1 -> Game#game.p1;
                           p2 -> Game#game.p2;
                           rules -> rules
                       end,
        gen_server:cast(TargetPlayer,
                        {game_req, {trgt, Hdr, Range}}),
        %% Update the state accordingly
        Game1 = Game#game{status = {trgt, TargetPlayer}},
        Games1 = dict:store(GID, Game1, Games),
        {noreply, State#state{games = Games1}}
    catch
        _:_ ->
            %% Should probably send back something to notify bad recv
            {noreply, State}
    end.
