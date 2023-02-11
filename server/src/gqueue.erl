-module(gqueue).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2]).

-export([start_link/0]).

-record(state,
        { in_queue = []
        , harni = []
        }).

-record(game_config,
        { p1 
        , p2 
        , start_config
        }).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    register(gqueue, self()),
    {ok, #state{}}.

%% Error catch all
handle_call({give_harness, Harness}, _From, State) ->
    {reply, ok, State#state{harni = [Harness]}};
handle_call(_Request, _From, State) ->
    {stop, unknown_request, State}.

%% Error catch all
handle_cast({queue, From, Rating, Location, Deck}, State) ->
    do_queue(From, Rating, Location, Deck, State);
handle_cast(_Requst, State) ->
    {stop, unknown_request, State}.

do_queue(From, Rating, Location, Deck, #state{in_queue = Queue, harni = Harni} = State) ->
    case suitable_player(Rating, Location, Queue) of
        none ->
            io:format("added to queue~n"),
            Queue1 = [{From, Rating, Location, Deck} | Queue],
            {noreply, State#state{in_queue = Queue1}};
        {Player, _Rating, PLocation, PDeck} ->
            Harness = suitable_harness(Location, PLocation, Harni),
            {P1, P1Deck, P2, P2Deck} = case rand:uniform(2) of
                           1 -> {From, Deck, Player, PDeck};
                           2 -> {Player, PDeck, From, Deck}
                       end,
            StartConfig = P1Deck ++ "~" ++ P2Deck,
            Config = #game_config{p1 = P1, p2 = P2, start_config = StartConfig},
            {GID} = gen_server:call(Harness, {start_game, Config}),
            gen_server:cast(P1, {start_game, GID, Harness}),
            gen_server:cast(P2, {start_game, GID, Harness}),
            Queue1 = lists:keydelete(Player, 1, Queue),
            {noreply, State#state{in_queue = Queue1}}
    end.

suitable_player(CurRating, CurLocation, Queue) ->
    Suitable = lists:filter(fun({_Player, Location, Rating, _Deck}) ->
                                   abs(Rating - CurRating) < 100
                                   andalso abs(Location - CurLocation) < 100
                            end, Queue),
    case Suitable of
        [] -> none;
        _ ->
            RandIdx = rand:uniform(length(Suitable)),
            lists:nth(RandIdx, Suitable)
    end.

suitable_harness(_Location1, _Location2, Harni) ->
    hd(Harni).
