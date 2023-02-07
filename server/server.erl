-module(server).

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% for users
-export([start/0, new_game/1, respond_okay/0, respond_order/1, respond_target/1]).

-record(state,
        { harness = undefined
        , games = []
        , client_sup = undefined
        , clients = [] 
        }).

-record(game, {game_id, client_1, client_2}).

-define(GAME_PORT, 3000).
-define(SRVR_PORT, 4056).

new_game({GameID, Config}) ->
    gen_server:cast(srvr, {new_game, GameID, Config}).

respond_okay() ->
    gen_server:cast(srvr, {porter, {info_conf, ok}}).

respond_order(Ordering) ->
    gen_server:cast(srvr, {porter, {ordered, Ordering}}).

respond_target(Target) ->
    gen_server:cast(srvr, {porter, {targeted, Target}}).

start() ->
    gen_server:start(?MODULE, init, [undefined]).

init(_) ->
    register(srvr, self()),
    {ok, Harness} = harness:start_link(?GAME_PORT),
    {ok, ClientSup} = client_sup:start_link(?SRVR_PORT, self()),
    {ok, #state{harness = Harness, client_sup = ClientSup}}.

%% Error catch all
handle_call(_Request, _From, State) ->
    {stop, unknown_request, State}.

handle_cast({new_game, GameID, Config}, #state{harness = Harness} = State) ->
    Harness ! {new_game, GameID, Config},
    {noreply, State};
handle_cast({register, Name, Pid}, #state{clients = Clients} = State) ->
    Clients1 = [{Name, Pid} | Clients],
    io:format("~p~n", [Clients1]),
    {noreply, State#state{clients = Clients1}};

%% Relaying a request from tcp game conn to the clients
handle_cast({_, started} = _Req, State) ->
    {noreply, State};
handle_cast({GameID, {ordr, _Hdrs}} = Req, State) ->
    {noreply, State};
handle_cast({GameID, {trgt, _Hdr, _Range}} = Req, State) ->
    {noreply, State};
handle_cast({GameID, {rand, Range}} = Req, State) ->
    RandCID = random_in_range(Range),
    gen_server:cast(self(), {porter, {randomized, RandCID}}),
    {noreply, State};
handle_cast({GameID, {info, _}} = Req, State) ->
    {noreply, State};

%% Relaying a response from a client to tcp game conn
handle_cast({porter, Response}, #state{harness = Harness} = State) ->
    Harness ! {response, Response},
    {noreply, State};

handle_cast(tcp_closed, State) ->
    {stop, tcp_closed, State};
handle_cast(port_terminated, State) ->
    {stop, port_terminated, State};
handle_cast(_Requst, State) ->
    {stop, unknown_request, State}.

terminate(_Reason, #state{harness = Harness} = _State) ->
    Harness ! {error, closed},
    ok.
    
random_in_range(Range) ->
    RandIdx = rand:uniform(length(Range)),
    lists:nth(RandIdx, Range).
