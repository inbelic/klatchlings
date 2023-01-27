-module(client).

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% for users
-export([start/0]).

-record(state,
        { port = undefined
        , harness = undefined
        }).

-define(GAME_PORT, 3000).

start() ->
    gen_server:start(?MODULE, init, [{}]).

init(_) ->
    {ok, Port} = porter:start(),
    {ok, Harness} = game_harness:start_link(?GAME_PORT),
    {ok, #state{port = Port, harness = Harness}}.


%% Error catch all
handle_call(_Request, _From, State) ->
    {stop, unknown_request, State}.

%% Relaying a request from tcp to the ui port
handle_cast({ordr, _Hdrs} = Req, State) ->
    porter:request(self(), Req),
    {noreply, State};

%% Relaying a response from ui port to tcp
handle_cast({porter, Response}, #state{harness = Harness} = State) ->
    gen_server:cast(Harness, {response, Response}),
    {noreply, State};

handle_cast(tcp_closed, State) ->
    {stop, tcp_closed, State};
handle_cast(port_terminated, State) ->
    {stop, port_terminated, State};
handle_cast(_Request, State) ->
    {stop, unknown_request, State}.

terminate(_Reason, #state{port = Port} = _State) ->
    Port ! stop.
