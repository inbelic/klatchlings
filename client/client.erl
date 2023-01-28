-module(client).

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% for users
-export([start/0, start/1]).

-record(state,
        { port = undefined
        , harness = undefined
        , script = undefined
        }).

-define(GAME_PORT, 3000).

start() ->
    gen_server:start(?MODULE, init, [undefined]).

start(ScriptPid) ->
    gen_server:start(?MODULE, init, [ScriptPid]).

init(ScriptPid) ->
    {ok, Port} = porter:start(),
    {ok, Harness} = harness:start_link(?GAME_PORT),
    {ok, #state{port = Port, harness = Harness, script = ScriptPid}}.


%% Error catch all
handle_call(_Request, _From, State) ->
    {stop, unknown_request, State}.

%% Relaying a request from tcp to the ui port
handle_cast({ordr, _Hdrs} = Req, State) ->
    io:format("<~p> ~p: ~p~n", [harness, f_ordr, Req]),
    porter:request(self(), Req),
    {noreply, State};
handle_cast({trgt, _Hdr, _Range} = Req, State) ->
    io:format("<~p> ~p: ~p~n", [harness, f_trgt, Req]),
    porter:request(self(), Req),
    {noreply, State};
handle_cast({rand, Range} = Req, State) ->
    io:format("<~p> ~p: ~p~n", [harness, f_rand, Req]),
    RandCID = random_in_range(Range),
    gen_server:cast(self(), {porter, {randomized, RandCID}}),
    {noreply, State};

%% Relaying a response from ui port to tcp
handle_cast({porter, Response}, #state{harness = Harness} = State) ->
    io:format("<~p> ~p: ~p~n", [harness, r_rep, Response]),
    Harness ! {response, Response},
    {noreply, State};

handle_cast(tcp_closed, State) ->
    {stop, tcp_closed, State};
handle_cast(port_terminated, State) ->
    {stop, port_terminated, State};
handle_cast(_Request, State) ->
    {stop, unknown_request, State}.

terminate(_Reason, #state{port = Port, script = ScriptPid} = _State) ->
    Port ! stop,
    ScriptPid ! stop.

random_in_range(Range) ->
    RandIdx = rand:uniform(length(Range)),
    lists:nth(RandIdx, Range).
