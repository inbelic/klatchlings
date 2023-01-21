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
    {ok, Port} = port_relayer:start_link(self()),
    {ok, Harness} = game_harness:start_link(?GAME_PORT),
    {ok, #state{port = Port, harness = Harness}}.

%% Relaying a request/response from tcp to the port
handle_call({trgt, _Hdr, _Rng} = Req, _From, State) ->
    {trgt, Response} = port_relayer:request(Req),
    {reply, {trgt, Response}, State};
handle_call({rand, _Hdr, _Rng} = Req, _From, State) ->
    {rand, Response} = port_relayer:request(Req),
    {reply, {rand, Response}, State};
handle_call({ordr, _Ordering} = Req, _From, State) ->
    {ordr, Response} = port_relayer:request(Req),
    {reply, {ordr, Response}, State};
handle_call({info, _GameState} = Req, _From, State) ->
    {info, ok} = port_relayer:request(Req),
    {reply, {info, ok}, State};

%% Error catch all
handle_call(_Request, _From, State) ->
    {stop, unknown_request, State}.

handle_cast(tcp_closed, State) ->
    {stop, tcp_closed, State};
handle_cast(port_terminated, State) ->
    {stop, port_terminated, State};
handle_cast(_Request, State) ->
    {stop, unknown_request, State}.

terminate(_Reason, #state{port = Port} = _State) ->
    Port ! stop.
