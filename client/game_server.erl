-module(game_server).

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% for users
-export([start/0]).

-record(state,
        { lsock = undefined
        , sock = undefined
        }).

start() ->
    gen_server:start(?MODULE, init, [{}]).

init(_) ->
    {ok, LSock} = gen_tcp:listen(3000, [binary, {packet, 0}, {active, false}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, _Port} = open_port({spawn, "../../klatchlings-game/build/klatch-game"},[]),
    {ok, #state{lsock = LSock, sock = Sock}}.

handle_call(get_request, _From, #state{sock = Sock} = State) ->
    {ok, Request} = gen_tcp:recv(Sock, 0),
    {reply, {request, Request}, State};
handle_call(_Request, _From, State) ->
    {stop, unknown_request, State}.

handle_cast({response, Response}, #state{sock = Sock} = State) ->
    ok = gen_tcp:send(Sock, Response),
    {noreply, State};
handle_cast(port_terminated, State) ->
    {stop, port_terminated, State};
handle_cast(_Request, State) ->
    {stop, unknown_request, State}.

terminate(_Reason, #state{lsock = LSock, sock = Sock} = _State) ->
    ok = gen_tcp:close(LSock),
    ok = gen_tcp:close(Sock),
    ok.
