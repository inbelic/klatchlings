-module(client).

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% for users
-export([start/0]).

-record(state,
        { port = undefined
        }).

start() ->
    gen_server:start(?MODULE, init, [{}]).

init(_) ->
    {ok, Port} = port_relayer:start_link(self()),
    {ok, #state{port = Port}}.

handle_call({trgt, Int}, _From, State) ->
    port_relayer:target(Int),
    receive
        {return, Return} ->
            {reply, {ok, Return}, State}
    end;
handle_call(_Request, _From, State) ->
    {stop, unknown_request, State}.

handle_cast(port_terminated, State) ->
    {stop, port_terminated, State};
handle_cast(_Request, State) ->
    {stop, unknown_request, State}.

terminate(_Reason, #state{port = Port} = _State) ->
    Port ! stop.
