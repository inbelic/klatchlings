-module(client_manager).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state,
        { clients = []
        }).

start() ->
    gen_server:start(?MODULE, init, [undefined]).

init(_) ->
    {ok, #state{}}.

%% Error catch all
handle_call(_Request, _From, State) ->
    {stop, unknown_request, State}.

%% Error catch all
handle_cast(_Requst, State) ->
    {stop, unknown_request, State}.
