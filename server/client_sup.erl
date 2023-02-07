-module(client_sup).

-behaviour(supervisor).

%% supervisor exports
-export([init/1]).
-export([start_link/2, start_client/0]).

start_link(Port, Server) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, Server]).

init([Port, Server]) ->
    {ok, ListenSocket}
        = gen_tcp:listen(Port,
                         [binary, {active, once}, {packet, 0}]),
    spawn_link(fun empty_listeners/0),
    {ok, {{simple_one_for_one, 60, 3600},
          [{socket,
            {client, start_link,
             [{Server, ListenSocket}]},
             temporary, 1000, worker, [client]}
            ]}}.

start_client() ->
  supervisor:start_child(?MODULE, []).

empty_listeners() ->
  [start_client() || _ <- lists:seq(1,20)],
  ok.
