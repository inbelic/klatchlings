-module(server).

-include("../include/command.hrl").

-export([run/0]).

run() ->
    {ok, ClientSup} = client_conn_mgr:start(?PORT, self()),
    {ok, ClientSup}.
