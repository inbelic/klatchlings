-module(server).

-include("../include/command.hrl").

-export([run/0]).

run() ->
    {ok, Harness} = harness:start_link(),
    {ok, _GQueue} = gqueue:start_link(),
    ok = gen_server:call(gqueue, {give_harness, Harness}),
    {ok, _ClientSup} = client_conn_mgr:start(?PORT),
    ok.
