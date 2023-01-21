-module(game_harness).

-export([start_link/1, init/1]).

start_link(Port) ->
    {ok, spawn_link(?MODULE, init, [{Port, self()}])}.

init({Port, Parent}) ->
    register(harness, self()),
    {ok, LSock} = gen_tcp:listen(Port, []),
    {ok, Sock} = gen_tcp:accept(LSock),
    loop(Parent, LSock, Sock).

loop(Parent, LSock, Sock) ->
    receive
        {tcp, Sock, Bin} ->
            Request = game_objects:decode(Bin),
            Response = forward_request(Parent, Request),
            OutBin = game_objects:encode(Response),
            gen_tcp:send(Sock, OutBin),
            loop(Parent, LSock, Sock);
        {error, closed} ->
            gen_tcp:close(LSock),
            gen_tcp:close(Sock),
            gen_server:cast(Parent, tcp_closed),
            {ok, connection_closed}
    end.

forward_request(Parent, Req) ->
    gen_server:call(Parent, Req).
