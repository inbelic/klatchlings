-module(game_harness).

-export([start_link/1, init/1]).

start_link(Port) ->
    spawn_link(?MODULE, init, [{Port, self()}]).

init({Port, Parent}) ->
    register(harness, self()),
    {ok, LSock} = gen_tcp:listen(Port, []),
    {ok, Sock} = gen_tcp:accept(LSock),
    loop(Parent, LSock, Sock).

loop(Parent, LSock, Sock) ->
    receive
        {tcp, Sock, Bin} ->
            Request = game_objects:decode(Bin),
            Response = handle_request(Parent, Request),
            OutBin = game_objects:encode(Response),
            gen_tcp:send(Sock, OutBin),
            loop(Parent, LSock, Sock);
        {error, closed} ->
            gen_tcp:close(LSock),
            gen_tcp:close(Sock),
            {ok, connection_closed}
    end.

handle_request(Parent, {ordr, Ordering}) ->
    {System, Player} = lists:partition(fun({system, _}) -> true;
                                          (_) -> false
                                       end, Ordering),
    SystemOrder = lists:map(fun({system, Posn}) -> Posn end, System),
    case Player of
        [] -> {ordr, SystemOrder};
        _ ->
            Parent ! {ordr, Player},
            receive
                {ordr, PlayerOrder} ->
                    {ordr, SystemOrder ++ PlayerOrder}
            end
    end;
handle_request(Parent, {trgt, _} = Req) ->
    Parent ! Req,
    receive
        Response ->
            Response
    end;
handle_request(Parent, {rand, _} = Req) ->
    Parent ! Req,
    receive
        Response ->
            Response
    end;
handle_request(Parent, {info, _} = Req) ->
    Parent ! Req,
    receive
        Response ->
            Response
    end.
