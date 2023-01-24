-module(port_relayer).

%% server esque exports
-export([start_link/1, init/2, stop/1]).

%% user exports
-export([request/1]).

-define(ORDER   , 0).
-define(TARGET  , 1).
-define(HEADER  , 2).
-define(INFO    , 3).
-define(ENDLIST , 4).
-define(OKAY    , 5).
-define(ENDHDR  , 6).

request(Req) ->
    relayer ! Req.

stop(Reason) ->
    relayer ! {stop, Reason}.

start_link(Parent) ->
    UIPath = "~/dev/trg/ui/build/trg-ui",
    Port = spawn_link(?MODULE, init, [UIPath, Parent]),
    {ok, Port}.

init(ExtPrg, Parent) ->
    register(relayer, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port, Parent).

loop(Port, Parent) ->
    receive
        {trgt, _Hdr, _Range} = Req ->
            handle_target(Port, Parent, Req),
            loop(Port, Parent);
        {rand, _Hdr, _Range} = Req ->
            handle_random(Port, Parent, Req),
            loop(Port, Parent);
        {ordr, _Ordering} = Req ->
            handle_ordering(Port, Parent, Req),
            loop(Port, Parent);
        {info, _GameState} = Req ->
            handle_info(Port, Parent, Req),
            loop(Port, Parent);
        {stop, _Reason} ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, _Reason} ->
            exit(port_terminated)
    end.

encode(ordr, Len) -> [?ORDER, Len];
encode(hdr, Type) -> [?HEADER, Type];
encode(eol, ok) -> [?ENDLIST, ?OKAY];
encode(eoh, Posn) -> [?ENDHDR, Posn];
encode(CID, AID) when is_integer(CID) andalso is_integer(AID) ->
    [CID, AID].

decode([?OKAY]) -> ok;
decode([Int]) -> Int.

handle_target(_Port, _Parent, {trgt, _Hdr, _Range}) ->
    ok.

handle_random(_Port, _Parent, {rand, _Hdr, _Range}) ->
    ok.

handle_ordering(Port, Parent, {ordr, Ordering}) ->
    ok = send_ordering(Port, Ordering),
    Response = recv_ordering(Port),
    gen_server:cast(Parent, {ordered, Response}).

handle_info(_Port, _Parent, {info, _GameState}) ->
    ok.

send_header(Port, {Type, Posn, CardID, AbilityID} = _Header) ->
    Port ! {self(), {command, encode(hdr, Type)}},
    check_received(Port),
    Port ! {self(), {command, encode(CardID, AbilityID)}},
    check_received(Port),
    Port ! {self(), {command, encode(eoh, Posn)}},
    check_received(Port),
    ok.

check_received(Port) ->
    receive
        {Port, {data, Data}} ->
            ok = decode(Data)
    end.

send_ordering(Port, Ordering) ->
    Port ! {self(), {command, encode(ordr, length(Ordering))}},
    check_received(Port),
    ok = lists:foreach(fun(Hdr) -> send_header(Port, Hdr) end, Ordering),
    Port ! {self(), {command, encode(eol, ok)}},
    check_received(Port),
    ok.

recv_ordering(Port) ->
    recv_ordering(Port, []).

recv_ordering(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            case decode(Data) of
                eol -> Acc;
                {val, Val} ->
                    recv_ordering(Port, [Val | Acc])
            end
    end.
