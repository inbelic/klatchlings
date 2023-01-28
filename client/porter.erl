-module(porter).
-export([start/0, init/0]).
-export([inquire/1, request/2]).

-include("header.hrl").

start() ->
    Port = spawn(?MODULE, init, []),
    {ok, Port}.

init() ->
    register(porter, self()),
    process_flag(trap_exit, true),
    Port =  open_port({spawn, "../../ui/build/klatchlings-ui"}, [{packet, 2}]),
    loop(Port).

%% Will respond directly
inquire(Msg) ->
    porter ! {inq, self(), Msg},
    receive
        {porter, Result} ->
            Result
    end.

%% Will respond with a cast
request(From, Msg) ->
    porter ! {req, From, Msg}.

loop(Port) ->
    receive
        {inq, Caller, Msg} ->
            Response = handle_request(Port, Msg),
            Caller ! {porter, Response},
            loop(Port);
        {req, From, Msg} ->
            Response = handle_request(Port, Msg),
            gen_server:cast(From, {porter, Response}),
            loop(Port)
    end.

handle_request(Port, {ordr, Hdrs}) ->
    send_msg(Port, {ordr, length(Hdrs)}),
    confirm_okay(Port),
    Fun = fun(Hdr) -> send_header(Port, Hdr) end,
    lists:foreach(Fun, Hdrs),
    Ordering = recv_order(Port),
    {ordered, Ordering};
handle_request(Port, {trgt, Hdr, Range}) ->
    send_msg(Port, {trgt, length(Range)}),
    confirm_okay(Port),
    send_header(Port, Hdr),
    Fun = fun(TCID) -> ok = send_cid(Port, TCID) end,
    lists:foreach(Fun, Range),
    TCID = recv_trgt(Port),
    {targeted, TCID}.

send_msg(Port, Msg) ->
    Port ! {self(), {command, encode(Msg)}}.

recv_msg(Port) ->
    receive
        {Port, {data, Data}} ->
            decode(Data)
    end.

send_okay(Port) ->
    send_msg(Port, {ok, ok}).

confirm_okay(Port) ->
    recv_msg(Port) == ok.

send_header(Port, #header{} = Hdr) ->
    send_msg(Port, {Hdr#header.system, Hdr#header.position}),
    confirm_okay(Port),
    send_msg(Port, {Hdr#header.cardID, Hdr#header.abilityID}),
    confirm_okay(Port),
    ok.

send_cid(Port, TCID) ->
    send_msg(Port, {value, TCID}),
    confirm_okay(Port),
    ok.

recv_order(Port) ->
    recv_order(Port, []).

recv_order(Port, Acc) ->
    Incoming = recv_msg(Port),
    send_okay(Port),
    case Incoming of
        eol -> Acc;
        {value, Val} -> recv_order(Port, [Val | Acc])
    end.

recv_trgt(Port) ->
    Incoming = recv_msg(Port),
    send_okay(Port),
    case Incoming of
        {value, Val} -> Val
    end.

%% Encoding and decoding towards the UI port
encode({value, Val}) -> [Val];
encode({ordr, Amt}) -> [?CMD_ORDER, Amt];
encode({trgt, Amt}) -> [?CMD_TARGET, Amt];
encode({true, Y}) -> [1, Y];
encode({false, Y}) -> [0, Y];
encode({X, Y}) when is_integer(X) andalso is_integer(Y) -> [X, Y];
encode({ok, ok}) -> [?OKAY, ?OKAY];
encode({X, Y}) -> [X, Y].

decode([?OKAY, ?OKAY]) -> ok;
decode([?VALUE, Val]) -> {value, Val};
decode([?EOL]) -> eol;
decode([?OKAY, _]) -> not_ok;
decode([Int]) -> Int.
