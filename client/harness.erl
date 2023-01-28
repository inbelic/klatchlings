-module(harness).

-include("header.hrl").

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
            Request = decode(Bin),
            io:format("<~p> ~p: ~p~n", [harness, f_req, Request]),
            Response = forward_request(Parent, Request),
            io:format("<~p> ~p: ~p~n", [harness, r_rep, Response]),
            OutBin = encode(Response),
            gen_tcp:send(Sock, OutBin),
            loop(Parent, LSock, Sock);
        {error, closed} ->
            gen_tcp:close(LSock),
            gen_tcp:close(Sock),
            gen_server:cast(Parent, tcp_closed),
            {ok, connection_closed}
    end.

forward_request(Parent, Req) ->
    gen_server:cast(Parent, Req),
    receive
        {response, Response} ->
            Response
    end.


%% Encoding and decoding towards the game harness
encode({ordered, Ordering}) ->
    list_to_binary("["
                   ++ lists:join(",",
                                 lists:map(fun integer_to_list/1,
                                           Ordering))
                   ++ "]");
encode({targeted, Trgt}) ->
    list_to_binary(integer_to_list(Trgt));
encode({randomized, Rand}) ->
    list_to_binary(integer_to_list(Rand));
encode({info_conf, ok}) ->
    <<"ok">>.

decode(Bin) when is_binary(Bin) ->
    decode(binary_to_list(Bin));
decode(StrRequest) when is_list(StrRequest) ->
    case StrRequest of
        "ordr: " ++ Token0 ->
            {ordr, decode_triggers(Token0, [], 1)};
        "trgt: " ++ Token0 ->
            {Hdr, Token1} = decode_header(Token0, 0),
            {trgt, Hdr, decode_range(Token1, [])};
        "rand: " ++ Token0 ->
            {Hdr, Token1} = decode_header(Token0, 0),
            {rand, Hdr, decode_range(Token1, [])};
        "info: " ++ GameState ->
            {info, GameState};
        _ ->
            no_parse
    end.

decode_header(Token, Posn) ->
    case Token of
        "sys(" ++ Token0 ->
            {CardID, ":" ++ Token1} = extract_number(Token0),
            {AbltyID, ")" ++ Token2} = extract_number(Token1),
            {#header{system = 0, position = Posn,
                     cardID = CardID, abilityID = AbltyID}, Token2};
        "ply(" ++ Token0 ->
            {CardID, ":" ++ Token1} = extract_number(Token0, ""),
            {AbltyID, ")" ++ Token2} = extract_number(Token1, ""),
            {#header{system = 1, position = Posn,
                     cardID = CardID, abilityID = AbltyID}, Token2}
    end.

extract_number(Token) ->
    extract_number(Token, []).

extract_number([], []) ->
    none;
extract_number([Cur|Token], Num) ->
    case is_digit(Cur) of
        true -> extract_number(Token, [Cur | Num]);
        false -> {list_to_integer(Num), [Cur | Token]}
    end.

is_digit(D) ->
    D =< $9 andalso D >= $0.

decode_triggers([], Acc, _Posn) ->
    lists:reverse(Acc);
decode_triggers(Token, Acc, Posn) ->
    case Token of
        "[" ++ Token0 ->
            decode_triggers(Token0, Acc, Posn);
        "]" ++ Token0 ->
            decode_triggers(Token0, Acc, Posn);
        "," ++ Token0 ->
            decode_triggers(Token0, Acc, Posn);
        " " ++ Token0 ->
            decode_triggers(Token0, Acc, Posn);
        Token0 ->
            {Hdr, Token1} = decode_header(Token0, Posn),
            Acc1 = [Hdr | Acc],
            decode_triggers(Token1, Acc1, Posn + 1)
    end.

decode_range([], Acc) ->
    lists:reverse(Acc);
decode_range(Token, Acc) ->
    case Token of
        "[" ++ Token0 ->
            decode_range(Token0, Acc);
        "]" ++ Token0 ->
            decode_range(Token0, Acc);
        "," ++ Token0 ->
            decode_range(Token0, Acc);
        " " ++ Token0 ->
            decode_range(Token0, Acc);
        Token0 ->
            {CardID, Token1} = extract_number(Token0),
            Acc1 = [CardID | Acc],
            decode_range(Token1, Acc1)
    end.
