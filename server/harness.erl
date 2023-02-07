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
            {GameID, Response} = forward_request(Parent, Request),
            OutBin = encode(GameID, Response),
            gen_tcp:send(Sock, OutBin),
            loop(Parent, LSock, Sock);
        {tcp_closed, Port} ->
            gen_server:cast(Parent, tcp_closed),
            {ok, connection_closed};
        {error, closed} ->
            gen_tcp:close(LSock),
            gen_tcp:close(Sock),
            gen_server:cast(Parent, tcp_closed),
            {ok, connection_closed}
    end.

forward_request(Parent, {GameID, _} = Request) ->
    gen_server:cast(Parent, Request),
    receive
        {new_game, NewGameID, Config} ->
            {NewGameID, {new_game, Config}};
        {response, Response} ->
            {GameID, Response}
    end.

encode_gid(GameID) ->
    integer_to_list(GameID) ++ ":".

%% Encoding and decoding towards the game harness
encode(GameID, {new_game, Config}) ->
    list_to_binary(encode_gid(GameID) ++ Config);
encode(GameID, {ordered, Ordering}) ->
    list_to_binary(encode_gid(GameID) 
                   ++ "["
                   ++ lists:join(",",
                                 lists:map(fun integer_to_list/1,
                                           Ordering))
                   ++ "]");
encode(GameID, {targeted, Trgt}) ->
    list_to_binary(encode_gid(GameID) ++ integer_to_list(Trgt));
encode(GameID, {randomized, Rand}) ->
    list_to_binary(encode_gid(GameID) ++ integer_to_list(Rand));
encode(GameID, {info_conf, ok}) ->
    list_to_binary(encode_gid(GameID) ++ "ok").

decode(Bin) when is_binary(Bin) ->
    decode(binary_to_list(Bin));
decode("connected") ->
    {undefined, started};
decode(GameRequest) when is_list(GameRequest) ->
    {GameID, ":" ++ StrRequest} = extract_number(GameRequest),
    case StrRequest of
        "ordr: " ++ Token0 ->
            {GameID, {ordr, decode_triggers(Token0, [], 1)}};
        "trgt: " ++ Token0 ->
            {Hdr, Token1} = decode_header(Token0, 0),
            {GameID, {trgt, Hdr, decode_range(Token1, [])}};
        "rand: " ++ Token0 ->
            {_Hdr, Token1} = decode_header(Token0, 0),
            {GameID, {rand, decode_range(Token1, [])}};
        "info: " ++ Token ->
            {GameID, {info, decode_gamestate(Token)}};
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

decode_gamestate(Token) ->
    decode_gamestate(Token, []).

decode_gamestate([], GameState) ->
    GameState;
decode_gamestate(Token0, GameState) ->
    {Zone, ": [" ++ Token1} = extract_zone(Token0),
    {FieldMapsToken, "], " ++ Token2} = extract_fieldmaps(Token1),
    FieldMaps = decode_fieldmaps(FieldMapsToken, []),
    decode_gamestate(Token2, [{Zone, FieldMaps} | GameState]).

extract_zone(Token) ->
    extract_zone(Token, []).

extract_zone([], []) ->
    none;
extract_zone([Cur|Token], Zone) ->
    case Cur == $: of
        true -> {list_to_atom(string:lowercase(lists:reverse(Zone))),
                 [Cur|Token]};
        false -> extract_zone(Token, [Cur | Zone])
    end.

extract_fieldmaps(Token) ->
    extract_fieldmaps(Token, []).

extract_fieldmaps([], []) ->
    none;
extract_fieldmaps([Cur|Token], FieldMapsToken) ->
    case Cur == $] of
        true -> {lists:reverse(FieldMapsToken), [Cur|Token]};
        false -> extract_fieldmaps(Token, [Cur | FieldMapsToken])
    end.

decode_fieldmaps([], FieldMaps) ->
    FieldMaps;
decode_fieldmaps(Token0, FieldMaps) ->
    {CardID, " {" ++ Token1} = extract_number(Token0),
    {FieldMapToken, "} " ++ Token2} = extract_fieldmap(Token1),
    FieldMap = decode_fieldmap(FieldMapToken, []),
    decode_fieldmaps(Token2, [{CardID, FieldMap} | FieldMaps]).

extract_fieldmap(Token) ->
    extract_fieldmap(Token, []).

extract_fieldmap([], []) ->
    none;
extract_fieldmap([Cur|Token], FieldMapToken) ->
    case Cur == $} of
        true -> {lists:reverse(FieldMapToken), [Cur|Token]};
        false -> extract_fieldmap(Token, [Cur | FieldMapToken])
    end.

decode_fieldmap([], FieldMap) ->
    FieldMap;
decode_fieldmap(Token0, FieldMap) ->
    {Field, " => " ++ Token1} = extract_field(Token0),
    {Value, ", " ++ Token2} = extract_number(Token1),
    decode_fieldmap(Token2, [{Field, Value} | FieldMap]).

extract_field(Token) ->
    extract_field(Token, []).

extract_field([], []) ->
    none;
extract_field([Cur|Token], Field) ->
    case Cur == 32 of
        true -> {list_to_atom(string:lowercase(lists:reverse(Field))),
                 [Cur | Token]};
        false -> extract_field(Token, [Cur | Field])
    end.
