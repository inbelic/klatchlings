-module(game_objects).

-export([encode/1, decode/1]).

encode({ordr, Ordering}) ->
    list_to_binary("["
                   ++ lists:join(",",
                                 lists:map(fun integer_to_list/1,
                                           Ordering))
                   ++ "]");
encode({trgt, Trgt}) ->
    list_to_binary(integer_to_list(Trgt));
encode({rand, Rand}) ->
    list_to_binary(integer_to_list(Rand));
encode({info, ok}) ->
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
            {{system, Posn, CardID, AbltyID}, Token2};
        "ply(" ++ Token0 ->
            {CardID, ":" ++ Token1} = extract_number(Token0, ""),
            {AbltyID, Token2} = extract_number(Token1, ""),
            {{player, Posn, CardID, AbltyID}, Token2}
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
        ")" ++ Token0 ->
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
        ")" ++ Token0 ->
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
