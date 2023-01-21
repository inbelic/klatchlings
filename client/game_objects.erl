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
enocde(info) ->
    <<"ok">>.

decode(Bin) when is_binary(Bin) ->
    decode(binary_to_list(Bin));
decode(StrRequest) when is_list(StrRequest) ->
    case StrRequest of
        "ordr: " ++ TrgToken ->
            {ordr, decode_triggers(TrgToken, [], 1)};
        "trgt: " ++ Range ->
            {trgt, decode_range(Range, [])};
        "rand: " ++ Range ->
            {rand, decode_range(Range, [])};
        "info: " ++ GameState ->
            {info, GameState};
        _ ->
            no_parse
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
            {CardID, Token1} = extract_number(Token0, ""),
            Acc1 = [CardID | Acc],
            decode_range(Token1, Acc1)
    end.

decode_triggers([], Acc, Posn) ->
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
        "sys(" ++ Token0 ->
            {_CardID, ":" ++ Token1} = extract_number(Token0, ""),
            {_AbltyID, Token2} = extract_number(Token1, ""),
            Acc1 = [{system, Posn} | Acc],
            decode_triggers(Token2, Acc1, Posn + 1);
        "ply(" ++ Token0 ->
            {CardID, ":" ++ Token1} = extract_number(Token0, ""),
            {AbltyID, Token2} = extract_number(Token1, ""),
            Acc1 = [{player, Posn, CardID, AbltyID} | Acc],
            decode_triggers(Token2, Acc1, Posn + 1)
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
