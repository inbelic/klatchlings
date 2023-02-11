-module(encode).

-include("../include/command.hrl").

-export([extract_num/1, extract_tuple/2, extract_owner/1,
        extract_header/1, extract_liable/1]).
-export([decode_enum/1, decode_request/1, decode_input/1, decode_range/2]).
-export([encode_order/1, encode_header/1, encode_range/2]).

extract_header(Token0) ->
    try
        {Liable, Token1} = extract_liable(Token0),
        {CID, ":" ++ Token2} = extract_num(Token1),
        {AID, ")" ++ Token3} = extract_num(Token2),
        {{Liable, CID, AID}, Token3}
    catch
        _:_ ->
            none
    end.

extract_liable("s(" ++ Token) ->
        {system, Token};
extract_liable("p(" ++ Token) ->
        {player, Token};
extract_liable("t(" ++ Token) ->
        {targeted, Token};
extract_liable(_Token) ->
    none.

extract_num("-" ++ Token) ->
    case extract_num(Token) of
        none ->
            none;
        {Num, Token1} ->
            {-Num, Token1}
    end;
extract_num(Token) ->
    extract_num(Token, []).

extract_num([], []) ->
    none;
extract_num([], Num) ->
    {list_to_integer(lists:reverse(Num)), []};
extract_num([Cur|Token], Num) ->
    case is_digit(Cur) of
        true -> extract_num(Token, [Cur | Num]);
        false when Num == [] ->
            none;
        false ->
            {list_to_integer(lists:reverse(Num)), [Cur | Token]}
    end.

is_digit(D) ->
    D =< $9 andalso D >= $0.

extract_tuple(Fun1, Fun2) ->
    fun(Token) ->
        try
            {Item1, Token1} = Fun1(Token),
            {Item2, Token2} = Fun2(Token1),
            {{Item1, Item2}, Token2}
        catch
            _:_ ->
                none
        end
    end.

extract_owner("P1:" ++ Token) ->
    {p1, Token};
extract_owner("P2:" ++ Token) ->
    {p2, Token};
extract_owner("RS:" ++ Token) ->
    {rules, Token};
extract_owner(_) ->
    none.


decode_enum(?ORDER) ->
    order;
decode_enum(?TARGET) ->
    target;
decode_enum(?PLAY) ->
    play;
decode_enum(?QUIT) ->
    quit;
decode_enum(_) ->
    no_cmd.

decode_request(Token) when is_list(Token) ->
    case Token of
        "view: " ++ Args ->
            {view, Args};
        "ordr: " ++ Args ->
            {ordr, Args};
        "trgt: " ++ Args ->
            {trgt, Args};
        "rand: " ++ Args ->
            {rand, Args};
        _ ->
            bad_req
    end;
decode_request(<<Cmd:6/binary, Args/binary>>) ->
    case Cmd of
        <<"view: ">> ->
            {view, binary_to_list(Args)};
        <<"ordr: ">> ->
            {ordr, binary_to_list(Args)};
        <<"trgt: ">> ->
            {trgt, binary_to_list(Args)};
        <<"rand: ">> ->
            {rand, binary_to_list(Args)};
        _ ->
            bad_req
    end.
decode_range(Fun, Token) ->
    case Token of
        "[" ++ Token0 ->
            decode_range(Fun, Token0, []);
        _ ->
            none
    end.

decode_input(Input) ->
    case encode:extract_num(Input) of
        none ->
            bad_cmd;
        {CmdStr, Args} ->
            case encode:decode_enum(CmdStr) of
                no_cmd ->
                    bad_cmd;
                Cmd ->
                    {Cmd, Args}
            end
    end.

decode_range(_, [], _) ->
    none;
decode_range(Fun, Token, Acc) ->
    case Token of
        "]" ++ Token0 ->
            {lists:reverse(Acc), Token0};
        "," ++ Token0 ->
            decode_range(Fun, Token0, Acc);
        " " ++ Token0 ->
            decode_range(Fun, Token0, Acc);
        Token0 ->
            try
                {Item, Token1} = Fun(Token0),
                Acc1 = [Item | Acc],
                decode_range(Fun, Token1, Acc1)
            catch
                _:_ ->
                    none
            end
    end.

encode_order(OrderStr) ->
    encode_range(fun encode_header/1, OrderStr).

encode_header({Liable, CID, AID} = _Hdr) ->
    LStr = case Liable of
               system ->
                   "s(";
               player ->
                   "p(";
               targeted ->
                   "t("
           end,
    LStr ++ integer_to_list(CID) ++ ":" ++ integer_to_list(AID) ++ ")".

encode_range(Fun, Range) ->
    "[" ++ lists:join(", ", lists:map(Fun, Range)) ++ "]".
