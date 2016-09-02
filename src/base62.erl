-module(base62).

-export([encode/1, decode/1, digit_to_char/1, char_to_digit/1]).

encode(I) ->
    encode(I, []).

decode(S) ->
    decode(lists:reverse(S), 1, 0).

digit_to_char(D) when D < 10 ->
    $0 + D;
digit_to_char(D) when D < 36 ->
    $0 + 7 + D;
digit_to_char(D) when D < 62 ->
    $0 + 13 + D.

char_to_digit(C) when C < (10 + $0) ->
    C - $0;
char_to_digit(C) when C < (43 + $0) ->
    C - 7 - $0;
char_to_digit(C) when C < (75 + $0) ->
    C - 13 - $0.

encode(I, Acc) when I < 62 ->
    [digit_to_char(I) | Acc];
encode(I, Acc) ->
    I1 = I div 62,
    encode(I1, [digit_to_char(I rem 62)| Acc]).

decode([C | T], N, Acc) ->
    decode(T, N * 62, char_to_digit(C) * N + Acc);
decode([], _N, Acc) ->
    Acc.
