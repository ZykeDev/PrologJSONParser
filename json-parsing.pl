%%%% -*- Mode: Prolog -*-

%%%% Author: Marco Vincenzi
%%%% Matricola: 795694

%%%% json-parsing.pl


% Parsing
json_parse(JSONString, json_obj(O)) :-
    % Convert the string into a list of charcodes
    string_to_list(JSONString, [H | R]),

    parse_value([H | R], json_obj(O), _Rest).

json_parse([H | R], json_obj(O)) :-
    parse_value([H | R], json_obj(O), _Rest).


parse(In, json_obj([(K, V) | KVs]), Rest) :-
    remove_whitespaces(In, HR),
    parse_pair(HR, K, V, VRest),

    % if the next char is a comma , continue parsing.
    is_comma_char(VRest, CRest),
    parse_members(CRest, KVs, Rest).


parse(In, json_obj([(K, V)]), Rest) :-
    remove_whitespaces(In, HR),
    parse_pair(HR, K, V, VRest),

    % if it's a } exit
    is_closed_obj_char(VRest, Rest).


parse_members(In, (K, V), Rest) :-
    remove_whitespaces(In, HR),
    parse_pair(HR, K, V, VRest),

    % if it's a } exit
    is_closed_obj_char(VRest, Rest).


parse_members(In, [(K, V) | KVs], Rest) :-
    remove_whitespaces(In, HR),
    parse_pair(HR, K, V, VRest),

    % if the next char is a comma , continue parsing.
    is_comma_char(VRest, CRest),
    parse_members(CRest, KVs, Rest).


parse_pair(HR, K, V, Rest) :-
     % Parse the key
    parse_key(HR, K, KRest),

    % Check that there is a colon : (removes whitespaces)
    is_colon_char(KRest, SCRest),

    % Parse the corresponding value
    parse_value(SCRest, V, Rest).



% String -> Rest, Key
parse_key([H | R], K, Rest) :-
    is_DQ_char(H), !,
    parse_string_DQ(R, Kchars, Rest),
    string_to_list(K, Kchars).

% String -> Rest, Key
parse_key([H | R], K, Rest) :-
    is_SQ_char(H), !,
    parse_string_SQ(R, Kchars, Rest),
    string_to_list(K, Kchars).

% Undefined value
parse_value([U | [N | [D | [E | [F | [I | [N | [E | [D | _]]]]]]]]], _, _) :-
    char_code("u", U),
    char_code("n", N),
    char_code("d", D),
    char_code("e", E),
    char_code("f", F),
    char_code("i", I),
    !,
    fail.

% String Value (DQ or SQ)
parse_value([H | R], V, Rest) :-
    is_DQ_char(H),
    !,
    parse_string_DQ(R, VChars, Rest),
    string_to_list(V, VChars),
    is_value(V).

parse_value([H | R], V, Rest) :-
    is_SQ_char(H),
    !,
    parse_string_SQ(R, VChars, Rest),
    string_to_list(V, VChars),
    is_value(V).

% Number Value
parse_value([H | R], V, Rest) :-
    atom_char(A, H),
    atom_number(A, N),
    number(N),

    parse_number([H | R], V, Rest),
    is_value(V).

% Negative Number Value
parse_value([HD | [HN | R]], V, Rest) :-
    is_dash_char(HD),
    !,
    atom_char(A, HN),
    atom_number(A, N),
    number(N),

    parse_number([HD | [HN | R]], V, Rest),
    is_value(V).

% Explicitly Positive Number Value
parse_value([HP | [HN | R]], V, Rest) :-
    is_plus_char(HP),
    !,
    atom_char(A, HN),
    atom_number(A, N),
    number(N),

    parse_number([HN | R], V, Rest),
    is_value(V).


% Array Value
parse_value([H | R], json_array(V), Rest) :-
    is_opened_arr_char(H),

    parse_array(R, V, Rest).

% Empty Arrays []
parse_value(O, json_array([]), Rest) :-
    remove_whitespaces(O, [H | R]),
    is_opened_arr_char(H),
    remove_whitespaces(R, [RH | Rest]),
    is_closed_arr_char(RH).

% JSON Value
parse_value(O, V, Rest) :-
    remove_whitespaces(O, [H | R]),
    is_opened_obj_char(H),

    parse(R, V, Rest).

% Empty JSON objects {}
parse_value(O, json_obj([]), Rest) :-
    remove_whitespaces(O, [H | R]),
    is_opened_obj_char(H),
    remove_whitespaces(R, [RH | Rest]),
    is_closed_obj_char(RH).


parse_array(S, [V | Vs], Rest) :-
    remove_whitespaces(S, NWS),
    parse_value(NWS, V, VR),

    is_comma_char(VR, NWVR),
    !,
    parse_array(NWVR, Vs, Rest).


parse_array(S, [V], Rest) :-
    remove_whitespaces(S, NWS),
    parse_value(NWS, V, VR),

    % Check for ]
    remove_whitespaces(VR, NWVR),
    is_closed_arr_char(NWVR, Rest).



% DQ
parse_string_DQ([H | R], [H | V], Rest) :-
    is_not_DQ(H),
    !,
    parse_string_DQ(R, V, Rest).

% String end
parse_string_DQ([H | Rest], [], Rest) :-
    is_DQ_char(H).

% SQ
parse_string_SQ([H | R], [H | V], Rest) :-
    is_not_SQ(H),
    !,
    parse_string_SQ(R, V, Rest).

% String end
parse_string_SQ([H | Rest], [], Rest) :-
    is_SQ_char(H).



is_digit(C) :- char_type(C, digit).
is_string([C | Cs]) :- atom(C), char_type(C, alpha), is_string(Cs).
is_string(S) :- string(S).
is_whitespace(C) :- char_type(C, white).
is_punctuation(C) :- char_type(C, punctuation).
not_is_digit(C) :- is_alpha(C).
not_is_digit(C) :- is_whitespace(C).

json_obj([]).
json_obj([M | Ms]) :-
    is_member(M),
    json_obj(Ms).

json_array([]).
json_array([E | Es]) :-
    is_element(E),
    json_array(Es).

json_array([M | Ms]) :-
    is_element(M),
    json_array(Ms).


is_object(X) :- json_obj(X).
is_object(X) :- json_array(X).

is_member([]).
is_member(X) :- is_pair(X).
is_member([KV | M]) :- is_pair(KV), is_member(M).

is_pair((K, V)) :- is_string(K), is_value(V).

is_value(X) :- is_string(X).
is_value(X) :- number(X).
is_value(X) :- is_object(X).

is_element([]).
is_element([E | Es]) :- is_value(E), is_element(Es).


is_whitespace_char(C) :- char_code(X, C), char_type(X, white).
is_whitespace_char(10).
is_not_whitespace_char(C) :- C \= 20, C \= 10, C \= 9.
is_DQ_char(34).
is_SQ_char(39).
is_not_DQ(C) :- C \= 34.
is_not_SQ(C) :- C \= 39.
is_colon_char(58).
is_comma_char(44).
is_dot_char(46).
is_dot_char([C | R], R) :- is_dot_char(C).
is_dash_char(45).
is_plus_char(43).
is_opened_obj_char(123).
is_closed_obj_char(125).
is_opened_arr_char(91).
is_closed_arr_char(93).
is_closed_arr_char([H | R], R) :- is_closed_arr_char(H).



% Comma
is_comma_char([H | R], Rest) :-
    is_whitespace_char(H), !,
    is_comma_char(R, Rest).

is_comma_char([H | R], Rest) :-
    is_comma_char(H), !,
    remove_whitespaces(R, Rest).

% colon
is_colon_char([], _) :- fail.
is_colon_char(In, Rest) :-
    remove_whitespaces(In, [RH | RR]),
    is_colon_char(RH),
    remove_whitespaces(RR, Rest).

% Opened {
is_opened_obj_char([H | R], Rest) :-
    is_whitespace_char(H), !,
    is_opened_obj_char(R, Rest).

is_opened_obj_char([H | R], Rest) :-
    is_opened_obj_char(H), !,
    remove_whitespaces(R, Rest).

% Closed }
is_closed_obj_char([H | R], Rest) :-
    is_whitespace_char(H), !,
    is_closed_obj_char(R, Rest).

is_closed_obj_char([H | R], R) :-
    is_closed_obj_char(H).


% Remove whitespaces
remove_whitespaces([], []).

remove_whitespaces([H | R], Rest) :-
    is_whitespace_char(H),
    remove_whitespaces(R, Rest).

remove_whitespaces([H | R], [H | R]) :-
    is_not_whitespace_char(H).


% For negative numbers
parse_number([H | R], V, Rest) :-
    is_dash_char(H),
    !,
    parse_int(R, Vn, Rest),
    V is 0 - Vn.

% For explicitly positve numbers
parse_number([H | R], V, Rest) :-
    is_plus_char(H),
    !,
    parse_int(R, V, Rest).

% For positive numbers
parse_number([H | R], V, Rest) :-
    parse_int([H | R], V, Rest).


% V is a number
parse_int([H | R], V, Rest) :-
    parse_digits([H | R], Vr, Rest),
    atom_chars(Va, Vr),
    atom_number(Va, V),
    number(V).


% eof -> fail
parse_digits([], _, _) :- fail.

% ] -> end
parse_digits([DH | DR], [], [DH | DR]) :-
    is_closed_arr_char(DH).

% } -> end
parse_digits([DH | DR], [], [DH | DR]) :-
    is_closed_obj_char(DH).

% ws -> end
parse_digits([DH | DR], [], DR) :-
    is_whitespace_char(DH).

% Comma -> end
parse_digits([DH | DR], [], [DH | DR]) :-
    is_comma_char(DH).

% Dot -> is a float
parse_digits([DH | DR], [DH | Dec], Rest) :-
    is_dot_char(DH),
    !,
    parse_digits(DR, Dec, Rest).

% Digit -> next
parse_digits([DH | DR], V, R) :-
    code_type(DH, digit),
    !,
    parse_digits(DR, Num, R),
    append([DH], Num, V).











% If the "Fields" field is empty, return the entire obj.
json_get(V, [], V).


% json_obj -> [(K, V)].
json_get(json_obj([(K, V) | KVs]), Fields, Res) :-
    json_get([(K, V) | KVs], Fields, Res).



% If the Key = FH
json_get([(K, V) | _], [FH | FR], Res) :-
    % If the Key is equal to the Field.
    atom_string(AK, FH),
    atom_string(AK, K),
    \+ number(FR),
    % Go deeper in the Value with the next Field.
    json_get(V, FR, Res).

% K =/= FH
json_get([(K, _V) | KVs], [FH | FR], Res) :-
    % If the Key is equal to the Field
    atom_string(AK, FH),
    \+ atom_string(AK, K),
    % Go deeper in the Value with the next Field
    json_get(KVs, [FH | FR], Res).


% K = F
json_get([(K, V) | _], F, V) :-
    % If the Key is equal to the (last) Field.
    \+ number(F),
    \+ is_list(F),
    atom_string(AK, F),
    atom_string(AK, K).

% K =/= F
json_get([(K, _V) | KVs], F, Res) :-
    \+ number(F),
    \+ is_list(F),
    atom_string(AK, F),
    \+ atom_string(AK, K),
    json_get(KVs, F, Res).


% K = FH
% Where FH is an Array and FIndex is its Index.
json_get([(K, json_array(V)) | _KVs], [FH | [FIndex | FR]], Res) :-
    atom_string(AK, FH),
    atom_string(AK, K),

    number(FIndex),
    FIndex >= 0,

    nth0(FIndex, V, VNext),

    FR \= [],
    !,
    json_get(VNext, FR, Res).

% K = FH
% Where FH is an Array and FIndex is its Index
json_get([(K, json_array(V)) | _KVs], [FH | [FIndex | []]], Res) :-
    atom_string(AK, FH),
    atom_string(AK, K),

    number(FIndex),
    FIndex >= 0,
    !,
    nth0(FIndex, V, Res).




% K = FH
json_get((K, V), [FH | FR], Res) :-
    % If the Key is equal to the Field.
    atom_string(AK, FH),
    atom_string(AK, K),
    \+ number(FR),
    % Go deeper in the Value with the next Field.
    json_get(V, FR, Res).


% K = F
json_get((K, V), F, V) :-
    % If the Key is equal to the (last) Field.
    \+ number(F),
    \+ is_list(F),
    atom_string(AK, F),
    atom_string(AK, K).


% K = FH
% Where FH is an Array and FIndex is its Index
json_get((K, json_array(V)), [FH | [FIndex | FR]], Res) :-
    atom_string(AK, FH),
    atom_string(AK, K),

    number(FIndex),
    FIndex >= 0,

    nth0(FIndex, V, VNext),

    FR \= [],
    !,
    json_get(VNext, FR, Res).

% K = FH
% Where FH is an Array and FIndex is its Index
json_get((K, json_array(V)), [FH | [FIndex | []]], Res) :-
    atom_string(AK, FH),
    atom_string(AK, K),

    number(FIndex),
    FIndex >= 0,

    nth0(FIndex, V, Res).











% File I/O
% Loads a text file from FileName and parses it.
json_load(FileName, JSON) :-
   open(FileName, read, In),
   read_stream_to_codes(In, Str),
   close(In),
   json_parse(Str, JSON).

% Saves a JSON object in FileName.
json_write(JSON, FileName) :-
    convert_to_JSON(JSON, JSONString),
    open(FileName, write, Out),
    write(Out, JSONString),
    close(Out).


convert_to_JSON(json_obj([]), '{}').

convert_to_JSON(json_obj([(K, V) | KVs]), JSONString) :-
    KVs \= [],
    !,
    convert_pair((K, V), JM),
    convert_pair(KVs, JSONRest),

    atom_concat("{", JM, JS),
    atom_concat(JS, ", ", JS1),
    atom_concat(JS1, JSONRest, JS2),
    atom_concat(JS2, "}", JSONString).

convert_to_JSON(json_obj([(K, V) | []]), JSONString) :-
    convert_pair((K, V), JM),
    atom_concat("{", JM, JS),
    atom_concat(JS, "}", JSONString).

convert_to_JSON(V, V) :-
    number(V).

convert_to_JSON(V, VS) :-
    is_string(V),
    atom_concat('"', V, VS1),
    atom_concat(VS1, '"', VS).

convert_to_JSON(json_array([V | Vs]), VS) :-
    atom_concat("", '[', KH),
    concat_array_elements(KH, [V | Vs], FinalV),
    atom_concat(FinalV, "]", VS).


convert_pair((K, V), KV) :-
    is_value(V),
    convert_to_JSON(V, JV),
    concat_pair(K, JV, KV).

convert_pair((K, json_obj(V)), KV) :-
    convert_to_JSON(V, JV),
    concat_pair(K, JV, KV).

convert_pair((K, json_array(V)), KV) :-
    concat_array(K, V, KV).

convert_pair([(K, V) | []], KV) :-
    convert_pair((K, V), KV).

convert_pair([(K, V) | KVs], KV) :-
    convert_pair((K, V), KV1),
    convert_pair(KVs, KV2),
    atom_concat(KV1, ", ", KVC),
    atom_concat(KVC, KV2, KV).


% Concatenates a pair (k : v).
concat_pair(K, JV, KV) :-
    atom_concat('"', K, K1),
    atom_concat(K1, '" : ', K2),
    atom_concat(K2, JV, KV).


concat_array(K, JV, KV) :-
    atom_concat('"', K, K1),
    atom_concat(K1, '" : ', K2),
    atom_concat(K2, '[', KH),
    concat_array_elements(KH, JV, FinalV),
    atom_concat(FinalV, "]", KV).

% Concatenates array elements with commas.
concat_array_elements(S, V, V1) :-
    convert_to_JSON(V, VJ),
    atom_concat(S, VJ, V1).

concat_array_elements(S, [VH | VR], V) :-
    convert_to_JSON(VH, VHJ),
    atom_concat(S, VHJ, V1),
    atom_concat(V1, ", ", V2),
    concat_array_elements(V2, VR, V).













% end of file -- json-parsing.pl
