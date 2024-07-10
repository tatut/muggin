:- module(state, [json//1, parse/2, render/2]).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

%%% State namespace
% Support state: namespace attributes with JSON patch operations



%%%%%%%%%%
% Define language of JSON+patching+refs that is like regular JSON but can have patch operations
% with percent character and references with @.
% Examples:
%
% {id: 1, complete: %not}
% matches an object with id 1 and negates its complete value
%
% {items: %+{id: 2, label: "new thing", complete: true}, nextid: %+1}
% Adds a new map to list in items key, also increments nextid by 1
% plus works with both lists (appends value) and numbers (addition).
%
% You can also refer to another element by key via `@key`.
% You can further call methods on values by appending .method
% or .method(arg)
% Multiple method calls can be chained

% Whitespace that is space, tab, newline or carriage return
ws --> (" "|"\t"|"\n"|"\r"), ws.
ws --> [].


json(true) --> "true".
json(false) --> "false".
json(null) --> "null".
json(Num) --> number(Num).
json(List) --> "[", json_items(List), "]".
json(Dict) --> "{", json_keyvals(KVs), "}", { dict_pairs(Dict, json, KVs) }.
json(Str) --> "\"", string_without("\"", StrCs), "\"", { string_codes(Str, StrCs) }.
json(op(Op,Arg)) --> "%", json_binary_op(Op), json(Arg).
json(op(Op)) --> "%", json_unary_op(Op).
json(ref(Path,Methods)) --> "@", string_without(". \n\t\"{}[](),", Cs), { atom_codes(Path, Cs) }, ref_methods(Methods).
ref_methods([]) --> ws.
ref_methods([method(M,Args)|Methods]) --> ".", string_without(". \n\t\"{}[](),", Cs), { atom_codes(M, Cs) },
                                          ref_method_args(Args), ref_methods(Methods).
ref_method_args([]) --> ws.
ref_method_args(Args) --> "(", json_items(Args), ")".

json_items([]) --> ws.
json_items([Item|MoreItems]) --> ws, json(Item), more_json_items(MoreItems).
more_json_items([]) --> ws.
more_json_items(Items) --> ws, ",", ws, json_items(Items).
json_keyvals([]) --> ws.
json_keyvals([Key-Val|MoreKeyVals]) --> ws, string_without(" %.#-()\n\t:", Cs), { atom_codes(Key, Cs) }, ":", ws, json(Val), more_json_keyvals(MoreKeyVals).
more_json_keyvals([]) --> ws.
more_json_keyvals(KeyVals) --> ws, ",", ws, json_keyvals(KeyVals).

json_binary_op(+) --> "+". % add to number or append to list
json_binary_op(=) --> "=". % replace value
json_binary_op(exclude) --> "exclude". % exclude matching list items
json_binary_op(include) --> "include". % include matching list items
json_unary_op(not) --> "not". % negate boolean value


parse_attribute(patch, JsonOp) --> json(JsonOp).

matching_dict(Candidate, Dict) :- select_dict(Candidate, Dict, _).

patch(_, op(=, Result), Result).
patch(Num, op(+, Inc), Result) :-
    number(Num), Result is Num + Inc.
patch(List, op(+, New), Result) :-
    is_list(List),
    append(List, [New], Result).
patch(List, op(exclude, Match), Result) :- exclude(matching_dict(Match), List, Result).
patch(List, op(include, Match), Result) :- include(matching_dict(Match), List, Result).
patch(true, op(not), false).
patch(Item, op(not), true) :- \+ Item = true.

patch(Val, Val, Val). % match value, doesn't change


% Patch any list items that match
patch(Existing, ToPatch, Result) :-
    is_list(Existing), is_list(ToPatch),
    maplist(patch_items(ToPatch), Existing, Result).

patch(Dict, Patch, Result) :-
    is_dict(Dict), is_dict(Patch),
    dict_pairs(Patch, json, KVs),
    foldl({Dict}/[Key-Val,In,Out]>>(
              get_dict(Key, Dict, Old),
              patch(Old, Val, New),
              put_dict(Key, In, New, Out)),
          KVs, Dict, Result).

patch_items(PatchRules, Item, Item) :- \+ matching_patch_rule(PatchRules, Item, _).
patch_items(PatchRules, ItemIn, ItemOut) :-
    once(matching_patch_rule(PatchRules, ItemIn, PatchRule)),
    writeln(matching_rule(PatchRule)),
    patch(ItemIn, PatchRule, ItemOut).

matching_patch_rule(Rules, Dict, Rule) :-
    member(Rule, Rules),
    dict_pairs(Rule, json, Fields),
    % Get all keys of all non-op values
    exclude([_-V]>>is_op(V), Fields, MatchFields),
    dict_pairs(RuleDict, json, MatchFields),
    select_dict(RuleDict, Dict, _).

is_op(X) :- compound(X), compound_name_arguments(X, op, _).

%% Resolve references, this is done before patching
% Env is a dict of bindings

resolve(Env, ref(Key,Methods), Resolved) :-
    get_dict(Key, Env, Value),
    eval_methods(Value, Methods, Resolved).

resolve(Env, ItemsIn, ItemsOut) :-
    is_list(ItemsIn),
    maplist(resolve(Env), ItemsIn, ItemsOut).
resolve(Env, DictIn, DictOut) :-
    is_dict(DictIn),
    dict_pairs(DictIn, json, KVs1),
    maplist(resolve(Env), KVs1, KVs2),
    dict_pairs(DictOut, json, KVs2).
resolve(Env, Field-ValIn, Field-ValOut) :- resolve(Env, ValIn, ValOut).
resolve(Env, op(X,ArgIn), op(X,ArgOut)) :- resolve(Env, ArgIn, ArgOut).
% Pass through atomic values and unary operations
resolve(_, X, X) :- number(X).
resolve(_, X, X) :- string(X).
resolve(_, true, true).
resolve(_, false, false).
resolve(_, null, null).
resolve(_, op(X), op(X)).

eval_methods(X, [], X).
eval_methods(In, [method(Name, Args)|Methods], Out) :-
    method(Name, In, Args, Intermediate),
    eval_methods(Intermediate, Methods, Out).

method(words, Str, [], List) :- split_string(Str, " \t\n", " \t\n", List).
method(interpolate, Args, [Pattern], Out) :-
    interpolate("", Pattern, Args, Out).

interpolate(Acc, Pattern, Args, Out) :-
    \+ re_match("^(.*?){(\\d+)}(.*)$", Pattern),
    string_concat(Acc, Pattern, Out).
interpolate(Acc, Pattern, Args, Out) :-
    re_matchsub("^(.*?){(\\d+)}(.*)$", Pattern, re_match{0:_, 1:Before, 2:Num, 3: After}),
    number_string(N, Num),
    (nth0(N, Args, Val) -> format(string(Arg), "~w", Val); format(string(Arg), "{MISSING:~w}", [N])),
    string_concat(Acc, Before, Out0),
    string_concat(Out0, Arg, Out1),
    interpolate(Out1, After, Args, Out).


parse(Input, Json) :- once(phrase((json(Json),ws), Input, [])).

% Toplevel object resolve and patch
resolve_and_patch(Current, Patch, Result) :-
    resolve(Current, Patch, Resolved),
    patch(Current, Resolved, Result).

%%%%%%%%%%%%%%
% Unit tests

:- begin_tests(state).
:- use_module(state).

% Basic parsing
p("{}", json{}).
p("[]", []).
p("42", 42).
p("-666", -666).
p("\"hello world\"", "hello world").
p("true", true).
p("false", false).
p("null", null).

% Dictionary and list
p("{  foo:  420 }", json{foo: 420}).
p("{firstname: \"John\", lastname: \"Doe\", age: 42}", json{firstname: "John", lastname: "Doe", age: 42}).
p("[ \"hello\", 42, null, \"world\" ]", ["hello",42,null,"world"]).

% Patch operations and ref
p("{foo: %+10}", json{foo: op(+, 10)}).
p("{alive: %not}", json{alive: op(not)}).
p("{id: @nextid}", json{id: ref(nextid,[])}).
p("{greeting: @name.words.format(\"Hello, {0}\")}", json{greeting: ref(name, [method(words,[]),method(format,["Hello, {0}"])])}).
p("@items.include(@filter)", ref(items, [method(include, [ref(filter,[])])])).

test(parsing, [ forall(p(Input,Parsed)) ]) :- string_codes(Input, Cs), state:parse(Cs, P), P=Parsed.

%%% Tests patching

p(json{count: 1}, "{count: %+41}", json{count: 42}).
p([1,2], "%+3", [1,2,3]).
p(json{alive: false}, "{alive: %not}", json{alive: true}).

test(patching, [ forall(p(Initial, Input, Output)) ]) :-
    string_codes(Input, Cs),
    state:parse(Cs, Patch),
    once(state:patch(Initial, Patch, Output)).

split_examples(Codes, [Ex|Examples]) :-
    once(append([Ex, [10,45,45,10], Rest], Codes)),
    split_examples(Rest, Examples).
split_examples(Codes, [Codes]) :-
    \+ append([_,[10,45,45,10],_], Codes).

example(File) :-
    read_file_to_codes(File, Codes, []),
    split_examples(Codes, Examples),
    maplist([Codes,Str]>>string_codes(Str, Codes), Examples, Strs),
    %writeln(examples(Strs)),
    maplist(state:parse, Examples, JSONs),
    append([[InitialState], Patches, [FinalState]], JSONs),
    %writeln(log(initial(InitialState), patches(Patches), final(FinalState))),
    foldl([Patch,Current,Next]>>(state:resolve_and_patch(Current, Patch, Next)), Patches, InitialState, FinalState).


test(patch1) :- example("test/01-patch.json").



:- end_tests(state).
