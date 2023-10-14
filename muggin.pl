:- module(muggin, [parse/2]).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, chars).


parse(Chars, Template) :-
    last(Chars, '\n'),
    phrase(template(Template), Chars), !.
parse(Chars, Template) :-
    \+ last(Chars, '\n'),
    append(Chars, "\n", CharsNl),
    parse(CharsNl, Template).

template(Template) --> element(0,Template).

%tagname_([]) --> [].
%tagname_([C|Cs]) --> [C], { char_type(C, alpha) }, tagname_(Cs). % FIXME: allow dash for custom elements
%tagname(N) --> tagname_(Cs), { atom_chars(N, Cs), length(Cs, Len), Len > 0 }.
tagname(N) --> string_without(" .#(\n", Cs), { atom_chars(N, Cs), length(Cs,Len), Len > 0 }.

varname(N) --> [FirstChar], { char_type(FirstChar, csymf) }, varname_(Cs), { atom_chars(N, [FirstChar|Cs]) }.
varname_([]) --> [].
varname_([C|Cs]) --> [C], { char_type(C, csym) }, varname_(Cs).

% Whitespace that is only space or tab
ws --> (" "|"\t"), ws.
ws --> [].

% At least one whitespace
ws1 --> [W], { char_type(W, space) }, ws.

nl --> [C], { char_type(C, end_of_line) }.

attributes([]) --> [].
attributes([Attr|Attrs]) --> attribute(Attr), more_attributes(Attrs).
more_attributes([]) --> [].
more_attributes([Attrs]) --> ws, attributes(Attrs).
attribute(Name-Value) --> tagname(Name), "=", expr(Value).

expr(str(Value)) --> "\"", string_without("\"", Value), "\"".
expr(var(Name)) --> varname(Name).

element(Ind,element(Name,Attrs,Content)) -->
    spaces(Ind), tagname(Name), id_and_class_attrs(Attrs), content(Ind,Content).
element(Ind,element(Name,AllAttrs,Content)) -->
    spaces(Ind), tagname(Name), id_and_class_attrs(IdClassAttrs),
    "(", attributes(Attrs), ")",
    { append(IdClassAttrs, Attrs, AllAttrs) }, content(Ind, Content).

content(_, []) --> ws.
content(_, [text(Txt)]) --> ws1, string_without("\n", Txt), nl.
content(Ind, Children) -->
    ws, nl,
    { Ind1 is Ind + 2 },
    children(Ind1, Children).

% Children of an element are elements at an increased indentation level
children(_,[]) --> [].
children(Ind,[Child|Children]) -->
    child(Ind, Child),
    children(Ind, Children).

child(Ind, Child) --> element(Ind, Child).
child(Ind, text(Txt)) --> spaces(Ind), "|", ws, string_without("\n", Txt), nl.

id_and_class_attrs(Attrs) --> idattr(Attrs).
id_and_class_attrs(Attrs) --> classattr(Attrs).
id_and_class_attrs(Attrs) --> idattr(IdAttrs), classattr(ClassAttrs), { append(IdAttrs, ClassAttrs, Attrs) }.

idattr([id-Id]) --> "#", tagname(Val), { atom_chars(Val, Id) }.
idattr([]) --> [].
classattr([class-Class]) --> ".", classnames(Classnames), { combine_classnames(Classnames, Class) }.
classnames([Class|Classes]) --> tagname(Classname), { atom_chars(Classname, Class) }, more_classnames(Classes).
more_classnames([]) --> [].
more_classnames(Classes) --> ".", classnames(Classes).
combine_classnames([C], C).
combine_classnames([C|Cs], Out) :-
    append(C, " ", Out1),
    append(Out1, Rest, Out),
    combine_classnames(Cs, Rest).
% Require exactly N spaces
spaces(0) --> [].
spaces(N) --> " ", { N > 0, N1 is N - 1 }, spaces(N1).


%%%%%%%%%%%%%%
% Unit tests

:- begin_tests(muggin).
:- set_prolog_flag(double_quotes, chars).
:- use_module(muggin).

% Short hand for parsing element with attrs only
pa(Chars, Tag, Attrs) :- parse(Chars, element(Tag, Attrs, [])).
% Short hand for parsing element with content only
pc(Chars, Tag, Content) :- parse(Chars, element(Tag, [], Content)).
% Short hand for parsing element with attrs and content
pac(Chars, Tag, Attrs, Content) :- parse(Chars, element(Tag, Attrs, Content)).

test(simple_element) :- pa("div", div, []).

test(element_w_id) :- pa("div#app\n", div, [id-"app"]).
test(element_w_class) :- pa("div.stylish", div, [class-"stylish"]).
test(element_w_id_and_class) :- pa("div#x.a", div, [id-"x",class-"a"]).
test(element_w_classes) :- pa("div.a.b.c", div, [class-"a b c"]).

test(pipe_text) :- pc("div\n  | this is my content\n", div, [text("this is my content")]).
test(pipe_text_and_elt) :- pc("div\n  | stuff\n  ul\n    li", div,
                              [text("stuff"), element(ul, [], [element(li,[],[])])]).
test(content_after) :- pc("div with content", div, [text("with content")]).
test(id_and_content_after) :- pac("div#foo bar", div, [id-"foo"], [text("bar")]).
:- end_tests(muggin).
