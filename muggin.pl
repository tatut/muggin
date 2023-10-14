:- module(muggin, [parse/2, render/1, render/2]).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, chars).

%%%%%%%%%%
% Parsing

parse(Chars, Template) :-
    last(Chars, '\n'),
    phrase(template(Template), Chars), !.
parse(Chars, Template) :-
    \+ last(Chars, '\n'),
    append(Chars, "\n", CharsNl),
    parse(CharsNl, Template).

template(Template) --> element(0,Template).

tagname(N) --> string_without(" .#(=\n|<>&", Cs), { atom_chars(N, Cs), length(Cs,Len), Len > 0 }.

varname(N) --> [FirstChar], { char_type(FirstChar, csymf) }, varname_(Cs), { atom_chars(N, [FirstChar|Cs]) }.
varname_([]) --> [].
varname_([C|Cs]) --> [C], { char_type(C, csym) }, varname_(Cs).

% Whitespace that is only space or tab
ws --> (" "|"\t"), ws.
ws --> [].

% At least one whitespace
ws1 --> (" "|"\t"), ws.

nl --> [C], { char_type(C, end_of_line) }.


attributes([Attr|Attrs]) --> attribute(Attr), more_attributes(Attrs).
more_attributes([]) --> ws.
more_attributes(Attrs) --> ws, attributes(Attrs).
attribute(Name-Value) --> tagname(Name), "=", expr(Value).

expr(text(Value)) --> "\"", string_without("\"", Value), "\"".
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

idattr([id-text(Id)]) --> "#", tagname(Val), { atom_chars(Val, Id) }.
idattr([]) --> [].
classattr([class-text(Class)]) --> ".", classnames(Classnames), { combine_classnames(Classnames, Class) }.
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
% HTML output

render(ParsedTemplate) :- render(ParsedTemplate, {}).
render(ParsedTemplate, Context) :-
    phrase(html(ParsedTemplate), [Context], _).


html(element(Tag,Attrs,Content)) -->
    { write('<'), write(Tag) },
    html_attrs(Attrs),
    { write('>') },
    html_content(Content),
    { write('</'), write(Tag), write('>') }.

html(text(Txt)) -->
    % Output text as is (only text coming from outside template should be escaped)
    { format('~s',[Txt]) }.

html_attrs([]) --> [].
html_attrs([Key-Val|Attrs]) -->
    { write(' '), write(Key), write('="') },
    html_attr_val(Val),
    { write('"') },
    html_attrs(Attrs).

html_attr_val(text(Txt)) -->
    { format('~s', [Txt]) }.

html_content([]) --> [].
html_content([C|Content]) -->
    html(C),
    html_content(Content).

%%%%%%%%%%%%%%
% Unit tests

:- begin_tests(muggin).
:- set_prolog_flag(double_quotes, chars).
:- use_module(muggin).

% Define parse tests a p("template", element(...))
p("div", element(div, [], [])).

p("div#app\n",   element(div, [id-text("app")], [])).
p("div.stylish", element(div, [class-text("stylish")], [])).
p("div#x.a",     element(div, [id-text("x"),class-text("a")], [])).
p("div.a.b.c",   element(div, [class-text("a b c")], [])).

p("div\n  | this is my content\n", element(div,[],[text("this is my content")])).
p("div\n  | stuff\n  ul\n    li",
  element(div,[],[text("stuff"), element(ul, [], [element(li,[],[])])])).
p("div with content", element(div,[],[text("with content")])).
p("div#foo bar", element(div, [id-text("foo")], [text("bar")])).

p("div(onclick=\"alert('foo')\")", element(div, [onclick-text("alert('foo')")], [])).
p("div(style=\"width:10vw;\" id=foo) hello",
  element(div,
          [style-text("width:10vw;"), id-var(foo)],
          [text("hello")])).

test(parsing, [forall(p(Chars, Tpl))]) :- parse(Chars, Tpl).

% Define render tests as r(template, expectedhtml)
r("div", "<div></div>").
r("div#foo bar", "<div id=\"foo\">bar</div>").
r("a.link(href=\"http://example.com\") click here", "<a class=\"link\" href=\"http://example.com\">click here</a>").
r("p\n  | my story", "<p>my story</p>").

test(render, [ forall(r(Tpl, Html)) ]) :-
    parse(Tpl, T),
    with_output_to(string(Str), render(T)),
    string_codes(Str, Html).


:- end_tests(muggin).
