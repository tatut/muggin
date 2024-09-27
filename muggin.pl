:- module(muggin, [parse/2, render/1, render/2]).
:- use_module(library(dcg/basics)).
:- use_module(state, [json//1]).
:- set_prolog_flag(double_quotes, codes).

%%%%%%%%%%
% Parsing

parse_error(Codes) :-
    \+ phrase(template(_), Codes, []),
    aggregate_all(min(Len, Rest),
                  (phrase(template(_), Codes, Rest), length(Rest, Len)),
                  min(_, MinRest)),
    string_codes(RestStr, MinRest),
    throw(template_syntax_error(at(RestStr))).

parse(Codes, _) :-
    last(Codes, 10),
    phrase(template(_), Codes, Rest),
    length(Rest, Len),
    Len > 0,
    parse_error(Codes).

parse(Codes, Template) :-
    last(Codes, 10),
    phrase(template(Template), Codes), !.
parse(Codes, Template) :-
    \+ last(Codes, 10),
    append(Codes, "\n", CodesNl),
    parse(CodesNl, Template).
parse_string(String, Template) :-
    string(String),
    string_codes(String, Codes),
    parse(Codes, Template).
parse_file(File, Template) :-
    read_file_to_codes(File, Codes,[]),
    parse(Codes, Template).



template(Template) --> element(0,Template).

tagname(N) --> string_without(" .#(=\n\t|<>&:", Cs), { atom_codes(N, Cs), length(Cs,Len), Len > 0 }.

varname(N) --> [FirstChar], { code_type(FirstChar, csymf) }, varname_(Cs), { atom_codes(N, [FirstChar|Cs]) }.
varname_([]) --> [].
varname_([C|Cs]) --> [C], { code_type(C, csym) }, varname_(Cs).

% Whitespace that is only space or tab
ws --> (" "|"\t"), ws.
ws --> [].

% At least one whitespace
ws1 --> (" "|"\t"), ws.

nl --> [C], { char_type(C, end_of_line) }.


attributes([Attr|Attrs]) --> attribute(Attr), more_attributes(Attrs).
more_attributes([]) --> ws.
more_attributes(Attrs) --> ws, ",", ws, attributes(Attrs).
attribute(Name-Value) --> tagname(Name), "=", expr(Value).
attribute(NameSpace-Name-Value) --> tagname(NameSpace), ":", tagname(Name), "=",
                                    ns_attribute_value(NameSpace, Value).
ns_attribute_value(state, Value) --> json(Value).

expr(text(Value)) --> "\"", string_without("\"", Value), "\"".
expr(var(Name)) --> varname(Name).

tag_and_attrs(Name,AllAttrs) -->
    tagname(Name),
    id_and_class_attrs(IdClassAttrs),
    paren_attrs(Attrs),
    { append(IdClassAttrs, Attrs, AllAttrs) }.

element(Ind,element(Name,Attrs,Content)) -->
    spaces(Ind), tag_and_attrs(Name, Attrs),
    inline_element_or_contents(Ind,Content).

inline_element_or_contents(Ind, [element(Name,Attrs,Content)]) -->
    ws, ">", ws, tag_and_attrs(Name,Attrs),
    inline_element_or_contents(Ind, Content).
inline_element_or_contents(Ind,Content) --> contents(Ind, Content).


paren_attrs([]) --> [].
paren_attrs(Attrs) --> "(", attributes(Attrs), ")".


contents(Ind, All) --> inline_content(Inline), nested_content(Ind, Nested),
                       { append(Inline, Nested, All) }.
inline_content([]) --> ws, nl.
inline_content([text(Txt)]) --> ws1, string_without(">\n", Txt), nl.
inline_content([var(Var)]) --> "=", ws, varname(Var), nl.
%%content(_, [state(State)]) --> "=", ws, json(State). % allow multiple space separated
inline_content(States) --> "=", state_contents(States).
nested_content(Ind, Children) -->
    { Ind1 is Ind + 2 },
    children(Ind1, Children).

% Allow multiple ws separated json states as content
state_contents([St|States]) --> ws, json(St), more_state_contents(States).
more_state_contents([]) --> ws, "\n".
more_state_contents(States) --> state_contents(States).

% Children of an element are elements at an increased indentation level
children(_,[]) --> [].
children(Ind,[Child|Children]) -->
    child(Ind, Child),
    children(Ind, Children).

child(Ind, Child) --> element(Ind, Child).
child(Ind, text(Txt)) --> spaces(Ind), "|", ws, string_without("\n", Txt), nl.
child(Ind, each(Goal, Vars, Children)) --> spaces(Ind), "@each", ws, string_without("\n", GoalCs), nl,
                                           { read_term_from_codes(GoalCs, Goal, [variable_names(Vars)]),
                                             Ind1 is Ind + 2 },
                                           children(Ind1, Children).
id_and_class_attrs(Attrs) --> idattr(Attrs).
id_and_class_attrs(Attrs) --> classattr(Attrs).
id_and_class_attrs(Attrs) --> idattr(IdAttrs), classattr(ClassAttrs), { append(IdAttrs, ClassAttrs, Attrs) }.

idattr([id-text(Id)]) --> "#", tagname(Val), { atom_codes(Val, Id) }.
idattr([]) --> [].
classattr([class-text(Class)]) --> ".", classnames(Classnames), { combine_classnames(Classnames, Class) }.
classnames([Class|Classes]) --> tagname(Classname), { atom_codes(Classname, Class) }, more_classnames(Classes).
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

% Basic DCG state nonterminals
state(S), [S] --> [S].
state(S0, S), [S] --> [S0].

html(element(Tag,Attrs,Content)) -->
    { write('<'), write(Tag) },
    html_attrs(Attrs),
    { write('>') },
    html_content(Content),
    { write('</'), write(Tag), write('>') }.

html(text(Txt)) -->
    % Output text as is (only text coming from outside template should be escaped)
    { format('~s',[Txt]) }.

html(var(V)) -->
    % FIXME: escape this
    state(Ctx),
    { %get_dict(V, Ctx, Chars),
        %format('~s', [Chars])
        format('~s', [V])
    }.

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
:- set_prolog_flag(double_quotes, codes).
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
p("div text\n  a(href=\"foo\") and link",
  element(div,[],[text("text"),
                  element(a,[href-text("foo")],[text("and link")])])).
p("div#foo bar", element(div, [id-text("foo")], [text("bar")])).

p("div(onclick=\"alert('foo')\")", element(div, [onclick-text("alert('foo')")], [])).
p("div(style=\"width:10vw;\", id=foo) hello",
  element(div,
          [style-text("width:10vw;"), id-var(foo)],
          [text("hello")])).
p("title= foo", element(title, [], [var(foo)])).
%p("title= @title", element(title, [], [state(ref(title,[]))])).

% Emmet style single line '>' separated elements
p("div#foo > ul.listing > li > a(href=\"http://example.com\") My link",
  element(div,[id-text("foo")],
          [element(ul,[class-text("listing")],
                   [element(li,[],
                            [element(a,[href-text("http://example.com")],
                                     [text("My link")])])])])).

test(parsing, [forall(p(Codes, Tpl))]) :-
    string_codes(Str, Codes), writeln(parsing(Str)),
    parse(Codes, Tpl).

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
