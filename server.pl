:- module(server, [start/0]).
:- use_module(muggin).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(gensym)).
:- http_handler(root(File), serve(File), [methods([get,post])]).

start :-
    http_server(http_dispatch, [port(8000)]).

% Try to load and parse a .mug template by appending the name
template(File, Parsed) :-
    atom_concat(File, '.mug', TemplateName),
    exists_file(TemplateName),
    once(phrase_from_file(muggin:template(Parsed), TemplateName)).

serve(File,Request) :-
    template(File, Parsed)
    -> serve_template(Request, Parsed)
    ; serve_file(Request, File).

serve_template(Request, Parsed) :-
    format('Content-Type: text/plain~n~n'),
    writeln(parsed(Parsed)),
    current_state(Parsed, State),
    render_template(Parsed, State).


serve_file(Request, File) :-
    http_reply_file(File, [], Request).


%%% Render state enhanced muggin template as a server side
%%% rendered application

path(element(T,A,C), [T], element(T,A,C)).
path(element(Tag,_,Content), [Tag,NextTag|Tags], Result) :-
    member(element(NextTag,A,C), Content),
    path(element(NextTag,A,C), [NextTag|Tags], Result).


% Find current state from session, or initialize a new one
current_state(_Tpl, State) :-
    http_session_data(state(State)).

current_state(Tpl, State) :-
    path(Tpl, [html, head, script], element(script, [state-init-State],_)),
    http_session_assert(state(State)).

render_template(_Tpl, State) :-
    writeln(got_state(State)).

out(Things) --> { maplist(write, Things) }.

state(S), [S] --> [S].
setstate(NewState), [NewState] --> [_]. % replace state completely

ref(R, V) --> state(S),
              { writeln(resolving(R)),
                state:resolve(S, R, V),
                writeln(resolved(V))
              }.

html(element(Tag,Attrs,Content)) -->
    { \+ member(state-each-_, Attrs) }, % No each, regular element
    out(['<', Tag]), html_attrs(Attrs), out(['>']),
    html_content(Content),
    out(['</', Tag, '>']).

html(element(Tag,Attrs,Content)) -->
    state(Saved),
    { select(state-each-Ref, Attrs, Attrs1) },
    out(['<', Tag]), html_attrs(Attrs1), out(['>']),
    ref(Ref, Items),
    html(each(Items, Content)),
    setstate(Saved).

html(each([], _)) --> [].
html(each([Item|Items], Content)) -->
    setstate(Item),
    html_content(Content),
    html(each(Items,Content)).

html(text(Cs)) --> { string_codes(Str, Cs) }, out([Str]).

html_content([]) --> [].
html_content([C|Cs]) --> html(C), html_content(Cs).
html_attrs([]) --> [].
html_attrs([A|Attrs]) --> out([' ']), html_attr(A), html_attrs(Attrs).

html_attr(Name-Value) -->
    { atom(Name) },
    out([Name,'="']),
    html(Value),
    out(['"']).

% Replace initial state with HTMX script loading
html_attr(state-init-_) --> out(['src="https://unpkg.com/htmx.org@2.0.2"']).

% Record onchange event for input
html_attr(state-patch-Val) -->
    { gensym(patch, ID) %, http_session_assert(patch(ID, Val))
    },
    out(['hx-post="?', ID, '" hx-target="body"']).
