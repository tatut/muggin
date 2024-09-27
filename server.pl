:- module(server, [start/0]).
:- use_module(muggin).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_parameters)).
:- use_module(library(gensym)).
:- use_module(library(yall)).
:- http_handler(root(File), serve(Method,File), [method(Method), methods([get,post])]).
:- dynamic template_loaded/3.

start :-
    http_server(http_dispatch, [port(8000)]).

load_time(File, Time) :-
    template_loaded(File, Time,_) -> true; Time = 0.

% Try to load and parse a .mug template by appending the name
template(File, Parsed) :-
    atom_concat(File, '.mug', TemplateName),
    exists_file(TemplateName),
    time_file(TemplateName, ChangeTime),
    load_time(TemplateName, LoadTime),
    dbg(time(load(LoadTime), file(ChangeTime))),
    (LoadTime >= ChangeTime
    -> template_loaded(TemplateName, _, Parsed)
    ; ( dbg(reloading(TemplateName)),
        get_time(Now),
        once(phrase_from_file(muggin:template(Parsed), TemplateName)),
        retractall(template_loaded(TemplateName, _, _)),
        asserta(template_loaded(TemplateName, Now, Parsed)) )).

serve(get,File,Request) :-
    template(File, Parsed)
    -> serve_template(Request, Parsed)
    ; serve_file(Request, File).

serve(post, File, Request) :-
    http_parameters(Request, [], [form_data(Params)]),
    dbg(params(Params)),
    member('_'=ID, Params),
    http_session_data(patch(ID, Patch, ClientMappings)),
    dbg(get_param(ClientMappings)),
    client_mappings(Params, ClientMappings, Client),
    current_state(session, State0),
    dbg(client_mappings(Client, state0(State0), patch(Patch))),
    state:resolve_and_patch(State0.put(['__client'=Client]), Patch, State1),
    del_dict('__client', State1, _, State2),
    dbg(state1(State2)),
    http_session_retractall(state(_)),
    http_session_retractall(patch(_,_,_)), % retract old patches before new render
    http_session_assert(state(State2)),
    dbg(posting(File, params(Params), id(ID), patch(Patch), new_state(State2))),
    template(File, Parsed),
    % Only render the body for HTMX updates
    path(Parsed, [html, body], Body),
    serve_template(Request, Body).

client_mappings(Form, Mappings, Client) :-
    foldl({Form,Mappings}/[RefName=FormName,M0,M1]>>(
              member(FormName=FormVal, Form),
              put_dict(RefName, M0, FormVal, M1)),
          Mappings, json{}, Client).

serve_template(_Request, Parsed) :-
    format('Content-Type: text/html~n~n'),
    http_open_session(Session, []),
    dbg(session(Session)),
    once(current_state(Parsed, State)),
    once(render_template(Parsed, State))
    -> true
    ; writeln('<div style="color: red;">ERROR</div>').


serve_file(Request, File) :-
    http_reply_file(File, [], Request).


%%% Render state enhanced muggin template as a server side
%%% rendered application

path(element(T,A,C), [T], element(T,A,C)).
path(element(Tag,_,Content), [Tag,NextTag|Tags], Result) :-
    member(element(NextTag,A,C), Content),
    path(element(NextTag,A,C), [NextTag|Tags], Result).


%dbg(Thing) :- with_output_to(user_error, writeln(Thing)).
dbg(_).

% Find current state from session, or initialize a new one
current_state(_Tpl, State) :-
    http_session_data(state(State)).

current_state(Tpl, State) :-
    path(Tpl, [html, head, script], element(script, [state-init-State],_)),
    http_session_retractall(state(_)),
    http_session_assert(state(State)).

render_template(Tpl, State) :-
    dbg(render_template_called),
    phrase(html(Tpl), [State], _).

out(Things) --> { maplist(write, Things) }.

state(S), [S] --> [S].
setstate(NewState), [NewState] --> [_]. % replace state completely

ref(R, V) --> state(S),
              { %writeln(resolving(R)),
                state:resolve(S, R, V)
                %writeln(resolved(V))
              }.

special_attr(when).
special_attr(each).
falsy(false).
falsy(null).

handle_when(true, E) --> html(E).
handle_when(false, _) --> [].

handle_special(when, Ref, E) -->
    ref(Ref, Val),
    { (falsy(Val) -> Cond=false; Cond=true) },
    handle_when(Cond, E).

handle_special(each, Ref, element(Tag,Attrs,Content)) -->
    state(Saved),
    out(['<', Tag]), html_attrs(element(Tag,Attrs,Content),Attrs), out(['>']),
    ref(Ref, Items),
    html(each(Saved, Items, Content)),
    out(['</', Tag, '>']),
    setstate(Saved).

has_special_attr(Attrs) :-
    special_attr(Special),
    memberchk(state-Special-_, Attrs).

html(element(Tag,Attrs,Content)) -->
    { \+ has_special_attr(Attrs) }, % No special, regular element
    out(['<', Tag]), html_attrs(element(Tag,Attrs,Content),Attrs), out(['>']),
    html_content(Content),
    out(['</', Tag, '>']).

html(element(Tag,Attrs,Content)) -->
    { special_attr(Special),
      select(state-Special-Val, Attrs, Attrs1),
      dbg(handle_special(Special, Val)),
      ! },
    handle_special(Special, Val, element(Tag,Attrs1,Content)).

html(each(_,[],_)) --> [].
html(each(S0,[Item|Items], Content)) -->
    { dbg(mergin(S0,Item)), S1 = S0.put(Item) },
    setstate(S1),
    html_content(Content),
    html(each(S0,Items,Content)).

html(text(Cs)) --> { string_codes(Str, Cs) }, out([Str]).
html(S) --> { string(S) }, out([S]).
html(ref(R,M)) --> ref(ref(R,M), V), out([V]).

html_content([]) --> [].
html_content([C|Cs]) --> html(C), html_content(Cs).
html_attrs(_,[]) --> [].
html_attrs(E, [A|Attrs]) --> html_attr(E,A), html_attrs(E,Attrs).

html_attr(_,Name-Value) -->
    { atom(Name) },
    out([' ',Name,'="']),
    html(Value),
    out(['"']).

% Replace initial state with HTMX script loading
html_attr(_,state-init-_) --> out([' src="htmx-2.0.2.min.js"']). % "https://unpkg.com/htmx.org@2.0.2"

% Record onchange event for input
html_attr(E,state-patch-Val) -->
    state(S),
    { gensym(patch, ID),
      %dbg(current_state_for_patch(S)),
      % Partially resolve the patch, for any id references
      state:resolve(S.put(['__partial'=true]), Val, Val1),
      patch_payload(E, Payload),
      http_session_assert(patch(ID, Val1, Payload))
    },
    out([' hx-post="" hx-vals=''{"_":"', ID, '"}'' " hx-target="body"']).

html_attr(_,state-on-Val) --> out([' hx-trigger="', Val, '"']).
html_attr(_,state-checked-Ref) -->
    ref(Ref, Checked),
    checked(Checked).

html_attr(_,state-value-Ref) -->
    ref(Ref, Value),
    out([' value="', Value, '"']).

% Get form value mappings, if there is a name attr, use that as $value
patch_payload(element(_,Attrs,_), [value=Name]) :-
    member(name-text(NameCs), Attrs),
    atom_codes(Name, NameCs).
patch_payload(element(_,Attrs,_), []) :-
    \+ memberchk(name-_, Attrs).


checked(false) --> [].
checked(true) --> out([' checked']).
