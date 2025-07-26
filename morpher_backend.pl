:- module(morpher_backend, [
               query/4
             , query_cached/4
             , connect_db/0
          ]).

:- use_module(library(http/http_open), [http_open/3]).
:- use_module(library(sgml), [load_structure/3]).
:- use_module(library(prosqlite)).


connect_db :-
    sqlite_connect('morpher.sqlite', morpher_cache,
                  []).

query_cached(_URL, _UUID, String, Row) :-
    format(atom(Q),
       'SELECT d.value, d.gen, d.mult FROM morph m JOIN decl d ON d.fk=m.pk WHERE m.value=~q;', [String]),
    sqlite_query(morpher_cache, Q, Row).

query_cached(URL, UUID, String, Row) :-
    get_fk(String, FK),
    (
        format(atom(QT),
               'SELECT d.pk FROM morph m JOIN decl d ON d.fk=m.pk WHERE m.pk=~q;',
               [FK]),
        sqlite_query(morpher_cache, QT, L)
        -> fail
    ;
        true
    ),
    forall(
        query(URL, UUID, String, row(Gen, Value, Mult)),
        (
            format(atom(Q2),
                   'INSERT INTO decl (fk, gen, value, mult) VALUES(~w, \'~w\', \'~w\', \'~w\') RETURNING pk;', [FK, Gen, Value, Mult]),
            sqlite_query(morpher_cache, Q2, PK)
        )),
    format(atom(Q),
       'SELECT d.value, d.gen, d.mult FROM decl d WHERE d.fk=~w;',
       [FK]), !,
    sqlite_query(morpher_cache, Q, Row).

get_fk(String, FK) :-
    format(atom(Q1),
           'SELECT pk FROM morph WHERE value=~q;',
           [String]),
    sqlite_query(morpher_cache, Q1, row(FK)), !.

get_fk(String, FK) :-
    format(atom(Q1),
           'INSERT INTO morph (value) VALUES (~q) RETURNING pk;',
           [String]),
    sqlite_query(morpher_cache, Q1, row(FK)).

query(URL, UUID, String, list(List)) :- !,
    query_morpher(URL, UUID, String, In),
    load_structure(stream(In), List,
                   [ dialect(xml)]),
    close(In).

query(URL, UUID, String, atom(S)) :- !,
    query_morpher(URL, UUID, String, In),
    read_string(In, _, S),
    format("~w~n",[[String, In, S]]),
    close(In),
    true.

query(URL, UUID, String, row(Gen, Value, Mult)) :- !,
    query_morpher(URL, UUID, String, In),
    load_structure(stream(In), List,
                   [ dialect(xml)]),
    close(In),
    proc_xml(List, row(Gen, Value, Mult)).

query_morpher(URL, UUID, String, In) :-
       http_open(
      [
         host(URL),
         path('/russian/declension'),
         search([
            s=String,
            token=UUID
         ])
      ],
      In, []).

parse(stream(In), List) :-
    load_structure(stream(In), List,
                   [ dialect(xml)]).

parse(atom(String), List) :-
    open_string(String, In),
    load_structure(stream(In), List,
                   [ dialect(xml)]),
    close(In).


% L = [element(xml, ['xmlns:xsi'='http://www.w3.org/2001/XMLSchema-instance', 'xmlns:xsd'='http://www.w3.org/2001/XMLSchema'], [element('Р', [], ['мамы мыла раму']), element('Д', [], ['маме мыла раму']), element('В', [], ['маму мыла раму']), element('Т', [], ['мамой мыла раму']), element('П', [], ['маме мыла раму']), element(множественное, [], [...|...])])].

proc_xml([element(xml, _, L)], Row) :-
    proc_xml(L, Row).

proc_xml([element('множественное', [], L)], row(Gen, Value, 'm')) :-
    !,
    proc_xml(L, row(Gen, Value, _)).

proc_xml([element(Tag, [], [Value]) | _], row(LTag, Value, 's')) :-
    tg(Tag, LTag).
proc_xml([_ | L], Row) :-
    proc_xml(L, Row).


tg('Р', gen).
tg('Д', dat).
tg('В', acc).
tg('Т', ins).
tg('П', loc).
tg('И', nom).
