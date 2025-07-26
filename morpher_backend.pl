:- module(morpher_backend, [
               query/4
             , query_cached/4
             , connect_db/0
          ]).

:- use_module(library(http/http_open), [http_open/3]).
:- use_module(library(sgml), [load_structure/3]).
:- use_module(prosqlite).


connect_db :-
    sqlite_connect('morpher.sqlite', morpher_cache,
                  [exists(false)]).

query_cached(URL, UUID, String, List) :-
    ds_find(morphing, source=String, [Bytes]), !,
    parse(atom(Bytes), List).

query_cached(URL, UUID, String, List) :-
    query(URL, UUID, String, atom(Bytes)),
    ds_store(morphing{source:String, string:Bytes}),
    parse(bytes(Bytes), List).

query(URL, UUID, String, List) :-
    query_morpher(URL, UUID, String, In),
    load_structure(stream(In), List,
                   [ dialect(xml)]),
    close(In).

query(URL, UUID, String, List) :-
    query_morpher(URL, UUID, String, In),
    load_structure(stream(In), List,
                   [ dialect(xml)]),
    close(In).

query(URL, UUID, String, atom(S)) :-
    query_morpher(URL, UUID, String, In),
    read_string(In, _, S),
    close(In).

query_morpher(URL, UUID, String, Stream) :-
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
