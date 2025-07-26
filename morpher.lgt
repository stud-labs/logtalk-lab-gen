
:- object(morpher_config).

	:- public(uuid/1).
	:- mode(uuid(-atom), one).
	:- info(uuid/1, [
		comment is 'Returns identifier of connection',
		argnames is ['UUID']
	]).

	uuid('5573e006-40f5-41c6-a145-f1d31858fb68').

	:- public(url/1).
	:- mode(url(-atom), one).
	:- info(url/1, [
		comment is 'Return basic url',
		argnames is ['URLSymbol']
	]).

	url('ws3.morpher.ru').

:- end_object.


:- object(morpher).

	:- use_module(morpher_backend,
		[query_cached/4 as m_query/4,
		connect_db/0]).

	:- public(query/2).
	:- mode(query(+atom, -list), one).
	:- info(query/2, [
		comment is 'Query morpher, return parsed xml',
		argnames is ['Russian sentence', 'Morphing result']
	]).

	query(String, Query) :-
		morpher_config::url(URL),
		morpher_config::uuid(UUID),
		m_query(URL, UUID, String, Query).

	:- initialization((
		connect_db,
		% debugger::trace,
		forall(::query('маленький японский шнобель',
						row(Value, gen, s)),
				format("RC:~w~n", [Value]))
	)).

:- end_object.
