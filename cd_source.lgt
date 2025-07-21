:- object(sql_connection).

   :- info([
       version is 1:0:0,
       author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
       date is 2025-07-22,
       comment is 'Singleton for storing connections'
   ]).

   :- public(connect/2).
   :- mode(connect(+atom, -atom), zero_or_one).
   :- info(connect/2, [
      comment is 'Connect to the database',
      argnames is ['FileName', 'Connection']
   ]).

	:- use_module(library(prosqlite),
		[sqlite_connect/3,
		sqlite_disconnect/1]).

   connect(FileName, Conn) :-
		sqlite_connect(FileName, Conn,
							[verbose(true),
							ext(sqlite)]),
		retractall(conn(_)),
		assertz(conn(Conn)).

   :- public(conn/1).
	:- dynamic(conn/1).
   :- mode(conn(-atom), zero_or_one).
   :- info(conn/1, [
      comment is 'Returns the connection',
      argnames is ['Connection']
   ]).

   :- public(close/0).
   :- mode(close, zero_or_one).
   :- info(close/0, [
      comment is 'Close the connection'
   ]).

	close :-
		::conn(Conn),
   	sqlite_disconnect(Conn),
		retractall(conn(_)).

	:- use_module(library(prosqlite),
		[sqlite_query/3]).

   :- public(query/2).
   :- mode(query(+atom, ?atom), zero_or_more).
   :- info(query/2, [
      comment is 'Make a query to the connection',
      argnames is ['SQLQuery', 'row(...)']
   ]).

	query(Query, Row) :-
		::conn(Conn),
		sqlite_query(Conn, Query, Row).

:- end_object.


:- object(cd_ministry,
	implements(departmentp)).

	title(Name) :-
 		sql_connection::query('SELECT RUPMinistry FROM дсПараметрыПлана;', row(Name)),!.

:- end_object.

:- object(cd_department(_Code_),
	implements(departmentp)).

   :- info([
       version is 1:0:0,
       author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
       date is 2025-07-22,
       comment is 'Deduce department level and its name'
   ]).

	parent(cd_ministry).

	title(Name) :-
		query(row(Name)).

	query(Row) :-
		sql_connection::query(
				'SELECT Полноеназвание FROM дсФилиалы WHERE Головноеучреждение == 1;',
				Row),!.

:- end_object.


:- object(cd_institute,
	implements(departmentp)).

	parent(cd_department(Code)) :-
		query(row(_, Code)).

	title(Name) :-
		query(row(Name, _)).

	query(Row) :-
		sql_connection::query(
				'SELECT Полноеназвание, вкРодителя FROM дсФилиалы WHERE Головноеучреждение != 1;',
				Row),!.


:- end_object.
