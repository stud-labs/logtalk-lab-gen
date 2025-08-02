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

	type(ministry).

:- end_object.

:- object(cd_department(_Code_),
	implements(departmentp)).

   :- info([
       version is 1:0:0,
       author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
       date is 2025-07-22,
       comment is 'Deduce department level and its name'
   ]).

	title(Name) :-
		query(row(Name)).

	type(department).

	query(Row) :-
		sql_connection::query(
				'SELECT Полноеназвание FROM дсФилиалы WHERE Головноеучреждение == 1;',
				Row),!.

:- end_object.


:- object(cd_university(_Code_),
	extends(cd_department(_Code_))).

	parent(cd_ministry).
	type(university).

:- end_object.

:- object(cd_institute,
	implements(departmentp)).

	parent(cd_university(Code)) :-
		query(row(_, Code)).

	title(Name) :-
		query(row(Name, _)).

	type(institute).

	query(Row) :-
		sql_connection::query(
				'SELECT Полноеназвание, вкРодителя FROM дсФилиалы WHERE Головноеучреждение != 1;',
				Row),!.

:- end_object.

:- object(cd_chair,
	implements(departmentp)).

	parent(cd_institute).

	title(S) :-
		query(row(Name)),
		downcase_atom(Name, DName),
		format(atom(S), 'Кафедра ~w', [DName]).

	query(Row) :-
		% debugger::trace,
		sql_connection::query(
			'SELECT К.Название FROM дсПрофили as П JOIN дсКафедры as К ON П.вкПодразделения = К.пк;',
			Row), !.

	type(chair).

:- end_object.

:- object(cd_approval,
	implements(approvalp)).

 	:- info([
 		version is 1:0:0,
 		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
 		date is 2025-07-25,
 		comment is 'Infers approval person from shakhty database'
 	]).

	type_title('УТВЕРЖДАЮ').
	short_name(Name) :-
		query(row(_, Name)).
	position(Position) :-
		query(row(Position, _)).

 	query(Row) :-
		sql_connection::query(
			'SELECT Должность, ФИО FROM дсДолжностныеЛица WHERE Должность="Директор";',
			Row),!.

 :- end_object.


:- object(cd_cd_title_page,
	implements(cd_title_pagep)).

	document_title('Рабочая программа дисциплины (модуля)').
	discipline('No code', 'No Name').
	direction('01.03.02', 'Прикладная математика и информатика').
	profile('Искусственный интеллект и системная аналитика').
	qualification('бакалавр').
	education_type('очная').
	approved_by(
		approval(
			'Согласовано с УМК института математики и информационных технологий',
			'Председатель',
			'В.Г. Антоник',
			none, none)).
	recommended_by(
		approval(RecChair,
			'зав. каф.',
			'Е.А. Черкашин',
			2025-05-31, 8)) :-
				cd_chair::title(Chair),
				% debugger::trace,
				downcase_atom(Chair, DChair),
				morpher::query(DChair, m(Value, ins, s)),
				% G = ins,
				format(atom(RecChair), 'Рекомендовано ~w', [Value]).

:- end_object.

:- object(cd_cd_title_page(_Code_, _Title_),
	implements(cd_title_pagep)).

	document_title('Рабочая программа дисциплины (модуля)').
	discipline(_Code_, _Title_).
	direction('01.03.02', 'Прикладная математика и информатика').
	profile('Искусственный интеллект и системная аналитика').
	qualification('бакалавр').
	education_type('очная').
	approved_by(
		approval(
			'Согласовано с УМК института математики и информационных технологий',
			'Председатель',
			'В.Г. Антоник',
			none, none)).
	recommended_by(
		approval(RecChair,
			'зав. каф.',
			'Е.А. Черкашин',
			2025-05-31, 8)) :-
				cd_chair::title(Chair),
				% debugger::trace,
				downcase_atom(Chair, DChair),
				morpher::query(DChair, m(Value, ins, s)),
				% G = ins,
				format(atom(RecChair), 'Рекомендовано ~w', [Value]).

:- end_object.

:- object(cd_cd_title_page(_Discipline_),
	extends(cd_cd_title_page)).

	discipline(Code, Title) :-
		_Discipline_::title(Code, Title).

:- end_object.

:- object(yaml_object(_YAML_),
	imports(yamlc)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-01,
		comment is 'Base class for YAML based virtual objects'
	]).

	:- protected(yaml_dom/1).
	:- mode(yaml_dom(-atom), one).
	:- info(yaml_dom/1, [
		comment is 'Returns yaml dom tree',
		argnames is ['YAML']
	]).

	yaml_dom(_YAML_).

:- end_object.

:- object(cd_indicator(_YAML_),
	extends(yaml_object(_YAML_))).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-01,
		comment is 'Description'
	]).

	:- public(title/1).
	:- mode(title(?atom), one).
	:- info(title/1, [
		comment is 'The title of the index',
		argnames is ['Title']
	]).

	title(Title) :-
		::yaml(title, Title).

	:- public(ksa/1).
	:- mode(ksa(?atom), zero_or_more).
	:- info(ksa/1, [
		comment is 'Returns knowledge-skills-ability object',
		argnames is ['KSA']
	]).

	ksa(Expr) :-
		Expr =.. [Key, Value], % Key(Value)
		::yaml(Key, Value).

:- end_object.

:- object(cd_competence(_YAML_),
	extends(yaml_object(_YAML_))).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-01,
		comment is 'Competence of a CRM'
	]).

	:- use_module(library(lists), [member/2]).

	indicator(cd_indicator(Index)) :-
		::yaml(indices, List),
		member(Index, List).

:- end_object.

:- object(cd_crm(_YAML_),
	extends(yaml_object(_YAML_))).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-01,
		comment is 'Describes Competence-Rule model'
	]).

	:- public(competence/2).
	:- mode(competence(+atom, -object), one_or_more).
	:- info(competence/2, [
		comment is 'Describes competence in CRM',
		argnames is ['TypeOfCompetene', 'CompetenceData']
	]).


	:- use_module(library(lists), [member/2]).

	competence(Type, cd_competence(X)) :-
		::yaml(Type, List),
		member(X, List).

:- end_object.


:- object(cd_aims_problems(_Discipline_),
	implements(aims_problemsp)).

	aim('Выработать у студентов навыки целенаправленного системного мышления.').

	problems([
		'Представить системный подход, его свойства и варианты;',
		'Рассмотреть разнообразные инженерные задачи, представимые при помощи системного подхода;',
		'Продемонстрировать целенаправленные операции над системными моделями приводящими к заданному результату;',
		'Освоить базовый набор опаерааций для решения задач.'
  ]).

:- end_object.
