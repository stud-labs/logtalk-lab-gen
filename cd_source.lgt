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

:- protocol(catalog_entryp).

	:- public(code/1).
	:- mode(code(?atom), zero_or_one).
	:- info(code/1, [
		comment is 'Catalog entry code',
		argnames is ['Code']
	]).

	:- public(title/1).
	:- mode(title(?atom), zero_or_one).
	:- info(title/1, [
		comment is 'Catalog entry title',
		argnames is ['Title']
	]).

:- end_protocol.

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

:- protocol(ksap).

	:- public(ksa/1).
	:- mode(ksa(?atom), zero_or_more).
	:- info(ksa/1, [
		comment is 'Returns knowledge-skills-ability object',
		argnames is ['KSA']
	]).

:- end_protocol.

:- object(y_indicator(_YAML_),
	implements(ksap),
	extends(yaml_object(_YAML_))).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-01,
		comment is 'Description'
	]).

	ksa(Expr) :-
		Expr =.. [Key, Value], % Key(Value)
		::yaml(Key, Value).

:- end_object.

:- protocol(indicatorp).

	:- public(indicator/1).
	:- mode(indicator(-object), one_or_more).
	:- info(indicator/1, [
		comment is 'Defines indicator for a competence',
		argnames is ['IndicatorObject']
	]).

:- end_protocol.

:- object(y_competence(_YAML_),
	implements(indicatorp),
	extends(yaml_object(_YAML_))).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-01,
		comment is 'Competence of a CRM'
	]).

	:- use_module(library(lists), [member/2]).

	indicator(y_indicator(Index)) :-
		::yaml(indices, List),
		member(Index, List).

:- end_object.

:- protocol(competence_listp).

	:- public(competence/2).
	:- mode(competence(+atom, -object), one_or_more).
	:- info(competence/2, [
		comment is 'Describes competence in CRM',
		argnames is ['TypeOfCompetene', 'CompetenceData']
	]).

:- end_protocol.

:- object(y_crm(_YAML_),
	implements(competence_listp),
	extends(yaml_object(_YAML_))).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-01,
		comment is 'Describes Competence-Rule model'
	]).

	:- use_module(library(lists), [member/2]).

	competence(Type, y_competence(X)) :-
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

:- protocol(requirementsp).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-03,
		comment is 'Describes competencies for a discipline'
	]).

	:- public(competence/2).
	:- mode(competence(+atom, -object), one_or_more).
	:- info(competence/2, [
		comment is 'Describes competence in CRM',
		argnames is ['TypeOfCompetene', 'CompetenceData']
	]).

:- end_protocol.

:- object(cd_indicator(_Code_, _Title_),
	implements([catalog_entryp])).

	title(_Title_).
	code(_Code_).

:- end_object.

:- object(cd_competence(_Code_, _Title_, _Indicators_),
	implements([indicatorp, catalog_entryp]),
	imports(exoptions)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-03,
		comment is 'Competence originating from database'
	]).

	title(_Title_).
	code(_Code_).

	:- use_module(library(lists), [member/2]).

	indicator(cd_indicator(Code, Title)) :-
		member(Code-Title, _Indicators_).

:- end_object.

:- object(cd_requirements(_Discipline_),
	implements([competence_listp, requirementsp])).

	:- use_module(library(lists), [member/2]).

	competence(undefined, cd_competence(Code, Title, Indicators)) :-
		D = _Discipline_, !,
		D::cd_competence(competence(Code), Title),
		findall(CCode-CTitle,
			D::cd_competence(indicator(CCode, Code), CTitle),
			Indicators),
		% write(cd_competence(Code, Title, Indicators)), nl.
		true.

:- end_object.

:- object(cd_discipline(_Code_, _Title_),
	implements(disciplinep),
	imports(exoptions)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-07-27,
		comment is 'Defines discipline object connected to the databse.'
	]).

	title(Code, Title) :-
		::get_where(W), !,
		format(atom(Q), 'SELECT l.ДисциплинаКод, l.Дисциплина FROM дсСтроки as l WHERE ~w;', [W]),
		sql_connection::query(Q, row(Code1, Title1)),
		Code1 = Code, Title1 = Title.

	:- public(cd_competence/2).
	:- mode(cd_competence(?atom, ?atom), one_or_more).
	:- info(cd_competence/2, [
		comment is 'Query database for competence',
		argnames is ['CompetenceCode', 'CompetenceTitle']
	]).

	cd_competence(indicator(ICode, Code), ITitle) :-
		::get_where(W), !,
		format(atom(Q),
			'
			SELECT
				c.ШифрКомпетенции, c.Наименование
			FROM дсСтроки as l
			JOIN дсКомпетенцииДисциплины as cd
				ON cd.вкСтроки = l.пк
			JOIN дсКомпетенции as c
				ON cd.вкКомпетенции = c.пк
			JOIN дсКомпетенции as pc
				ON c.вкРодителя = pc.пк
			WHERE
				pc.ШифрКомпетенции=\'~w\'
				AND
				~w
			ORDER BY
				c.ШифрКомпетенции;
			', [Code, W]),
		% write(Q), nl,
		sql_connection::query(Q, row(Code1, Title1)),
		ICode=Code1, ITitle=Title1.

	cd_competence(competence(Code), Title) :-
		::get_where(W), !,
		format(atom(Q),
			'
			SELECT DISTINCT
				pc.ШифрКомпетенции, pc.Наименование
			FROM дсСтроки as l
			JOIN дсКомпетенцииДисциплины as cd
				ON cd.вкСтроки = l.пк
			JOIN дсКомпетенции as c
				ON cd.вкКомпетенции = c.пк
			JOIN дсКомпетенции as pc
				ON c.вкРодителя = pc.пк
			WHERE
				~w
			ORDER BY
				pc.ШифрКомпетенции;
			', [W]),
		% write(Q), nl,
		sql_connection::query(Q, row(Code1, Title1)),
		Code=Code1, Title=Title1.

	:- protected(get_where/1).
	:- mode(get_where(-atom), one).
	:- info(get_where/1, [
		comment is 'Create Query WHERE constraint depending known set of the object arguments',
		argnames is ['WhereExpression']
	]).

	get_where(Where) :-
		::get_where(['l.ДисциплинаКод'(_Code_), 'l.Дисциплина'(_Title_)], Where).

	:- protected(get_where/2).
	:- mode(get_where(+list, -atom), one).
	:- info(get_where/2, [
		comment is 'Create Query WHERE constraint depending known set of the first argument',
		argnames is ['ListOfOptions', 'WhereExpression']
	]).

	get_where([Option], 'TRUE') :-
		Option =.. [_Name, Value],
		var(Value), !.

	get_where([Option], E) :-
		Option =.. [Name, Value],
		nonvar(Value), !,
		format(atom(E), '~w = \'~w\'', [Name, Value]).

	get_where([Option | T], E) :-
		get_where([Option], E1),
		get_where(T, E2),
		format(atom(E), '~w AND ~w', [E1, E2]).

:- end_object.
