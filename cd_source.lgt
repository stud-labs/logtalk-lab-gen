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

:- object(yamls).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-06,
		comment is 'Contain yaml connections reflecting an ID to a dictionary'
	]).

	:- public(yaml_dom/2).
	:- mode(yaml_dom(+atom, +atom), one).
	:- info(yaml_dom/2, [
		comment is 'Add a YAML dom identifying by an id term',
		argnames is ['ID', 'YamlDOM']
	]).

	yaml_dom(Key, Tree) :-
		retractall(yaml(Key, _)),
		assertz(yaml(Key, Tree)).

	:- protected(yaml/2).
	:- dynamic(yaml/2).
	:- mode(yaml(?atom, +atom), zero_or_more).
	:- info(yaml/2, [
		comment is 'Database predicate for storing YAML nodes',
		argnames is ['ID', 'Tree']
	]).

	:- public(current_yaml_dom/2).
	:- mode(current_yaml_dom(?atom, -atom), zero_or_more).
	:- info(current_yaml_dom/2, [
		comment is 'Enumerates YAMLs in the database',
		argnames is ['ID', 'Tree']
	]).

	current_yaml_dom(Key, Tree) :-
		::yaml(Key, Tree).

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

	:- protected(type/3).
	:- mode(type(?type, ?atom, ?string), zero_or_one).
	:- info(type/3, [
		comment is 'Convert type to its title',
		argnames is ['TypeSymbol', 'AtomTitle', 'StringTitle']
	]).

	type(pk, 'ПК', "ПК").
	type(ok, 'ОК', "ОК").
	type(uk, 'УК', "УК").

:- end_object.

:- protocol(ksap).

	:- public(ksa/2).
	:- mode(ksa(?atom, ?atom), zero_or_more).
	:- info(ksa/2, [
		comment is 'Returns knowledge-skills-ability object',
		argnames is ['Key', 'Value']
	]).

:- end_protocol.

:- object(y_indicator(_Type_, _CompIndex_, _YAML_),
	implements([ksap, catalog_entryp]),
	extends(yaml_object(_YAML_))).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-01,
		comment is 'Description'
	]).

	code(tuple(_Type_, N, M)) :- !,
		_CompIndex_ = tuple(N, M).

	code(Code) :-
		::type(_Type_, RuType, _),
		code(tuple(_Type_, N, M)),
		format(atom(Code),
			'~w-~w.~w', [RuType, N, M]).

	:- use_module(library(lists), [member/2]).

	ksa(Key, Value) :-
		member(Key, ['knows','able','skill']),
		::yaml(Key, Values),
		enumerate(Values, Value).

	enumerate(List, Value) :-
		is_list(List), !,
		member(Value, List).

	enumerate(Value, Value).

:- end_object.

:- protocol(indicatorp).

	:- public(indicator/1).
	:- mode(indicator(-object), one_or_more).
	:- info(indicator/1, [
		comment is 'Defines indicator for a competence',
		argnames is ['IndicatorObject']
	]).

:- end_protocol.

:- object(y_competence(_Type_, _YAML_),
	implements([indicatorp, catalog_entryp]),
	extends(yaml_object(_YAML_))).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-01,
		comment is 'Competence of a CRM'
	]).

	:- use_module(library(lists),
			[
				member/2,
				nth1/3
			]).

	code(tuple(_Type_,NCode)) :- !,
		::yaml(code, NCode).

	code(Code):-
		::type(_Type_, RuType, _),
		code(tuple(_Type_, NCode)),
		format(atom(Code), '~w-~w', [RuType, NCode]).

	title(Title):-
		::yaml(title, Title).

	indicator(y_indicator(_Type_,
			tuple(NCode, N), Index)) :-
		::yaml(indices, List),
		::yaml(code, NCode),
		nth1(N, List, Index).

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

	competence(Type, y_competence(Type, X)) :-
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
	implements([catalog_entryp, ksap])).

	title(_Title_).
	code(_Code_).

	ksa('knows', 'Знает, что делать.').

:- end_object.

:- object(y_cd_indicator(_Code_, _Title_),
	implements([catalog_entryp, ksap])).

	title(_Title_).
	code(_Code_).

	ksa(SynCode, KTitle) :-
		yamls::current_yaml_dom(crm, CRM),
		split(CType, _RuType, CN, IN),
		YCRM=y_crm(CRM),
		YCRM::competence(CType, Comp),
		% format(atom(CCode),
		%	'~w-~w', [RuType, CN]),
		Comp::code(tuple(CType, CN)),
		Comp::indicator(Ind),
		Ind::code(tuple(CType, CN, IN)), !,
		% debugger::trace,
		Ind::ksa(SynCode, KTitle).

	:- use_module(library(pcre), [re_matchsub/3]).

	split(Type, RuType, CN, IN) :-
	   re_matchsub('([ПУО]К)-(\\d+)\\.(\\d+)', _Code_, D),
		CNS=D.get(2),
		number_string(CN, CNS),
		INS=D.get(3),
		number_string(IN, INS),
		RuType = D.get(1),
		type(RuType, Type).

	type("ПК", pk).
	type("ОК", ok).
	type("УК", uk).

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

	indicator(y_cd_indicator(Code, Title)) :-
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
	implements([disciplinep, catalog_entryp]),
	imports(exoptions)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-07-27,
		comment is 'Defines discipline object connected to the databse.'
	]).

	title(Title) :-
		title(_Code, Title).

	code(Code) :-
		title(Code, _Title).

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

	:- protected(column/2).
	:- mode(column(+symbol, -symbol), zero_or_one).
	:- info(column/2, [
		comment is 'Query and return various data on the discipline',
		argnames is ['FieldName', 'FieldValue']
	]).

	column(number(Name), Value) :-!,
		column(Name, StringValue),
		(
			number(StringValue)
			->
			Value=StringValue
			;
			number_string(Value, StringValue)
		).

	column(Name, Value) :-
		::get_where(W), !,
		format(atom(Q),
			'
			SELECT DISTINCT
				l.~w
			FROM дсСтроки as l
			WHERE
				~w ;
			', [Name, W]),
		sql_connection::query(Q, row(Value1)),
		Value=Value1.

	:- protected(cd_value/2).
	:- mode(cd_value(+atom, -number), zero_or_more).
	:- info(cd_value/2, [
		comment is 'Return value from main data table',
		argnames is ['FieldName', 'FieldValue']
	]).

	value(number(Name), Value) :-!,
		value(Name, StringValue),
		(
			number(StringValue)
			->
			Value=StringValue
			;
			number_string(Value, StringValue)
		).

	value(Name, Value) :-
		(
			v_convert(Name, FieldName) -> true
			;
			FieldName=Name
		),
		::get_where(W), !,
		format(atom(Q),
			'
			SELECT DISTINCT
				v.Количество
			FROM дсСтроки as l
			JOIN дсНовыеЧасы as v ON v.вкОбъекта = l.пк
			JOIN дсВидыРабот as w ON v.вкВидаРаботы = w.пк
			WHERE
				~w AND
				w.Аббревиатура=\'~w\';
			', [W, FieldName]),
		sql_connection::query(Q, row(Value1)),
		Value=Value1.

	v_convert(total, 'Итого').
	v_convert(exam, 'Эк').
	v_convert(credit, 'За').
	v_convert(grade_credit, 'ЗаО').
	v_convert(vkr, 'ВКР').
	v_convert(course_work, 'КР').
	v_convert(control_work, 'К').
	v_convert(home_control_work, 'ДКР').
	v_convert(grade, 'Оц').
	v_convert(essay, 'Эс').
% Реф
% РГР
% Др
	v_convert(zet, 'ЗЕТ').
% Нед
% Руководство
% Рецензирование
% Внешнее рецензирование
% Консультации
% Председатель
% Член комиссии
% Секретарь
% Лекции
% Нормоконтроль
% Дежурство
	v_convert(lection, 'Лек').
	v_convert(labwork, 'Лаб').
	v_convert(practice, 'Пр').
	v_convert(seminary, 'Сем').
% ИЗ
% КСР
	v_convert(pw, 'СР').
	v_convert(control, 'КО').
% ГУ
% ГЗ
% ТЗ
% КШУ
% КоР
% КРП
% СРП
% Кл
% Мет
% ВИБ
% Конф
% МГЗ
% ИП
% Конс
% Ауд
% КО
% Переатт

	:- public(hour_class/2).
	:- mode(hour_class(?atom, ?atom), zero_or_more).
	:- info(hour_class/2, [
		comment is 'Classes of hours definition',
		argnames is ['Class', 'HourAtom']
	]).

	hour_class(contact, labwork).
	hour_class(contact, lection).
	hour_class(contact, practice).
	hour_class(contact, seminary).
	hour_class(personal, pw).
	hour_class(assessment, exam).
	hour_class(assessment, credit).
	hour_class(assessment, grade_credit).

	:- public(hours/2).
	:- mode(hours(+atom, -number), zero_or_more).
	:- info(hours/2, [
		comment is 'Returns various hour data',
		argnames is ['ParameterName', 'NumericValue']
	]).

	hours(Atom, Value) :-
		value(Atom, Value).

	:- public(credits/2).
	:- mode(credits(+atom, -number), zero_or_more).
	:- info(credits/2, [
		comment is 'Return credit data',
		argnames is ['ParameterName', 'NumericValue']
	]).

	credits(total, Value) :-
		value(number(zet), Value).

	:- public(assessment/2).
	:- mode(assessment(+atom, -atom), zero_or_more).
	:- info(assessment/2, [
		comment is 'Return assessment type depending context',
		argnames is ['ContextAtom', 'Value']
	]).

	assessment(final, 'экзамен') :-
		value(number(exam),_),!.
	assessment(final, 'зачет') :-
		value(number(credit),_),!.
	assessment(final, 'зачет с оценкой') :-
		value(number(grade_credit),_),!.

% SELECT мдrowOrder, вкООП, СчитатьБезЗЕТ, ДисциплинаКод, Оценка, РассредПрактика, ЗЕТфакт, ЧасыВарМакс, Дисциплина, Адаптационная, СкрытьВРПД, ТипОбъекта, DVnotEquals, ТрудоемкостьКредитов, пк, вкПлана, ЧасыВарАуд, ПодлежитИзучениюЧасов, Multiselect, ВидОбъекта, ТипПерезачета, ВидПрактики, NotCalcInSumKont, НестандартНедПрактики, Номер, ЧасовПоПлану, вкРодителя, ЧасовПоЗЕТ, вкКафедры, дгid, НеСчитатьКонтроль, Порядок, ЧасовВЗЕТ, ReadOnly, вкБлока, Свернуть, УровеньВложения, ПерезачтеноЧасовАуд, СчитатьВПлане, ПризнакФизкультуры, ЗаСчетПолевых FROM дсСтроки;

% SELECT Курс, мдrowOrder, Недель, дгid, ТипКомиссии, *вкВидаРаботы*, *вкОбъекта*, Дней, Количество, Семестр, вкТипаЧасов, Переаттестовано, пк, Сессия FROM дсНовыеЧасы;

% SELECT мдrowOrder, *Аббревиатура*, дгid, Аудиторный, вкТипРабот, HasPrPreparing, HasInter, Отображать, Назначение, Контактный, *пк*, HasDistr, *Название* FROM дсВидыРабот;

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

:- protocol(bodyp).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-08,
		comment is 'Describe interface of body of CD'
	]).

	:- public(annotation/1).
	:- mode(annotation(-string), zero_or_one).
	:- info(annotation/1, [
		comment is 'Return annotatopn if any',
		argnames is ['AnnotationString']
	]).

	:- public(part/1).
	:- mode(part(-string), zero_or_one).
	:- info(part/1, [
		comment is 'Return a kind of discipline, wrther it is standard or local',
		argnames is ['String']
	]).

	:- public(requires/1).
	:- mode(requires(-atom), zero_or_one).
	:- info(requires/1, [
		comment is 'Return list or item describing required disiplines',
		argnames is ['ListOrItem']
	]).

	:- public(ensures/1).
	:- mode(ensures(-atom), zero_or_one).
	:- info(ensures/1, [
		comment is 'Return list or item describing ensured disiplines',
		argnames is ['ListOrItem']
	]).

	:- public(pw_technique/1).
	:- mode(pw_technique(-atom), zero_or_one).
	:- info(pw_technique/1, [
		comment is 'Return text of technique description',
		argnames is ['Text']
	]).

:- end_protocol.


:- object(y_body(YAML),
	extends(yaml_object(YAML)),
	implements(bodyp)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-08,
		comment is 'View for CD body'
	]).

	annotation(Annotation) :-
		::yaml(cd/preface/annotation, Annotation).

	part('формируемой участниками образовательных отношений «Блок 1. Дисциплины (модули)').

	requires(Required) :-
		displacement(Node),
		::yaml_path(Node, requires, Required).

	ensures(Ensured) :-
		displacement(Node),
		::yaml_path(Node, ensures, Ensured).

	displacement(Node) :-
		::yaml(cd/preface/displacemet, Node).

:- end_object.


:- object(y_resources(_YAML_),
	extends(yaml_object(_YAML_)),
	implements(bodyp)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-08,
		comment is 'Defines common resources for CDs'
	]).

	pw_technique(Text) :-
		::yaml(
			cd/pw/
			handbook/content, Text).

:- end_object.

:- protocol(workp).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-10,
		comment is 'Protocol describing work done during education'
	]).

	:- public(hours/1).
	:- mode(hours(-number), one).
	:- info(hours/1, [
		comment is 'Hours spent to this work',
		argnames is ['NumberOfHours']
	]).

	:- public(part/1).
	:- mode(part(-atom), zero_or_more).
	:- info(part/1, [
		comment is 'Work part can / to be done',
		argnames is ['WorkTitle']
	]).

	:- public(competence/1).
	:- mode(competence(-atom), zero_or_more).
	:- info(competence/1, [
		comment is 'Competence related to the work',
		argnames is ['Competence']
	]).

	:- public(test/1).
	:- mode(test(-atom), zero_or_more).
	:- info(test/1, [
		comment is 'Describe test, which indicates competence ',
		argnames is ['Test']
	]).

:- end_protocol.

:- object(y_work(_YAML_),
	implements([catalog_entryp, workp]),
	extends(yaml_object(_YAML_))).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-10,
		comment is 'Represents a work (practice, personal, laboratory)'
	]).

	:- use_module(library(lists), [member/2]).

	competence(code(Code)) :-
		::yaml(competence, List),
		member(Code, List).

	test(Test) :-
		::yaml(test, Test).

	part(Part) :-
		member(Key, [work, parts]),
		::yaml(Key, Work),
		(
			is_list(Work)
			->
			member(Part, Work)
			;
			Part = Work
		).

	hours(Hours) :-
		::yaml(hours, String),
		(
			number(String)
			->
			String = Hours
			;
			number_string(Hours, String)
		).

:- end_object.

:- protocol(hour_tablep).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-09,
		comment is 'Interface for hour table components'
	]).

	:- public(section/6).
	:- mode(section(+compound, -string, -integer,
							-list, -integer, -integer),
							zero_or_more).
	:- info(section/6, [
		comment is 'Describes a level row of table',
		argnames is ['Level', 'SectionTitle', 'Semester',
							'MainHourList', 'PersonalWorkHours',
							'ControlHours']
	]).

:- end_protocol.


:- object(y_hour_table(_Body_, _Discipline_),
	implements(hour_tablep)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-09,
		comment is 'Describe topic-hour table'
	]).

	:- use_module(library(lists), [member/2]).

	section(1-Topic, Title, 1, [Le,Se,Co], Pw, 0) :-
		B = _Body_,
		B::yaml(cd/content, Topics), !,
		member(Topic, Topics),
		B::yaml_path(Topic, title, Title),
		(
			B::yaml_path(Topic, hours, [Le, Se, Co, Pw])
		->
			true
			;
			[Le, Se, Co, Pw] = ['-', '-', '-', '-']
		).

	section(2-Topic, Title, 1, [Le,Se,Co], Pw, 0) :-
		B = _Body_,
		B::yaml_path(Topic, content, Topics), !,
		member(T, Topics),
		(
			string(T)
			->
			Title = T
			;
			B::yaml_path(T, title, Title)
		),
		qh(B, T, pw, Pw, '-'),
		qh(B, T, preface, Se, '-'),
		qh(B, T, lection, Le, '-'),
		qh(B, T, consult, Co, '-'),
		true.

	qh(Body, Topic, Key, Value, _Default) :-
		Body::yaml_path(Topic, Key, Work),
		y_work(Work)::hours(Value), !.
	qh(_, _, _, Default, Default).

:- end_object.
