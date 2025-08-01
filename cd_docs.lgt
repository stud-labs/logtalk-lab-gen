

:- object(approval(
		_TypeTitle_,
		_Position_,
		_ShortName_,
		_Date_,
		_Number_
	),
	implements(approvalp)).

	type_title(_TypeTitle_).
	position(_Position_).
	short_name(_ShortName_).
	date(_Date_).
	number(_Number_).

:- end_object.


:- object(cd_title_page,
	implements(cd_title_pagep)).

	document_title('Рабочая программа дисциплины (модуля)').
	discipline('Б1.В.ДВ.01.01', 'Основы инженерного творчества').
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
		approval('Рекомендовано кафедрой информационных технологий',
			'зав. каф.',
			'Е.А. Черкашин',
			2025-05-31, 8)).

:- end_object.

:- object(cd_document(_Renderer_, _Discipline_),
	extends(document(_Renderer_)),
	imports([departmentc
			 , approvalc
			 , cd_titlec
			 , cd_aimsproblemsc
	])).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-07-21,
		comment is 'Draw course description document'
	]).

	:- public(department/1).
	:- mode(department(-object), zero_or_one).
	:- info(department/1, [
		comment is 'Defines department for the document',
		argnames is ['DepartmentObject']
	]).

	:- public(cd_title_page/1).
	:- mode(cd_title_page(-object), zero_or_one).
	:- info(cd_title_page/1, [
		comment is 'Defines title page data sources',
		argnames is ['CDTitlePageObject']
	]).


	% department(departement_imit).
	company_logo('isu-logo.png', [width='1.5cm']).

	department(cd_chair).
	approval(cd_approval).
	cd_title_page(cd_cd_title_page(_Discipline_)).
	cd_aims_problems(cd_aims_problems(_Discipline_)).

	draw:-
		::draw(plain,
			[
				add_line(ministry),
				vspace(ministry, '0.3em'),
				upcase(ministry),

				add_line(university),
				vspace(university, '0.3em'),
				upcase(university),

				add_line(institute),
				vspace(institute, '0.3em'),
				upcase(institute),

				add_line(department),
				upcase(department),
				vspace(department, '0.3em'),

				short_names(organization), % Did not implemented
				logo(document),
				vspace(after_logo, '0.3em'),

				vspace(approval, '0.7em'),
				% title(approval, 'УТВЕРЖДАЮ'),

				vspace(cd_type, '1em'),

				city('Иркутск', 2025)
			]).

	draw(plain, Options) :-
		% ^^draw_company_logo(centering, Options),
		^^draw_department_title(centering, Options),
		::renderer(R),
		T = tabularx(R, []),
		::option(width(Width), Options,
			width('\\columnwidth')),
		T::begin(Width,'XXX'),
		T::tab, T::tab,
		^^draw_approval(plain, Options),
		T::endrow,
		T::end,
		^^draw_cd_document_title(Options),
		::draw_city(Options),
		R::newpage,
		R::cmd(tableofcontents),
		R::newpage,
		^^draw_aims_problems(plain, Options)
		.

	:- protected(draw_city/1).
	:- mode(draw_city(+list), one).
	:- info(draw_city/1, [
		comment is 'Draw City -- Year at the end of the first page'
	]).

	draw_city(Options) :-
		::renderer(R),
		(::option(city(City, Year), Options) ->
			R::cmd(vfill),
			R::begin(center),
			R::run(City), R::run('~~--~~'), R::run(Year),
			R::run_ln,
			R::end(center); true ).

   :- public(connect_db/0).
   :- mode(connect_db, zero_or_one).
   :- info(connect_db/0, [
      comment is 'Connect PMI SQLIte database'
   ]).

	connect_db :-
		% debugger::trace,
		global::syllabus(path_name(PathName)),
		sql_connection::connect(PathName, _Conn).

	:- initialization(::connect_db).

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
		parameter(1, Code),
		parameter(2, Title),
		::get_where(['ДисциплинаКод'(Code), 'Дисциплина'(Title)], W), !,
		format(atom(Q), 'SELECT ДисциплинаКод, Дисциплина FROM дсСтроки WHERE ~w;', [W]),
		sql_connection::query(Q, row(Code1, Title1)),
		Code1 = Code, Title1 = Title.

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

:- object(cd_latex_renderer(_FileName_),
	extends(latex_renderer(_FileName_))).

	document_class(['12pt', '732'], studrep).

	setup(after_begin_document) :-
		::cmd('renewcommand{\\baselinestretch}{1.5}').

:- end_object.

:- object(cd_documents(_Discipline_),
		extends(documents)
	).

	gen :-
		forall(
			(
				::discipline(D),
				::file_name(D, FileName),
				R = cd_latex_renderer(FileName)
			),
			(
				^^start(R),
				Doc=cd_document(R, D),
				Doc::draw,
				^^end(R)
			)
		).

	:- protected(file_name/2).
	:- mode(file_name(+object, -atom), one).
	:- info(file_name/2, [
		comment is 'Figure out filename on the base of discipline data.',
		argnames is ['DisciplineObject', 'FileName.tex']
	]).

	file_name(Discilpine, FileName) :-
		Discilpine::title(Code, Title), !,
		format(atom(FileName), '~w-~w.tex',
			[Code, Title]).

	:- public(discipline/1).
	:- mode(discipline(-object), one).
	:- info(discipline/1, [
		comment is 'Defines discipline object for the document generation',
		argnames is ['DisciplineObject']
	]).

	discipline(_Discipline_).

	% :- initialization(::gen).
:- end_object.
