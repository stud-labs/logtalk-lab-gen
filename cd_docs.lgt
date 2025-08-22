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
			 , aims_problemsc
			 , yamlc
			 , requirementsc
			 , displacementc
			 , cd_contentc
			 , cd_final_partc
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

	:- public(cd_aims_problems/1).
	:- mode(cd_aims_problems(-object), zero_or_one).
	:- info(cd_aims_problems/1, [
		comment is 'Defines aim and problems data sources',
		argnames is ['CDAimsProblemsObject']
	]).

	:- public(cd_requirements/1).
	:- mode(cd_requirements(-object), zero_or_one).
	:- info(cd_requirements/1, [
		comment is 'Defines requirements aspect for the discipline',
		argnames is ['CDRequirimentsObject']
	]).

	% department(departement_imit).
	company_logo('isu-logo.png', [width='1.5cm']).

	department(cd_chair).
	approval(cd_approval).
	cd_title_page(cd_cd_title_page(_Discipline_)).
	cd_aims_problems(cd_aims_problems(_Discipline_)).
	cd_requirements(cd_requirements(_Discipline_)).

	:- protected(cd_crm/1).
	cd_crm(y_crm(YAML)) :-
		::yaml_dom(crm, YAML).

	:- protected(cd_resources/1).
	cd_resources(y_resources(YAML)) :-
		::yaml_dom(resources, YAML).

	:- protected(cd_body/1).
	cd_body(y_body(YAML)) :-
		::yaml_dom(body, YAML).

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
		::renderer(R),
		^^draw_department_title(centering, Options),
		::connect_yaml(resources, resources),
		_Discipline_::title(Title),
		% debugger::trace,
		::connect_yaml(body(
			content/disciplines/(title=Title)/file),
			body),
		T = tabularx(R, []),
		::option(width(Width), Options,
			width('\\columnwidth')),
		T::begin(Width,'XXX'),
		T::tab, T::tab,
		^^draw_approval(plain, Options),
		T::end,
		R::vspace('2em'),
		^^draw_cd_document_title(Options),
		R::cmd(vfill),
		::draw_city(Options),
		R::newpage,
		R::cmd(tableofcontents),
		R::newpage,
		^^draw_aims_problems(plain, Options),
		^^draw_displacement(plain, Options),
		^^draw_requirements(plain, Options),
		^^draw_cd_content(plain, Options),

		^^draw_final_part(plain, Options),
		true.

	:- protected(draw_city/1).
	:- mode(draw_city(+list), one).
	:- info(draw_city/1, [
		comment is 'Draw City -- Year at the end of the first page'
	]).

	draw_city(Options) :-
		::renderer(R),
		(::option(city(City, Year), Options) ->
			R::begin(center),
			R::run(City), R::run('~~--~~'), R::run(Year),
			R::run_ln,
			R::end(center); true ).

   :- protected(connect_db/0).
   :- mode(connect_db, zero_or_one).
   :- info(connect_db/0, [
      comment is 'Connect PMI SQLIte database'
   ]).

	connect_db :-
		global::syllabus(path_name(PathName)),
		sql_connection::connect(PathName, _Conn).

	:- protected(connect_yaml/2).
	:- mode(connect_yaml(+atom, +atom), zero_or_one).
	:- info(connect_yaml/2, [
		comment is 'Connect adressed YAML data',
		argnames is ['YAMLSelector', 'StorageKey']
	]).

	% connect_yaml(Query, Key) :-
	% 	global::connect_yaml(Query,
	% 		path_name(PathName)),
	% 	^^yaml_load(PathName, YAML),
	% 	yamls::yaml_dom(Key, YAML).

	% :- use_module(user, [loc_eq/2]).

	connect_yaml(Query, Key) :-
		connect_yaml(Query, Key, =).

	:- protected(connect_yaml/3).
	:- mode(connect_yaml(+atom, +atom, +compound), zero_or_one).
	:- info(connect_yaml/3, [
		comment is 'Connect adressed YAML data with application of a Goal/2 as preprocessing',
		argnames is ['YAMLSelector', 'StorageKey', 'PreprocessingGoal']
	]).

	:- meta_predicate(connect_yaml(*,*,2)).

	connect_yaml(Query, Key, Goal) :-
		global::connect_yaml(Query,
			path_name(PathName)),
		^^yaml_load(PathName, YAML),
		call(Goal, YAML, PYAML),
		yamls::yaml_dom(Key, PYAML).

	:- protected(yaml_dom/2).
	:- mode(yaml_dom(+atom, -atom), zero_or_one).
	:- info(yaml_dom/2, [
		comment is 'Get a YAML with a context',
		argnames is ['ContextAtom', 'YAMLAtom']
	]).

	yaml_dom(Key, YAML) :-
		yamls::current_yaml_dom(Key, YAML).

	:- initialization(
			(
				::connect_db,
				::connect_yaml(crm, crm)
			)
		).

	:- public(discipline/1).
	:- mode(discipline(-object), one).
	:- info(discipline/1, [
		comment is 'Returns Discipline object',
		argnames is ['DisciplineObject']
	]).

	discipline(_Discipline_).

	:- public(atom_title/2).
	:- mode(atom_title(?atom, ?atom), zero_or_one).
	:- info(atom_title/2, [
		comment is 'Juxtapose atom to its screen title',
		argnames is ['AtomName', 'ScreenName']
	]).

	atom_title(Atom, Title) :-
		_Discipline_::atom_title(Atom, Title).

:- end_object.

:- object(cd_latex_renderer(_FileName_),
	extends(latex_renderer(_FileName_))).

	document_class(['12pt', '732'], syllabus).

	% require_package([final], hyperref).
	% require_package([protrusion=false,expansion=false],microtype).
	% require_package([russian,english], babel).
	require_package([], none).

	require_package(Package) :-
		^^require_package(Package).
	require_package(ninecolors).

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
