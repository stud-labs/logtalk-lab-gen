
:- object(minobr,
	implements(departmentp)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-07-21,
		comment is 'Defines MinObrNauki of RF'
	]).

	title("МИНОБРНАУКИ РОСИИ").

:- end_object.



:- object(university_isu,
	implements(departmentp)).

	title('ФЕДЕРАЛЬНОЕ ГОСУДАРСТВЕННОЕ ОБРАЗОВАТЕЛЬНОЕ УЧРЕЖДЕНИЕ ВЫСШЕГО ОБРАЗОВАНИЯ «ИРКУТСКИЙ ГОСУДАРСТВЕННЫЙ УНИВЕРСИТЕТ»').

	parent(cd_ministry).

:- end_object.


:- object(approval,
	implements(approvalp)).

	type_title('УТВЕРЖДАЮ').
	short_name('И. И. Иванов').
	position('директор организации').
	date(none).
	% no number.

:- end_object.

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

:- object(institute_imit,
	implements(departmentp)).

	title('ИНСТИТУТ МАТЕМАТИКИ И ИНФОРМАЦИОННЫХ ТЕХНОЛОГИЙ').

	parent(university_isu).

:- end_object.


:- object(test_doc(_Renderer_),
	extends(document(_Renderer_)),
	imports([departmentc, approvalc, cd_titlec])).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-07-21,
		comment is 'Draw simple document'
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
	cd_title_page(cd_cd_title_page).

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
		::draw_city(Options).

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
%		sql_connection::connect("/home/eugeneai/projects/text/docent/isu/2025/cirricullum-2025/pmi", _).
		sql_connection::connect("/home/eugeneai/projects/text/docent/isu/2025/cirricullum/pmi", _).
%		sql_connection::connect("/home/eugeneai/projects/text/docent/isu/2025/dev/cirricullum-2025/pmi", _).

	:- initialization(::connect_db).

:- end_object.


:- object(documents_test_simple,
	extends(documents(latex_renderer("docs_test_simple.tex")))
	% extends(documents(latex_renderer(user)))
	).

	gen :-
		::renderer(R),
		::gen(true, [test_doc(R)]).

	:- initialization(::gen).
:- end_object.
