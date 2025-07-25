
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
	implements(approvalp),
	imports(approvalc)).

	short_name('И. И. Иванов').
	position('директор организации').
	year_field('20', true, 'г.').

:- end_object.


:- object(institute_imit,
	implements(departmentp)).

	title('ИНСТИТУТ МАТЕМАТИКИ И ИНФОРМАЦИОННЫХ ТЕХНОЛОГИЙ').

	parent(university_isu).

:- end_object.


:- object(test_doc(_Renderer_),
	extends(document(_Renderer_)),
	imports([departmentc, approvalc])).

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

	% department(departement_imit).
	company_logo('isu-logo.png', [width='1.5cm']).

	department(cd_chair).
	approval(approval).

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
				title(approval, 'УТВЕРЖДАЮ')
			]).

	draw(plain, Options) :-
		% ^^draw_company_logo(centering, Options),
		^^draw_department_title(centering, Options),
		^^draw_approval(semicentered, Options).

   :- public(connect_db/0).
   :- mode(connect_db, zero_or_one).
   :- info(connect_db/0, [
      comment is 'Connect PMI SQLIte database'
   ]).

	connect_db :-
%		sql_connection::connect("/home/eugeneai/projects/text/docent/isu/2025/cirricullum-2025/pmi", _).
		sql_connection::connect("/home/eugeneai/projects/text/docent/isu/2025/cirricullum/pmi", _).

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
