
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

:- object(institute_imit,
	implements(departmentp)).

	title('ИНСТИТУТ МАТЕМАТИКИ И ИНФОРМАЦИОННЫХ ТЕХНОЛОГИЙ').

	parent(university_isu).

:- end_object.


:- object(test_doc(_Renderer_),
	extends(document(_Renderer_)),
	imports(departmentc)).

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
	department(cd_institute).

	draw:-
		::draw(plain,
			[add_line(departments),
			upcase(departments),
			vskip(departments, '0.3em')]).

	draw(plain, Options) :-
		% debugger::trace,
		^^draw_department_title(centering, Options).

   :- public(connect_db/0).
   :- mode(connect_db, zero_or_one).
   :- info(connect_db/0, [
      comment is 'Connect PMI SQLIte database'
   ]).

	connect_db :-
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
