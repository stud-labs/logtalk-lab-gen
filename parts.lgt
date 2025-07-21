:- category(partsc).

   :- info([
       version is 1:0:0,
       author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
       date is 2025-07-21,
       comment is 'Category of parts of documents'
   ]).

   :- public(draw/2).
   % :- mode(draw, one).
   :- info(draw/2, [
       comment is 'Draw contents on canvas'
   ]).
   draw(_,_):-
      self(Self),
      ::renderer(R),
      R::run_ln('ERROR: Define a category descendant for \\uscore{~w}~n', [Self]).

:- end_category.

:- protocol(departmentp).

   :- info([
       version is 1:0:0,
       author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
       date is 2025-07-21,
       comment is 'Protocol of Departement, which can have a parent department.'
   ]).

   :- public(title/1).
   :- mode(title(-atom), one).
   :- info(title/1, [
      comment is 'Return the title of organization',
      argnames is ['OrganizationTitle']
   ]).

   :- public(logo/2).
   :- mode(logo(-atom, -atom), one).
   :- info(logo/2, [
      comment is 'Returns logo file and its size',
      argnames is ['OrganizationLogo.png', 'Width of the logo']
   ]).

   :- public(parent/1).
   :- mode(parent(-object), zero_or_one).
   :- info(parent/1, [
      comment is 'Defines wether the department has a parent departement.',
      argnames is ['ParentDepartement']
   ]).

:- end_protocol.

:- category(departmentc,
   extends([partsc, options])).

   :- info([
       version is 1:0:0,
       author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
       date is 2025-07-21,
       comment is 'Category drawing headers of documants'
   ]).
   :- public(draw_department_title/2).
   :- mode(draw_department_title(-atom, -list), zero_or_one).
   :- info(draw_department_title/2, [
      comment is 'Draw the title of the department',
      argnames is ['ModeOfRendering', 'OptionsOfRendering']
   ]).

   draw_department_title(centering, Options) :-
      ::renderer(R),
      R::begin(center),
      ::draw_title_stack(Options),
      R::end(center).

   :- protected(draw_title_stack/1).
   :- mode(draw_title_stack(+list), one).
   :- info(draw_title_stack/1, [
       comment is 'Draw names of department stacked',
       argnames is ['ListOfOptions']
   ]).


	draw_title(Title, Renderer, Options) :-
      (::option(upcase(departments), Options) ->
			upcase_atom(Title, UTitle); UTitle = Title),
      (::option(add_line(departments), Options) -> Renderer::vspace('0.7em'); true),
      Renderer::run_ln(UTitle).

   draw_title_stack(Options) :-
      ::renderer(R),
      ::department(D),
      draw_parents(D, R, Options),
      D::title(Title),
		draw_title(Title, R, Options).

   draw_parents(Department, Renderer, Options):-
      Department::parent(Parent), !,
      draw_parents(Parent, Renderer, Options),
      Parent::title(Title),
		draw_title(Title, Renderer, Options).

   draw_parents(_, _, _).

:- end_category.
