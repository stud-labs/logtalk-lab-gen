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

   :- public(type/1).
   :- mode(type(-object), zero_or_more).
   :- info(type/1, [
      comment is 'Defines types of departement: institute, chair, etc.',
      argnames is ['TypeOfDepartment']
   ]).

:- end_protocol.

:- protocol(approvalp).

   :- public(short_name/1).
   :- mode(short_name(-atom), zero_or_one).
   :- info(short_name/1, [
      comment is 'Approval person short name. И.И.Ванов',
      argnames is ['PersonName']
   ]).

   :- public(position/1).
   :- mode(position(-atom), zero_or_one).
   :- info(position/1, [
      comment is 'Approval person work position',
      argnames is ['WorkPosition']
   ]).

	:- public(year_field/3).
   :- mode(year_field(-atom, -atom, -atom), zero_or_one).
   :- info(year_field/3, [
      comment is 'Year stub, like 20 __ г',
      argnames is ['YearDigits', 'PlaceholderNeeded', 'YearWord']
   ]).

:- end_protocol.

:- category(departmentc,
   extends([partsc, exoptions])).

   :- info([
       version is 1:0:0,
       author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
       date is 2025-07-21,
       comment is 'Category drawing headers of documants'
   ]).
   :- public(draw_department_title/2).
   :- mode(draw_department_title(+atom, +list), zero_or_one).
   :- info(draw_department_title/2, [
      comment is 'Draw the title of the department',
      argnames is ['ModeOfRendering', 'OptionsOfRendering']
   ]).

   draw_department_title(centering, Options) :-
	   (::option(logo(document), Options) ->
		 ::draw_company_logo(centering, Options); true),
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

   :- public(draw_company_logo/2).
   :- mode(draw_company_logo(+atom, +list), zero_or_one).
   :- info(draw_company_logo/2, [
      comment is 'Add company logo according to argunet values',
      argnames is ['ModeOfRendering', 'OptionsOfRendering']
   ]).

   draw_company_logo(centering, Options) :-
		::company_logo(LogoFileName, Opts),!,
      ::renderer(R),
      R::begin(center),
      R::include_graphics(Opts, LogoFileName),
		(::option(vspace(logo, Size), Options)->
		 R::vspace(Size); true),
      R::end(center).

	draw_company_logo(_, _).

	draw_title(Department, Renderer, Options) :-
		Department::title(Title), !,
		% debugger::trace,
		(Department::type(Type) -> true; Type=department),
      (::option(upcase(Type), Options) ->
			upcase_atom(Title, UTitle); UTitle = Title),
      (::option(add_line(Type), Options),
		 Department::parent(_)
		 ->
		 ::option(vspace(Type, Skip), Options, vspace(Type, '0.7em')),
		 Renderer::vspace(Skip)
		 ;
		 true),
      Renderer::run_ln(UTitle).

   draw_title_stack(Options) :-
      ::renderer(R),
      ::department(D),
      draw_parents(D, R, Options),
		draw_title(D, R, Options).

   draw_parents(Department, Renderer, Options):-
      Department::parent(Parent), !,
      draw_parents(Parent, Renderer, Options),
		draw_title(Parent, Renderer, Options).

   draw_parents(_, _, _).

:- end_category.

:- category(approvalc,
   extends([partsc, exoptions])).

   :- info([
       version is 1:0:0,
       author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
       date is 2025-07-23,
       comment is 'Category drawing headers of documants'
   ]).
   :- public(draw_approval/2).
   :- mode(draw_approval(+atom, +list), zero_or_one).
   :- info(draw_approval/2, [
      comment is 'Draw approval',
      argnames is ['ModeOfRendering', 'OptionsOfRendering']
   ]).

   draw_approval(semicentered, Options) :-
      ::renderer(R),
		::approval(App),
		App::short_name(Name),
		App::year_field(Y, U, YS),
		App::position(Position), !,
		T = tabularx(R, []),
		::option(vspace(approval, SkipSize), Options, vspace(approval, none)),
		T::begin('\\linewidth','XXX'),
		T::tab, T::tab,
      R::begin(center),
		::option(title(approval, AppTitle), Options, title(approval, 'Утверждаю')),
		R::run(AppTitle),
		R::vspace('-1em'),
		R::vspace(SkipSize),
      R::end(center),
		R::run(Position),
		R::par(SkipSize),
		R::cmd(noindent),
		R::makebox('\\linewidth',
			(R::cmd(hrulefill),
			 R::nbsp,
			 R::run(Name))
		),
		R::par(SkipSize),
		R::makebox('\\linewidth',
			(R::run('"~~'), R::underscore_fill('5mm'),
			 R::run('~~"~~'), R::cmd(hrulefill),
			 R::run('~~'),
			 R::run('~w', [Y]),
			 R::run('~~'),
			 (U == true -> R::underscore_fill('5mm'); true),
			 (YS \= '' -> R::run('~~'), R::run(YS); true))
		),
		T::endrow,
		T::end,
		true.

	% draw_approval(_, _).

:- end_category.


:- category(cdc,
   extends([partsc, exoptions])).

	:- public(draw_cd_document_type/1).
	:- mode(draw_cd_document_type(+list), zero_or_one).
	:- info(draw_cd_document_type/1, [
		comment is 'Draws document type',
		argnames is ['ListOfOptions']
	]).

	draw_cd_document_type(Options) :-
		::renderer(R),
		::option(vspace(cd_type, Size), Options, vspace(cd_type, '0.7em')),
		R::begin(center),
		R::boldface(
			R::run('Рабочая программа дисциплины (модуля)')),
		R::nl(Size),
		T = tabularx(R, Options),
		T::begin('\\linewidth', 'lp{1em}X'),
		R::run('Наименование дисциплины (модуля)'),
		T::tab, T::tab,
		R::run('asdasd'),
		T::endrow,
		T::end,
		R::end(center).



% Б1.В.ДВ.01.01 Основы инженерного
% творчества
% (индекс дисциплины по учебному плану, наименование дисциплины
% (модуля))
% Направление подготовки:
% 01.03.02 Прикладная математика и
% информатика
% (код, наименование направления подготовки)
% Направленность (профиль) подготовки:
% Искусственный интеллект и системная
% аналитика
% Квалификация выпускника: бакалавр
% Форма обучения: очная

:- end_category.
