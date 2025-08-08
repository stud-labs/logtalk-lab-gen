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

	:- public(type_title/1).
	:- mode(type_title(-atom), zero_or_one).
	:- info(type_title/1, [
		comment is 'Approval title like УТВЕРЖДАЮ',
		argnames is ['ApprovalTitle']
	]).

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

	:- public(date/1).
   :- mode(date(-atom), zero_or_one).
   :- info(date/1, [
      comment is 'Return approval date',
      argnames is ['Date']
   ]).

	:- public(number/1).
	:- mode(number(-atom), zero_or_one).
	:- info(number/1, [
		comment is 'Defines approval document number',
		argnames is ['DocumentNumber']
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

:- object(date_field(_Renderer_, _Date_),
	imports(exoptions)).

	:- public(draw/2).
	:- mode(draw(+atom, +list), one).
	:- info(draw/2, [
		comment is 'Draw date or <<__>> _____ 20__ г. if none',
		argnames is ['Width','OptionList']
	]).

	draw(Width, Options) :-
		D = _Date_,
		R = _Renderer_,
		::option(century(Y), Options, century('20')),
		::option(year_sign(YS), Options, year_sign('г.')),
		::option(undersore_after_century(U),
			Options,
			undersore_after_century(true)),
		::option(before(GoalBefore), Options, before(true)),
		R::makebox(Width,
			(
				call(GoalBefore),
				(D == none ->
					(R::run('<<~~'), R::underscore_fill('5mm'),
					 R::run('~~>>~~'), R::cmd(hrulefill),
					 R::run('~~'),
					 R::run('~w', [Y]),
					 R::run('~~'),
					 (U == true -> R::underscore_fill('5mm'); true),
					 (YS \= '' -> R::run('~~'), R::run(YS); true))
					 ;
					 D = DY-DM-DD,!,
					 (GoalBefore == true -> R::cmd(hfill); true),
					 % R::run('"~~'), R::run(DD),
					 % R::run('~~"~~'), R::run(DM),
					 % R::run('~~'),
					 % R::run('~w', [DY]),
					 % R::run('~~'),
					 % (YS \= '' -> R::run('~~'), R::run(YS); true))
					 R::run(DD),
					 R::run('.'),
					 R::run(DM),
					 R::run('.'),
					 R::run('~w', [DY]),
					 (YS \= '' -> R::run('~~'), R::run(YS); true)),
					 (GoalBefore \= true -> R::cmd(hfill); true)
				)).

:- end_object.

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

   draw_approval(Variant, Options) :-
		::approval(App),
		::draw_approval(App, Variant, Options).

   :- public(draw_approval/3).
   :- mode(draw_approval(+object, +atom, +list), zero_or_one).
   :- info(draw_approval/3, [
      comment is 'Draw approval',
      argnames is ['ApprovalObject', 'ModeOfRendering', 'OptionsOfRendering']
   ]).

   draw_approval(App, plain, Options) :-
      ::renderer(R),
		App::short_name(Name),
		(App::date(Date)->true; Date = none),
		(App::number(Number)->true; Number = none),
		App::position(Position),
		App::type_title(AAppTitle),
		!,
		::option(vspace(approval, SkipSize), Options, vspace(approval, none)),
      %R::begin(center),
		::option(title(approval, AppTitle), Options, title(approval, AAppTitle)),
		::option(width(Width), Options, width('\\linewidth')),
		R::run(AppTitle),
		% R::vspace('-1em'),
		R::par(SkipSize),
      %R::end(center),
		R::run(Position),
		R::par(SkipSize),
		R::makebox(Width,
			(R::cmd(hrulefill),
			 R::nbsp,
			 R::run(Name))
		),
		R::par(SkipSize),
		DF = date_field(R, Date),
		DF::draw(Width,
			[
				before((Number \= none ->
					R::run('Протокол №~~'),
					R::run(Number),
					R::run('~~от~~')
					;
					true))
			| Options]),
		true.

	% draw_approval(_, _).

:- end_category.

:- protocol(cd_title_pagep).

	:- public(document_title/1).
	:- mode(document_title(-atom), one).
	:- info(document_title/1, [
		comment is 'Name of document, eg. Course description',
		argnames is ['TitleOfDocument']
	]).

	:- public(discipline/2).
	:- mode(discipline(-atom, -atom), one).
	:- info(discipline/2, [
		comment is 'Returns discipline code abd title',
		argnames is ['DisciplineCode', 'DisciplineTitle']
	]).

	:- public(direction/2).
	:- mode(direction(-atom, -atom), one).
	:- info(direction/2, [
		comment is 'Returns direction code abd title',
		argnames is ['DirectionlineCode', 'DirectionTitle']
	]).

	:- public(profile/1).
	:- mode(profile(-atom), one).
	:- info(profile/1, [
		comment is 'Title of education program profile',
		argnames is ['ProfileTitle']
	]).

	:- public(qualification/1).
	:- mode(qualification(-atom), one).
	:- info(qualification/1, [
		comment is 'Return qualification of the student studying the discipline',
		argnames is ['QualificationTitle']
	]).

	:- public(education_type/1).
	:- mode(education_type(-atom), one).
	:- info(education_type/1, [
		comment is 'Return education type',
		argnames is ['EductionType']
	]).

	:- public(approved_by/1).
	:- mode(approved_by(-object), one).
	:- info(approved_by/1, [
		comment is 'Return approval department, person, date',
		argnames is ['ApprovalObject']
	]).

	:- public(recommended_by/1).
	:- mode(recommended_by(-object), one).
	:- info(recommended_by/1, [
		comment is 'Return recommending person, e.g. chair supervisor, date',
		argnames is ['ApprovalObject']
	]).

:- end_protocol.

:- category(cd_titlec,
   extends([partsc, exoptions, approvalc])).

	:- public(draw_cd_document_title/1).
	:- mode(draw_cd_document_title(+list), zero_or_one).
	:- info(draw_cd_document_title/1, [
		comment is 'Draws document type',
		argnames is ['ListOfOptions']
	]).

	draw_cd_document_title(Options) :-
		::cd_title_page(CD),
		CD::document_title(DTitle),
		CD::discipline(DiscCode, DiscTitle),
		CD::direction(DirCode, DirTitle),
		CD::profile(ProfileTitle),
		CD::qualification(Qual),
		CD::education_type(Type),
		!,
		::renderer(R),
		::option(vspace(cd_type, Size), Options, vspace(cd_type, '0.7em')),
		R::begin(center),
		R::boldface(
			R::run(DTitle)
		),
		R::nl(Size),
		R::vspace(Size),
		T = tabularx(R, Options),
		T::begin('\\linewidth', 'p{0.45\\linewidth}Xp{0.45\\linewidth}'),
		R::run('Наименование дисциплины (модуля):'),
		T::tab, T::tab,
		R::boldface((
			% R::cmd(raggedleft),
			R::run(DiscCode),
			R::run('~~'),
			R::run(DiscTitle)
		)),
		T::endrow,
		R::run('Направление подготовки:'),
		T::tab, T::tab,
		R::boldface((
			% R::cmd(raggedleft),
			R::run(DirCode),
			R::run('~~'),
			R::run(DirTitle)
		)),
		T::endrow,
		R::run('Направленность (профиль) подготовки:'),
		T::tab, T::tab,
		R::boldface((
			% R::cmd(raggedleft),
			R::run(ProfileTitle)
		)),
		T::endrow,
		R::run('Квалификация выпускника:'),
		T::tab, T::tab,
		R::boldface((
			% R::cmd(raggedleft),
			R::run(Qual)
		)),
		T::endrow,
		R::run('Форма обучения:'),
		T::tab, T::tab,
		R::boldface((
			% R::cmd(raggedleft),
			R::run(Type)
		)),
		R::vspace(Size),
		R::vspace(Size),
		T::endrow,
		AOptions = [width('\\linewidth') | Options],
		(CD::approved_by(AppBy) ->
		 ::draw_approval(AppBy, plain, AOptions); true),
		T::tab, T::tab,
		(CD::recommended_by(RecBy) ->
		 ::draw_approval(RecBy, plain, AOptions); true),
		T::end,
		R::end(center).

:- end_category.

:- protocol(disciplinep).

	:- public(title/2).
	:- mode(title(?atom, ?atom), zero_or_more).
	:- info(title/2, [
		comment is 'Returns discipline code and title',
		argnames is ['Code','Title']
	]).

:- end_protocol.

:- protocol(aims_problemsp).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-01,
		comment is 'Defines protocol for Discilpine Aims and problems'
	]).

	:- public(aim/1).
	:- mode(aim(-list), one).
	:- info(aim/1, [
		comment is 'Define set of aims, normally just one',
		argnames is ['AtomOrListOfSentences']
	]).

	:- public(problems/1).
	:- mode(problems(-list), one).
	:- info(problems/1, [
		comment is 'Define set of problems, normally many',
		argnames is ['AtomOrListOfSentences']
	]).

:- end_protocol.

:- category(enumerationc,
	extends(exoptions)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-01,
		comment is 'Various enumerators for single data'
	]).

	:- public(itemize_list/2).
	:- mode(itemize_list(+list, +list), zero_or_more).
	:- info(itemize_list/2, [
		comment is 'Draw list items as items of item list',
		argnames is ['List', 'Options']
	]).

	itemize_list(List, Options) :-
		is_list(List), !,
		::option(itemize(Environment), Options, itemize(itemize)),
		::renderer(R),
		R::begin(Environment),
		draw_list(R, List, Options),
		R::end(Environment).

	itemize_list(Atom, Options) :-
		(
			::option(single_as_list, Options) ->
			itemize_list([Atom], Options)
			;
			::renderer(R),
			draw_item(R, Atom, Options),
			R::par
		).

	:- use_module(library(lists), [member/2]).

	draw_list(Renderer, List, Options) :-
		forall(member(Item, List),
			(
				Renderer::cmd(item),
				draw_item(Renderer, Item, Options),
				Renderer::run_ln
			)
		).

	draw_item(Renderer, Item, Options) :-
		(
			::option(replace(Item, Shown), Options)
			-> true
		 	;
			Shown = Item
		),
		Renderer::run(' ~w', [Shown]).

:- end_category.

:- category(aims_problemsc,
	extends(enumerationc)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-01,
		comment is 'Defines routines drawing Discilpine Aims and problems'
	]).

	:- public(draw_aims_problems/2).
	:- mode(draw_aims_problems(+atom, +list), one).
	:- info(draw_aims_problems/2, [
		comment is 'Draws part with aims and problems of a CD',
		argnames is ['Style', 'Options']
	]).

	draw_aims_problems(plain, _Options) :-
		::cd_aims_problems(D),
		::renderer(R),
		R::section(1,'ЦЕЛИ И ЗАДАЧИ ДИСЦИПЛИНЫ (МОДУЛЯ)', 'sec:aims'),
		R::par,
		R::boldface(R::run('Цель: ')),
		D::aim(Aims),
		^^itemize_list(Aims, [itemize(itemize)]),
		R::boldface(R::run('Задачи: ')),
		D::problems(Problems),
		^^itemize_list(Problems, [itemize(enumerate)])
		.

:- end_category.


:- category(requirementsc,
	extends(exoptions)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-02,
		comment is 'Processes requirements section'
	]).

	:- public(draw_requirements/2).
	:- mode(draw_requirements(+atom, +list), zero_or_one).
	:- info(draw_requirements/2, [
		comment is 'Draws requirements section in document',
		argnames is ['Style', 'Options']
	]).

	:- use_module(user, [length/2]).
	:- use_module(library(lists), [member/2]).

	draw_requirements(plain, Options) :-
		::renderer(R),
		::cd_requirements(Req),
		R::section(1, 'ТРЕБОВАНИЯ К РЕЗУЛЬТАТАМ ОСВОЕНИЯ ДИСЦИПЛИНЫ'),
		R::run('Процесс освоения дисциплины направлен на формирование компетенций (элементов следующих компетенций) в соответствии с ФГОС ВО и ОП ВО по данному направлению подготовки.'),
		R::begin(center),
		R::boldface(R::run('Перечень планируемых результатов обучения по дисциплине (модулю), соотнесенных с индикаторами достижения компетенций')),
		R::end(center),
		T=longtblr(R, [hline(_), Options]),
		T::begin(
			[
				'caption=empty'
			],
			[
				'colspec={|X[18,l]|X[36,l]|X[60,l]|}'
				, 'width=\\linewidth'
				, 'cell{1}{1-3} = {c,cmd=\\bfseries}'
				, 'rowhead=1'
			]),
		R::run('Компетенция'), T::tab,
		R::run('Индикаторы компетенций'), T::tab,
		R::run('Результаты обучения'),
		forall(Req::competence(_Type, Comp),
			(
				findall(Ind, Comp::indicator(Ind), Indicators),
				length(Indicators, LI),
				Indicators = [FirstI | Others],
				T::endrow,
				R::run('\\SetCell[r=~w]{l}', [LI]),
				draw_catalog_entry(Comp, T, R),
				T::tab,
				draw_catalog_entry(FirstI, T, R),
				T::tab,
				draw_ksms(FirstI, T, R),
				forall(member(Ind, Others),
					(
						T::endrow,
						T::tab,
						draw_catalog_entry(Ind, T, R),
						T::tab,
						draw_ksms(Ind, T, R)
					)
				)
			)
		),
		T::end.
% https://mirror.funkfreundelandshut.de/latex/macros/latex/contrib/tabularray/tabularray.pdf

	draw_catalog_entry(Entry, _T, R) :-
		Entry::code(Code),
		Entry::title(Title),
		R::boldface(
			(
				R::cmd(noindent),
				R::run(Code)
			)
		),
		R::par,
		R::run(Title).

	draw_ksms(Ind, _T, R) :-
		%R::begin(itemize),
		forall(Ind::ksa(_Key, Value),
			(
				%R::cmd(item),
				R::run('•~~'),
				R::run(Value),
				R::par
			)
		),
		% R::end(itemize),
		true.

:- end_category.

:- category(displacementc,
	extends(enumerationc)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-08,
		comment is 'Category for drawing displacements'
	]).

	:- public(draw_displacement/2).
	:- mode(draw_displacement(+atom, +list), zero_or_one).
	:- info(draw_displacement/2, [
		comment is 'Draw displacenent section',
		argnames is ['Style', 'OptionList']
	]).

	draw_displacement(plain, _Options) :-
		::renderer(R),
		::cd_body(Body),
		R::section(1, 'МЕСТО ДИСЦИПЛИНЫ В СТРУКТУРЕ ОПОП ВО'),
		::discipline(Discilpine),
		Discilpine::title(DTitle),
		Body::part(BPart),
%		R::run('Учебная дисциплина (модуль) «Основы инженерного творчества» относится к части, формируемой участниками образовательных отношений «Блок 1. Дисциплины (модули)».'),
		R::run('Учебная дисциплина (модуль) «~w» относится к части, ~w».', [DTitle, BPart]),
		R::par,
		Body::annotation(Annotation),
		R::run(Annotation), R::par,
		R::run('Для изучения данной учебной дисциплины (модуля) необходимы знания, умения и навыки, формируемые предшествующими дисциплинами:'), R::par,
		Body::requires(Required),
		ItemizeOptions = [
			itemize(itemize),
			single_as_list,
			replace("none", 'нет')
		],
		^^itemize_list(Required, ItemizeOptions),
		R::run('Перечень последующих учебных дисциплин, для которых необходимы знания, умения и навыки, формируемые данной учебной дисциплиной:'), R::par,
		Body::ensures(Ensured),
		^^itemize_list(Ensured, ItemizeOptions),
		true.

:- end_category.

:- category(cd_contentc,
	extends(enumerationc)).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-08,
		comment is 'Category for drawing the content of the discipline'
	]).

	:- public(draw_cd_content/2).
	:- mode(draw_cd_content(+atom, +list), zero_or_one).
	:- info(draw_cd_content/2, [
		comment is 'Draws the main CD part of the content',
		argnames is ['Style', 'OptionList']
	]).

	draw_cd_content(plain, _Options) :-
		::renderer(R),
		R::section(1, 'СОДЕРЖАНИЕ И СТРУКТУРА ДИСЦИПЛИНЫ'),
		::discipline(D),
		D::hours(total, HTotal),
		D::hours(control, CtrlTotal),
		D::hours(pw, PWTotal),
		D::credits(total, CTotal),
		D::assessment(final, Assessment),
		R::run('Трудоемкость дисциплины составляет ~w зачетных единицы, ~w часов, в том числе ~w часов на контроль.',
		[CTotal, HTotal, CtrlTotal]),
		R::par,
		R::run('Из них реализуется с использованием электронного обучения и дистанционных образо вательных технологий ~w часа самостоятельной работы.', [PWTotal]),
		R::par,
		R::run('Форма промежуточной аттестации: ~w.', [Assessment]),
		R::section(2,'Содержание дисциплины, структурированное по темам, c указанием видов учебных занятий и СРС, отведенного на них количества академических часов'),

		::cd_body(Body),
		::cd_resources(Res),
		R::section(2, 'Методические указания по организации самостоятельной работы студентов'),
		(
			Body::pw_technique(Text)
			->
			R::run(Text)
			;
			Res::pw_technique(ResText),
			R::run(ResText)
		),
		R::par,

		true.



:- end_category.
