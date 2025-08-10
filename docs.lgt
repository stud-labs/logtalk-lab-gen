:- protocol(documentp).
	:- public(gen/0).
	:- protected(subject_definition/0).
	:- protected(main_subject/0).
	:- protected(title/0).
	:- protected(signature/0).
	:- protected(company_logo/2).
	:- protected(approval/1).
:- end_protocol.

:- category(exoptions).

   :- public(option/2).
   :- mode(option(+atopm, +list), zero_or_more).
   :- info(option/2, [
      comment is 'Behaves as standard option/2, but respects repetative options.',
      argnames is ['Option', 'ListOfOptions']
   ]).

	:- use_module(library(lists), [member/2]).

	option(_, NotAList) :-
		\+ is_list(NotAList), !,
		throw(type_error(NotAList,'is not a list')).

	option(Option, Options) :-
   	member(Option, Options), !.

   :- public(option/3).
   :- mode(option(+atom, +list, +atom), zero_or_more).
   :- info(option/3, [
      comment is 'Behaves as standard option/3 (with default), but respects repetative options.',
      argnames is ['Option', 'ListOfOptions', 'DefaultOption']
   ]).

	option(Option, Options, _) :-
   	member(Option, Options), !.

	option(Default, _Options, Default).

:- end_category.


:- category(russianc,
	extends(exoptions)).
	:- public(choice/3).
	choice(Gender, Variants, Variant):-
		Query =.. [Gender, Variant],
		::option(Query,Variants),!.
:- end_category.


:- object(latex_renderer(_FileName_),
	imports(exoptions)).
	:- use_module(library(lists), [member/2]).

	:- public(file_name/1).
	:- mode(file_name(-atom), one).
	:- info(file_name/1, [
		comment is 'Returns file name of the stream',
		argnames is ['FileName']
	]).

	file_name(_FileName_).

	:- public(file_preamble/0).
	file_preamble:-
		::document_class(ClassOptions, Class),
		::run('\\documentclass~w{~w}~n', [ClassOptions, Class]),
		::cmd('pagestyle{plain}'),
		forall(
			::require_package(Package),
			(
				Package \= none ->
					::run('\\usepackage{~w}~n', [Package]) ; true
			)),
		forall(
			::require_package(Options, Package),
			(
				Package \= none ->
					::run('\\usepackage~w{~w}~n', [Options, Package]) ; true
			)),
		::style_config,
		::aux_preamble,
		::begin_document,
		::cmd('thispagestyle{empty}'),
		::setup_ign(after_begin_document).

	:- public(document_class/2).
	:- mode(document_class(-list, -atom), one).
	:- info(document_class/2, [
		comment is 'Define options and documentclass dontent',
		argnames is ['Options', 'DocumentClass']
	]).

	document_class(['12pt'],scrreprt).

	:- public(setup/1).
	:- mode(setup(+atom), zero_or_one).
	:- info(setup/1, [
		comment is 'Add text at some points',
		argnames is ['SetupPointTerm']
	]).

	:- public(setup_ign/1).
	:- mode(setup_ign(+atom), one).
	:- info(setup_ign/1, [
		comment is 'Try run setup at point, ignore it it did not defined',
		argnames is ['SetupPointTerm']
	]).

	setup_ign(Term) :-
		(::setup(Term) -> true; true).

	:- public(begin_document/0).
	begin_document:-
		::begin(document).

	:- public(end_document/0).
	end_document:-
		::end(document).

	:- public(file_postamble/0).
	file_postamble:-
		::end_document.

	:- public(newpage/0).
	newpage:-
		::cmd(newpage).

	:- public(cmd/1).
	cmd([]).
	cmd([Cmd|T]):-
		cmd(Cmd),
		cmd(T).
	cmd(Cmd):-
		::run('\\~w~n', [Cmd]).

	:- public(cmd/2).
	cmd(Pattern, Parameters):-
		format(atom(S), Pattern, Parameters),
		::run('\\~w', [S]).

	:- public(run/1).
	run(Number):-
		number(Number), !,
		::output_stream(O),
		format(O, '~w', [Number]),
		true.

	run(String):-
		::output_stream(O),
		format(O, String, []),
		true.

	run([]).
	run([S|T]):-
		forall(member(L, [S|T]), run_ln(L)).

	sanl(S):-
		string(S),!.
	sanl(S):-
		atom(S),!.

	:- public(run/2).
	run(FormatString, Args):-
		format(atom(S), FormatString, Args),
		run(S).

	:- public(run_ln/1).
	run_ln(String):-
		  run(String),
		::output_stream(O),
		format(O, '\\\\~n', []).

	:- public(run_ln/2).
	run_ln(FormatString, Args):-
		run(FormatString, Args),
		::output_stream(O),
		format(O, '\\\\~n', []).

	:- public(begin/1).
	begin(Environment):-
		format(atom(S), 'begin{~w}', [Environment]),
		::cmd(S).
	:- public(begin/2).
	begin(Environment,Args):-
		format(atom(S), 'begin{~w}', [Environment]),
		::cmd(S),
		::run(Args).
	:- public(end/1).
	end(Environment):-
		format(atom(S), 'end{~w}', [Environment]),
		::cmd(S).

	:- public(run_ln/0).
	:- mode(run_ln, one).
	:- info(run_ln/0, [
		comment is 'Add to the target text just newline'
	]).

	run_ln :-
		::output_stream(O),
		format(O,'~n', []).

	:- public(nbsp/0).
	:- mode(nbsp, one).
	:- info(nbsp/0, [
		comment is 'Put non-breakable-sign'
	]).

	nbsp :-
		 run('~~').

	:- public(nl/0).
	nl:-
	 run('\\\\~n').

	:- public(nl/1).
	nl(Size):-
		::output_stream(O),
		(Size \= none ->
			format(O,'\\\\[~w]~n',[Size])
			;
			format(O,'\\\\~n', [])
		).

	:- public(par/0).
	par:-
		::cmd(par),
		::output_stream(O),
		format(O,'\n',[]).

	:- public(par/1).
	par(Size):-
		::cmd(par),
		::vspace(Size),
		::output_stream(O),
		format(O,'\n',[]).

	:- public(vspace/1).
	vspace(Size):-
		(Size \= none ->
			::output_stream(O),
			format(O,'\\vspace{~w}~n',[Size])
		; true).

	:- public(boldface/1).
	:- meta_predicate(boldface(0)).
	boldface(Goal):-
		::other_face(bfseries, Goal).

	:- public(other_face/2).
	:- meta_predicate(other_face(*, 0)).
	other_face(Face, Goal) :-
		::run("{"), ::cmd(Face),
		call(Goal),
		::run("}").

	:- public(empty_line/0).
	empty_line:-
		vspace('1em').

	:- public(date/1).
	date(Date):-
		::date(Date, String),
		::run(String).
	:- public(date/2).
	date(YYYY-MM-DD, Output):-
		::two_dig(DD, D),
		::two_dig(MM, M),
		format(atom(Output), '~w.~w.~w',[D, M, YYYY]).

	:- protected(two_dig/2).
	two_dig(D, Output):-D>=10,!,
		format(atom(Output), '~w', [D]).
	two_dig(D, Output):-
		format(atom(Output), '0~w', [D]).

	:- public(underscore_fill/1).
	underscore_fill(Size):-
		::run('\\makebox[~w]{\\hrulefill}',[Size]).

	:- public(makebox/2).
	:- meta_predicate(makebox(*, 0)).
	makebox(Size, Goal):-
		::run('\\makebox[~w]{',[Size]),
		call(Goal),
		::run('}').

	:- public(tab/0).
	tab:-
		run('&').

	:- public(hfill/0).
	hfill:-
		::cmd(hfill).

	:- public(vfill/0).
	vfill:-
		::cmd(vfill).

	:- public(strut/0).
	strut:-
		::cmd(null).

	:- public(benv/0).
	benv:-
		::run('{').
	:- public(eenv/0).
	eenv:-
		::run('}').

	:- public(cutting_line/0).
	cutting_line:-
		::cmd([noindent, dotfill]).
		% ::cmd([]).

	:- public(include_graphics/2).
	include_graphics(Options, Filename):-
		run_ln('\\includegraphics~w{~w}',[Options, Filename]).

	:- public(style_config/0).
	style_config:-
		::cmd('defaultfontfeatures{Ligatures={TeX,Required},Scale=MatchLowercase}'),
		::cmd('geometry{paper=a4paper,includehead, left=1.5cm, right=1.5cm, top=0.0cm, bottom=1.5cm}'),
		% ::cmd('geometry{paper=a4paper,showframe, includehead, left=1.5cm, right=1.5cm, top=0.0cm, bottom=1.5cm}'),
		::run('\\let\\headwidth=\\textwidth'),
		::cmd('setmainfont[Scale=1,ItalicFont=timesi.ttf,BoldFont=timesbd.ttf,BoldItalicFont=timesbi.ttf]{times.ttf}'),
		%::cmd('setmonofont[Numbers=SlashedZero,Scale=1,ItalicFont=couri.ttf,BoldFont=courbd.ttf,BoldItalicFont=courbi.ttf,]{cour.ttf}'),
		::cmd('setmonofont[Numbers=SlashedZero,Scale=1]{Fira Code Regular}'),
		::cmd('setsansfont[Scale=1,ItalicFont=Fira Sans Italic,BoldFont=Fira Sans Bold,BoldItalicFont=Fira Sans Bold Italic,]{Fira Sans Regular}'),
		::cmd('newcounter{mytableline}'),
		::run('\\DeclareUrlCommand\\uscore{\\urlstyle{tt}}'),
		::run('
			\\DefTblrTemplate{contfoot-text}{default}{}
			\\DefTblrTemplate{conthead-text}{default}{}
			\\DefTblrTemplate{caption}{default}{}
			\\DefTblrTemplate{conthead}{default}{}
			\\DefTblrTemplate{capcont}{default}{}
		'),
		true.

	:- public(open_stream/0).
	:- use_module(user, [open/4, close/1]).
	open_stream:-
		( _FileName_==user -> OutputStream=user;
			 open(_FileName_, write, OutputStream, [alias(outputStream)])),
		::assertz(output_stream(OutputStream)).

	:- public(close_stream/0).
	close_stream:-
		::output_stream(O),
		( O\=user -> close(O); true),
		::retractall(output_stream(_)).

	:- protected(output_stream/1).
	:- dynamic(output_stream/1).

	:- protected(aux_preamble/0).
	aux_preamble.

	:- protected(require_package/1).
	%require_package(isudoc).
	require_package(longtable).
	require_package(tabularx).
	require_package(graphicx).
	require_package(geometry).
	require_package(indentfirst).
	require_package(luatextra).
	require_package('unicode-math').
	require_package(color).
	require_package(tabularray).
	require_package(url).

	:- protected(require_package/2).
	require_package([final], hyperref).
	require_package([protrusion=false,expansion=false],microtype).
	require_package([russian,english], babel).

	:- public(section/3).
	:- mode(section(+integer, +atom, +atom), one).
	:- info(section/3, [
		comment is 'Starts section of a level with a label.',
		argnames is ['Level', 'Title', 'Label']
	]).

	section(Level, Title, Label) :-
		section_name(Level, SectionName),
		::cmd('~w{~w}', [SectionName, Title]),
		(Label \= none ->
			::cmd('label{~w}', [Label]) ; true),
		::run_ln.

	section_name(1, section).
	section_name(2, subsection).
	section_name(3, subsubsection).
	section_name(4, subsubsubsection).
	section_name(5, paragraph).
	section_name(6, subparagraph).

	:- public(section/2).
	:- mode(section(+integer, +atom), one).
	:- info(section/2, [
		comment is 'Starts section of a level without labels.',
		argnames is ['Level', 'Title']
	]).

	section(Level, Title) :-
		section_name(Level, SectionName),
		::cmd('~w{~w}', [SectionName, Title]),
		::run_ln.

:- end_object.


:- object(document(_Renderer_),
	implements(documentp)).

	:- info([
		 version is 1:0:0,
		 author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		 date is 2025-07-21,
		 comment is 'A general root of documents. Draws itself on _Renderer_'
	]).

	:- public(renderer/1).
	% :- mode(renderer/1, one).
	:- info(renderer/1, [
		 comment is 'Returns the _Renderer_'
	]).

	renderer(_Renderer_).

	:- public(draw/0).
	% :- mode(draw, one).
	:- info(draw/0, [
		 comment is 'Draws itself on _Renderer_'
	]).

	draw:-
		self(Self),
		::renderer(R),
		R::run_ln('ERROR: Document descendant of \\uscore{~w} is not defined', [Self]).
:- end_object.

:- set_logtalk_flag(suspicious_calls, silent).

:- object(documents).

	:- protected(start/1).
	start(Renderer):-
		Renderer::open_stream,
		Renderer::file_preamble.

	:- protected(end/1).
	end(Renderer):-
		Renderer::file_postamble,
		Renderer::close_stream.

	:- use_module(library(lists), [member/2]).
	:- public(gen/2).
	:- meta_predicate(gen(0,0)).
	gen(DataGoal, ListOrGoal):-
		forall(
			call(DataGoal),
				(
					% call(DocumentGoal),
					forall(
						member(Doc, ListOrGoal),
						(
							(Doc == none ->
								format('ERROR: Redefine run/0, supply list of documents to be generated to gen/2!',[])
								;
								(
									Doc::renderer(R),
									Doc::draw -> R::newpage
									;
									R::run('% Rendering failed ~w', [Doc])
								)
							)
						)
					)
				)
			).

	:- public(gen/0).
	gen:-
		% debugger::trace,
		% gen(true, (
		% 	format('ERROR: Redefine run/0, supply list of documents to be generated to gen/2!',[]),
		% 	false
		% ))
		gen(true, [none])
		.

:- end_object.

:- set_logtalk_flag(suspicious_calls, warning).

:- category(local_documentc,
	implements(documentp)).

	draw:-
		::support,
		::title,
		::main_subject,
		::signature.

	% default definitions to sign errors.
	:- public(support/0).

	support:-
		err("No support section defined").
	title:-
		err("No title defined").
	main_subject:-
		err("No main_subject defined").
	signature:-
		err("No signature section defined").

	:- protected(err/1).
	err(Text):-
		::renderer(R),
		R::run('% ERROR: ~w', [Text]).
:- end_category.

:- category(long_tblrc).

	:- public(longtblr_style/0).
	longtblr_style:-
		::renderer(R),
		R::cmd('DefTblrTemplate{contfoot-text}{default}{}'),
		R::cmd('DefTblrTemplate{conthead-text}{default}{}'),
		R::cmd('DefTblrTemplate{caption-text}{default}{}'),
		R::cmd('DefTblrTemplate{caption-tag}{default}{}'),
		R::cmd('DefTblrTemplate{caption-sep}{default}{}'),
		true.

	:- private(table_header/0).
	table_header:-
		::renderer(R),
		R::begin(longtblr,[
			'[caption={}]',
			'{',
			'width=1\\linewidth,rowhead=1,colspec={XXX}, row{1} = {l}, column{1} = {c}, column{3} = {l}}'
			]).

	:- private(table_footer/0).
	table_footer:-
		::renderer(R),
		R::end(longtblr).

:- end_category.

:- object(tabular(_Renderer_, _Options_),
	imports(exoptions)).

	:- protected(env_name/1).
	:- mode(env_name(-atom), one).
	:- info(env_name/1, [
		comment is 'Name of the environment',
		argnames is ['NameOfEnvironment']
	]).

	env_name(tabular).

	:- public(begin/2).
	:- mode(begin(+atom, +atom), one).
	:- info(begin/2, [
		comment is 'Draw table header.',
		argnames is ['Width', 'ColumnDefinition']
	]).

	begin(Width, Columns) :-
		R = _Renderer_,
		format(atom(Args), '{~w}{~w}', [Width, Columns]),
		::env_name(EnvName),
		R::begin(EnvName, Args),
		cond_hline.

	:- public(cond_hline/0).
	:- mode(cond_hline, one).
	:- info(cond_hline/0, [
		comment is 'Add cmd(hline) if _Options_ has hline(..)'
	]).

	cond_hline :-
		::env_name(EnvName),
		(::option(hline(EnvName), _Options_) ->
			::hline; true).

	:- public(endrow/0).
	:- mode(endrow, one).
	:- info(endrow/0, [
		comment is 'Ends row'
	]).

	endrow :-
		R = _Renderer_,
		R::nl,
		cond_hline.

	:- public(hline/0).
	:- mode(hline, one).
	:- info(hline/0, [
		comment is 'Add hline to the table'
	]).

	hline :-
		_Renderer_::cmd(hline),
		_Renderer_::run_ln.

	:- public(tab/0).
	:- mode(tab, one).
	:- info(tab/0, [
		comment is 'Add tab character, a column divisor'
	]).

	tab :-
		 _Renderer_::run(" & ").

	:- public(tab/1).
	:- mode(tab(+integer), one).
	:- info(tab/1, [
		comment is 'Draws a number of tabs',
		argnames is ['Number']
	]).

	tab(1) :-
		::tab.
	tab(N) :-
		N > 1,
		tab(1),
		N1 is N-1,
		tab(N1).

	:- public(end/0).
	:- mode(end, one).
	:- info(end/0, [
		comment is 'Finishes tabularx environment',
		argnames is []
	]).

	end :-
		::endrow,
		::env_name(EnvName),
		_Renderer_::end(EnvName).

:- end_object.

:- object(tabularx(_Renderer_, _Options_),
   extends(tabular(_Renderer_, _Options_))).

	env_name(tabularx).

:- end_object.

:- object(longtable(_Renderer_, _Options_),
   extends(tabular(_Renderer_, _Options_))).

	env_name(longtable).

:- end_object.

:- object(tblr(_Renderer_, _Options_),
   extends(tabular(_Renderer_, _Options_))).

	:- public(begin/2).
	:- mode(begin(+list, +list), one).
	:- info(begin/2, [
		comment is 'Draw *tblr header',
		argnames is ['TableParameterList',
					'TableSpecificationList']
	]).

	:- use_module(library(lists), [member/2]).

	begin(Parameters, Specifications) :-
		R = _Renderer_,
		::env_name(EnvName),
		R::begin(EnvName),
		R::run('['),R::run_ln,
		forall(member(Par, Parameters),
			(
				R::run('~w,',[Par]), R::run_ln
			)
		),
		R::run("]"),
		R::run('{'),R::run_ln,
		forall(member(Spec, Specifications),
			(
				R::run('~w,',[Spec]), R::run_ln
			)
		),
		R::run("}"), R::run_ln,
		^^cond_hline.

	env_name(tblr).

	:- public(set_cell/2).
	:- mode(set_cell(+list, +list), one).
	:- info(set_cell/2, [
		comment is 'Constructs SetCell[]{} structure',
		argnames is ['AuxiliaryOptions', 'MandatoryOptions']
	]).

	set_cell(AOptions, MOptions) :-
		R = _Renderer_,
		R::run('\\SetCell['),
		forall(member(O, AOptions),
			(::draw_option(O), R::run(','))),
		R::run(']{'),
		forall(member(O, MOptions),
			(::draw_option(O), R::run(','))),
		R::run('}').

	:- public(set_cells/2).
	:- mode(set_cells(+list, +list), one).
	:- info(set_cells/2, [
		comment is 'Constructs SetCells[]{} structure',
		argnames is ['AuxiliaryOptions', 'MandatoryOptions']
	]).

	set_cells(AOptions, MOptions) :-
		R = _Renderer_,
		R::run('\\SetCells['),
		forall(member(O, AOptions),
			(::draw_option(O), R::run(','))),
		R::run(']{'),
		forall(member(O, MOptions),
			(::draw_option(O), R::run(','))),
		R::run('}').

	:- protected(draw_option/1).
	:- mode(draw_option(+atom), one).
	:- info(draw_option/1, [
		comment is 'Option'
	]).

	draw_option(cmd=Cmd) :-!,
		_Renderer_::run('cmd=\\~w', [Cmd]).
	draw_option(A=B) :-!,
		_Renderer_::run('~w=~w', [A,B]).
	draw_option(A-B) :-!,
		_Renderer_::run('~w-~w', [A,B]).
	draw_option(O) :-
		_Renderer_::run(O).

	:- public(rotatebox/2).
	:- mode(rotatebox(+integer, +goal), one).
	:- info(rotatebox/2, [
		comment is 'Draw text rotated',
		argnames is ['Degrees', 'Goal']
	]).

	:- meta_predicate(rotatebox(*,0)).

	rotatebox(Degree, String) :-
		String = run(Run), !,
		R = _Renderer_,
		R::run('\\rotatebox{~w}{~w}', [Degree, Run]).

	rotatebox(Degree, Goal) :-
		R = _Renderer_,
		R::run('\\rotatebox{~w}{', [Degree]),
		call(Goal),
		R::run('}').

	:- public(bfcell/1).
	:- mode(bfcell(+atom), one).
	:- info(bfcell/1, [
		comment is 'Produce pfseries cell if String',
		argnames is ['String']
	]).

	bfcell(String) :-
		set_cell([c=1],[cmd=bfseries]),
		_Renderer_::run(String).

:- end_object.

:- object(longtblr(_Renderer_, _Options_),
   extends(tblr(_Renderer_, _Options_))).

	env_name(longtblr).

:- end_object.
