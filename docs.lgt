
:- protocol(documentp).
   :- public(gen/0).
   :- protected(subject_definition/0).
   :- protected(main_subject/0).
   :- protected(title/0).
   :- protected(signature/0).
:- end_protocol.

:- category(russianc,
   extends(options)).
   :- public(choice/3).
   choice(Gender, Variants, Variant):-
       Query =.. [Gender, Variant],
       ::option(Query,Variants).
:- end_category.


:- object(latex_renderer(_FileName_),
   imports(options)).
   :- use_module(library(lists), [member/2]).

   :- public(file_preamble/0).
   file_preamble:-
        ::run('\\documentclass[12pt]{scrreprt}'),
        ::cmd('pagestyle{empty}'),
        forall(::require_package(Package), ::run('\\usepackage{~w}', [Package])),
        forall(::require_package(Options, Package), ::run('\\usepackage~w{~w}', [Options, Package])),
        ::style_config,
        ::aux_preamble,
        ::begin_document.

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
        ::run('\\~w ', [Cmd]).

   :- public(run/1).
   run(String):-
		sanl(String),!,
        ::output_stream(O),
        format(O, '~w\n', [String]).

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
        format(O, '\\\\n', []).

   :- public(run_ln/2).
   run_ln(FormatString, Args):-
        run(FormatString, Args),
        ::output_stream(O),
        format(O, '\\\\n', []).

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

   :- public(nl/0).
   nl:-
    run_ln('').
   :- public(nl/1).
   nl(Size):-
        ::output_stream(O),
        format(O,'\\\\[~w]\n',[Size]).

   :- public(par/0).
   par:-
        ::cmd(par),
        ::output_stream(O),
        format(O,'\n',[]).

   :- public(vspace/1).
   vspace(Size):-
        ::output_stream(O),
        format(O,'\\vspace{~w}\n',[Size]).

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
        run_ln('\\includegraphics[~w]{~w}',[Options, Filename]).

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

   :- protected(require_package/2).
   require_package([final], hyperref).
   require_package([protrusion=false,expansion=false],microtype).
   require_package([russian,english], babel).

:- end_object.


:- object(documents(_Renderer_)).
   :- protected(start/0).
   start:-
        _Renderer_::open_stream,
        _Renderer_::file_preamble.

   :- protected(end/0).
   end:-
        _Renderer_::file_postamble,
        _Renderer_::close_stream.

   :- use_module(library(lists), [member/2]).
   :- public(gen/2).
   :- meta_predicate(gen(0,0)).
   gen(DataGoal, DocumentGoalList):-
        ::start,
        forall(
            call(DataGoal),
            (
                forall(member(Doc, DocumentGoalList),
                    (
                        (Doc == none ->
                             format('ERROR: Redefine run/0, supply list of documents to be generated to gen/2!',[]);
                             (
                                % debugger::trace,

                                Doc::gen -> _Renderer_::newpage
                                ;
                                _Renderer_::run('% Rendering failed ~w', [Doc])
                             )
                        )
                    ))
            )),
        ::end,
        format('% Files "~w" created.\n', [_Renderer_]).

   :- protected(gen/0).
   gen:-
	  % debugger::trace,
      gen(true, [none]).

:- end_object.


:- category(local_documentc(_Renderer_),
   implements(documentp)).

   gen:-
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
        _Renderer_::run('% ERROR: ~w', [Text]).
:- end_category.


:- category(long_tblrc(_Renderer_)).

   :- public(longtblr_style/0).
   longtblr_style:-
        R = _Renderer_,
        R::cmd('DefTblrTemplate{contfoot-text}{default}{}'),
        R::cmd('DefTblrTemplate{conthead-text}{default}{}'),
        R::cmd('DefTblrTemplate{caption-text}{default}{}'),
        R::cmd('DefTblrTemplate{caption-tag}{default}{}'),
        R::cmd('DefTblrTemplate{caption-sep}{default}{}'),
        true.

   :- private(table_header/0).
   table_header:-
        _Renderer_ = R,
        R::begin(longtblr,[
            '[caption={}]',
            '{',
            'width=1\\linewidth,rowhead=1,colspec={XXX}, row{1} = {l}, column{1} = {c}, column{3} = {l}}'
            ]).

   :- private(table_footer/0).
   table_footer:-
        _Renderer_ = R,
        R::end(longtblr).

:- end_category.
