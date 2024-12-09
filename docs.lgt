
:- protocol(documentp).
   :- public(gen/0).
   :- protected(subjectDefinition/0).
:- end_protocol.

:- category(optionc).
   :- use_module(library(option), [option/2 as locoption]).

   % Example:   ::option(tag(Value), [tag(1), tag=2, other=foo, baz(foo)])
   % Results:   Value = 1, Value = 2.

   :- public(option/2).
   option(Query, List):-
       locoption(Query, List).

:- end_category.


:- category(russianc,
   extends(optionc)).
   :- public(choice/3).
   choice(Gender, Variants, Variant):-
       Query =.. [Gender, Variant],
       ::option(Query,Variants).
:- end_category.


:- object(latexRenderer(_FileName_),
   imports(optionc)).
   :- use_module(library(lists), [member/2]).

   :- public(preamble/0).
   preamble:-
        ::run('\\documentclass[12pt]{scrreprt}'),
        ::cmd('pagestyle{empty}'),
        forall(::requirePackage(Package), ::run('\\usepackage{~w}', [Package])),
        forall(::requirePackage(Options, Package), ::run('\\usepackage~w{~w}', [Options, Package])),
        ::styleConfig,
        ::auxPreamble,
        ::begin(document).

   :- public(postamble/0).
   postamble:-
        ::end(document).

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
        ::outputStream(O),
        format(O, '~w\n', [String]).
   :- public(run/2).
   run(FormatString, Args):-
        format(atom(S), FormatString, Args),
        run(S).

   :- public(runLn/1).
   runLn(String):-
        ::outputStream(O),
        format(O, '~w\\\\\n', [String]).

   :- public(runsLn/1).
   runsLn([]).
   runsLn([S|T]):-
        forall(member(L, [S|T]), runLn(L)).

   :- public(runs/1).
   runs([]).
   runs([S|T]):-
        forall(member(L, [S|T]), run(L)).

   :- public(runLn/2).
   runLn(FormatString, Args):-
        format(atom(S), FormatString, Args),
        runLn(S).

   :- public(begin/1).
   begin(Environment):-
        format(atom(S), 'begin{~w}', [Environment]),
        ::cmd(S).
   :- public(begin/2).
   begin(Environment,Args):-
        format(atom(S), 'begin{~w}', [Environment]),
        ::cmd(S),
        ::runs(Args).
   :- public(end/1).
   end(Environment):-
        format(atom(S), 'end{~w}', [Environment]),
        ::cmd(S).

   :- public(nl/0).
   nl:-
    runLn('').
   :- public(nl/1).
   nl(Size):-
        ::outputStream(O),
        format(O,'\\\\[~w]\n',[Size]).

   :- public(par/0).
   par:-
        ::cmd(par).

   :- public(vspace/1).
   vspace(Size):-
        ::outputStream(O),
        format(O,'\\vspace{~w}\n',[Size]).

   :- public(emptyLine/0).
   emptyLine:-
        vspace('1em').

   :- public(date/1).
   date(Date):-
        ::date(Date, String),
        ::run(String).
   :- public(date/2).
   date(YYYY-MM-DD, Output):-
        ::twodig(DD, D),
        ::twodig(MM, M),
        format(atom(Output), '~w.~w.~w',[D, M, YYYY]).

   :- protected(twodig/2).
   twodig(D, Output):-D>=10,!,
        format(atom(Output), '~w', [D]).
   twodig(D, Output):-
        format(atom(Output), '0~w', [D]).


   :- public(underscoreFill/1).
   underscoreFill(Size):-
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

   :- public(cuttingLine/0).
   cuttingLine:-
        ::cmd([noindent, dotfill]).
        % ::cmd([]).

   :- public(initAffiliationFlat/0).
   initAffiliationFlat:-
        ::cmd(noindent),
        ::begin(center),
        ::isuLogo('width=17mm'),
        ::runsLn(['МИНОБРНАУКИ РОССИИ',
                   'Федеральное государственное бюджетное образовательное учреждение высшего образования',
                   '«Иркутский государственный университет»',
                   '(ФГБОУ ВО «ИГУ»)',
                   'Институт математики и информационных технологий',
                   'б.~Гагарина, д.~20, Иркутск, 664003,',
                   'Тел.: (3952) 24-22-14, Факс: (3952) 24-39-63',
                   'ОКПО 02068226, ОГРН 1033801008218, ИНН/КПП 3808013278/380801001',
                   '\\texttt{http://www.math.isu.ru}, e-mail: \\texttt{ime@math.isu.ru}']),
        ::vspace('0.7em'),
        ::letterRegistration('25ex','12ex'),
        ::end(center).

   :- public(initAffiliation/0).
   initAffiliation:-
        ::initAffiliation([]).

   :- public(initAffiliation/1).
   initAffiliation(Options):-
        ::cmd(noindent),
        ::begin(tblr, ['{width=\\linewidth, colspec={X[6]X[5]}, column{1}={c}}']),
        %::begin(center),
        ::run('{'),
        ::cmd(footnotesize),
        ::isuLogo('width=13mm'),
        ::runsLn(['МИНОБРНАУКИ РОССИИ',
                   'Федеральное государственное бюджетное',
                   'образовательное учреждение',
                   'высшего образования',
                   '«Иркутский государственный университет»',
                   '(ФГБОУ ВО «ИГУ»)',
                   'Институт математики и информационных технологий',
                   'б.~Гагарина, д.~20, Иркутск, 664003,',
                   'Тел.: (3952) 24-22-14, Факс: (3952) 24-39-63',
                   'ОКПО 02068226, ОГРН 1033801008218,',
                   'ИНН/КПП 3808013278/380801001',
                   '\\texttt{http://www.math.isu.ru},',
                   'e-mail: \\texttt{ime@math.isu.ru}']),
        ::vspace('0.7em'),
        ::letterRegistration('25ex','12ex'),
        %::end(center),
        ::run('}'),
        ::tab,
        %(::option(center(Center), Options) -> call(Center); true),
        %::tab,
        (::option(right(Right), Options) -> call(Right); true),
        ::end(tblr).

   :- public(isuLogo/1).
   isuLogo(Option):-
        runLn('\\includegraphics[~w]{~w}',[Option, 'isu-logo.png']).

   :- public(styleConfig/0).
   styleConfig:-
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

   :- protected(letterRegistration/2).
      %_______________ № ________________
      %На № __________ от _______________
   letterRegistration(_AllSize, HruleSize):-
        % ::begin(tabularx,['{Size}{XX}']),
        ::underscoreFill('{18ex}'), ::run('№ '),  ::underscoreFill('{12ex}'), ::nl('0.5em'),
        ::run('На № '), ::underscoreFill(HruleSize), ::run(' от '), ::underscoreFill(HruleSize),
        %::end(tabularx).
        true.

   :- public(openStream/0).
   :- use_module(user, [open/4, close/1]).
   openStream:-
        ( _FileName_==user -> OutputStream=user;
          open(_FileName_, write, OutputStream, [alias(outputStream)])),
        ::assertz(outputStream(OutputStream)).

   :- public(closeStream/0).
   closeStream:-
        ::outputStream(O),
        ( O\=user -> close(O); true),
        ::retractall(outputStream(_)).

   :- protected(outputStream/1).
   :- dynamic(outputStream/1).

   :- protected(auxPreamble/0).
   auxPreamble.

   :- protected(requirePackage/1).
   %requirePackage(isudoc).
   requirePackage(longtable).
   requirePackage(tabularx).
   requirePackage(graphicx).
   requirePackage(geometry).
   requirePackage(indentfirst).
   requirePackage(luatextra).
   requirePackage('unicode-math').
   requirePackage(color).
   requirePackage(tabularray).

   :- protected(requirePackage/2).
   requirePackage([final], hyperref).
   requirePackage([protrusion=false,expansion=false],microtype).
   requirePackage([russian,english], babel).

:- end_object.


:- object(documents(_Renderer_)).
   :- protected(start/0).
   start:-
        _Renderer_::openStream,
        _Renderer_::preamble.

   :- protected(end/0).
   end:-
        _Renderer_::postamble,
        _Renderer_::closeStream.

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
                        (Doc==none ->
                             format('ERROR: Redefine run/0, supply list of documents to be generated to gen/2!',[]);
                             _Renderer_::newpage,
                             Doc::gen
                        )
                    ))
            )),
        ::end,
        format('% Files "~w" created.\n', [_Renderer_]).

   :- public(gen/0).
   gen:-
      gen(true, [none]).

:- end_object.
