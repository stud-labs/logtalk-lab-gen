:- object(group_anon,
   extends(studentGroup(02223-'ДМ'))).

   el(maz, student([nom='Мазаева Оксана Анатольевна',
                    dat='Мазаевой Оксане Анатольевне'], f, '****-**-**')).
:- end_object.


:- object(notes_anon,
   implements(notesp)).
   % 55 +44+4555  545 5+5+55555
   % 55 +55+5555  545 4+4+44455
   % 55 +55+5555  545 5+5+55555
   notes(maz,
    [
        predp-4
    ]).
:- end_object.


% ---- Goal ----

:- object(documents_anon).
   :- use_module(library(lists), [member/2]).
   :- public(gen/0).
   gen :-
	true.
   c:-
       % debugger::trace,
       forall(member(FileName,['doc-anon.tex']),  % add/replace by user to screen output
       % forall(member(FileName,[user]),  % add/replace by user to screen output
              potanin_documents(latex_renderer(FileName),
                   notes_anon, group_anon,
                   curriculum2023,
                   fal)::gen).

   :- initialization(gen).
:- end_object.
