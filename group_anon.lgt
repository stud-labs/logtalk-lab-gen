:- object(group_anon,
   extends(studentGroup(02299-'ДМ'))).

   el(maz, student([nom='Иванова Наталья Александровна',
                    dat='Ивановой Наталье Александровне'], f, 1987-05-30)).
   el(kur, student([nom='Петрова Елена Максимовна',
                    dat='Петровой Елене Максимовне'], f, 1983-12-12)).
   el(pop, student([nom='Сидоров Николай Петрович',
                    dat='Сидорову Николаю Петровичу'], m,  1999-09-13)).
:- end_object.


:- object(notes_anon,
   implements(notesp)).
   % 55 +44+4555  545 5+5+55555
   % 55 +55+5555  545 4+4+44455
   % 55 +55+5555  545 5+5+55555
   notes(kur,
    [
        akm-5,
        is-5,

        fl1-ok,
        com-4,
        cmm-4,
        tpc-ok,
        iot-4,
        ddb-5,
        ml-5,
        spr-5,

        dis-5,
        sw-4,
        ssp-5,

        ais-5,
        fl2-ok,
        rec-5,
        rnd-ok,
        nn-5,
        ds-5,
        dda-5,
        kp1-5,
        ppr-5
    ]).
   notes(maz,
    [
        akm-5,
        is-5,

        fl1-ok,
        com-5,
        cmm-5,
        tpc-ok,
        iot-5,
        ddb-5,
        ml-5,
        spr-5,

        dis-5,
        sw-4,
        ssp-5,

        ais-4,
        fl2-ok,
        rec-4,
        rnd-ok,
        nn-4,
        ds-4,
        dda-4,
        kp1-5,
        ppr-5
    ]).
   notes(pop,
    [
        akm-5,
        is-5,

        fl1-ok,
        com-5,
        cmm-5,
        tpc-ok,
        iot-5,
        ddb-5,
        ml-5,
        spr-5,

        dis-5,
        sw-4,
        ssp-5,

        ais-5,
        fl2-ok,
        rec-5,
        rnd-ok,
        nn-5,
        ds-5,
        dda-5,
        kp1-5,
        ppr-5
    ]).
:- end_object.


% ---- Goal ----

:- object(documents_anon).
   :- use_module(library(lists), [member/2]).
   :- public(gen/0).
   gen :-
       % debugger::trace,
       forall(member(FileName,['doc-anon.tex']),  % add/replace by user to screen output
       % forall(member(FileName,[user]),  % add/replace by user to screen output
              potaninDocuments(latexRenderer(FileName),
                   notes_anon, group_anon,
                   curriculum2023,
                   fal)::gen).

   :- initialization(gen).
:- end_object.
