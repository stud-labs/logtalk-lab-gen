
% :- category()


:- category(isdct_supportc(_Renderer_, _InstData_),
   extends(long_tblrc(_Renderer_))).
   :- public(support/0).
   support :-
           R = _Renderer_,
           % I = _InstData_,
           ::longtblr_style,
           ::table_header,
           trunl('{УТВЕРЖДАЮ}',[]),
           _InstData_::director(DName, DPos, DDegree),
           _InstData_::shortname(ISName),
           trunl('{~w ~w ~w}',[DPos, ISName, DDegree]),
           trunl('{Оценка}',[]),
           ::table_footer.

   trunl(FormatString, Args):-
        R = _Renderer_,
        R::tab,
        R::tab,
        R::run(FormatString, Args),
        R::nl.

:- end_category.

:- category(isdct_signaturec(_Renderer_),
   extends(long_tblrc(_Renderer_))).

:- end_category.

:- object(permission_customs(_Renderer_, _InstData_, _Whom_Where_),
    imports([
          isdct_supportc(_Renderer_, _InstData_)
        , local_documentc(_Renderer_)
%        , longtblr_style(_Renderer_)
    ]),
    implements(documentp)).

    main_subject:-
        R = _Renderer_,
        R::run("Main Subject"), R::nl.

    title:-
        R = _Renderer_,
        R::run("Tile").

:- end_object.
