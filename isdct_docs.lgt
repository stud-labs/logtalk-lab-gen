
% :- category()


:- category(isdct_supportc(_Renderer_, _InstData_)).
   :- public(support/0).
   support :-
           % R = _Renderer_,
           % I = _InstData_,
           ::tableHeader,
           trunl('\\textbf{УТВЕРЖДАЮ}',[]),
           trunl('\\textbf{}',[]),
           trunl('\\textbf{Оценка}',[]),
           ::tableFooter.

   trunl(FormatString, Args):-
        R = _Renderer_,
        R::tab,
        R::tab,
        R::run(FormatString, Args),
        R::nl.

   :- private(tableHeader/0).
   tableHeader:-
        _Renderer_ = R,
        % ::longtblrStyle(default),
        R::begin(longtblr,[
            '[caption={}]',
            '{',
            'width=1\\linewidth,rowhead=1,colspec={XXX}, row{1} = {l}, column{1} = {c}, column{3} = {l}}'
            ]).

   :- private(tableFooter/0).
   tableFooter:-
        _Renderer_ = R,
        R::end(longtblr).

:- end_category.


:- object(permission_customs(_Renderer_, _InstData_),
    imports([
          isdct_supportc(_Renderer_, _InstData_)
        , local_documentc(_Renderer_)
    ]),
    implements(documentp)).

    main_subject:-
        R = _Renderer_,
        R::run("Main Subject"), R::nl.

    title:-
        R = _Renderer_,
        R::run("Tile").

:- end_object.
