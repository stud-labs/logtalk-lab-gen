:- category(isdct_data).
   :- public(director/3).
   :- public(departmentone/3).
   :- public(expertheader/3).

   director('И.В.Бычков', 'Директор', 'академик').
   departmentone('Л.Ф.Зеленова', none, none).
   expertheader('А.Г.Феоктистов', 'Заместитель директора по научной работе', 'д.т.н.')
:- end_category.


:- category(isdct_support(_Renderer_, _InstData_)).
   :- public(support/0).
   support :-
           R = _Renderer_,
           I = _InstData_,
           ::tableHeader,
           trun('\\textbf{УТВЕРЖДАЮ} '), R::nl,
           trun('\\textbf{} '), R::nl,
           trun('\\textbf{Оценка} '), R::nl,
           ::tableFooter.

   trun(Arg):-
        R = _Renderer_,
        R::tab,
        R::tab,
        R::run(Arg),
        R::

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

:- object(permission_customs(_Renderer_),
    imports([
          local_document(_Renderer_)
        , isdct_support(_Renderer_)]),
    implements(documentp)).
:- end_object.