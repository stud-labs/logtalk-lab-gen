:- object(isdct_datac).
   :- public(director/3).
   :- public(departmentone/3).
   :- public(expertheader/3).
   :- public(shortname/1).

   director('И.В.~Бычков', 'Директор', 'академик').
   departmentone('Л.Ф.~Зеленова', none, none).
   expertheader('А.Г.~Феоктистов', 'Заместитель директора по научной работе', 'д.т.н.').
   shortname("ИДСТУ~СО~РАН").
:- end_object.

:- object(ytu_2024_12_data).
   :- public(article/1).
   article([
        type("Презентация")
      , title("Matrosov Institute for System Dynamics and Control Theory of Siberian Branch of the Russian Academy of Sciences")
      , author("академик~И.В.~Бычков")]).

   article([
        type("Презентация")
      , title("Использование спутниковых данных для мониторинга загрязнения атмосферы")
      , altertitle("Remote sensing data for air pollution monitoring")
      , author("к.т.н.~А.К.~Попова")
   ]).

   article([
        type("Презентация")
      , title("Классификация данных Sentinel-2")
      , author("к.т.н.~Р.К.~Федоров")
   ]).

   article([
        type("Презентация")
      , title("Исследования ИДСТУ СО РАН в области геоинформатики: прошлое, настоящее, будущее")
      , author("академик И.В.Бычков, А.Е. Хмельнов, Р.К. Фёдоров, Е.С. Фереферов, А.С. Гаченко")
   ]).

:- end_object.


:- object(whom_where).
   :- public(whom/1).
   whom("Черкашину Евгению Александровичу, к.т.н.,~с.н.с.").
   :- public(where/1).
   where("КНР, с целью демонстрации в ходе рабочего визита в Яньтайском университете (г.~Яньтай, КНР) в рамках обсуждения возможного сотрудничества в области информационных технологий").
:- end_object.


:- object(isdct_papers_documents(_Renderer_, _PaperData_),
   extends(documents(_Renderer_))).
   % :- public(gen/0).
   gen:-
        _PD = _PaperData_,
        R = _Renderer_,
        ::gen(
            (_PaperData_::article(Article), ::check_article(Article)),
            [permission_customs(R, isdct_datac,whom_where)]).

   % articles(Articles):-
   %     findall(A, (_PaperData_::article(A), ::check_article(A)), Articles).

   :- protected(check_article/1).
   check_article(_).
:- end_object.
