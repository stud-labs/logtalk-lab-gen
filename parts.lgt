:- category(partsc).

   :- info([
       version is 1:0:0,
       author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
       date is '2025-07-21',
       comment is 'Category of parts of documents'
   ]).

   :- public(draw_on/1).
   :- mode(draw_on, one).
   :- info(draw_on/1, [
       comment is 'Draw contents on canvas'
   ]).

   draw_on(Cn)


:- end_category.

:- category(departmentc(_ParentDepartment_),
   extends(partsc)).

   :- info([
       version is 1:0:0,
       author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
       date is '2025-07-21',
       comment is 'Category wrawing headers of documants'
   ]).

   :- public(parent/1).
   :- mode(parent, one).
   :- info(parent/1, [
       comment is 'Draw a parent organization'
   ]).

   :- public(org_name/1).
   :- mode(org_name, one).
   :- info(org_name/1, [
       comment is 'Sraw organization Name'
   ]).
