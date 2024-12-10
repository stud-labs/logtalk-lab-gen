:- object(documents_test_simple,
   extends(documents(latexRenderer("doc-simple.tex")))).
   :- use_module(library(lists), [member/2]).

   :- initialization(::gen).
:- end_object.
