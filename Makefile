.PHONY: clean resp

resp: doc-anon.pdf

%.pdf: %.tex
	lualatex $<

doc-simple.tex: docs.lgt loader.lgt Makefile
	swilgt -l loader.lgt -g halt

doc-anon.tex: docs.lgt study-docs-loader.lgt Makefile
	swilgt -l study-docs-loader.lgt -g halt

isdct_docs.tex: docs.lgt isdct-papers-loader.lgt Makefile
	swilgt -l isdct-papers-loader.lgt -g halt

clean:
	latexmk -C || true

cleanall: clean
	rm -f *.tex isdct_docs.tex
	rm -f *.pdf isdct_docs.pdf
