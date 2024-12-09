.PHONY: clean resp

resp: doc-anon.pdf


%.pdf: %.tex
	lualatex $<

doc-simple.tex: docs.lgt loader.lgt Makefile
	swilgt -l loader.lgt -g halt

doc-anon.tex: docs.lgt study-docs-loader.lgt Makefile
	swilgt -l study-docs-loader.lgt -g halt

clean:
	latexmk -C

cleanall: clean
	rm -f *.tex
	rm -f *.pdf
