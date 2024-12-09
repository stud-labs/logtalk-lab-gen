.PHONY: clean resp

resp: doc-anon.pdf 


%.pdf: %.tex
	lualatex $<

doc.tex doc-anon.tex: docs.lgt
	swilgt -l loader.lgt -g halt

clean:
	latexmk -C

cleanall: clean
	rm *.tex
	rm *.pdf
	