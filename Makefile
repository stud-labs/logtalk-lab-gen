.PHONY: clean resp view

TARGET=cd_documents
SRC=docs.lgt parts.lgt docs_test_simple.lgt loader.lgt cd_source.lgt \
	morpher.lgt morpher_backend.pl cd_docs.lgt global.lgt

tex: $(TARGET).tex

doc: $(TARGET).pdf

%.pdf: %.tex
	test -f $< && lualatex --halt-on-error $<

view: resp
	evince $(TARGET).pdf &

$(TARGET).tex: $(SRC) Makefile
	swilgt -l loader.lgt -g halt

#$(TARGET).tex: docs.lgt study-docs-loader.lgt Makefile#
#	swilgt -l study-docs-loader.lgt -g halt

isdct_docs.tex: docs.lgt isdct-papers-loader.lgt Makefile
	swilgt -l isdct-papers-loader.lgt -g halt

clean:
	latexmk -C || true

cleanall: clean
	rm -f *.tex isdct_docs.tex
	rm -f *.pdf isdct_docs.pdf
