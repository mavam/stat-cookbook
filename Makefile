CFG  := config.tex
DOC  := cookbook.tex
DIST := stat-cookbook.tar.gz
THIS := $(shell basename $(CURDIR))

EN  := $(DOC:.tex=-en.tex)
ES  := $(DOC:.tex=-es.tex)
DE  := $(DOC:.tex=-de.tex)

RERUN := "(undefined references|Rerun to get (cross-references|the bars|point totals) right|Table widths have changed. Rerun LaTeX.|Linenumber reference failed)"
RERUNBIB := "No file.*\.bbl|Citation.*undefined"

TEXINPUTS := $(TEXINPUTS):translations:

.PHONY: all figs en es english spanish purge clean dist

all: figs en

figs:
	@$(MAKE) -C $@

en: $(DOC)
	@echo '\uselanguage{english}' > $(CFG)
	@test -e $(EN) && diff -q $< $(EN) > /dev/null || cp $(DOC) $(EN)
	@make english

es: $(DOC)
	@echo '\uselanguage{spanish}' > $(CFG)
	@test -e $(ES) && diff -q $< $(ES) > /dev/null || cp $(DOC) $(ES)
	@make spanish

english: $(EN:.tex=.pdf)

spanish: $(ES:.tex=.pdf)

%.pdf: %.tex
	pdflatex $<
	@egrep -q $(RERUNBIB) $*.log && bibtex $* && pdflatex $<; true
	@egrep -q $(RERUN) $*.log && pdflatex $<; true
	@egrep -q $(RERUN) $*.log && pdflatex $<; true

purge:
	-rm -f $(DIST) $(DOC:.tex=-*) \
		*.{aux,dvi,log,bbl,blg,brf,toc,thm,out,fdb_latexmk}

clean: purge
	$(MAKE) -C figs $@
	-rm -f $(CFG) $(EN) $(EN:.tex=.pdf)

dist: clean
	@git submodule update
	@echo $(THIS)
	@cd .. && \
		tar cLzf $(DIST) --exclude literature $(THIS) && \
		mv $(DIST) $(THIS)
