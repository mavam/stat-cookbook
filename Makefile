CFG := config.tex
DOC := cheatsheet.tex

EN  := $(DOC:.tex=-en.tex)
ES  := $(DOC:.tex=-es.tex)
DE  := $(DOC:.tex=-de.tex)

REMOTE := probstat
WEB := login.eecs.berkeley.edu:public_html/dl

RERUN := "(undefined references|Rerun to get (cross-references|the bars|point totals) right|Table widths have changed. Rerun LaTeX.|Linenumber reference failed)"
RERUNBIB := "No file.*\.bbl|Citation.*undefined"

all: en

en: $(DOC)
	@echo '\input{lang-en}' > $(CFG)
	@test -e $(EN) && diff -q $< $(EN) > /dev/null || cp $(DOC) $(EN)
	@make english

es: $(DOC)
	@echo '\input{lang-es}' > $(CFG)
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
	-rm -f *.{aux,dvi,log,bbl,blg,brf,toc,thm,out,fdb_latexmk}

clean: purge
	-rm -f $(CFG) $(EN) $(EN:.tex=.pdf)

#www: all
#	scp $(LOCAL).pdf $(WEB)
