#NOTE: next line controls whether to use latex (false) or pdflatex (true)
USE_PDFLATEX = true
CURRTARGET = hw4template
TARGETS = hw4template


ifeq ($(USE_PDFLATEX), true) 
	EXTENSION = pdf
	LATEX = pdflatex
	VIEWER = xpdf
else 
	EXTENSION = dvi
	LATEX = latex
	VIEWER = xdvi
endif
OUTDIR = ./output

# default target; build the document referenced by the CURRTARGET variable
# note that we do latex, followed by bibtex, followed by latex two more times.
# The first call builds the document and figures out what references are needed,
# the call to bibtex builds the bibliography items,
# the next call to latex adds the bibliography to the document,
# and the final call fills in the correct references for your \cite{} commands
default:
	$(LATEX) $(CURRTARGET).tex; \
	bibtex $(CURRTARGET); \
	$(LATEX) $(CURRTARGET).tex; \
	$(LATEX) $(CURRTARGET).tex; \
	for i in log aux blg bbl $(EXTENSION); \
		do cp $(CURRTARGET).$$i /tmp/; mv $(CURRTARGET).$$i $(OUTDIR); done


# wildcard rule, so you can just do `make myfile.tex` without needing a
# special target for every file
%: %.tex
	$(LATEX) -output-directory $(OUTDIR) -file-line-error -interaction nonstopmode $@.tex

		
view:
	$(VIEWER) $(OUTDIR)/$(CURRTARGET).$(EXTENSION) &

ps:
	dvips -o $(OUTDIR)/$(CURRTARGET).ps $(OUTDIR)/$(CURRTARGET).dvi &

pdf:
	dvipdf $(OUTDIR)/$(CURRTARGET).dvi ; mv $(CURRTARGET).pdf $(OUTDIR) & 

ifeq ($(USE_PDFLATEX), true)
all:
	for i in $(TARGETS); do $(LATEX) $$i.tex; bibtex $$i; $(LATEX) $$i.tex; $(LATEX) $$i.tex; dvips -o $$i.ps $$i.dvi; dvipdf $$i.dvi; done; mv *dvi *ps *pdf $(OUTDIR)
else
all:
	for i in $(TARGETS); do $(LATEX) $$i.tex; bibtex $$i; $(LATEX) $$i.tex; $(LATEX) $$i.tex; done; mv *pdf $(OUTDIR)
endif


clean:
	rm *.log *.blg *.dvi *.aux *.ppl *~ $(OUTDIR)/*.log $(OUTDIR)/*.bib $(OUTDIR)/*.aux $(OUTDIR)/*.bbl $(OUTDIR)/*.blg $(OUTDIR)/*.dvi ; mv *.ps *.pdf $(OUTDIR)
