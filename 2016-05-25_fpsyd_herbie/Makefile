SLIDES=slides
OUT=$(SLIDES).pdf

all: $(OUT)  img/loop.svg

clean:
	rm -f $(OUT)  img/loop.svg

$(OUT): $(SLIDES).tex $(SLIDES).bib img/loop.png img/quadratic-b.png img/subsamples.png
	pdflatex -shell-escape $(SLIDES).tex
	bibtex $(SLIDES).aux
	pdflatex -shell-escape $(SLIDES).tex
	pdflatex -shell-escape $(SLIDES).tex

img/loop.svg: img/loop.dot
	dot -Tsvg img/loop.dot > img/loop.svg

.PHONY: clean
.PHONY: all
