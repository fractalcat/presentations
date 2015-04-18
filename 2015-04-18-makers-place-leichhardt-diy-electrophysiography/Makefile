MARKDOWNS=$(wildcard *.md)
PDFS=$(MARKDOWNS:md=pdf)

all: $(PDFS)
clean:
	rm -f $(PDFS)

%.pdf: %.md template.tex img/design.png
	pandoc -H template.tex --filter columnfilter.py -t beamer -s $< -o $@

img/design.png: data/design.dot
	dot -Grotate=90 -Tpng -o img/design.png data/design.dot
