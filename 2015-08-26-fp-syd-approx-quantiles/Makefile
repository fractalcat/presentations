MARKDOWNS=$(wildcard *.md)
PDFS=$(MARKDOWNS:md=pdf)

all: $(PDFS)

clean:
	rm -f $(PDFS) $(SHINY_PDFS)

%.pdf: %.md
	pandoc --filter pandoc-citeproc -f markdown+tex_math_dollars -t beamer -s $< -o $@
