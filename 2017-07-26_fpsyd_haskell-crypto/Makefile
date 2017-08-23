SLIDES=slides
OUT=$(SLIDES).pdf

all: $(OUT)

clean:
	rm -f $(OUT)
	rm -f $(addsuffix .png, $(basename $(wildcard img/*.dot)))

$(OUT): $(SLIDES).tex $(addsuffix .png, $(basename $(wildcard img/*.dot)))
	pdflatex -shell-escape $(SLIDES).tex
	pdflatex -shell-escape $(SLIDES).tex
	pdflatex -shell-escape $(SLIDES).tex

img/%.png: img/%.dot
	dot $< -Tpng -o $@

.PHONY: clean
.PHONY: all
