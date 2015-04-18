all: talk.html

talk.html: slides.md template.html img/*.svg
	pandoc --template template.html -t dzslides -s slides.md -o talk.html

talk.pdf: slides.md
	pandoc --template=beamer.latex -t beamer slides.md -o slides.pdf

slides.tex: slides.md
	pandoc -t beamer slides.md -o slides.tex

clean:
	rm -f talk.pdf talk.html slides.tex

.PHONY: clean
.PHONY: all
