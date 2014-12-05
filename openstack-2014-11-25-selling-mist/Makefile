mist.html: template.html selling-mist-slides.md
	pandoc  --template template.html -t dzslides -s selling-mist-slides.md -o mist.html 

mist.pdf: selling-mist-slides.md
	pandoc -t beamer selling-mist-slides.md -o mist.pdf

.PHONY: all
