all: fare-lambdaconf2017.html # better-stories.PDF

install: better-stories.pdf
	rsync -av $< ~/files/better-stories/
	rsync -av $< fare@bespin.org:files/better-stories/

%.pdf: %.tex
	pdflatex $*
	bibtex $*
	pdflatex $*

%.PDF: %.pdf
	xpdf -z page -fullscreen $< ${p}

fare-lambdaconf2017.html: fare-lambdaconf2017.rkt
	racket $< > $@
