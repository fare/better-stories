all: snapl2015-fare.pdf

all: snapl2015-fare.PDF

%.pdf: %.tex
	pdflatex $*
	bibtex $*
	pdflatex $*

%.PDF: %.pdf
	view.sh $<
