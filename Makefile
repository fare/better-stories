all: snapl2015-fare.pdf

all: snapl2015-fare.PDF

snapl2015-fare.pdf: snapl2015-fare.tex
	pdflatex $<

snapl2015-fare.PDF: snapl2015-fare.pdf
	view.sh $<
