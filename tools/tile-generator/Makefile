.PHONY: all tex2pdf pdf pdf2svgs

all: tex2pdf pdf2svgs

tex2pdf pdf:
	pdflatex tiles.tex

pdf2svgs:
	sh convert-pdf-to-svgs.sh

clean:
	rm tiles.aux tiles.log tiles.pdf convert-pdf-to-svgs.sh line-east-south.svg line-east-west.svg line-north-east.svg line-north-south.svg line-north-west.svg line-south-west.svg
