.PHONY: all tex2pdf pdf pdf2svgs

all: tex2pdf pdf2svgs

tex2pdf pdf:
	pdflatex tiles.tex

pdf2svgs:
	sh convert-pdf-to-svgs.sh

clean:
	# remove *.svg from last build
	rm --force $$(rev convert-pdf-to-svgs.sh | cut -d' ' -f1 | rev)
	# remove TeX build artifacts and conversion script
	rm --force tiles.aux tiles.log tiles.pdf convert-pdf-to-svgs.sh
