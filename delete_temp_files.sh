#!/bin/bash
find . \( \
	-name "*.aux" -o  \
	-name "*.bbl" -o  \
	-name "*.blg" -o  \
	-name "*.maf" -o  \
	-name "*.log" -o \
	-name "*.synctex.gz" -o \
	-name "*.mtc*" -o \
	-name "*.run.xml" -o \
	-name "*.out" -o \
	-name "*.toc" -o \
	-name "*.fls" -o \
	-name "*.fdb_latexmk" -o \
	-name "*.lof" -o \
	-name "*.lot" -o \
	-name "*.encadre" -o \
	-name "*.listencadres" -o \
	-name "*-blx.bib" \
	\) -type f -delete
