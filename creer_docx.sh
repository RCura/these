#!/bin/bash
pandoc -s These_Robin_Master.tex \
--pdf-engine=xelatex \
--toc \
--reference-doc base_style.docx \
--bibliography=chap3/biblio_chap3.bib \
-o chapitre3.docx
