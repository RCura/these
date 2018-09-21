#!/bin/bash
pandoc -s These_TOC.tex \
--pdf-engine=xelatex \
--toc \
--reference-doc base_style.docx \
--bibliography=chap5/biblio_chap5.bib \
-o chapitre5.docx
