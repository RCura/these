#!/bin/bash
pandoc -s These_Robin_Master.tex \
--latex-engine=xelatex \
--bibliography=chap6/biblio_chap6.bib \
-o chapitre6.docx
