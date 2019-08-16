#!/bin/bash
pandoc -s These_Robin_Master.tex \
--latex-engine=xelatex \
--bibliography=chap2/biblio_chap2.bib \
-o chapitre2.docx
