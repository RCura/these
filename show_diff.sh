#!/bin/bash
git latexdiff --latexmk --latexopt "-xelatex" HEAD~1 --main These_TOC.tex
