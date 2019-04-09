#!/bin/bash
git latexdiff -o diff_these.pdf --latexmk --latexopt "-xelatex" HEAD~2 --main These_Robin_Master.tex
