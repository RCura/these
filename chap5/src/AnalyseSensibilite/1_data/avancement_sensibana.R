#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
suppressPackageStartupMessages(library(tidyverse))
read_csv(args[1], col_types =cols()) %>%
group_by(sensibility_parameter) %>%
tally()
