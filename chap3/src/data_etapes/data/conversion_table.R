library(tidyverse)
library(readxl)
library(knitr)
library(kableExtra)

etapes_table <- read_xlsx("Tableau_etapes.xlsx")

kable(etapes_table, format = "latex") %>%
  kable_styling(full_width = F) %>%
  column_spec(1, width = "2em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "25em") %>%
  column_spec(5, width = "2em")
