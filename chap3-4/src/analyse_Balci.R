library(tidyverse)
library(readxl)
raw_data_Balci <- read_excel("Tableau_Methodes_VVeT_Balci.xls") %>%
  replace(., is.na(.), 0) %>%
  replace(., . == "T", 1) %>%
  mutate_at(vars(starts_with("Fig")), .funs = as.numeric) %>%
  rename_all(funs(str_replace_all(., '-', "_")))

sum_Balci <- raw_data_Balci %>%
  mutate(Sum = rowSums(select(., Fig1_1:Fig2_10))) %>%
  mutate(Coverage = Sum / 18) %>%
  arrange(Coverage, desc(Method)) %>%
  mutate(Method = factor(Method, levels = Method)) %>%
  mutate(Taxonomy = factor(Taxonomy, levels = c("Informal", "Static", "Dynamic", "Formal")))

library(scico)

ggplot(sum_Balci) +
  aes(x = Coverage, y = Method, colour = Taxonomy) +
  geom_segment(aes(x = 0, xend = Coverage, yend = Method)) + 
  geom_point(size = 2) +
  scale_x_continuous(labels = scales::percent) +
  scale_colour_scico_d(name = "Type de méthode", palette = "roma") +
  labs(title = "Applicabilité des techniques de VV&T",
       caption = "Taux de couverture : d'après Balci 1998\nTaxonomie des méthodes : d'après Balci 1997",
       x = "Taux de couverture",
       y = "Méthode") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(last_plot(), filename = "../img/VVetT_Balci.pdf", width = 20, height = 30, units = "cm")  
