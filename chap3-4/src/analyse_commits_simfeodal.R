library(tidyverse)
library(gitsum)

commits_simfeodal <- gitsum::parse_log_detailed(path = "~/SimFeodal/") %>%
  mutate(jour = lubridate::date(date)) %>%
  select(hash, message, total_files_changed, total_insertions, total_deletions, commit_nr) %>%
  mutate(diff_lines = abs(total_insertions - total_deletions)) %>%
  mutate(taille_commit = case_when(
    diff_lines <= 10 ~ "<10",
    diff_lines <= 50 ~ '10-50',
    diff_lines <= 100 ~ "50-100", 
    TRUE ~ ">100")
    ) %>%
  mutate(taille_commit = factor(taille_commit, levels = c("<10", "10-50", "50-100", ">100")))

ggplot(commits_simfeodal) +
  aes(commit_nr, colour = taille_commit, fill = taille_commit) +
  geom_bar(y = 1) +
  facet_wrap(~taille_commit, ncol = 1) +
  scale_y_continuous(name = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

