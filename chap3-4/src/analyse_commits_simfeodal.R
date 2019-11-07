library(tidyverse)
library(gitsum)

#commits_simfeodal_raw <- gitsum::parse_log_detailed(path = "~/SimFeodal/") %>%
#  mutate(jour = lubridate::date(date))

#saveRDS(commits_simfeodal_raw, "commits_simfeodal_raw.RDS")
commits_simfeodal_raw <- readRDS("commits_simfeodal_raw.RDS")

all_commits <- readRDS("master.RDS") %>%
  mutate(branch = "master") %>%
  bind_rows(readRDS("develop.RDS") %>% mutate(branch = "develop")) %>%
  bind_rows(readRDS("feat_gama17.RDS") %>% mutate(branch = "feat_gama17")) %>%
  bind_rows(readRDS("gama161.RDS") %>% mutate(branch = "gama161") ) %>%
  bind_rows(readRDS("Gama17.RDS") %>% mutate(branch = "gama17")) %>%
  bind_rows(readRDS("sensibanalysis.RDS") %>% mutate(branch = "sensib")) %>%
  distinct(hash, .keep_all = TRUE)

commits_simfeodal_raw <- all_commits


tags <- tribble(
  ~hash, ~tag,
  "595f78886804a994ad29ad344b5dc0ab5a726629", "v6.6.1", 
  "7812e694c88a148eb3079da3365737899988c07c", "v6.6.0",
  "deffe39b9ecd0f3006b2e3e8d6987c3d78425867", "v6.5.3",
  "03a8364f9b002baacd7eaf56479784372175c62d", "v6.5.2",
  "3cd1afdcbc998ed9b2a010ae27bd638b13d08f7c", "v6.4.2",
  "31ad1a643510ce3fdc5cbef91146e39018289023", "v6.3",
  "0abf23a4499a3292e1b5e8d8c7fa98e08f0f50e9", "v6.2",
  "c99361803f0d735190fe1a3a796c9b522be5253a", "v6.1.0",
  "397bcba4a8fc59f946d8e38c39769f3074e8682d", "v6.0",
  "1a41c42ae944b949931e6408c1a59123337f577a", "v6.0.0",
  "1906baba7d4efaf553c0310e1b698872f76c91b5", "v5.1.2",
  "4f8f4dfc48a0c4b855c527734f445b7ac1836c04", "v5.0.4",
  "cecbdff03da3b3a4b67de020b081cbe277b96b7c", "v5.0.2",
  "94d4a19a45d7167a26442b515b9f61807fc3f182", "v5.0",
  "cc06a17081d778f6da63d79f26d2924094831a97", "v4.5.0",
  "7696677bfad1a4bfe35a361913f07f12f7cf89ea", "v4.4",
  "3c2839c7ce3874e4dc85a352f6d8becdab6b7ddf", "v4.4.4",
  "613ea2f91db677d02c79f9e0c799efdb9c552cf0", "v4.4.0",
  "0719dbed77287c1a988107e310cf5fa770f89551", "v4.3.3",
  "8948ed789432372989407a5efa9828a90b9122c8", "v4.2",
  "7c850563de5ef938fee485fe4289cce666a02d3a", "v4.0",
  "f3f239cbd9d92391b3ef2be518814f32a661ddcd", "vJIAP",
  "cb9bed65cb1419b61f9d743cd03deb7e5137ee75", "v3.2",
  "fd745d3a71b0d2de79227effd63c9fcb5f175966", "v3.1",
  "5d0aba7aa979da2eb594bc0711c2b8b5e43862b6", "v3.0",
  "d328df0e01cfc26dc4a0042bd4e6d36e826a45be", "v2.3",
  "f54ec68d9fa5f161364b5bf4f391d433515ba5b6", "v2",
  "991f8df9629c3f6f419b3f29c2cfa4f4631b09a7", "v2.0",
  "a85598682cda2350b08ea789b966e613dacb1b05", "v0"
)

tags_full <- tags %>%
  filter(tag %in% c("v0", "v2.0", "v3.0", "v4.0", "v5.0", "v6.0", "v6.3", "v6.5.3","v6.6.0")) %>%
  left_join(commits_simfeodal_raw %>% select(hash, date), by = "hash") %>%
  mutate(date = lubridate::date(date))

tags_full

commits_simfeodal <- commits_simfeodal_raw %>%
  select(hash, commit_nr, nested, date) %>%
  unnest(nested) %>%
  filter(endsWith(changed_file, suffix = ".gaml")) %>%
  filter(startsWith(changed_file, prefix = "models/")) %>%
  filter(str_detect(changed_file, pattern = "GUI", negate = TRUE)) %>%
  mutate(date = lubridate::date(date))
  
commits_and_tags <- commits_simfeodal %>%
  group_by(hash, date) %>%
  summarise(
            first_nr = first(commit_nr),
            last_nr = last(commit_nr),
            sum_edits = sum(edits, na.rm = TRUE),
            mean_edits = mean(edits, na.rm = TRUE),
            med_edits = median(edits, na.rm = TRUE)
            ) %>%
  ungroup() %>%
  bind_rows(tags_full) %>%
  arrange(date) %>%
  mutate(new_nr = if_else(is.na(last_nr), true = lag(last_nr), false = last_nr)) %>%
  mutate(row_nr = row_number())
  
tags_ranked <- commits_and_tags %>%
  filter(!is.na(tag)) %>%
  select(date, hash, tag, new_nr, row_nr)

library(ggrepel)

commits_plot <- commits_and_tags %>%
  filter(is.na(tag)) %>%
  select(new_nr, row_nr, sum_edits, med_edits) %>%
  mutate(
    sum_edits = scale(sum_edits, center = FALSE),
    med_edits = scale(med_edits, center = FALSE)
  ) %>%
  gather(key = type_edit, value = number,- new_nr, - row_nr) %>%
  mutate(type_edit = str_replace(type_edit, pattern = "_edits", replacement = '')) %>%
  mutate(number = if_else(type_edit == "sum", number, -number))

library(extrafont)
ggplot(commits_plot) +
  geom_col( aes(row_nr, number, fill = type_edit)) +
#  facet_wrap(~type_edit, ncol = 1, scales = "free_y") +
  # geom_vline(data = tags_ranked,
  #            aes(xintercept = row_nr, ), size = .5,
  #            linetype = "dotted"
  #            ) +
  geom_segment(data = tags_ranked,
             aes(x = row_nr, xend = row_nr), y = -6, yend = 6.8, size = .5,
             linetype = "dotted"
  ) +
  geom_label_repel(data = tags_ranked,
                   aes(x = row_nr, y = 8, label = tag), force = .05, box.padding = .1,
                   size = 4, direction = "y",segment.color = NA) +
  theme_minimal() +
  scale_x_continuous(breaks = NULL, name = NULL, expand = c(0,1),  limits= c(0, 310)) +
  scale_y_continuous(breaks = NULL, name = NULL, expand = c(0,0), limits = c(-8, 8.5)) +
  theme(panel.grid.major.x = element_blank()) +
  labs(caption = "Relevé effectué le 22/09/2019 - Commit #595f788",
       y = "Nombre de lignes modifiées\n(normalisé)") +
  geom_text(x = 80, y = -4, label = "Nombre médian\nde modifications", colour = "#F8766D", size = 5) +
  geom_text(x = 80, y = 4, label = "Nombre total (somme)\ndes modifications", colour = "#00BFC4", size = 5) +
  geom_label_repel(data = tags_ranked %>%
                     select(date, tag, row_nr) %>%
                     filter(tag %in% c("v0","v2.0", "v4.0", "v5.0", "v6.0","v6.6.0")) %>%
                     #bind_rows(tibble(date = as.Date("2014-04-21"), new_nr = 1, row_nr = 1, tag = "v0")) %>%
                     mutate(dateFr = paste0(lubridate::day(date),
                                     "/", lubridate::month(date),
                                     "\n", lubridate::year(date))),
            aes(x = row_nr, label = dateFr), y = -14, size = 3, segment.color = NA) +
  scale_fill_discrete(guide = FALSE) +
  theme(text = element_text(family = "Charis SIL", face = "plain"))
  
  ggsave(plot = last_plot(), filename = "explo_edits_code.pdf", width = 20, height = 10, units = "cm",
         scale = 1)
# 
# 
# group_by(hash, commit_nr) %>%
#   summarize(nb_edits = sum(edits)) %>%
#   ungroup()
#   select(hash, message, total_files_changed, total_insertions, total_deletions, commit_nr) %>%
#   mutate(diff_lines = abs(total_insertions - total_deletions)) %>%
#   mutate(taille_commit = case_when(
#     diff_lines <= 50 ~ '<50',
#     diff_lines <= 100 ~ "50-100", 
#     TRUE ~ ">100")
#     ) %>%
#   mutate(taille_commit = factor(taille_commit, levels = c("<50", "50-100", ">100"))) %>%
#   mutate(y = as.numeric(taille_commit))
# 
# 
#   
# 
# p <- ggplot(commits_simfeodal) +
#   geom_segment(aes(
#     x = commit_nr,
#     xend = commit_nr,
#     y = as.numeric(taille_commit),
#     yend = as.numeric(taille_commit) + 1,
#     colour = taille_commit
#     )) +
#   #facet_wrap(~taille_commit, ncol = 1) +
#   scale_y_continuous(name = NULL) +
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank()) +
#   theme_minimal()
# 
# p
# 
# tags <- tribble(
#   ~hash, ~tag,
#   "03a8364f9b002baacd7eaf56479784372175c62d", "v6.5.2",
#   "3cd1afdcbc998ed9b2a010ae27bd638b13d08f7c", "v6.4.2",
#   "31ad1a643510ce3fdc5cbef91146e39018289023", "v6.3",
#   "0abf23a4499a3292e1b5e8d8c7fa98e08f0f50e9", "v6.2",
#   "c99361803f0d735190fe1a3a796c9b522be5253a", "v6.1.0",
#   "397bcba4a8fc59f946d8e38c39769f3074e8682d", "v6.0",
#   "1a41c42ae944b949931e6408c1a59123337f577a", "v6.0.0",
#   "1906baba7d4efaf553c0310e1b698872f76c91b5", "v5.1.2",
#   "4f8f4dfc48a0c4b855c527734f445b7ac1836c04", "v5.0.4",
#   "cecbdff03da3b3a4b67de020b081cbe277b96b7c", "v5.0.2",
#   "94d4a19a45d7167a26442b515b9f61807fc3f182", "v5.0",
#   "cc06a17081d778f6da63d79f26d2924094831a97", "v4.5.0",
#   "7696677bfad1a4bfe35a361913f07f12f7cf89ea", "v4.4",
#   "3c2839c7ce3874e4dc85a352f6d8becdab6b7ddf", "v4.4.4",
#   "613ea2f91db677d02c79f9e0c799efdb9c552cf0", "v4.4.0",
#   "0719dbed77287c1a988107e310cf5fa770f89551", "v4.3.3",
#   "8948ed789432372989407a5efa9828a90b9122c8", "v4.2",
#   "7c850563de5ef938fee485fe4289cce666a02d3a", "v4.0",
#   "f3f239cbd9d92391b3ef2be518814f32a661ddcd", "vJIAP",
#   "cb9bed65cb1419b61f9d743cd03deb7e5137ee75", "v3.2",
#   "fd745d3a71b0d2de79227effd63c9fcb5f175966", "v3.1",
#   "5d0aba7aa979da2eb594bc0711c2b8b5e43862b6", "v3.0",
#   "d328df0e01cfc26dc4a0042bd4e6d36e826a45be", "v2.3",
#   "f54ec68d9fa5f161364b5bf4f391d433515ba5b6", "v2",
#   "991f8df9629c3f6f419b3f29c2cfa4f4631b09a7", "v2.0"
# )
# 
# tags_full <- tags %>%
#   filter(tag %in% c("v2.0", "v3.0", "v4.0", "v5.0", "v6.0"))
# 
# versions_simfeodal <- commits_simfeodal_raw %>%
#   left_join(tags_full, by = "hash") %>%
#   filter(!is.na(tag)) %>%
#   select(commit_nr, tag)
# 
# p2 <- p +
#   geom_vline(data = versions_simfeodal, 
#              aes(xintercept = commit_nr), size = 1) +
#   geom_label(data = versions_simfeodal %>% mutate(taille_commit = ">100"),
#              aes(x = commit_nr,
#                  y = 1,
#                  label = tag),
#              colour = "white", fill = "black")
# 
# p2
# 
# 
# commits_simfeodal %>%
#   ggplot() +
#   geom_col(aes(commit_nr, nb_edits)) +
#   geom_vline(data = versions_simfeodal, 
#              aes(xintercept = commit_nr),
#              size = 1) +
#   geom_label(data = versions_simfeodal,
#              aes(x = commit_nr,
#                  y = 1,
#                  label = tag),
#              colour = "white", fill = "black")
#   