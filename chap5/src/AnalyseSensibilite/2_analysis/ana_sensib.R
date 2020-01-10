library(tidyverse)

valeurs_base <- read_csv("../1_data/analyse_sensibilite_contexte.csv") %>%
  mutate(type = "contexte") %>%
  bind_rows(
    read_csv("../1_data/analyse_sensibilite_contexte_fix.csv") %>%
      mutate(sensibility_value = as.character(sensibility_value )) %>%
      mutate(type = "contexte")
  ) %>%
  bind_rows(
    read_csv("../1_data/analyse_sensibilite_inputs.csv") %>%
      mutate(sensibility_value = as.character(sensibility_value )) %>%
      mutate(type = "input")
    ) %>%
  bind_rows(
    read_csv("../1_data/analyse_sensibilite_technique.csv") %>%
      mutate(sensibility_value = as.character(sensibility_value )) %>%
      mutate(type = "technique")
    ) %>%
  bind_rows(
    read_csv("../1_data/analyse_sensibilite_mecanisme1.csv") %>%
      mutate(sensibility_value = as.character(sensibility_value )) %>%
      mutate(type = "mecanismes")
  ) %>%
  filter(!(sensibility_parameter == "distance_detection_agregat" & sensibility_value == "500")) %>% ## Ces valeurs ont buggé
  bind_rows(
    read_csv("../1_data/analyse_sensibilite_mecanisme1_fix.csv") %>% ## pour distance_detection_agregat, ré-execution à 300 au lieu de 500
      mutate(sensibility_value = as.character(sensibility_value )) %>%
      mutate(type = "mecanismes")
  ) %>%
  bind_rows(
    read_csv("../1_data/analyse_sensibilite_mecanisme2.csv") %>% ## pour distance_detection_agregat, ré-execution à 300 au lieu de 500
      mutate(sensibility_value = as.character(sensibility_value )) %>%
      mutate(type = "mecanismes")
  ) %>%
  select(-c(seed_sensib, sim_name, annee_sensib, nb_fp, nb_chateaux, nb_eglises, distance_eglises_sensib)) %>%
  rename(distance_eglises_paroissiales = distance_eglises_paroissiales_sensib,
         prop_fp_isoles = prop_fp_isoles_sensib,
         ratio_charge_fiscale = ratio_charge_fiscale_sensib)
  

valeurs_base %>%
  group_by(sensibility_parameter, type) %>%
  summarise(n = n(), values = list(unique(sensibility_value))) %>%
  mutate(values = paste0(values, collapse = " , "))


valeurs_normalisees <- valeurs_base %>%
  mutate(
    nb_agregats_normalise = (nb_agregats - 200) / 10.45,
    nb_grands_chateaux_normalise = (nb_grands_chateaux - 10) / 2.87,
    nb_eglises_paroissiales_normalise = (nb_eglises_paroissiales - 300) / 12.96,
    distance_eglises_paroissiales_normalise = (distance_eglises_paroissiales - 3000) / 97,
    prop_fp_isoles_normalise = (prop_fp_isoles - 0.2) / 0.08,
    ratio_charge_fiscale_normalise = (ratio_charge_fiscale - 3) / 0.03
  ) %>%
  select_at(vars(ends_with("_normalise"), starts_with("sensibility"), "type"))


library(ggalt)
  valeurs_normalisees %>%
    gather(Indicateur, Valeur_norm, -type, -sensibility_parameter, -sensibility_value) %>%
    group_by(sensibility_parameter, type) %>%
    #summarise(sensibilite = abs(max(Valeur_norm) - min(Valeur_norm))) %>%
    summarise(sensibilite = mean(abs(Valeur_norm), na.rm = TRUE)) %>%
    arrange(desc(sensibilite)) %>%
    mutate(type = factor(type, levels = c("input", "contexte", "mecanismes", "technique"))) %>%
    mutate(type = fct_recode(type, Input = "input", Contexte = "contexte", Mécanisme = "mecanismes", Technique = "technique")) %>%
    ggplot() +
    aes(fct_reorder(sensibility_parameter, sensibilite), sensibilite, colour = type) +
    geom_lollipop(point.size=2.5, size = 1.25, horizontal = FALSE) +
    geom_label(aes(x = fct_reorder(sensibility_parameter, sensibilite), y = sensibilite, label = sensibility_parameter),
              hjust = 0, nudge_y = 0.25, colour = "grey20", size = 3,
              label.size = NA) +
    scale_y_continuous(breaks = seq(0, 12, 2), minor_breaks = seq(1, 11, 2), limits = c(0, 14.25), expand = c(0, 0)) +
    coord_flip() +
    labs(
      title = "Sensibilité moyenne des paramètres",
      subtitle = "Sensibilité : moyenne des valeurs absolues des valeurs normalisées",
      x = "Paramètres",
      y = "Sensibilité"
    ) +
    scale_colour_viridis_d() +
    theme_light() +
    theme(legend.position="bottom") +
    guides(colour = guide_legend(title = "Type de paramètre",
                                 override.aes = list(size=6),
                                 title.position = "top",
                                 title.hjust = .5,
                                 label.position = "bottom")) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank()) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  #ggsave(last_plot(), filename = "../3_results/sensibilite_globale.pdf", width = 17.5, height = 30, units = "cm")
  
  # On garde les 10 premiers, après les "seuils" sont moins forts
  
  main_params <- valeurs_normalisees %>%
    gather(Indicateur, Valeur_norm, -type, -sensibility_parameter, -sensibility_value) %>%
    group_by(sensibility_parameter, type) %>%
    summarise(sensibilite = mean(abs(Valeur_norm), na.rm = TRUE)) %>%
    ungroup() %>%
    top_n(10, wt = sensibilite) %>%
    select(sensibility_parameter)
  
  # 3 premiers pour chaque indicateur
  
  params_to_keep <- valeurs_normalisees %>%
    select(-sensibility_value) %>%
    gather(Indicateur, Valeur_norm, -type, -sensibility_parameter) %>%
    group_by(sensibility_parameter, type, Indicateur) %>%
    summarise(sensibilite = mean(abs(Valeur_norm), na.rm = TRUE)) %>%
    arrange(desc(sensibilite)) %>%
    group_by(Indicateur) %>%
    mutate(rang = dplyr::dense_rank(desc(sensibilite))) %>%
    ungroup() %>%
    filter(rang <= 3) %>%
    group_by(sensibility_parameter) %>%
    summarise(n = n(), indicateurs = list(Indicateur)) %>%
    mutate(indicateurs = map_chr(indicateurs, ~paste(.x, collapse = ", "))) %>%
    arrange(desc(n), indicateurs) %>% # stop ici pour tableau
    select(-n) %>%
    mutate(type_selection = "Locale") %>%
    full_join(main_params %>% mutate(indicateurs = NA, type_selection = "Globale"), by = "sensibility_parameter") %>%
    select(sensibility_parameter) %>%
    distinct(sensibility_parameter) %>%
    pull()
  
  
  valeurs_normalisees %>%
    select(-sensibility_value) %>%
    gather(Indicateur, Valeur_norm, -type, -sensibility_parameter) %>%
    group_by(sensibility_parameter, type, Indicateur) %>%
    summarise(sensibilite = mean(abs(Valeur_norm), na.rm = TRUE)) %>%
    arrange(desc(sensibilite)) %>%
    group_by(Indicateur) %>%
    mutate(rang = dplyr::dense_rank(desc(sensibilite))) %>%
    ungroup() %>%
    filter(rang <= 3) %>%
    group_by(sensibility_parameter) %>%
    summarise(n = n(), indicateurs = list(Indicateur)) %>%
    mutate(indicateurs = map_chr(indicateurs, ~paste(.x, collapse = "\n"))) %>%
    arrange(desc(n), indicateurs) %>% # stop ici pour tableau
    select(-n) %>%
    mutate(type_selection = "Locale") %>%
    full_join(main_params %>% mutate(type_selection = "Globale"), by = "sensibility_parameter") %>%
    mutate(type_selection = if_else(!is.na(type_selection.y), type_selection.y, indicateurs)) %>%
    select(-indicateurs, -type_selection.x, -type_selection.y) %>%
    rename(`Paramètre` = sensibility_parameter,
           `Origine de la sélection` = type_selection) %>%
    {print(.) ; .} %>%
    kableExtra::kable("latex")
  
  label_both_oneline <- function(labels){
    label_value(labels = labels, multi_line = FALSE)
  }
  
  
  plot_params_to_keep <- valeurs_base %>%
    filter(sensibility_parameter %in% params_to_keep) %>%
    gather(Indicateur, Valeur, -sensibility_parameter, -sensibility_value, -type) %>%
    mutate(sensibility_parameter = as.factor(sensibility_parameter),
           sensibility_value = as.factor(sensibility_value)) %>%
  ggplot() +
    aes(sensibility_value, Valeur, fill = sensibility_value) +
    geom_boxplot() +
    facet_wrap(sensibility_parameter~Indicateur, ncol = 6, scales = "free", labeller = label_both_oneline) +
    guides(fill = FALSE)
  

  foo <- valeurs_base %>%
    gather(Indicateur, Valeur, -sensibility_parameter, -sensibility_value, -type) %>%
    mutate(sensibility_parameter = as.factor(sensibility_parameter),
         sensibility_value = as.factor(sensibility_value))




p1 <- ggplot(foo %>% filter(type == "input")) +
  aes(sensibility_value, Valeur, fill = sensibility_value) +
  geom_boxplot() +
  facet_wrap(sensibility_parameter~Indicateur, ncol = 6, scales = "free") +
  guides(fill = FALSE)


p2 <- ggplot(foo %>% filter(type == "contexte")) +
    aes(sensibility_value, Valeur, fill = sensibility_value) +
    geom_boxplot() +
    facet_wrap(sensibility_parameter~Indicateur, ncol = 8, scales = "free", labeller = mylabel) +
    guides(fill = FALSE)
  
p3 <- ggplot(foo %>% filter(type == "technique")) +
  aes(sensibility_value, Valeur, fill = sensibility_value) +
  geom_boxplot() +
  facet_wrap(sensibility_parameter~Indicateur, ncol = 8, scales = "free", labeller = mylabel) +
  guides(fill = FALSE)


  
# ggsave(p1, filename = "test_input.pdf", width = 21, height = 29.7, units = "cm", scale = 4)
# ggsave(p2, filename = "test_contexte.pdf", width = 21, height = 29.7, units = "cm", scale = 4)
# ggsave(p3, filename = "test_technique.pdf", width = 21, height = 29.7, units = "cm", scale = 4)

foo_scaled <- foo %>%
  group_by(Indicateur) %>%
  mutate(scaled_Valeur = scale(Valeur, center = TRUE, scale = TRUE)) %>%
  ungroup()

p1_scaled <- ggplot(foo_scaled %>% filter(type == "input")) +
  aes(Indicateur, scaled_Valeur, fill = Indicateur) +
  geom_violin() +
  facet_grid(sensibility_parameter~.) +
  scale_y_continuous(limits = c(-10.1, 10.1)) +
  guides(fill = FALSE)

p2_scaled <- ggplot(foo_scaled %>% filter(type == "contexte")) +
  aes(Indicateur, scaled_Valeur, fill = Indicateur) +
  geom_violin() +
  facet_grid(sensibility_parameter~.) +
  scale_y_continuous(limits = c(-10.1, 10.1)) +
  guides(fill = FALSE)

p3_scaled <- ggplot(foo_scaled %>% filter(type == "technique")) +
  aes(Indicateur, scaled_Valeur, fill = Indicateur) +
  geom_violin() +
  facet_grid(sensibility_parameter~.) +
  scale_y_continuous(limits = c(-10.1, 10.1)) +
  guides(fill = FALSE)

# ggsave(p1_scaled, filename = "test_input_scaled.pdf", width = 21, height = 29.7, units = "cm", scale = 1.5)
# ggsave(p2_scaled, filename = "test_contexte_scaled.pdf", width = 21, height = 29.7, units = "cm", scale = 1.5)
# ggsave(p3_scaled, filename = "test_technique_scaled.pdf", width = 21, height = 29.7, units = "cm", scale = 1.5)


p1_scaled_all <- ggplot(foo_scaled %>% filter(type == "input")) +
  aes(sensibility_value, scaled_Valeur, fill = sensibility_value) +
  geom_boxplot() +
  facet_wrap(sensibility_parameter~Indicateur,ncol = 8, labeller = mylabel, scales = "free_x") +
  scale_y_continuous(limits = c(-10.1, 10.1)) +
  guides(fill = FALSE)

p2_scaled_all <- ggplot(foo_scaled %>% filter(type == "contexte")) +
  aes(sensibility_value, scaled_Valeur, fill = sensibility_value) +
  geom_boxplot() +
  facet_wrap(sensibility_parameter~Indicateur, ncol = 8,labeller = mylabel, scales = "free_x") +
  scale_y_continuous(limits = c(-10.1, 10.1)) +
  guides(fill = FALSE)

p3_scaled_all <- ggplot(foo_scaled %>% filter(type == "technique")) +
  aes(sensibility_value, scaled_Valeur, fill = sensibility_value) +
  geom_boxplot() +
  facet_wrap(sensibility_parameter~Indicateur, ncol = 8, labeller = mylabel, scales = "free_x") +
  scale_y_continuous(limits = c(-10.1, 10.1)) +
  guides(fill = FALSE)

# ggsave(p1_scaled_all, filename = "test_input_all.pdf", width = 21, height = 29.7, units = "cm", scale = 4)
# ggsave(p2_scaled_all, filename = "test_contexte_all.pdf", width = 21, height = 29.7, units = "cm", scale = 4)
# ggsave(p3_scaled_all, filename = "test_technique_all.pdf", width = 21, height = 29.7, units = "cm", scale = 4)


valeurs_base %>%
  filter(sensibility_parameter %in% params_to_keep) %>%
  group_by(type) %>%
  tally()


library(ggthemes)
source("utils_recode.R")

for (i in 1:length(params_to_keep)){
  indicateur <- params_to_keep[i]
  thisplot <- valeurs_base %>%
    filter(sensibility_parameter %in% indicateur) %>%
    gather(Indicateur, Valeur, -sensibility_parameter, -sensibility_value, -type) %>%
    mutate(Indicateur = fct_recode(as.factor(Indicateur),
                                   "Nombre\nd'agrégats" = "nb_agregats",
                                   "Nombre de\ngrands\nchâteaux" = 'nb_grands_chateaux',
                                   "Nombre\nd'églises\nparoissiales" = "nb_eglises_paroissiales",
                                   "Distance\nentre\néglises" = "distance_eglises_paroissiales",
                                   "Taux de\nfoyers paysans\nisolés" = "prop_fp_isoles",
                                   "Augmentation\nde la charge\nfiscale" = "ratio_charge_fiscale"
    )) %>%
    mutate(Indicateur = fct_relevel(Indicateur, c("Nombre\nd'agrégats",
                                                  "Nombre de\ngrands\nchâteaux",
                                                  "Nombre\nd'églises\nparoissiales",
                                                  "Distance\nentre\néglises",
                                                  "Taux de\nfoyers paysans\nisolés",
                                                  "Augmentation\nde la charge\nfiscale"
    ))) %>%
    mutate(objectif = case_when(
      Indicateur == "Nombre\nd'agrégats" ~ 200,
      Indicateur == "Nombre de\ngrands\nchâteaux" ~ 10,
      Indicateur == "Nombre\nd'églises\nparoissiales" ~ 300,
      Indicateur == "Distance\nentre\néglises" ~ 3000,
      Indicateur == "Taux de\nfoyers paysans\nisolés" ~ 0.20,
      Indicateur == "Augmentation\nde la charge\nfiscale" ~ 3
      )) %>%
    rename(Parametre = sensibility_parameter) %>%
    mutate(sensibility_value = rename_and_recode_valeurs(indicateur = indicateur,
                                                         valeurs = sensibility_value)) %>%
    mutate(Indicateur = fct_relabel(Indicateur, ~str_replace_all(.x, "\n", " "))) %>%
    ggplot() +
    aes(sensibility_value, Valeur, fill = sensibility_value) +
    #geom_boxplot(lwd = .1, outlier.size = .75, outlier.shape = 4) +
    geom_hline(aes(yintercept = objectif), linetype = "dashed", lwd = .5) +
    geom_violin(lwd = 0.2) + 
    geom_boxplot(coef = 0, width = 0.2, lwd = 0.2, outlier.shape = NA, colour = "white") +
    facet_wrap(~Indicateur, scales = "free", drop = TRUE, ncol = 6, labeller = label_wrap_gen(width = 20)) +
    scale_fill_viridis_d(name = indicateur, end = .8) +
    labs(x = "", y = "") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(colour = "black", size = 8, margin = margin(t = 0,r = 15,b = 0,l = 0, unit = "pt")) ) +
    theme(legend.position="bottom",
          legend.justification="left",
          legend.title.align = 0,
          legend.title = element_text(face = "bold", vjust = 1),
          legend.key.width = unit(x = .75, units = "cm"),
          legend.key.height = unit(x = .75, units = "cm"),
          legend.margin=margin(-20,0,0,0),
          legend.box.margin=margin(0,0,0,0)) +
    theme(strip.text.x = element_text(margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "pt")))
  #thisplot
  ggsave(paste0("../3_results/params/sensibilite_", indicateur, ".pdf"), plot = thisplot, width = 20, height = 5, units = "cm", scale = 1.2)
}






