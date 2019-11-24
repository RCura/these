library(tidyverse)

results_base <- tribble(
  ~Indicateur, ~Objectif, ~Moyenne, ~Mediane, ~Q1, ~Q3, ~StDev,
  "nb_agregats", 200, 249, 248, 244, 253, 10.45,
  "nb_grands_chateaux", 10, 15, 15, 13, 17, 2.87,
  "nb_eglises_paroissiales", 300, 348, 348, 338, 359, 12.96,
  "distance_eglises_paroissiales", 3000, 1459, 1456, 1391, 1537, 97,
  "prop_fp_isoles", 0.2, 0.3, 0.3, 0.3, 0.3, 0.008,
  "ratio_charge_fiscale", 3, 2.4, 2.4, 2.4, 2.5, 0.03
  ) %>%
  mutate(CV = StDev/Moyenne) %>%
  print(.)

results_base %>%
  bind_rows(results_base %>% summarise_all(~mean(.)) %>%
              mutate(Indicateur = "Moyenne",
                     StDev = NULL, Moyenne = NULL)) %>%
  select(Indicateur, Moyenne, StDev, CV) %>%
  kableExtra::kable("latex") %>%
  pack_rows("Moyenne",7,6,latex_gap_space ="2em")



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


valeurs_reduites <- valeurs_base %>%
  mutate(
    nb_agregats_normalise = nb_agregats / 10.45,
    nb_grands_chateaux_normalise = nb_grands_chateaux / 2.87,
    nb_eglises_paroissiales_normalise = nb_eglises_paroissiales / 12.96,
    distance_eglises_paroissiales_normalise = distance_eglises_paroissiales / 97,
    prop_fp_isoles_normalise = prop_fp_isoles / 0.008,
    ratio_charge_fiscale_normalise = ratio_charge_fiscale / 0.05
  )  %>%
  select_at(vars(ends_with("_normalise"), starts_with("sensibility"), "type"))

randomness <- valeurs_reduites %>%
  gather(Indicateur, valeur, -sensibility_parameter, -sensibility_value, -type) %>%
  mutate(Indicateur = str_remove(Indicateur, "_normalise")) %>%
  mutate(valeur = as.numeric(valeur)) %>%
  group_by(sensibility_parameter, sensibility_value, Indicateur) %>%
  summarise(ecartype = sd(valeur, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sensibility_parameter = str_replace_all(sensibility_parameter, "_", " ")) %>%
  mutate(indicateur_ordre = case_when(
    Indicateur == "nb_agregats" ~ 1,
    Indicateur == "nb_grands_chateaux" ~ 2,
    Indicateur == "nb_eglises_paroissiales" ~ 3,
    Indicateur == "distance_eglises_paroissiales" ~ 4,
    Indicateur == "prop_fp_isoles" ~ 5,
    Indicateur == "ratio_charge_fiscale" ~ 6,
  )) %>%
  mutate(Indicateur = fct_recode(as.factor(Indicateur),
                                 "Nombre\nd'agrégats" = "nb_agregats",
                                 "Nombre de\ngrands\nchâteaux" = 'nb_grands_chateaux',
                                 "Nombre\nd'églises\nparoissiales" = "nb_eglises_paroissiales",
                                 "Distance\nentre\néglises" = "distance_eglises_paroissiales",
                                 "Taux de\nfoyers paysans\nisolés" = "prop_fp_isoles",
                                 "Augmentation\nde la charge\nfiscale" = "ratio_charge_fiscale"
  ))  %>%
  mutate(Indicateur = fct_reorder(Indicateur, indicateur_ordre)) %>%
  select(-indicateur_ordre)
  
foo <- ggplot(randomness %>%
                filter(ecartype > 1.666) %>%
                mutate(sensibility_parameter = fct_reorder(sensibility_parameter, ecartype, .desc = FALSE))
              ) +
  aes(sensibility_value, ecartype, fill = Indicateur) +
  geom_col(width = .5, position = position_dodge()) +
  facet_grid(Indicateur~sensibility_parameter, scales = "free_x",labeller = labeller(
    .rows = label_wrap_gen(width = 20),
    .cols = label_wrap_gen(width = 8))
    )+
  scale_fill_viridis_d() +
  labs(title = "Valeurs de paramètres présentant les plus fortes variabilités à l'aléa",
       subtitle = "Les valeurs isolées sont celles qui présentent un écart-type supérieur de 66.6% à l'écart-type des simulations calibrées",
       x = "Valeurs de paramètres",
       y = "Écart-type des réplications") +
  guides(fill = FALSE) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, size = 8, vjust = 1, hjust = 1), 
        axis.title.x = element_text(margin = margin(t = -16, unit = "pt")),
        strip.text.y = element_text(margin = margin(l = 2, r = 2, unit = "pt")),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
        )

foo

ggsave(foo, filename = "../3_results/sensiblite_alea.pdf", width = 20, height = 15, units = "cm", scale = 1.65)

histo <- ggplot(randomness) +
  aes(ecartype) +
  geom_histogram(aes(fill = Indicateur), binwidth = 0.1) +
  facet_wrap(~Indicateur, nrow = 1, labeller = label_wrap_gen(width = 20)) +
  guides(fill = FALSE) +
  scale_fill_viridis_d(end = 0.8) +
  labs(title = "Distribution de la sensibilité à l'aléa",
       x = "Incertitude des simulations",
       y = "Fréquence") +
  theme_light()

histo

ggsave(histo, filename = "../3_results/histo_sensibilite_alea.pdf", width = 20, height = 7, units = "cm", scale = 1)


alea_new <- randomness %>%
  filter(
    (Indicateur == "Nombre\nd'agrégats" & (ecartype > 1.5 | ecartype < 0.5)) |
    (Indicateur == "Nombre de\ngrands\nchâteaux" & ecartype > 2) |
    (Indicateur == "Nombre\nd'églises\nparoissiales" & ecartype > 1.8) |
    (Indicateur == "Distance\nentre\néglises" & ecartype > 1.6) |
    (Indicateur == "Taux de\nfoyers paysans\nisolés" & ecartype < 0.5) |
    (Indicateur == "Augmentation\nde la charge\nfiscale" & ecartype > 1.5)
    ) %>%
  mutate(sensibility_parameter = fct_reorder(sensibility_parameter, ecartype, .desc = FALSE)) %>%
  ggplot() +
  aes(sensibility_value, ecartype, fill = Indicateur) +
  geom_col(width = .5, position = position_dodge()) +
  facet_grid(Indicateur~sensibility_parameter, scales = "free_x",labeller = labeller(
    .rows = label_wrap_gen(width = 15),
    .cols = label_wrap_gen(width = 8))
  )+
  scale_fill_viridis_d() +
  labs(title = "Valeurs de paramètres présentant une variabilité à l'aléa atypique",
       x = "Valeurs de paramètres",
       y = "Rapport à la variabilité de référence (écart-type)") +
  guides(fill = FALSE) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, size = 8, vjust = 1, hjust = 1), 
        axis.title.x = element_text(margin = margin(t = -16, unit = "pt")),
        strip.text.y = element_text(margin = margin(l = 2, r = 2, unit = "pt")),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  )

ggsave(alea_new, filename = "../3_results/sensibilite_alea_outliers.pdf",width = 20, height = 15, units = "cm", scale = 1.35)
#extrafont::embed_fonts("../3_results/sensibilite_alea_outliers.pdf")
