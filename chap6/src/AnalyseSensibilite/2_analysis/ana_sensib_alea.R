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


valeurs_normalisees <- valeurs_base %>%
  mutate(
    nb_agregats_normalise = (nb_agregats - 200) / 10.45,
    nb_grands_chateaux_normalise = (nb_grands_chateaux - 10) / 2.87,
    nb_eglises_paroissiales_normalise = (nb_eglises_paroissiales - 300) / 12.96,
    distance_eglises_paroissiales_normalise = (distance_eglises_paroissiales - 3000) / 97,
    prop_fp_isoles_normalise = (prop_fp_isoles - 0.2) / 0.008,
    ratio_charge_fiscale_normalise = (ratio_charge_fiscale - 3) / 0.05
  )  %>%
  select_at(vars(ends_with("_normalise"), starts_with("sensibility"), "type"))

randomness <- valeurs_normalisees %>%
  gather(Indicateur, valeur, -sensibility_parameter, -sensibility_value, -type) %>%
  mutate(Indicateur = str_remove(Indicateur, "_normalise")) %>%
  mutate(valeur = as.numeric(valeur)) %>%
  group_by(sensibility_parameter, sensibility_value, Indicateur) %>%
  summarise(ecartype = sd(valeur, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sensibility_parameter = str_replace_all(sensibility_parameter, "_", " ")) %>%
  mutate(Indicateur = str_replace_all(Indicateur, "_", " "))
  
foo <- ggplot(randomness %>%
                filter(ecartype > 1.666) %>%
                mutate(sensibility_parameter = fct_reorder(sensibility_parameter, ecartype, .desc = FALSE)) %>%
                mutate(Indicateur = fct_reorder(Indicateur, ecartype, .desc = FALSE))
              ) +
  aes(sensibility_value, ecartype, fill = Indicateur) +
  geom_col(width = .5, position = position_dodge()) +
  facet_grid(Indicateur~sensibility_parameter, scales = "free_x",labeller = label_wrap_gen(width=8)) +
  scale_fill_viridis_d() +
  labs(title = "Valeurs de paramètres présentant les plus fortes variabilités à l'aléa",
       subtitle = "Les valeurs isolées sont celles qui présentent un écart-type supérieur de 66.6% à l'écart-type des simulations calibrées",
       x = "Valeurs de paramètres",
       y = "Écart-type des réplications") +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(angle = 45, size = 8, vjust = 1, hjust = 1), 
        axis.title.x = element_text(margin = margin(t = -16, unit = "pt")),
        strip.text.y = element_text(margin = margin(l = 2, r = 2, unit = "pt")))

ggsave(foo, filename = "../3_results/sensiblite_alea.pdf", width = 20, height = 15, units = "cm", scale = 1.5)

histo <- ggplot(randomness) +
  aes(ecartype) +
  geom_histogram(aes(fill = Indicateur), binwidth = 0.1) +
  facet_wrap(~Indicateur, nrow = 1, labeller = label_wrap_gen(width = 12)) +
  guides(fill = FALSE) +
  labs(title = "Distribution de la sensibilité à l'aléa",
       x = "Sensibilité des réplications",
       y = "Fréquence")

histo
ggsave(histo, filename = "../3_results/histo_sensibilite_alea.pdf", width = 20, height = 8, units = "cm", scale = 1)


