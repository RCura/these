NombreAgregatParAnnee <- tbl(conMapD, "agregats") %>% # Connexion à la table agregats de la BDD
  filter(sim_name == "5_0") %>% # Filtre de la table en ne conservant que les experiences "5_0"
  group_by(sim_name, seed, annee) %>% # Agrégation sur les identifiants de simulation (seed et sim_name) et par annee
  summarise(NbAgregats = n()) %>% # Calcul du nombre total de lignes pour chaque agrégation
  arrange(sim_name, seed, annee) %>% # Tri de la table selon les trois variables
  collect() # Récupération du résultat de la requête en mémoire