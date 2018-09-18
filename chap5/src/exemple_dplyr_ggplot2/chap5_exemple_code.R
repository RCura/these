library(RJDBC)
library(DBI)
library(dplyr)
library(ggplot2)
library(ggthemes)

options( java.parameters = c("-Xss2560k", "-Xmx7g") ) # Needed fix for rJava (JDBC) + ggplot2

drv <- JDBC("com.mapd.jdbc.MapDDriver",
            "/data/user/c/rcura/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
            identifier.quote="'")
conMapD <- dbConnect(drv, "jdbc:mapd:mapdi.cura.info:9091:mapd", "mapd", "HyperInteractive")

NombreAgregatParAnnee <- tbl(conMapD, "agregats") %>% # Connexion à la table agregats de la BDD
  filter(sim_name == "5_0") %>% # Filtre de la table en ne conservant que les experiences "5_0"
  group_by(sim_name, seed, annee) %>% # Agrégation sur les identifiants de simulation (seed et sim_name) et par annee
  summarise(NbAgregats = n()) %>% # Calcul du nombre total de lignes pour chaque agrégation
  arrange(sim_name, seed, annee) %>% # Tri de la table selon les trois variables
  collect() %>% # Récupération du résultat de la requête en mémoire
  mutate(annee = factor(annee)) # Conversion du champ "annee" de numérique à facteur

NombreAgregatParAnnee %>% head(5)
# # A tibble: 5 x 4
# # Groups:   sim_name, seed [4]
# sim_name seed               annee NbAgregats
# <chr>    <chr>              <fct>      <dbl>
# 1 5_0      0.0121401919575407 820          25.
# 2 5_0      0.0121401919575407 840          27.
# 3 5_0      0.0121401919575407 860          34.
# 4 5_0      0.0121401919575407 880          41.
# 5 5_0      0.0121401919575407 900          62.

NombreAgregatParAnnee %>% tail(5)
# # A tibble: 5 x 4
# # Groups:   sim_name, seed [1]
# sim_name seed              annee NbAgregats
# <chr>    <chr>             <fct>      <dbl>
# 1 5_0      0.909195338056078 1080        104.
# 2 5_0      0.909195338056078 1100         98.
# 3 5_0      0.909195338056078 1120        106.
# 4 5_0      0.909195338056078 1140        118.
# 5 5_0      0.909195338056078 1160        113.

tbl(conMapD, "agregats") %>% # Connexion à la table agregats de la BDD
  filter(sim_name == "5_0") %>% # Filtre de la table en ne conservant que les simulations de la version "5_0"
  group_by(sim_name, seed, annee) %>% # Agrégation sur les identifiants de simulation (seed et sim_name) et par annee
  summarise(NbAgregats = n()) %>% # Calcul du nombre total de lignes pour chaque agrégation
  arrange(sim_name, seed, annee) %>% # Tri de la table selon les trois variables
  show_query()

# SELECT "sim_name", "seed", "annee", COUNT() AS "NbAgregats"
# FROM "agregats"
# WHERE ("sim_name" = '5_0')
# GROUP BY "sim_name", "seed", "annee"
# ORDER BY "sim_name", "seed", "annee"

ggplot(NombreAgregatParAnnee) +
  aes(x = annee, y = NbAgregats) +
  geom_tufteboxplot()



  


