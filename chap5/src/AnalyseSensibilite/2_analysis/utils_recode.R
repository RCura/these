rename_and_recode_valeurs <- function(indicateur, valeurs){
  if (indicateur == "croissance_demo") {
    base <- c("0", "0.0153", "0.0372", "0.0589", "0.1289")
    names(base) <- c("0%", "1.53%", "3.72%", "5.89%", "12.89%")
  }
  if (indicateur == "debut_construction_chateaux") {
    base <- c("820", "880", "940", "1000", "1060")
    names(base) <- base
  }
  if (indicateur == "dist_minmax_eglise") {
    base <- c("dynamique_reduit", "statique_reduit", "base", "statique_large", "dynamique_large")
    names(base) <- base
  }
  if (indicateur == "distance_detection_agregat") {
    base <- c("50", "100", "150", "200", "300")
    names(base) <- base
  }
  if (indicateur == "droits_fonciers_zp") {
    base <- c("0", "0.5", "1", "1.5", "2")
    names(base) <- base
  }
  if (indicateur == "nb_min_fp_agregat") {
    base <- c("3", "5", "7", "10", "15")
    names(base) <- base
  }
  if (indicateur == "nb_tirages_chateaux_ps") {
    base <- c("0", "1", "2", "3", "4")
    names(base) <- base
  }
  if (indicateur == "periode_promotion_chateaux") {
    base <- c("map([800::false,940::true,1020::false])",
              "map([800::false,940::true,1060::false])", "map([800::false,1000::true,1120::false])",
              "map([800::false,1100::true])", "map([800::false,940::true])")
    names(base) <- c("940-1000", "940-1040", "1000-1100", "1100-1200", "940-1200")
  }
  if (indicateur == "ponderation_creation_paroisse_agregat") {
    base <- c("500", "1000", "2000", "3000", "5000")
    names(base) <- base
  }
  if (indicateur == "proba_creation_zp_autres_droits_ps") {
    base <- c("0.0", "0.05", "0.15", "0.25", "0.35")
    names(base) <- c("0%", "5%", "15%", "25%", "35%")
  }
  if (indicateur == "proba_gain_haute_justice_gs") {
    base <- c( "map([800::0.0])", "map([800::0.0,1000::0.5])",
               "map([800::0.0,900::0.1,920::0.2,940::0.3,960::0.4,980::0.5,1000::0.6,1020::0.7,1040::0.8,1060::0.8,1080::0.9,1100::1.0])",
               "map([800::0.0,900::0.2,1000::1.0])",
               "map([800::1.0])")
    names(base) <- c("statique_nul", "croissant_seuil", "croissant_regulier", "base", "statique_fort")
  }
  if (indicateur == "rayon_migration_locale_fp") {
    base <- c("map([800::1000])", "map([800::1000,1000::2500])", "map([800::2500])",
              "map([800::2500,1000::5000])", "map([800::5000,1000::10000])")
    names(base) <- c("statique_reduit","dynamique_reduit","base","dynamique_croissant","dynamique_large")
  }
  if (indicateur == "taille_cote_monde") {
    base <- c("50", "75", "100", "125", "150")
    names(base) <- base
  }
  if (indicateur == "taux_prelevement_zp_chateau") {
    base <- c("0.0", "0.25", "0.5", "0.75", "1.0")
    names(base) <- c("0%", "25%", "50%", "75%", "100%")
  }
  new_factor <- fct_recode(valeurs, base[1], base[2], base[3], base[4], base[5])
  recoded_factor <- fct_relevel(new_factor, names(base))
}