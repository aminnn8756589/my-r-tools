# ====================== INSPECTION PROFESSIONNELLE - VERSION POUR BEAUCOUP DE VARIABLES ======================
# Fonction : inspection()
# Adaptée aux dataframes avec un grand nombre de variables

library(tidyverse)
library(skimr)
library(naniar)
library(visdat)
library(janitor)
library(gridExtra)

inspection <- function(df, title = "Rapport d'Inspection des Données") {
  
  if (!is.data.frame(df)) stop("L'argument doit être un dataframe ou tibble.")
  
  cat("\n", strrep("=", 90), "\n")
  cat("               ", title, "\n")
  cat("               Dataframe :", nrow(df), "lignes ×", ncol(df), "colonnes\n")
  cat(strrep("=", 90), "\n\n")
  
  # 1. Structure générale
  cat("1. STRUCTURE GÉNÉRALE\n")
  cat("Nombre d'observations :", nrow(df), "\n")
  cat("Nombre de variables   :", ncol(df), "\n")
  cat("Types de variables    :\n")
  glimpse(df)   # Plus lisible que skimr complet quand il y a beaucoup de colonnes
  cat("\n")
  
  # 2. Doublons
  cat("2. DOUBLONS\n")
  dupes <- janitor::get_dupes(df)
  
  if (nrow(dupes) == 0) {
    cat("✓ Aucune ligne dupliquée détectée.\n\n")
  } else {
    nb_doublons <- nrow(dupes) / 2
    cat("⚠ Nombre de lignes dupliquées :", round(nb_doublons), "\n")
    cat("Pourcentage de lignes dupliquées :", round(100 * nb_doublons / nrow(df), 2), "%\n")
    cat("Exemple des premières doublons :\n")
    print(head(dupes, 6))
    cat("\n")
  }
  
  # 3. Tableau récapitulatif variable par variable (très lisible)
  cat("3. RÉCAPITULATIF VARIABLE PAR VARIABLE\n")
  cat("(Trié par pourcentage de valeurs manquantes décroissant)\n\n")
  
  recap <- df %>%
    summarise(across(everything(), list(
      type = ~ class(.)[1],
      n_missing = ~ sum(is.na(.)),
      pct_missing = ~ round(100 * mean(is.na(.)), 2),
      n_unique = ~ length(unique(.)),
      pct_unique = ~ round(100 * length(unique(.)) / n(), 2)
    ))) %>%
    pivot_longer(everything(), names_to = c(".value", "variable"), names_sep = "_") %>%
    mutate(variable = names(df)) %>%
    select(variable, type, n_missing, pct_missing, n_unique, pct_unique) %>%
    arrange(desc(pct_missing))
  
  print(recap, n = Inf)   # Affiche tout le tableau
  
  # Pourcentage global de missing
  pct_global <- round(100 * mean(is.na(df)), 2)
  cat("\nPourcentage global de valeurs manquantes dans tout le dataframe :", pct_global, "%\n\n")
  
  # 4. Visualisations des valeurs manquantes (toujours utiles même avec beaucoup de variables)
  cat("4. VISUALISATIONS DES VALEURS MANQUANTES\n")
  
  p1 <- visdat::vis_miss(df, cluster = TRUE, warn_large_data = FALSE) + 
        ggtitle("Carte des données manquantes (vis_miss)")
  
  p2 <- naniar::gg_miss_var(df, show_pct = TRUE) + 
        ggtitle("Pourcentage de valeurs manquantes par variable") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 8))   # Plus lisible avec beaucoup de vars
  
  print(p1)
  print(p2)
  
  # 5. Résumé rapide des outliers (sans boxplots détaillés)
  cat("\n5. RÉSUMÉ OUTLIERS (variables numériques uniquement)\n")
  num_vars <- df %>% select(where(is.numeric)) %>% names()
  
  if (length(num_vars) == 0) {
    cat("Aucune variable numérique détectée.\n")
  } else {
    cat("Nombre de variables numériques :", length(num_vars), "\n")
    cat("Pour une analyse détaillée des outliers, utilisez une fonction de nettoyage séparée.\n")
  }
  
  cat("\n", strrep("=", 90), "\n")
  cat("FIN DU RAPPORT D'INSPECTION\n")
  cat("Aucune modification apportée au dataframe.\n")
  cat("Ce rapport est optimisé pour les jeux de données avec un grand nombre de variables.\n")
  cat(strrep("=", 90), "\n\n")
  
  # Retour invisible
  invisible(list(
    dimensions = dim(df),
    duplicates = if(exists("nb_doublons")) nb_doublons else 0,
    recap_table = recap,
    global_missing_pct = pct_global
  ))
}
