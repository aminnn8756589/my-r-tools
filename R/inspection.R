# ====================== INSPECTION PROFESSIONNELLE DES DONNÉES ======================
# Fonction : inspection()
# Version optimisée pour R Markdown + grand nombre de variables

library(tidyverse)
library(skimr)
library(naniar)
library(visdat)
library(janitor)

inspection <- function(df, title = "Rapport d'Inspection des Données") {
  
  if (!is.data.frame(df)) {
    stop("L'argument df doit être un dataframe ou un tibble.")
  }
  
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("               ", title, "\n")
  cat("               ", nrow(df), "lignes ×", ncol(df), "variables\n")
  cat(strrep("=", 90), "\n\n")
  
  # 1. Structure générale
  cat("**1. STRUCTURE GÉNÉRALE**\n\n")
  cat("- Nombre d'observations :", nrow(df), "\n")
  cat("- Nombre de variables   :", ncol(df), "\n\n")
  
  cat("Aperçu des types de variables :\n")
  print(glimpse(df))
  cat("\n")
  
  # 2. Doublons
  cat("**2. DOUBLONS**\n\n")
  dupes <- janitor::get_dupes(df)
  
  if (nrow(dupes) == 0) {
    cat("✓ Aucune ligne dupliquée détectée.\n\n")
  } else {
    nb_doublons <- nrow(dupes) / 2
    cat("⚠ Nombre de lignes dupliquées :", round(nb_doublons), "\n")
    cat("Pourcentage de doublons :", round(100 * nb_doublons / nrow(df), 2), "%\n")
    cat("\nExemple des premières lignes dupliquées :\n")
    print(head(dupes, 6))
    cat("\n")
  }
  
  # 3. Tableau récapitulatif variable par variable (CORRIGÉ)
  cat("**3. RÉCAPITULATIF VARIABLE PAR VARIABLE**\n")
  cat("(Trié par pourcentage de valeurs manquantes décroissant)\n\n")
  
  recap <- df %>%
    summarise(across(everything(), list(
      type       = ~ class(.)[1],
      n_missing  = ~ sum(is.na(.)),
      pct_missing = ~ round(100 * mean(is.na(.)), 2),
      n_unique   = ~ n_distinct(.),
      pct_unique = ~ round(100 * n_distinct(.) / n(), 2)
    ))) %>%
    pivot_longer(everything(), 
                 names_to = c(".value", "variable"), 
                 names_sep = "_") %>%
    arrange(desc(pct_missing))
  
  # Affichage propre du tableau
  print(recap, n = Inf)
  
  # Pourcentage global
  pct_global <- round(100 * mean(is.na(df)), 2)
  cat("\n**Pourcentage global de valeurs manquantes :**", pct_global, "%\n\n")
  
  # 4. Visualisations des valeurs manquantes
  cat("**4. VISUALISATIONS DES VALEURS MANQUANTES**\n\n")
  
  p1 <- vis_miss(df, cluster = TRUE, warn_large_data = FALSE) + 
        ggtitle("Carte des données manquantes")
  
  p2 <- gg_miss_var(df, show_pct = TRUE) + 
        ggtitle("Pourcentage de valeurs manquantes par variable") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 9))
  
  print(p1)
  print(p2)
  
  # 5. Résumé outliers (léger)
  cat("\n**5. RÉSUMÉ OUTLIERS**\n")
  num_vars <- names(df)[sapply(df, is.numeric)]
  cat("Nombre de variables numériques :", length(num_vars), "\n")
  if (length(num_vars) > 0) {
    cat("Variables :", paste(num_vars, collapse = ", "), "\n")
  }
  cat("→ Pour une analyse détaillée des outliers, utilisez une fonction de nettoyage séparée.\n\n")
  
  cat(strrep("=", 90), "\n")
  cat("FIN DU RAPPORT D'INSPECTION\n")
  cat("Aucune modification n'a été apportée au dataframe.\n")
  cat(strrep("=", 90), "\n\n")
  
  # Retour invisible (utile pour knitr)
  invisible(list(
    dimensions = dim(df),
    recap_table = recap,
    global_missing_pct = pct_global
  ))
}
