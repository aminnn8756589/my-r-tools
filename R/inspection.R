# ====================== INSPECTION PROFESSIONNELLE DES DONNÉES ======================
# Fonction : inspection()
# Auteur  : Style Dr. BELAIDI Mohamed Amine - Ultra professionnel pour mémoires

library(tidyverse)
library(skimr)      # Résumé élégant et complet
library(naniar)     # Analyse et visualisation des données manquantes
library(visdat)     # Carte visuelle des données manquantes
library(janitor)    # Détection de doublons
library(gridExtra)  # Pour organiser plusieurs graphiques

inspection <- function(df, title = "Rapport d'Inspection des Données") {
  
  cat("\n", strrep("=", 80), "\n")
  cat("               ", title, "\n")
  cat(strrep("=", 80), "\n\n")
  
  # 1. Structure générale
  cat("1. STRUCTURE GÉNÉRALE DU DATAFRAME\n")
  cat("Nombre de lignes (observations) :", nrow(df), "\n")
  cat("Nombre de colonnes (variables)  :", ncol(df), "\n")
  cat("Types de variables :\n")
  print(skimr::skim(df))
  cat("\n")
  
  # 2. Doublons
  cat("2. DOUBLONS\n")
  dup_rows <- janitor::get_dupes(df)
  nb_doublons <- nrow(dup_rows) / 2   # car chaque doublon apparaît 2 fois minimum
  
  if (nrow(dup_rows) == 0) {
    cat("✓ Aucune ligne dupliquée détectée.\n\n")
  } else {
    cat("⚠ Nombre de lignes dupliquées :", round(nb_doublons), "\n")
    cat("Exemple des premières doublons :\n")
    print(head(dup_rows, 6))
    cat("\n")
  }
  
  # 3. Valeurs manquantes
  cat("3. VALEURS MANQUANTES\n")
  miss_summary <- naniar::miss_var_summary(df)
  print(miss_summary)
  
  total_missing <- sum(is.na(df))
  pct_global <- round(100 * total_missing / (nrow(df) * ncol(df)), 2)
  cat("\nPourcentage global de valeurs manquantes :", pct_global, "%\n\n")
  
  # Visualisations des données manquantes
  cat("Visualisations des valeurs manquantes :\n")
  
  p1 <- visdat::vis_miss(df, cluster = TRUE) + 
        ggtitle("Carte des données manquantes (vis_miss)")
  
  p2 <- naniar::gg_miss_var(df, show_pct = TRUE) + 
        ggtitle("Pourcentage de valeurs manquantes par variable")
  
  p3 <- naniar::gg_miss_upset(df, nsets = min(8, ncol(df))) + 
        ggtitle("Combinaisons de valeurs manquantes (UpSet plot)")
  
  grid.arrange(p1, p2, ncol = 1)
  print(p3)
  
  # 4. Valeurs aberrantes (Outliers) - uniquement variables numériques
  cat("\n4. VALEURS ABERRANTES (Outliers)\n")
  num_vars <- df %>% select(where(is.numeric)) %>% names()
  
  if (length(num_vars) == 0) {
    cat("Aucune variable numérique détectée.\n")
  } else {
    cat("Variables numériques analysées :", paste(num_vars, collapse = ", "), "\n\n")
    
    # Boxplots pour détecter les outliers
    plots <- list()
    for (var in num_vars) {
      p <- ggplot(df, aes_string(y = var)) +
        geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
        theme_minimal() +
        labs(title = paste("Boxplot -", var), y = var)
      plots[[var]] <- p
    }
    
    # Afficher les boxplots (jusqu'à 6 par ligne)
    do.call(grid.arrange, c(plots, ncol = min(3, length(plots))))
    
    # Résumé statistique des outliers potentiels
    cat("Résumé des outliers potentiels (règle des 1.5 * IQR) :\n")
    for (var in num_vars) {
      q1 <- quantile(df[[var]], 0.25, na.rm = TRUE)
      q3 <- quantile(df[[var]], 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower <- q1 - 1.5 * iqr
      upper <- q3 + 1.5 * iqr
      
      n_outliers <- sum(df[[var]] < lower | df[[var]] > upper, na.rm = TRUE)
      pct_out <- round(100 * n_outliers / nrow(df), 2)
      
      cat(sprintf("- %s : %d outliers (%.2f%%) | Bornes : [%.2f ; %.2f]\n", 
                  var, n_outliers, pct_out, lower, upper))
    }
  }
  
  cat("\n", strrep("=", 80), "\n")
  cat("FIN DU RAPPORT D'INSPECTION - Aucune modification apportée au dataframe.\n")
  cat("Utilisez ce rapport pour décider des traitements (nettoyage, imputation, etc.)\n")
  cat(strrep("=", 80), "\n\n")
  
  # Retourne invisiblement le résumé pour pouvoir l'assigner si besoin
  invisible(list(
    dimensions = dim(df),
    duplicates = nb_doublons,
    missing_summary = miss_summary,
    numeric_vars = num_vars
  ))
}
