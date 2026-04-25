# ====================== INSPECTION PROFESSIONNELLE DES DONNÉES ======================
# Fonction : inspection()
# Version corrigée et robuste - Compatible avec tous types de données
# Auteur : Style Dr. BELAIDI Mohamed Amine

library(tidyverse)
library(skimr)
library(naniar)
library(visdat)
library(janitor)
library(gridExtra)
library(patchwork)   # Pour une meilleure organisation des graphiques

inspection <- function(df, title = "Rapport d'Inspection des Données") {
  
  if (!is.data.frame(df)) {
    stop("L'argument doit être un dataframe ou un tibble.")
  }
  
  cat("\n", strrep("=", 85), "\n")
  cat("               ", title, "\n")
  cat(strrep("=", 85), "\n\n")
  
  # 1. Structure générale
  cat("1. STRUCTURE GÉNÉRALE\n")
  cat("Nombre de lignes     :", nrow(df), "\n")
  cat("Nombre de colonnes   :", ncol(df), "\n\n")
  
  cat("Résumé détaillé des variables :\n")
  print(skimr::skim(df))
  cat("\n")
  
  # 2. Doublons
  cat("2. DOUBLONS\n")
  dupes <- janitor::get_dupes(df)
  
  if (nrow(dupes) == 0) {
    cat("✓ Aucune ligne dupliquée détectée.\n\n")
  } else {
    nb_doublons <- nrow(dupes) / 2   # Chaque doublon apparaît au moins 2 fois
    cat("⚠ Nombre de lignes dupliquées :", round(nb_doublons), "\n")
    cat("Exemple des doublons :\n")
    print(head(dupes, 8))
    cat("\n")
  }
  
  # 3. Valeurs manquantes
  cat("3. VALEURS MANQUANTES\n")
  miss_summary <- naniar::miss_var_summary(df)
  print(miss_summary)
  
  total_missing <- sum(is.na(df))
  pct_global <- round(100 * total_missing / (nrow(df) * ncol(df)), 2)
  cat("\nPourcentage global de valeurs manquantes :", pct_global, "%\n\n")
  
  # Visualisations des manquants
  cat("Visualisations des valeurs manquantes :\n")
  
  p1 <- visdat::vis_miss(df, cluster = TRUE) + 
        ggtitle("Carte des données manquantes")
  
  p2 <- naniar::gg_miss_var(df, show_pct = TRUE) + 
        ggtitle("Pourcentage de NA par variable") +
        theme_minimal()
  
  print(p1)
  print(p2)
  
  # UpSet plot si pas trop de variables
  if (ncol(df) <= 15) {
    p3 <- naniar::gg_miss_upset(df, nsets = min(10, ncol(df))) + 
          ggtitle("Combinaisons de valeurs manquantes")
    print(p3)
  } else {
    cat("Trop de variables pour afficher l'UpSet plot (limité à 15 variables).\n")
  }
  
  # 4. Valeurs aberrantes (uniquement variables numériques)
  cat("\n4. VALEURS ABERRANTES (Outliers)\n")
  
  # Sélection sécurisée des variables numériques
  num_vars <- df %>% 
    select(where(is.numeric)) %>% 
    names()
  
  if (length(num_vars) == 0) {
    cat("Aucune variable numérique détectée dans le jeu de données.\n")
  } else {
    cat("Variables numériques analysées (", length(num_vars), ") : ", 
        paste(num_vars, collapse = ", "), "\n\n")
    
    # Création des boxplots
    plot_list <- list()
    for (var in num_vars) {
      p <- ggplot(df, aes(y = .data[[var]])) +
        geom_boxplot(fill = "#2c7be5", alpha = 0.8, 
                     outlier.colour = "red", outlier.shape = 16, outlier.size = 2.5) +
        theme_minimal(base_size = 11) +
        labs(title = paste("Boxplot -", var), y = var)
      plot_list[[var]] <- p
    }
    
    # Affichage des boxplots (3 par ligne maximum)
    n_plots <- length(plot_list)
    ncol_plots <- min(3, n_plots)
    do.call(grid.arrange, c(plot_list, ncol = ncol_plots))
    
    # Résumé détaillé des outliers
    cat("\nDétection des outliers (règle des 1.5 × IQR) :\n")
    for (var in num_vars) {
      x <- df[[var]]
      if (all(is.na(x))) {
        cat(sprintf("- %s : toutes les valeurs sont NA\n", var))
        next
      }
      
      q1 <- quantile(x, 0.25, na.rm = TRUE)
      q3 <- quantile(x, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      
      n_outliers <- sum(x < lower_bound | x > upper_bound, na.rm = TRUE)
      pct_out <- round(100 * n_outliers / length(x), 2)
      
      cat(sprintf("- %-15s : %4d outliers (%5.2f%%) | Bornes : [%8.2f ; %8.2f]\n", 
                  var, n_outliers, pct_out, lower_bound, upper_bound))
    }
  }
  
  cat("\n", strrep("=", 85), "\n")
  cat("FIN DU RAPPORT D'INSPECTION\n")
  cat("Aucune modification n'a été apportée à votre dataframe 'df'.\n")
  cat("Utilisez ce rapport pour décider des étapes de nettoyage.\n")
  cat(strrep("=", 85), "\n\n")
  
  # Retour invisible pour pouvoir l'assigner si besoin
  invisible(list(
    dimensions = dim(df),
    duplicates = if(exists("nb_doublons")) nb_doublons else 0,
    missing_summary = miss_summary,
    numeric_variables = num_vars
  ))
}
