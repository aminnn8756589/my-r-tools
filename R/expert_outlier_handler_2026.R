library(dplyr)
library(ggplot2)

expert_outlier_handler_2026 <- function(df,
                                        columns = NULL,                    # NULL = toutes les numériques
                                        detection_method = "ensemble",     # "iqr", "mad" ou "ensemble" (le plus pro)
                                        treatment = "cap",                 # "cap", "remove", "impute_median", "none"
                                        cap_type = "percentile",           # "percentile" (recommandé 2026), "iqr" ou "mad"
                                        percentile_bounds = c(0.05, 0.95), # 5% - 95% = choix optimal 2026
                                        visualize = TRUE,
                                        print_report = TRUE) {
  
  # Sélection des colonnes numériques
  if (is.null(columns)) {
    columns <- names(df)[sapply(df, is.numeric)]
  }
  
  df_original <- df
  df_treated <- df
  outlier_summary <- data.frame(
    Column = character(),
    Outliers_IQR = numeric(),
    Outliers_MAD = numeric(),
    Outliers_Ensemble = numeric(),
    Outliers_Selected = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (col in columns) {
    x <- df[[col]]
    x_clean <- x[!is.na(x)]
    if (length(x_clean) == 0) next
    
    # === 1. Détection IQR (classique mais toujours utile) ===
    Q1 <- quantile(x_clean, 0.25, na.rm = TRUE)
    Q3 <- quantile(x_clean, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    lower_iqr <- Q1 - 1.5 * IQR_val
    upper_iqr <- Q3 + 1.5 * IQR_val
    outliers_iqr <- x < lower_iqr | x > upper_iqr
    
    # === 2. Détection MAD / Modified Z-score (méthode robuste 2026) ===
    med <- median(x_clean, na.rm = TRUE)
    mad_val <- mad(x_clean, na.rm = TRUE)
    if (mad_val == 0) mad_val <- 1e-6
    mod_z <- 0.6745 * (x - med) / mad_val
    outliers_mad <- abs(mod_z) > 3.5
    
    # === 3. Ensemble (le plus fiable : flagged par les DEUX méthodes) ===
    outliers_ensemble <- outliers_iqr & outliers_mad
    
    # Choix de la méthode de détection
    if (detection_method == "iqr") {
      outliers <- outliers_iqr
      lower_bound <- lower_iqr
      upper_bound <- upper_iqr
      num_out <- sum(outliers, na.rm = TRUE)
    } else if (detection_method == "mad") {
      outliers <- outliers_mad
      lower_bound <- med - (3.5 / 0.6745) * mad_val
      upper_bound <- med + (3.5 / 0.6745) * mad_val
      num_out <- sum(outliers, na.rm = TRUE)
    } else {  # ensemble (recommandé)
      outliers <- outliers_ensemble
      lower_bound <- min(lower_iqr, med - (3.5 / 0.6745) * mad_val)
      upper_bound <- max(upper_iqr, med + (3.5 / 0.6745) * mad_val)
      num_out <- sum(outliers, na.rm = TRUE)
    }
    
    # Ajout au rapport
    outlier_summary <- rbind(outlier_summary, data.frame(
      Column = col,
      Outliers_IQR = sum(outliers_iqr, na.rm = TRUE),
      Outliers_MAD = sum(outliers_mad, na.rm = TRUE),
      Outliers_Ensemble = sum(outliers_ensemble, na.rm = TRUE),
      Outliers_Selected = num_out,
      stringsAsFactors = FALSE
    ))
    
    # === TRAITEMENT ===
    if (treatment == "remove") {
      df_treated <- df_treated[!outliers | is.na(df_treated[[col]]), ]
    } else if (treatment == "cap") {
      if (cap_type == "percentile") {
        lower_cap <- quantile(x_clean, percentile_bounds[1], na.rm = TRUE)
        upper_cap <- quantile(x_clean, percentile_bounds[2], na.rm = TRUE)
      } else if (cap_type == "iqr") {
        lower_cap <- lower_iqr
        upper_cap <- upper_iqr
      } else {
        lower_cap <- lower_bound
        upper_cap <- upper_bound
      }
      df_treated[[col]] <- pmax(pmin(df_treated[[col]], upper_cap), lower_cap)
    } else if (treatment == "impute_median") {
      df_treated[[col]][outliers] <- med
    }
    
    # === VISUALISATION AVANT / APRÈS ===
    if (visualize) {
      plot_data <- data.frame(
        Value = c(df_original[[col]], df_treated[[col]]),
        Type = rep(c("Avant", "Après"), each = nrow(df_original))
      ) %>% na.omit()
      
      p <- ggplot(plot_data, aes(x = Type, y = Value, fill = Type)) +
        geom_boxplot(alpha = 0.8) +
        labs(title = paste("Outliers :", col, 
                           "- Détection :", detection_method,
                           "| Traitement :", treatment),
             subtitle = paste("Nombre d'outliers détectés :", num_out),
             y = col) +
        scale_fill_manual(values = c("Avant" = "#1f77b4", "Après" = "#2ca02c")) +
        theme_minimal(base_size = 14)
      
      print(p)   # affiche immédiatement chaque graphique
    }
  }
  
  # === RAPPORT FINAL ===
  if (print_report) {
    cat("\n=== RAPPORT EXPERT OUTLIER HANDLER 2026 ===\n")
    cat("Méthode de détection :", detection_method, "\n")
    cat("Traitement appliqué   :", treatment, "\n")
    cat("Capping (si cap)      :", paste0(percentile_bounds[1]*100, "% - ", percentile_bounds[2]*100, "%\n\n"))
    print(outlier_summary, row.names = FALSE)
    cat("\n✅ Traitement terminé ! Ton dataframe traité s'appelle maintenant `df_treated`.\n")
  }
  
  invisible(list(
    treated_df = df_treated,
    report = outlier_summary
  ))
}
