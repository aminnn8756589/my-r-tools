# === CHARGE D'ABORD LES PACKAGES (obligatoire) ===
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)       # VIF
library(corrplot)  # heatmap

expert_multilinear <- function(df,
                                             target_var,
                                             predictors = NULL,
                                             scale_numerics = TRUE,
                                             create_dummies = TRUE,
                                             interaction_terms = NULL,
                                             vif_threshold = 5,
                                             print_report = TRUE,
                                             visualize = TRUE) {
  
  df_original <- df
  df_prepared <- df
  
  # 1. Sélection des prédicteurs
  if (is.null(predictors)) {
    predictors <- names(df)[names(df) != target_var]
  }
  
  # 2. Séparation numérique / catégorielle
  num_vars <- predictors[sapply(df[predictors], is.numeric)]
  cat_vars <- predictors[!sapply(df[predictors], is.numeric)]
  
  # 3. Création des dummies (version corrigée et robuste)
  if (create_dummies && length(cat_vars) > 0) {
    cat_data <- df[cat_vars]
    mm <- model.matrix(~ ., data = cat_data)          # référence automatiquement droppée
    dummies <- as.data.frame(mm)[, -1, drop = FALSE]   # supprime la colonne intercept
    names(dummies) <- gsub("`", "", names(dummies))    # nettoyage noms
    
    # Reconstruction du dataframe
    df_prepared <- df_prepared %>%
      dplyr::select(-dplyr::all_of(cat_vars)) %>%
      dplyr::bind_cols(dummies)
    
    cat("\n✅ Variables catégorielles transformées en dummies (référence droppée) :", 
        length(cat_vars), "variables →", ncol(dummies), "colonnes\n")
  }
  
  # 4. Scaling robuste (médiane + MAD)
  if (scale_numerics && length(num_vars) > 0) {
    for (col in num_vars) {
      med <- median(df_prepared[[col]], na.rm = TRUE)
      mad_val <- mad(df_prepared[[col]], na.rm = TRUE)
      if (mad_val == 0) mad_val <- sd(df_prepared[[col]], na.rm = TRUE) + 1e-6
      df_prepared[[col]] <- (df_prepared[[col]] - med) / mad_val
    }
    cat("✅ Scaling robuste (médiane + MAD) appliqué sur", length(num_vars), "variables numériques\n")
  }
  
  # 5. Interactions (optionnel)
  if (!is.null(interaction_terms)) {
    for (inter in interaction_terms) {
      vars <- strsplit(inter, "\\*")[[1]]
      if (length(vars) == 2 && all(vars %in% names(df_prepared))) {
        new_name <- paste(vars[1], vars[2], sep = "_x_")
        df_prepared[[new_name]] <- df_prepared[[vars[1]]] * df_prepared[[vars[2]]]
      }
    }
    cat("✅ Interactions ajoutées :", paste(interaction_terms, collapse = ", "), "\n")
  }
  
  # 6. Vérification multicolinéarité (VIF)
  formula_vif <- as.formula(paste(target_var, "~ ."))
  model_vif <- lm(formula_vif, data = df_prepared)
  vif_table <- car::vif(model_vif) %>% 
    as.data.frame() %>%
    tibble::rownames_to_column("Variable") %>%
    rename(VIF = `GVIF`, `VIF_adj` = `GVIF^(1/(2*Df))`) %>%
    arrange(desc(VIF))
  
  high_vif <- vif_table %>% filter(VIF > vif_threshold)
  
  # 7. Rapport
  if (print_report) {
    cat("\n=== RAPPORT EXPERT MULTILINEAR PREPARER 2026 (version corrigée) ===\n")
    cat("Variable cible          :", target_var, "\n")
    cat("Nombre de prédicteurs   :", length(predictors), "\n")
    cat("Variables numériques    :", length(num_vars), "\n")
    cat("Variables catégorielles :", length(cat_vars), "\n")
    cat("Colonnes après préparation :", ncol(df_prepared) - 1, "\n\n")
    
    cat("📊 Tableau VIF :\n")
    print(vif_table, row.names = FALSE)
    
    if (nrow(high_vif) > 0) {
      cat("\n⚠️  ATTENTION : Variables avec VIF >", vif_threshold, ":\n")
      print(high_vif)
    } else {
      cat("\n✅ Aucune multicolinéarité forte détectée !\n")
    }
  }
  
  # 8. Heatmap de corrélation
  if (visualize) {
    numeric_prepared <- df_prepared %>% 
      dplyr::select(where(is.numeric)) %>% 
      dplyr::select(-dplyr::all_of(target_var))
    
    if (ncol(numeric_prepared) > 1) {
      cor_matrix <- cor(numeric_prepared, use = "complete.obs")
      corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
                         tl.cex = 0.8, tl.col = "black", 
                         title = "Heatmap des corrélations après préparation",
                         mar = c(0,0,2,0))
    }
  }
  
  cat("\n✅ Préparation terminée ! Ton dataframe prêt s'appelle `df_prepared`.\n")
  
  invisible(list(
    prepared_df = df_prepared,
    vif_table = vif_table,
    target_var = target_var
  ))
}
