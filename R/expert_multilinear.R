# === CHARGE CES PACKAGES UNE SEULE FOIS ===
library(dplyr)
library(car)
library(corrplot)
library(tibble)

expert_multilinear<- function(df,
                                             target_var,
                                             predictors = NULL,
                                             scale_numerics = TRUE,
                                             create_dummies = TRUE,
                                             interaction_terms = NULL,
                                             vif_threshold = 5,
                                             print_report = TRUE,
                                             visualize = TRUE) {
  
  df_prepared <- df
  
  # 1. Sélection des prédicteurs
  if (is.null(predictors)) {
    predictors <- names(df)[names(df) != target_var]
  }
  
  # 2. Suppression colonnes constantes
  constant_cols <- predictors[sapply(df[predictors], function(x) length(unique(na.omit(x))) <= 1)]
  if (length(constant_cols) > 0) {
    df_prepared <- df_prepared %>% dplyr::select(-dplyr::all_of(constant_cols))
    predictors <- setdiff(predictors, constant_cols)
    cat("⚠️  Colonnes constantes supprimées :", paste(constant_cols, collapse = ", "), "\n")
  }
  
  # 3. Création des dummies (ultra-robuste)
  num_vars <- predictors[sapply(df_prepared[predictors], is.numeric)]
  cat_vars <- predictors[!sapply(df_prepared[predictors], is.numeric)]
  
  if (create_dummies && length(cat_vars) > 0) {
    cat_data <- df_prepared[cat_vars]
    mm <- model.matrix(~ . - 1, data = cat_data)
    dummies <- as.data.frame(mm)
    names(dummies) <- gsub("`", "", names(dummies))
    
    df_prepared <- df_prepared %>%
      dplyr::select(-dplyr::all_of(cat_vars)) %>%
      dplyr::bind_cols(dummies)
    
    cat("\n✅ Variables catégorielles → dummies :", 
        length(cat_vars), "variables →", ncol(dummies), "colonnes\n")
  }
  
  # 4. Scaling robuste
  if (scale_numerics && length(num_vars) > 0) {
    for (col in num_vars) {
      if (col %in% names(df_prepared)) {
        med <- median(df_prepared[[col]], na.rm = TRUE)
        mad_val <- mad(df_prepared[[col]], na.rm = TRUE)
        if (mad_val == 0) mad_val <- sd(df_prepared[[col]], na.rm = TRUE) + 1e-6
        df_prepared[[col]] <- (df_prepared[[col]] - med) / mad_val
      }
    }
    cat("✅ Scaling robuste (médiane + MAD) appliqué\n")
  }
  
  # 5. Interactions
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
  
  # 6. VIF ULTRA-ROBUSTE (gère les deux formats de car::vif)
  formula_vif <- as.formula(paste(target_var, "~ ."))
  model_vif <- lm(formula_vif, data = df_prepared)
  
  vif_result <- car::vif(model_vif)
  
  # Gestion des deux cas possibles
  if (is.vector(vif_result) && !is.list(vif_result)) {
    # Cas classique : toutes variables numériques → vecteur simple
    vif_table <- data.frame(
      Variable = names(vif_result),
      VIF = as.numeric(vif_result),
      stringsAsFactors = FALSE
    )
  } else {
    # Cas avec facteurs/dummies → data.frame avec GVIF
    vif_table <- as.data.frame(vif_result) %>%
      tibble::rownames_to_column("Variable") %>%
      rename(VIF = any_of("GVIF"),
             VIF_adj = any_of("GVIF^(1/(2*Df))")) %>%
      mutate(VIF = ifelse(is.na(VIF), .[[2]], VIF))  # fallback sécurisé
  }
  
  vif_table <- vif_table %>% arrange(desc(VIF))
  
  high_vif <- vif_table %>% filter(VIF > vif_threshold)
  
  # 7. Rapport
  if (print_report) {
    cat("\n=== RAPPORT EXPERT MULTILINEAR PREPARER 2026 (VIF corrigé) ===\n")
    cat("Variable cible          :", target_var, "\n")
    cat("Colonnes après préparation :", ncol(df_prepared) - 1, "\n\n")
    cat("📊 Tableau VIF :\n")
    print(vif_table, row.names = FALSE)
    
    if (nrow(high_vif) > 0) {
      cat("\n⚠️  ATTENTION :", nrow(high_vif), "variables avec VIF >", vif_threshold, "\n")
      print(high_vif)
    } else {
      cat("\n✅ Aucune multicolinéarité forte détectée !\n")
    }
  }
  
  # 8. Heatmap
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
  
  cat("\n✅ Préparation terminée avec succès ! `df_prepared` est prêt.\n")
  
  invisible(list(
    prepared_df = df_prepared,
    vif_table = vif_table,
    target_var = target_var
  ))
}
