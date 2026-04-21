# =============================================================================
#  TOOLKIT EXPERT — RÉGRESSION MULTILINÉAIRE ROBUSTE
#  Fonctions : expert_multilinear() | expert_model() |
#              expert_model_comparison_2026() | expert_best_model()
#  Auteur    : généré par Claude (Anthropic) — 2026
#  Usage     : source("expert_multilinear_toolkit.R")
# =============================================================================
 
# ── Packages requis ──────────────────────────────────────────────────────────
required_pkgs <- c("dplyr", "tidyr", "caret", "leaps", "glmnet",
                   "car", "MASS", "ggplot2", "tibble", "purrr",
                   "stringr", "scales")
 
invisible(lapply(required_pkgs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installation de : ", pkg)
    install.packages(pkg, quiet = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))
 
 
# =============================================================================
# FONCTION 1 — expert_multilinear()
# Prépare automatiquement les données pour une régression multilinéaire :
#   • Scaling robuste (médiane / IQR)
#   • Encodage dummy des variables catégorielles
#   • Création d'interactions optionnelles
#   • Diagnostic de multicolinéarité (VIF + corrélation)
# =============================================================================
 
expert_multilinear <- function(data,
                                target,
                                exclude_vars    = NULL,
                                scale_method    = c("robust", "standard", "none"),
                                add_interactions = FALSE,
                                interaction_pairs = NULL,
                                vif_threshold   = 10,
                                corr_threshold  = 0.85,
                                verbose         = TRUE) {
 
  # POURQUOI CE BLOC ?
  # Valider les entrées utilisateur avant tout traitement pour
  # éviter des erreurs cryptiques plus tard dans le pipeline.
  scale_method <- match.arg(scale_method)
 
  if (!target %in% names(data))
    stop("La variable cible '", target, "' est introuvable dans le data.frame.")
 
  if (!is.null(exclude_vars))
    data <- data %>% dplyr::select(-dplyr::all_of(exclude_vars))
 
  y      <- data[[target]]
  X_data <- data %>% dplyr::select(-dplyr::all_of(target))
 
  if (verbose) cat("\n╔══════════════════════════════════════════════════╗\n")
  if (verbose) cat("║       EXPERT_MULTILINEAR — Préparation données   ║\n")
  if (verbose) cat("╚══════════════════════════════════════════════════╝\n\n")
 
  # ── 1. Séparation numériques / catégorielles ─────────────────────────────
  num_vars  <- names(X_data)[sapply(X_data, is.numeric)]
  cat_vars  <- names(X_data)[sapply(X_data, function(v) is.character(v) | is.factor(v))]
 
  if (verbose) {
    cat("📊 Variables numériques  :", length(num_vars), "→", paste(num_vars, collapse = ", "), "\n")
    cat("🔤 Variables catégorielles:", length(cat_vars), "→",
        if (length(cat_vars) > 0) paste(cat_vars, collapse = ", ") else "aucune", "\n\n")
  }
 
  # ── 2. Scaling ────────────────────────────────────────────────────────────
  # POURQUOI CE BLOC ?
  # Le scaling robuste (médiane/IQR) est préféré au z-score standard
  # car il est insensible aux valeurs aberrantes (outliers), fréquentes
  # en données réelles institutionnelles.
  X_num <- X_data %>% dplyr::select(dplyr::all_of(num_vars))
  scale_params <- list()
 
  if (scale_method == "robust") {
    for (v in num_vars) {
      med_v <- median(X_num[[v]], na.rm = TRUE)
      iqr_v <- IQR(X_num[[v]], na.rm = TRUE)
      if (iqr_v == 0) iqr_v <- 1   # éviter division par zéro
      scale_params[[v]] <- list(center = med_v, scale = iqr_v)
      X_num[[v]] <- (X_num[[v]] - med_v) / iqr_v
    }
    if (verbose) cat("✅ Scaling ROBUSTE appliqué (médiane / IQR)\n")
 
  } else if (scale_method == "standard") {
    for (v in num_vars) {
      m_v <- mean(X_num[[v]], na.rm = TRUE)
      s_v <- sd(X_num[[v]], na.rm = TRUE)
      if (s_v == 0) s_v <- 1
      scale_params[[v]] <- list(center = m_v, scale = s_v)
      X_num[[v]] <- (X_num[[v]] - m_v) / s_v
    }
    if (verbose) cat("✅ Scaling STANDARD appliqué (moyenne / écart-type)\n")
 
  } else {
    if (verbose) cat("⚠️  Aucun scaling appliqué.\n")
  }
 
  # ── 3. Encodage dummy ─────────────────────────────────────────────────────
  # POURQUOI CE BLOC ?
  # R accepte les facteurs dans lm(), mais travailler avec une matrice
  # numérique explicite facilite la détection de multicolinéarité et
  # l'interface avec glmnet (LASSO).
  X_cat_dummies <- data.frame(row.names = seq_len(nrow(X_data)))
 
  if (length(cat_vars) > 0) {
    for (v in cat_vars) {
      f <- as.factor(X_data[[v]])
      lvls <- levels(f)
      # on drop le premier niveau (référence) pour éviter le piège de dummy parfaite
      for (lvl in lvls[-1]) {
        col_name <- paste0(v, "_", make.names(lvl))
        X_cat_dummies[[col_name]] <- as.integer(f == lvl)
      }
    }
    if (verbose) cat("✅ Encodage dummy :", ncol(X_cat_dummies), "colonnes créées\n")
  }
 
  # ── 4. Interactions optionnelles ──────────────────────────────────────────
  X_interact <- data.frame(row.names = seq_len(nrow(X_data)))
 
  if (add_interactions) {
    if (is.null(interaction_pairs)) {
      # Toutes paires de variables numériques
      num_pairs <- combn(num_vars, 2, simplify = FALSE)
    } else {
      num_pairs <- interaction_pairs
    }
    for (pair in num_pairs) {
      col_name <- paste0(pair[[1]], "_x_", pair[[2]])
      X_interact[[col_name]] <- X_num[[pair[[1]]]] * X_num[[pair[[2]]]]
    }
    if (verbose) cat("✅ Interactions créées :", ncol(X_interact), "\n")
  }
 
  # ── 5. Assemblage final ───────────────────────────────────────────────────
  X_final <- dplyr::bind_cols(X_num, X_cat_dummies, X_interact)
  X_final <- X_final[, sapply(X_final, function(col) var(col, na.rm = TRUE) > 0), drop = FALSE]
 
  df_final <- cbind(X_final, setNames(data.frame(y), target))
 
  # ── 6. Diagnostic multicolinéarité ───────────────────────────────────────
  # POURQUOI CE BLOC ?
  # On ajuste un modèle complet temporaire pour calculer les VIF.
  # Les prédicteurs à VIF > seuil ou corrélation > seuil sont signalés
  # mais NON supprimés automatiquement — c'est à l'utilisateur de décider.
  if (verbose) cat("\n── Diagnostic multicolinéarité ────────────────────\n")
 
  formula_full <- as.formula(paste(target, "~ ."))
  tryCatch({
    tmp_lm <- lm(formula_full, data = df_final)
    vif_vals <- car::vif(tmp_lm)
    if (is.matrix(vif_vals)) vif_vals <- vif_vals[, 1]
 
    high_vif <- vif_vals[vif_vals > vif_threshold]
    if (length(high_vif) > 0) {
      cat("⚠️  Variables avec VIF >", vif_threshold, ":\n")
      print(round(high_vif, 2))
    } else {
      cat("✅ Tous les VIF <", vif_threshold, "(pas de multicolinéarité grave)\n")
    }
  }, error = function(e) {
    cat("ℹ️  VIF non calculable (modèle singulier?) :", conditionMessage(e), "\n")
    vif_vals <- NULL
  })
 
  # Corrélations entre prédicteurs
  corr_mat  <- cor(X_final, use = "pairwise.complete.obs")
  high_corr <- which(abs(corr_mat) > corr_threshold & upper.tri(corr_mat), arr.ind = TRUE)
  if (nrow(high_corr) > 0) {
    cat("⚠️  Paires fortement corrélées (|r| >", corr_threshold, ") :\n")
    for (i in seq_len(nrow(high_corr))) {
      r1 <- rownames(corr_mat)[high_corr[i, 1]]
      r2 <- colnames(corr_mat)[high_corr[i, 2]]
      cat("   •", r1, "—", r2, ": r =",
          round(corr_mat[high_corr[i,1], high_corr[i,2]], 3), "\n")
    }
  } else {
    cat("✅ Aucune paire de prédicteurs avec |r| >", corr_threshold, "\n")
  }
 
  if (verbose) cat("\n📦 Données préparées :", nrow(df_final), "obs. ×",
                   ncol(df_final) - 1, "prédicteurs\n\n")
 
  # ── Retour ────────────────────────────────────────────────────────────────
  invisible(list(
    data_prepared  = df_final,
    target         = target,
    predictors     = setdiff(names(df_final), target),
    scale_params   = scale_params,
    scale_method   = scale_method,
    vif_values     = if (exists("vif_vals")) vif_vals else NULL,
    corr_matrix    = corr_mat
  ))
}
 
 
