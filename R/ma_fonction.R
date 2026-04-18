# =====================================================
# imputeNA_setup.R
# UN SEUL FICHIER → tout est dedans (installation + fonction)
# Version experte 2026 (missRanger)
# =====================================================

# ====================== INSTALLATION (une seule fois) ======================
install_if_needed <- function(packages) {
  new <- packages[!packages %in% installed.packages()[,"Package"]]
  if (length(new) > 0) {
    cat("📦 Installation des packages manquants...\n")
    install.packages(new, dependencies = TRUE, quiet = TRUE)
  }
}

install_if_needed(c("tidyverse", "naniar", "visdat", "patchwork", "missRanger"))

# ====================== CHARGEMENT DES LIBRAIRIES ======================
library(tidyverse)
library(naniar)
library(visdat)
library(patchwork)
library(missRanger)

# ====================== FONCTION imputeNA() ======================
imputeNA <- function(df, 
                     m = 1,          
                     pmm.k = 5,      
                     num.trees = 500,
                     maxiter = 10,
                     seed = 42,
                     verbose = 1) {
  
  set.seed(seed)
  cat("🚀 imputeNA() - Version experte 2026 (missRanger)\n")
  cat("Seed :", seed, "| Imputations :", m, "| Arbres :", num.trees, "\n\n")
  
  n_lignes <- nrow(df)
  n_col <- ncol(df)
  
  # ====================== RAPPORT AVANT ======================
  cat("=== AVANT IMPUTATION ===\n")
  cat("Dimensions :", n_lignes, "lignes ×", n_col, "colonnes\n")
  pct_global <- mean(is.na(df)) * 100
  cat("Taux global de NA :", round(pct_global, 2), "%\n\n")
  
  missing_par_var <- df %>%
    summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 2))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "%_NA") %>%
    arrange(desc(`%_NA`))
  print(missing_par_var, n = Inf)
  
  # ====================== VISUALISATIONS ======================
  cat("\n=== CARTES DES NA (avant) ===\n")
  print(vis_miss(df, cluster = TRUE) + labs(title = "Carte des valeurs manquantes"))
  print(gg_miss_var(df) + labs(title = "NA par variable") + theme_minimal())
  
  # ====================== IMPUTATION ======================
  cat("\n⏳ Imputation avec missRanger...\n")
  
  if (m == 1) {
    df_imp <- missRanger(df, 
                         pmm.k = pmm.k, 
                         num.trees = num.trees,
                         maxiter = maxiter,
                         seed = seed,
                         verbose = verbose)
  } else {
    cat("→ Mode multiple imputation (", m, "jeux)\n")
    df_imp <- replicate(m, 
                        missRanger(df, pmm.k = pmm.k * 2, num.trees = num.trees,
                                   maxiter = maxiter, seed = seed + .x, verbose = 0),
                        simplify = FALSE)
    df_imputee <- df_imp[[1]]
  }
  
  # ====================== RAPPORT APRÈS ======================
  cat("\n=== APRÈS IMPUTATION ===\n")
  cat("Taux de NA après :", round(mean(is.na(if(m==1) df_imp else df_imputee)) * 100, 2), "%\n")
  
  num_vars <- names(df)[sapply(df, is.numeric)]
  if (length(num_vars) > 0) {
    cat("\nComparaison avant / après (variables numériques) :\n")
    for (v in num_vars) {
      avant_m <- round(mean(df[[v]], na.rm = TRUE), 3)
      avant_med <- round(median(df[[v]], na.rm = TRUE), 3)
      apres_m <- round(mean(if(m==1) df_imp[[v]] else df_imputee[[v]]), 3)
      apres_med <- round(median(if(m==1) df_imp[[v]] else df_imputee[[v]]), 3)
      cat("→", v, " | Avant : moy=", avant_m, " méd=", avant_med,
          " | Après : moy=", apres_m, " méd=", apres_med, "\n")
    }
  }
  
  # ====================== GRAPHES ======================
  cat("\n=== DISTRIBUTIONS AVANT / APRÈS ===\n")
  if (length(num_vars) > 0) {
    plots <- lapply(num_vars[1:min(6, length(num_vars))], function(v) {
      ggplot() +
        geom_density(data = df, aes(x = !!sym(v), fill = "Avant"), alpha = 0.4) +
        geom_density(data = if(m==1) df_imp else df_imputee, 
                     aes(x = !!sym(v), fill = "Après"), alpha = 0.4) +
        labs(title = paste("Distribution :", v), fill = "Statut") +
        theme_minimal()
    })
    print(wrap_plots(plots, ncol = 2) + 
            plot_annotation(title = "Comparaison avant / après imputation"))
  }
  
  cat("\n✅ Imputation terminée avec succès !\n")
  
  # ====================== RETOUR ======================
  if (m == 1) {
    return(list(data = df_imp, method = "missRanger_single", seed = seed))
  } else {
    return(list(data = df_imputee, 
                imputed_datasets = df_imp,
                method = "missRanger_multiple", 
                seed = seed))
  }
}

cat("✅ imputeNA() est prête ! Tu peux l'utiliser directement.\n")
