# ==============================================================================
# imputeNA() - Fonction experte d'imputation 2026 (missRanger)
# Outputs : dossier "output/" avec rapport Excel + graphique PNG
# ==============================================================================

# ---- Packages nécessaires ----
# install.packages(c("missRanger", "naniar", "ggplot2", "patchwork",
#                    "dplyr", "tidyr", "openxlsx", "rlang"))

library(missRanger)
library(naniar)
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(openxlsx)
library(rlang)

# ==============================================================================
imputeNA <- function(df,
                     m         = 1,
                     pmm.k     = 5,
                     num.trees = 500,
                     maxiter   = 10,
                     seed      = 42,
                     verbose   = 1,
                     output_dir = "output") {

  # POURQUOI CE CHUNK ?
  # Initialisation : fixer la graine aléatoire, créer le dossier output/,
  # et afficher un en-tête synthétique dans la console.
  set.seed(seed)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  cat("🚀 imputeNA() - Version experte 2026 (missRanger)\n")
  cat("Seed :", seed, "| Imputations :", m, "| Arbres :", num.trees, "\n\n")

  n_lignes <- nrow(df)
  n_col    <- ncol(df)

  # ============================================================
  # 1. RAPPORT AVANT IMPUTATION (console)
  # ============================================================

  # POURQUOI CE CHUNK ?
  # Calculer le taux global de NA et le taux par variable AVANT imputation,
  # pour alimenter le rapport Excel et la console.
  cat("=== AVANT IMPUTATION ===\n")
  cat("Dimensions :", n_lignes, "lignes ×", n_col, "colonnes\n")
  pct_global <- mean(is.na(df)) * 100
  cat("Taux global de NA :", round(pct_global, 2), "%\n\n")

  missing_avant <- df %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "N_NA_avant") %>%
    mutate(Pct_NA_avant = round(N_NA_avant / n_lignes * 100, 2)) %>%
    arrange(desc(Pct_NA_avant))

  print(missing_avant, n = Inf)

  # ============================================================
  # 2. GRAPHIQUE PNG : NB & % NA PAR VARIABLE
  # ============================================================

  # POURQUOI CE CHUNK ?
  # Produire un graphique à barres horizontales montrant le nombre et le
  # pourcentage de NA par variable. On adapte automatiquement la hauteur
  # du graphique au nombre de variables pour éviter les superpositions.
  cat("\n=== GRAPHIQUE DES NA (avant) → output/graphique_NA.png ===\n")

  n_vars       <- nrow(missing_avant)
  hauteur_px   <- max(500, n_vars * 28 + 120)   # hauteur dynamique
  largeur_px   <- 1000

  p_na <- missing_avant %>%
    filter(N_NA_avant > 0) %>%
    mutate(Variable = factor(Variable, levels = rev(Variable)),
           etiquette = paste0(N_NA_avant, "  (", Pct_NA_avant, "%)")) %>%
    ggplot(aes(x = Pct_NA_avant, y = Variable, fill = Pct_NA_avant)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    geom_text(aes(label = etiquette), hjust = -0.05, size = 3.2) +
    scale_fill_gradient(low = "#FFC107", high = "#D32F2F") +
    scale_x_continuous(
      limits = c(0, min(100, max(missing_avant$Pct_NA_avant, na.rm = TRUE) * 1.35)),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title    = "Valeurs manquantes par variable",
      subtitle = paste0("Total lignes : ", n_lignes,
                        " | Taux global de NA : ", round(pct_global, 2), "%"),
      x = "% de valeurs manquantes",
      y = NULL,
      caption = "Source : élaboré par l'auteur à l'aide du logiciel R (R Core Team, 2024)."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey40"),
      plot.caption  = element_text(color = "grey50", hjust = 0),
      panel.grid.major.y = element_blank()
    )

  # Si aucune variable n'a de NA on affiche un message plutôt qu'un graphe vide
  if (all(missing_avant$N_NA_avant == 0)) {
    p_na <- ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = "Aucune valeur manquante détectée ✅",
               size = 6, color = "darkgreen") +
      theme_void()
  }

  ggsave(file.path(output_dir, "graphique_NA.png"),
         plot = p_na, width = largeur_px / 100, height = hauteur_px / 100,
         dpi = 100, bg = "white")

  # ============================================================
  # 3. IMPUTATION
  # ============================================================

  # POURQUOI CE CHUNK ?
  # Appliquer missRanger en mode simple (m=1) ou multiple (m>1).
  # En mode multiple on retient le 1er jeu comme jeu "principal".
  cat("\n⏳ Imputation avec missRanger...\n")

  if (m == 1) {
    df_imp     <- missRanger(df,
                             pmm.k     = pmm.k,
                             num.trees = num.trees,
                             maxiter   = maxiter,
                             seed      = seed,
                             verbose   = verbose)
    df_resultat <- df_imp

  } else {
    cat("→ Mode multiple imputation (", m, "jeux)\n")
    df_imp <- lapply(seq_len(m), function(i)
      missRanger(df, pmm.k = pmm.k * 2, num.trees = num.trees,
                 maxiter = maxiter, seed = seed + i, verbose = 0))
    df_resultat <- df_imp[[1]]
  }

  # ============================================================
  # 4. RAPPORT APRÈS IMPUTATION (console)
  # ============================================================

  # POURQUOI CE CHUNK ?
  # Calculer les statistiques descriptives avant/après pour les variables
  # numériques, et afficher les résultats dans la console.
  cat("\n=== APRÈS IMPUTATION ===\n")
  cat("Taux de NA après :",
      round(mean(is.na(df_resultat)) * 100, 2), "%\n")

  num_vars <- names(df)[sapply(df, is.numeric)]

  if (length(num_vars) > 0) {
    cat("\nComparaison avant / après (variables numériques) :\n")
    for (v in num_vars) {
      avant_m   <- round(mean(df[[v]], na.rm = TRUE), 3)
      avant_med <- round(median(df[[v]], na.rm = TRUE), 3)
      apres_m   <- round(mean(df_resultat[[v]]), 3)
      apres_med <- round(median(df_resultat[[v]]), 3)
      cat("→", v,
          "| Avant : moy=", avant_m, " méd=", avant_med,
          "| Après : moy=", apres_m, " méd=", apres_med, "\n")
    }
  }

  # ============================================================
  # 5. FICHIER EXCEL DE RAPPORT
  # ============================================================

  # POURQUOI CE CHUNK ?
  # Construire un classeur Excel avec 3 onglets :
  #   • "Résumé NA"        → taux global et par variable
  #   • "Valeurs imputées" → tableau avant/après (uniquement les lignes avec NA)
  #   • "Stats avant-après"→ comparaison des moyennes/médianes numériques
  cat("\n=== RAPPORT EXCEL → output/rapport_imputation.xlsx ===\n")

  wb <- createWorkbook()

  # --- Styles ---
  style_titre  <- createStyle(fontSize = 13, fontColour = "#FFFFFF",
                               fgFill = "#1F4E79", halign = "center",
                               textDecoration = "bold", wrapText = TRUE)
  style_header <- createStyle(fontSize = 11, fontColour = "#FFFFFF",
                               fgFill = "#2E75B6", halign = "center",
                               textDecoration = "bold", wrapText = TRUE,
                               border = "TopBottomLeftRight")
  style_num    <- createStyle(numFmt = "0.00", halign = "right",
                               border = "TopBottomLeftRight")
  style_pct    <- createStyle(numFmt = "0.00\"%\"", halign = "right",
                               border = "TopBottomLeftRight")
  style_centre <- createStyle(halign = "center",
                               border = "TopBottomLeftRight")
  style_avant  <- createStyle(fgFill = "#FFF2CC",
                               border = "TopBottomLeftRight")
  style_apres  <- createStyle(fgFill = "#E2EFDA",
                               border = "TopBottomLeftRight")
  style_method <- createStyle(fgFill = "#DAEEF3", fontColour = "#1F4E79",
                               textDecoration = "bold",
                               border = "TopBottomLeftRight",
                               halign = "center")

  # ---- Onglet 1 : Résumé NA ----
  addWorksheet(wb, "Résumé NA")

  writeData(wb, "Résumé NA",
            data.frame(Info = c("Nombre de lignes", "Nombre de colonnes",
                                "Taux global de NA (%)",
                                "Méthode d'imputation"),
                       Valeur = c(n_lignes, n_col,
                                  round(pct_global, 2),
                                  ifelse(m == 1,
                                         "missRanger (imputation simple)",
                                         paste0("missRanger (", m,
                                                " imputations multiples)")))),
            startRow = 2, startCol = 1)

  writeData(wb, "Résumé NA",
            missing_avant %>%
              rename(`Nb NA` = N_NA_avant, `% NA` = Pct_NA_avant),
            startRow = 8, startCol = 1)

  addStyle(wb, "Résumé NA", style_header,
           rows = 8, cols = 1:3, gridExpand = TRUE)
  setColWidths(wb, "Résumé NA", cols = 1:3, widths = c(30, 12, 12))

  # ---- Onglet 2 : Valeurs imputées ----
  addWorksheet(wb, "Valeurs imputées")

  # Construire la table : une ligne par (variable × observation imputée)
  lignes_na <- which(rowSums(is.na(df)) > 0)

  if (length(lignes_na) > 0) {

    liste_rows <- lapply(lignes_na, function(i) {
      vars_na <- names(df)[is.na(df[i, ])]
      if (length(vars_na) == 0) return(NULL)
      do.call(rbind, lapply(vars_na, function(v) {
        type_var <- class(df[[v]])[1]
        methode  <- ifelse(type_var %in% c("numeric", "integer"),
                           "missRanger + PMM (prédictive mean matching)",
                           "missRanger (classification)")
        data.frame(
          Observation     = i,
          Variable        = v,
          Type            = type_var,
          Valeur_avant    = NA_character_,
          Valeur_apres    = as.character(df_resultat[i, v]),
          Methode         = methode,
          stringsAsFactors = FALSE
        )
      }))
    })

    tbl_impute <- do.call(rbind, liste_rows)
    tbl_impute$Valeur_avant <- "NA (manquant)"

    writeData(wb, "Valeurs imputées", tbl_impute,
              startRow = 2, startCol = 1)
    addStyle(wb, "Valeurs imputées", style_header,
             rows = 2, cols = 1:6, gridExpand = TRUE)
    addStyle(wb, "Valeurs imputées", style_avant,
             rows = 3:(nrow(tbl_impute) + 2), cols = 4,
             gridExpand = TRUE)
    addStyle(wb, "Valeurs imputées", style_apres,
             rows = 3:(nrow(tbl_impute) + 2), cols = 5,
             gridExpand = TRUE)
    addStyle(wb, "Valeurs imputées", style_method,
             rows = 3:(nrow(tbl_impute) + 2), cols = 6,
             gridExpand = TRUE)
    setColWidths(wb, "Valeurs imputées",
                 cols = 1:6, widths = c(14, 22, 14, 18, 18, 44))

  } else {
    writeData(wb, "Valeurs imputées",
              data.frame(Message = "Aucune valeur manquante trouvée."),
              startRow = 2, startCol = 1)
  }

  # ---- Onglet 3 : Stats avant-après ----
  addWorksheet(wb, "Stats avant-après")

  if (length(num_vars) > 0) {
    stats_tbl <- do.call(rbind, lapply(num_vars, function(v) {
      data.frame(
        Variable       = v,
        Moy_avant      = round(mean(df[[v]], na.rm = TRUE), 4),
        Med_avant      = round(median(df[[v]], na.rm = TRUE), 4),
        SD_avant       = round(sd(df[[v]], na.rm = TRUE), 4),
        Moy_apres      = round(mean(df_resultat[[v]]), 4),
        Med_apres      = round(median(df_resultat[[v]]), 4),
        SD_apres       = round(sd(df_resultat[[v]]), 4),
        Delta_moy      = round(mean(df_resultat[[v]]) -
                                 mean(df[[v]], na.rm = TRUE), 4),
        stringsAsFactors = FALSE
      )
    }))

    writeData(wb, "Stats avant-après", stats_tbl, startRow = 2, startCol = 1)
    addStyle(wb, "Stats avant-après", style_header,
             rows = 2, cols = 1:8, gridExpand = TRUE)

    # Colorer avant en jaune clair, après en vert clair
    addStyle(wb, "Stats avant-après", style_avant,
             rows = 3:(nrow(stats_tbl) + 2), cols = 2:4,
             gridExpand = TRUE)
    addStyle(wb, "Stats avant-après", style_apres,
             rows = 3:(nrow(stats_tbl) + 2), cols = 5:7,
             gridExpand = TRUE)
    setColWidths(wb, "Stats avant-après",
                 cols = 1:8,
                 widths = c(22, 13, 13, 12, 13, 13, 12, 13))
  } else {
    writeData(wb, "Stats avant-après",
              data.frame(Message = "Aucune variable numérique détectée."),
              startRow = 2, startCol = 1)
  }

  # ---- Sauvegarde ----
  chemin_excel <- file.path(output_dir, "rapport_imputation.xlsx")
  saveWorkbook(wb, chemin_excel, overwrite = TRUE)
  cat("✅ Fichier Excel sauvegardé :", chemin_excel, "\n")

  # ============================================================
  # 6. GRAPHIQUE DISTRIBUTIONS AVANT / APRÈS
  # ============================================================

  # POURQUOI CE CHUNK ?
  # Afficher les densités avant/après pour les variables numériques ayant
  # des NA. On limite à 12 variables max par page et on adapte la mise en
  # page (ncol) selon le nombre de graphes.
  if (length(num_vars) > 0) {
    vars_avec_na <- num_vars[sapply(num_vars, function(v) any(is.na(df[[v]])))]

    if (length(vars_avec_na) > 0) {
      cat("\n=== DISTRIBUTIONS AVANT/APRÈS → output/distributions.png ===\n")

      vars_plot <- vars_avec_na[1:min(12, length(vars_avec_na))]
      ncols     <- ifelse(length(vars_plot) <= 3, length(vars_plot),
                          ifelse(length(vars_plot) <= 8, 2, 3))

      plots_dist <- lapply(vars_plot, function(v) {
        df_long <- bind_rows(
          data.frame(val = df[[v]], statut = "Avant"),
          data.frame(val = df_resultat[[v]], statut = "Après")
        )
        ggplot(df_long, aes(x = val, fill = statut)) +
          geom_density(alpha = 0.45, color = NA) +
          scale_fill_manual(values = c("Avant" = "#EF5350",
                                       "Après" = "#42A5F5")) +
          labs(title = v, x = NULL, y = "Densité", fill = NULL) +
          theme_minimal(base_size = 10) +
          theme(legend.position  = "bottom",
                plot.title       = element_text(face = "bold", size = 9),
                panel.grid.minor = element_blank())
      })

      p_dist <- wrap_plots(plots_dist, ncol = ncols) +
        plot_annotation(
          title    = "Distributions avant / après imputation",
          subtitle = "Rouge = avant  |  Bleu = après",
          caption  = "Source : élaboré par l'auteur à l'aide du logiciel R (R Core Team, 2024).",
          theme    = theme(plot.title = element_text(face = "bold", size = 13))
        )

      h_dist <- ceiling(length(vars_plot) / ncols) * 3 + 1.5
      ggsave(file.path(output_dir, "distributions.png"),
             plot = p_dist, width = ncols * 4, height = h_dist,
             dpi = 120, bg = "white")
    }
  }

  # ============================================================
  # 7. RETOUR
  # ============================================================
  cat("\n✅ Imputation terminée avec succès !\n")
  cat("📁 Outputs disponibles dans :", output_dir, "/\n")
  cat("   • rapport_imputation.xlsx\n")
  cat("   • graphique_NA.png\n")
  if (length(num_vars) > 0 &&
      any(sapply(num_vars, function(v) any(is.na(df[[v]]))))) {
    cat("   • distributions.png\n")
  }

  if (m == 1) {
    return(list(data   = df_imp,
                method = "missRanger_single",
                seed   = seed))
  } else {
    return(list(data             = df_resultat,
                imputed_datasets = df_imp,
                method           = "missRanger_multiple",
                seed             = seed))
  }
}


# ==============================================================================
# EXEMPLE D'UTILISATION
# ==============================================================================

# # Charger vos données, par exemple :
# df_brut <- read.csv("mes_donnees.csv")
#
#Lancer l'imputation (outputs générés automatiquement dans output/)


# # Récupérer le jeu de données imputé
# df_impute <- resultat$data
