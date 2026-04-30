# ============================================================
#   FONCTION : analyse_exploratoire()
#   Objectif : EDA complète — figures + Excel enrichi
#   Auteur   : Généré pour usage académique (mémoire de fin d'études)
# ============================================================

analyse_exploratoire <- function(df,
                                  var_interet  = NULL,   # nom de la variable dépendante (chaîne)
                                  output_dir   = "output",
                                  outlier_seuil = 3,     # seuil Z-score pour outliers sévères
                                  dpi          = 300) {

  # ── 0. Packages requis ──────────────────────────────────────────────────────
  pkgs <- c("ggplot2", "openxlsx", "dplyr", "tidyr", "scales",
            "ggthemes", "patchwork", "moments")
  invisible(lapply(pkgs, function(p) {
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
    library(p, character.only = TRUE)
  }))

  # ── 1. Création de l'arborescence output/ ───────────────────────────────────
  # POURQUOI CE CHUNK ?
  # On crée les sous-dossiers figures/ et data/ dans le répertoire output/.
  # Cela garantit une organisation propre et reproductible.
  dir_fig  <- file.path(output_dir, "figures")
  dir_data <- file.path(output_dir, "data")
  dir.create(dir_fig,  recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_data, recursive = TRUE, showWarnings = FALSE)
  message("✔ Dossiers créés : ", output_dir, "/figures  &  ", output_dir, "/data")

  # ── 2. Séparation variables numériques / catégorielles ──────────────────────
  vars_num <- names(df)[sapply(df, is.numeric)]
  vars_cat <- names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]

  if (!is.null(var_interet) && !(var_interet %in% names(df)))
    stop("var_interet '", var_interet, "' introuvable dans df.")

  vars_exp_num <- setdiff(vars_num, var_interet)

  # ── 3. Thème graphique commun ────────────────────────────────────────────────
  # POURQUOI CE CHUNK ?
  # Définir un thème ggplot2 uniforme pour toutes les figures améliore
  # la cohérence visuelle du rapport final.
  theme_rapport <- theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle    = element_text(size = 11, hjust = 0.5, color = "grey40"),
      axis.title       = element_text(size = 12),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA),
      plot.margin      = margin(10, 15, 10, 15)
    )

  palette_fill <- "#3A7DC9"   # bleu académique
  palette_out  <- "#E84040"   # rouge outlier

  # ── 4. FIGURES : Histogramme + Boxplot par variable numérique ────────────────
  # POURQUOI CE CHUNK ?
  # Pour chaque variable numérique on génère deux graphiques de qualité
  # publication : un histogramme (distribution) et un boxplot (dispersion/outliers).
  # Les deux sont assemblés côte à côte avec patchwork et exportés en PNG haute résolution.
  message("\n── Génération des figures ──────────────────────────────────────────")
  for (vn in vars_num) {
    vals <- df[[vn]][!is.na(df[[vn]])]
    if (length(vals) == 0) next

    # -- Histogramme --
    p_hist <- ggplot(df, aes(x = .data[[vn]])) +
      geom_histogram(aes(y = after_stat(density)),
                     bins    = max(10, round(1 + 3.322 * log10(length(vals)))),
                     fill    = palette_fill, color = "white", alpha = 0.85) +
      geom_density(color = "#E84040", linewidth = 0.8, linetype = "dashed") +
      geom_vline(xintercept = mean(vals, na.rm = TRUE),
                 color = "black", linewidth = 0.7, linetype = "solid") +
      scale_x_continuous(labels = comma) +
      labs(title    = paste("Distribution —", vn),
           subtitle = paste0("n = ", length(vals),
                             "  |  Moy = ", round(mean(vals), 2),
                             "  |  σ = ", round(sd(vals), 2)),
           x = vn, y = "Densité") +
      theme_rapport

    # -- Boxplot --
    p_box <- ggplot(df, aes(y = .data[[vn]])) +
      geom_boxplot(fill = palette_fill, color = "#1a3d6e",
                   alpha = 0.75, outlier.colour = palette_out,
                   outlier.size = 2.5, width = 0.4) +
      geom_jitter(aes(x = 0), width = 0.08, alpha = 0.25,
                  color = "grey30", size = 0.8) +
      scale_y_continuous(labels = comma) +
      labs(title    = paste("Boxplot —", vn),
           subtitle = paste0("Méd = ", round(median(vals), 2),
                             "  |  IQR = ", round(IQR(vals), 2)),
           x = "", y = vn) +
      theme_rapport +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

    # -- Assemblage --
    p_final <- p_hist + p_box +
      plot_annotation(
        caption = "Source : élaboré par l'auteur à l'aide du logiciel R (R Core Team, 2024).",
        theme   = theme(plot.caption = element_text(size = 8, hjust = 1, color = "grey50"))
      )

    nom_fichier <- file.path(dir_fig, paste0("fig_", vn, ".png"))
    ggsave(nom_fichier, plot = p_final, width = 12, height = 5, dpi = dpi, bg = "white")
    message("  ✔ ", nom_fichier)
  }

  # ── 5. FIGURES : Barplot pour variables catégorielles ───────────────────────
  for (vc in vars_cat) {
    freq_tbl <- df %>%
      count(.data[[vc]]) %>%
      mutate(pct = n / sum(n))

    p_bar <- ggplot(freq_tbl, aes(x = reorder(.data[[vc]], -n), y = pct)) +
      geom_col(fill = palette_fill, alpha = 0.85, color = "white") +
      geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
                vjust = -0.4, size = 3.5) +
      scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.08))) +
      labs(title    = paste("Fréquences —", vc),
           x = vc, y = "Proportion",
           caption  = "Source : élaboré par l'auteur à l'aide du logiciel R (R Core Team, 2024).") +
      theme_rapport +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))

    nom_fichier <- file.path(dir_fig, paste0("fig_cat_", vc, ".png"))
    ggsave(nom_fichier, plot = p_bar, width = 8, height = 5, dpi = dpi, bg = "white")
    message("  ✔ ", nom_fichier)
  }

  # ── 6. EXCEL — construction du workbook ─────────────────────────────────────
  # POURQUOI CE CHUNK ?
  # On initialise un classeur Excel multi-feuilles avec openxlsx.
  # Les styles sont définis une seule fois pour être réutilisés dans toutes les feuilles.
  message("\n── Création du fichier Excel ───────────────────────────────────────")
  wb <- createWorkbook()

  # Styles réutilisables
  st_header <- createStyle(fontName = "Arial", fontSize = 12, fontColour = "white",
                           fgFill = "#1F4E79", halign = "CENTER", valign = "CENTER",
                           textDecoration = "bold", border = "Bottom",
                           borderColour = "#FFFFFF")

  st_titre  <- createStyle(fontName = "Arial", fontSize = 14, textDecoration = "bold",
                           fontColour = "#1F4E79")

  st_normal <- createStyle(fontName = "Arial", fontSize = 10, halign = "LEFT",
                           border = "TopBottomLeftRight", borderColour = "#D9D9D9")

  st_num    <- createStyle(fontName = "Arial", fontSize = 10, numFmt = "#,##0.0000",
                           halign = "RIGHT", border = "TopBottomLeftRight",
                           borderColour = "#D9D9D9")

  st_zebra  <- createStyle(fontName = "Arial", fontSize = 10,
                           fgFill = "#EBF3FB", border = "TopBottomLeftRight",
                           borderColour = "#D9D9D9")

  # ─────────────────────────────────────────────────────────────────────────────
  # FEUILLE 1 : Statistiques descriptives
  # POURQUOI CE CHUNK ?
  # On calcule un panel complet de statistiques par variable numérique :
  # tendance centrale, dispersion, forme (asymétrie, kurtosis), quantiles.
  # C'est la base de tout rapport exploratoire sérieux.
  # ─────────────────────────────────────────────────────────────────────────────
  addWorksheet(wb, "1_Stats_Descriptives")

  stats_desc <- lapply(vars_num, function(v) {
    x <- df[[v]]
    data.frame(
      Variable    = v,
      N           = sum(!is.na(x)),
      Manquants   = sum(is.na(x)),
      Pct_Manq    = round(mean(is.na(x)) * 100, 2),
      Min         = round(min(x, na.rm = TRUE), 4),
      Q1          = round(quantile(x, 0.25, na.rm = TRUE), 4),
      Mediane     = round(median(x, na.rm = TRUE), 4),
      Moyenne     = round(mean(x, na.rm = TRUE), 4),
      Q3          = round(quantile(x, 0.75, na.rm = TRUE), 4),
      Max         = round(max(x, na.rm = TRUE), 4),
      Ecart_type  = round(sd(x, na.rm = TRUE), 4),
      CV_pct      = round(sd(x, na.rm = TRUE) / abs(mean(x, na.rm = TRUE)) * 100, 2),
      Asymetrie   = round(moments::skewness(x, na.rm = TRUE), 4),
      Kurtosis    = round(moments::kurtosis(x, na.rm = TRUE), 4),
      IQR         = round(IQR(x, na.rm = TRUE), 4),
      stringsAsFactors = FALSE
    )
  })
  stats_df <- do.call(rbind, stats_desc)

  writeData(wb, "1_Stats_Descriptives",
            "STATISTIQUES DESCRIPTIVES", startRow = 1, startCol = 1)
  addStyle(wb, "1_Stats_Descriptives", st_titre, rows = 1, cols = 1)

  writeDataTable(wb, "1_Stats_Descriptives", stats_df,
                 startRow = 3, startCol = 1,
                 tableStyle = "TableStyleMedium2", headerStyle = st_header)

  # Colorier Pct_Manq > 20 % en orange, > 50 % en rouge
  col_manq <- which(names(stats_df) == "Pct_Manq")
  for (i in seq_len(nrow(stats_df))) {
    val <- stats_df$Pct_Manq[i]
    if (!is.na(val) && val > 50) {
      addStyle(wb, "1_Stats_Descriptives",
               createStyle(fgFill = "#FF4444", fontColour = "white", fontName = "Arial",
                           fontSize = 10, numFmt = "#,##0.00"),
               rows = i + 3, cols = col_manq, stack = FALSE)
    } else if (!is.na(val) && val > 20) {
      addStyle(wb, "1_Stats_Descriptives",
               createStyle(fgFill = "#FFA500", fontColour = "white", fontName = "Arial",
                           fontSize = 10, numFmt = "#,##0.00"),
               rows = i + 3, cols = col_manq, stack = FALSE)
    }
  }

  setColWidths(wb, "1_Stats_Descriptives", cols = 1:ncol(stats_df),
               widths = c(20, rep(11, ncol(stats_df) - 1)))

  # ─────────────────────────────────────────────────────────────────────────────
  # FEUILLE 2 : Corrélations entre variables explicatives
  # POURQUOI CE CHUNK ?
  # La matrice de corrélation de Pearson entre variables explicatives permet
  # de détecter la multicolinéarité. On trie par valeur absolue décroissante
  # et on colorie les corrélations critiques (|r| > 0.7 → rouge, > 0.5 → orange).
  # ─────────────────────────────────────────────────────────────────────────────
  addWorksheet(wb, "2_Corr_Explicatives")
  writeData(wb, "2_Corr_Explicatives",
            "CORRÉLATIONS ENTRE VARIABLES EXPLICATIVES (triées |r| décroissant)",
            startRow = 1, startCol = 1)
  addStyle(wb, "2_Corr_Explicatives", st_titre, rows = 1, cols = 1)

  if (length(vars_exp_num) >= 2) {
    mat_corr <- cor(df[, vars_exp_num, drop = FALSE], use = "pairwise.complete.obs",
                    method = "pearson")

    # Mise en forme longue
    corr_long <- as.data.frame(as.table(mat_corr)) %>%
      rename(Var1 = Var1, Var2 = Var2, Correlation = Freq) %>%
      filter(as.character(Var1) < as.character(Var2)) %>%
      mutate(Abs_Corr = abs(Correlation),
             Correlation = round(Correlation, 4),
             Abs_Corr    = round(Abs_Corr, 4)) %>%
      arrange(desc(Abs_Corr))

    writeDataTable(wb, "2_Corr_Explicatives", corr_long,
                   startRow = 3, startCol = 1,
                   tableStyle = "TableStyleMedium2", headerStyle = st_header)

    col_r <- which(names(corr_long) == "Correlation")
    for (i in seq_len(nrow(corr_long))) {
      r <- abs(corr_long$Correlation[i])
      if (!is.na(r) && r >= 0.7) {
        addStyle(wb, "2_Corr_Explicatives",
                 createStyle(fgFill = "#FF4444", fontColour = "white", fontName = "Arial",
                             fontSize = 10, numFmt = "#,##0.0000", halign = "CENTER"),
                 rows = i + 3, cols = col_r, stack = FALSE)
      } else if (!is.na(r) && r >= 0.5) {
        addStyle(wb, "2_Corr_Explicatives",
                 createStyle(fgFill = "#FFA500", fontColour = "white", fontName = "Arial",
                             fontSize = 10, numFmt = "#,##0.0000", halign = "CENTER"),
                 rows = i + 3, cols = col_r, stack = FALSE)
      }
    }
    setColWidths(wb, "2_Corr_Explicatives", cols = 1:4, widths = c(22, 22, 14, 14))
  } else {
    writeData(wb, "2_Corr_Explicatives",
              "Moins de 2 variables explicatives numériques — corrélations non calculées.",
              startRow = 3)
  }

  # ─────────────────────────────────────────────────────────────────────────────
  # FEUILLE 3 : Corrélations avec la variable d'intérêt
  # POURQUOI CE CHUNK ?
  # On mesure l'association linéaire de chaque variable explicative avec la
  # variable cible. Cela guide la sélection des variables et l'interprétation.
  # ─────────────────────────────────────────────────────────────────────────────
  addWorksheet(wb, "3_Corr_Var_Interet")
  writeData(wb, "3_Corr_Var_Interet",
            paste("CORRÉLATIONS AVEC LA VARIABLE D'INTÉRÊT :", var_interet),
            startRow = 1, startCol = 1)
  addStyle(wb, "3_Corr_Var_Interet", st_titre, rows = 1, cols = 1)

  if (!is.null(var_interet) && var_interet %in% vars_num && length(vars_exp_num) > 0) {
    corr_vi <- sapply(vars_exp_num, function(v) {
      cor(df[[var_interet]], df[[v]], use = "pairwise.complete.obs")
    })
    corr_vi_df <- data.frame(
      Variable_Explicative = names(corr_vi),
      Correlation          = round(corr_vi, 4),
      Abs_Corr             = round(abs(corr_vi), 4),
      Interpretation       = ifelse(abs(corr_vi) >= 0.7, "Forte",
                              ifelse(abs(corr_vi) >= 0.4, "Modérée",
                              ifelse(abs(corr_vi) >= 0.2, "Faible", "Négligeable")))
    ) %>% arrange(desc(Abs_Corr))

    writeDataTable(wb, "3_Corr_Var_Interet", corr_vi_df,
                   startRow = 3, startCol = 1,
                   tableStyle = "TableStyleMedium2", headerStyle = st_header)

    col_r2 <- which(names(corr_vi_df) == "Correlation")
    for (i in seq_len(nrow(corr_vi_df))) {
      r <- abs(corr_vi_df$Correlation[i])
      couleur <- if (!is.na(r) && r >= 0.7) "#27AE60" else
                 if (!is.na(r) && r >= 0.4) "#F39C12" else NA
      if (!is.na(couleur)) {
        addStyle(wb, "3_Corr_Var_Interet",
                 createStyle(fgFill = couleur, fontColour = "white", fontName = "Arial",
                             fontSize = 10, numFmt = "#,##0.0000", halign = "CENTER"),
                 rows = i + 3, cols = col_r2, stack = FALSE)
      }
    }
    setColWidths(wb, "3_Corr_Var_Interet", cols = 1:4, widths = c(28, 14, 14, 18))
  } else {
    writeData(wb, "3_Corr_Var_Interet",
              "var_interet non spécifiée ou non numérique — feuille non renseignée.",
              startRow = 3)
  }

  # ─────────────────────────────────────────────────────────────────────────────
  # FEUILLE 4 : Rapport sur les valeurs manquantes
  # POURQUOI CE CHUNK ?
  # Un diagnostic complet des NA est indispensable avant toute modélisation.
  # On calcule le taux de manquants par variable ET par observation,
  # et on identifie les patterns de co-occurrence.
  # ─────────────────────────────────────────────────────────────────────────────
  addWorksheet(wb, "4_Valeurs_Manquantes")
  writeData(wb, "4_Valeurs_Manquantes",
            "RAPPORT SUR LES VALEURS MANQUANTES", startRow = 1, startCol = 1)
  addStyle(wb, "4_Valeurs_Manquantes", st_titre, rows = 1, cols = 1)

  # 4a) Taux par variable
  na_var <- data.frame(
    Variable     = names(df),
    N_total      = nrow(df),
    N_manquants  = sapply(df, function(x) sum(is.na(x))),
    Pct_manquants = round(sapply(df, function(x) mean(is.na(x)) * 100), 2),
    Statut       = sapply(df, function(x) {
      p <- mean(is.na(x)) * 100
      if (p == 0) "Complet" else if (p < 5) "Acceptable" else
      if (p < 20) "Attention" else "Critique"
    }),
    stringsAsFactors = FALSE
  ) %>% arrange(desc(Pct_manquants))

  writeData(wb, "4_Valeurs_Manquantes", "Taux de manquants par variable :",
            startRow = 3, startCol = 1)
  writeDataTable(wb, "4_Valeurs_Manquantes", na_var,
                 startRow = 4, startCol = 1,
                 tableStyle = "TableStyleMedium2", headerStyle = st_header)

  col_pct <- which(names(na_var) == "Pct_manquants")
  for (i in seq_len(nrow(na_var))) {
    statut <- na_var$Statut[i]
    coul <- switch(statut,
      "Critique"   = "#FF4444",
      "Attention"  = "#FFA500",
      "Acceptable" = "#FFF176",
      NA)
    if (!is.na(coul)) {
      fc <- if (statut %in% c("Critique", "Attention")) "white" else "black"
      addStyle(wb, "4_Valeurs_Manquantes",
               createStyle(fgFill = coul, fontColour = fc, fontName = "Arial",
                           fontSize = 10, numFmt = "#,##0.00"),
               rows = i + 4, cols = col_pct, stack = FALSE)
    }
  }

  # 4b) Observations avec trop de NA
  ligne_depart <- nrow(na_var) + 7
  na_obs_pct <- rowMeans(is.na(df)) * 100
  na_obs_df  <- data.frame(
    Observation  = seq_len(nrow(df)),
    N_manquants  = rowSums(is.na(df)),
    Pct_manquants = round(na_obs_pct, 2)
  ) %>% filter(N_manquants > 0) %>% arrange(desc(Pct_manquants))

  writeData(wb, "4_Valeurs_Manquantes",
            paste0("Observations avec au moins 1 valeur manquante (", nrow(na_obs_df), " sur ", nrow(df), ") :"),
            startRow = ligne_depart, startCol = 1)
  if (nrow(na_obs_df) > 0) {
    writeDataTable(wb, "4_Valeurs_Manquantes", na_obs_df,
                   startRow = ligne_depart + 1, startCol = 1,
                   tableStyle = "TableStyleLight9", headerStyle = st_header)
  }

  setColWidths(wb, "4_Valeurs_Manquantes", cols = 1:5, widths = c(25, 12, 14, 16, 14))

  # ─────────────────────────────────────────────────────────────────────────────
  # FEUILLE 5 : Détection des outliers
  # POURQUOI CE CHUNK ?
  # On combine deux techniques complémentaires :
  #   - la règle des clôtures de Tukey (IQR × 1.5) → outliers modérés
  #   - le Z-score standardisé (|z| > seuil) → outliers sévères à exclure
  # Chaque observation suspecte est nominativement listée avec ses valeurs.
  # ─────────────────────────────────────────────────────────────────────────────
  addWorksheet(wb, "5_Outliers")
  writeData(wb, "5_Outliers",
            paste0("DÉTECTION DES OUTLIERS — Méthode : Tukey IQR + Z-score (seuil = ±", outlier_seuil, ")"),
            startRow = 1, startCol = 1)
  addStyle(wb, "5_Outliers", st_titre, rows = 1, cols = 1)

  legende <- data.frame(
    Couleur      = c("ROUGE (à exclure)", "ORANGE (à surveiller)", "Sans couleur"),
    Critere      = c(paste0("|Z-score| > ", outlier_seuil, " (outlier sévère)"),
                     "En dehors des clôtures Tukey mais |Z| ≤ seuil",
                     "Valeur normale")
  )
  writeData(wb, "5_Outliers", "Légende :", startRow = 3, startCol = 1)
  writeDataTable(wb, "5_Outliers", legende, startRow = 4, startCol = 1,
                 tableStyle = "TableStyleLight1")

  outliers_list <- lapply(vars_num, function(v) {
    x  <- df[[v]]
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iq <- IQR(x, na.rm = TRUE)
    mu <- mean(x, na.rm = TRUE)
    sg <- sd(x, na.rm = TRUE)

    borne_inf <- q1 - 1.5 * iq
    borne_sup <- q3 + 1.5 * iq
    z_scores  <- (x - mu) / sg

    idx_tukey  <- which(!is.na(x) & (x < borne_inf | x > borne_sup))
    if (length(idx_tukey) == 0) return(NULL)

    data.frame(
      Observation  = idx_tukey,
      Variable     = v,
      Valeur       = round(x[idx_tukey], 4),
      Z_score      = round(z_scores[idx_tukey], 4),
      Borne_Inf_Tukey = round(borne_inf, 4),
      Borne_Sup_Tukey = round(borne_sup, 4),
      Decision     = ifelse(abs(z_scores[idx_tukey]) > outlier_seuil,
                            "EXCLURE", "Surveiller"),
      stringsAsFactors = FALSE
    )
  })
  outliers_df <- do.call(rbind, outliers_list)

  if (!is.null(outliers_df) && nrow(outliers_df) > 0) {
    outliers_df <- outliers_df %>% arrange(desc(abs(Z_score)))

    writeDataTable(wb, "5_Outliers", outliers_df,
                   startRow = 8, startCol = 1,
                   tableStyle = "TableStyleMedium2", headerStyle = st_header)

    col_dec <- which(names(outliers_df) == "Decision")
    col_z   <- which(names(outliers_df) == "Z_score")
    for (i in seq_len(nrow(outliers_df))) {
      if (outliers_df$Decision[i] == "EXCLURE") {
        addStyle(wb, "5_Outliers",
                 createStyle(fgFill = "#FF4444", fontColour = "white", fontName = "Arial",
                             fontSize = 10, textDecoration = "bold", halign = "CENTER"),
                 rows = i + 8, cols = col_dec, stack = FALSE)
        addStyle(wb, "5_Outliers",
                 createStyle(fgFill = "#FFD7D7", fontName = "Arial", fontSize = 10,
                             numFmt = "#,##0.0000"),
                 rows = i + 8, cols = col_z, stack = FALSE)
      } else {
        addStyle(wb, "5_Outliers",
                 createStyle(fgFill = "#FFA500", fontColour = "white", fontName = "Arial",
                             fontSize = 10, halign = "CENTER"),
                 rows = i + 8, cols = col_dec, stack = FALSE)
      }
    }
    setColWidths(wb, "5_Outliers", cols = 1:ncol(outliers_df),
                 widths = c(14, 22, 12, 12, 18, 18, 14))
  } else {
    writeData(wb, "5_Outliers", "Aucun outlier détecté par la méthode Tukey.", startRow = 8)
  }

  # ── 7. Sauvegarde du fichier Excel ──────────────────────────────────────────
  chemin_excel <- file.path(dir_data, "analyse_exploratoire.xlsx")
  saveWorkbook(wb, chemin_excel, overwrite = TRUE)
  message("\n✔ Fichier Excel sauvegardé : ", chemin_excel)
  message("✔ Analyse terminée ! Tout est dans : ", output_dir, "/")

  # ── 8. Retour invisible ──────────────────────────────────────────────────────
  invisible(list(
    stats_desc  = stats_df,
    outliers    = outliers_df,
    na_rapport  = na_var,
    excel_path  = chemin_excel,
    figures_dir = dir_fig
  ))
}
