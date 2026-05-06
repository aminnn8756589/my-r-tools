# ============================================================================
# FONCTION D'ANALYSE DESCRIPTIVE PROFESSIONNELLE
# Analyse complète : stats descriptives, visualisations, export Excel
# ============================================================================

# Installation des packages nécessaires (à exécuter une fois)
# install.packages(c("dplyr", "tidyr", "ggplot2", "gridExtra", "corrplot", 
#                    "openxlsx", "writexl", "DescTools", "moments"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(openxlsx)
library(DescTools)
library(moments)

# ============================================================================
# FONCTION PRINCIPALE D'ANALYSE DESCRIPTIVE
# ============================================================================

analyse_descriptive_pro <- function(df, 
                                    output_file = "Analyse_Descriptive.xlsx",
                                    output_dir = "./resultats_analyse/",
                                    plot_width = 14,
                                    plot_height = 10) {
  
  # Créer le répertoire de sortie s'il n'existe pas
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Initialiser une liste pour les workbooks Excel
  wb <- createWorkbook()
  
  cat("📊 DÉBUT DE L'ANALYSE DESCRIPTIVE PROFESSIONNELLE\n")
  cat("================================================\n\n")
  
  # Identifier les types de variables
  quant_vars <- names(df)[sapply(df, is.numeric)]
  qual_vars <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  
  cat(paste0("✓ Variables quantitatives trouvées: ", length(quant_vars), "\n"))
  cat(paste0("✓ Variables catégorielles trouvées: ", length(qual_vars), "\n\n"))
  
  # ========================================================================
  # 1. STATISTIQUES DESCRIPTIVES DES VARIABLES QUANTITATIVES
  # ========================================================================
  
  if (length(quant_vars) > 0) {
    cat("1️⃣ Calcul des statistiques descriptives quantitatives...\n")
    
    stats_quant <- data.frame()
    
    for (var in quant_vars) {
      x <- df[[var]]
      x_clean <- x[!is.na(x)]
      
      stats_row <- data.frame(
        Variable = var,
        N = length(x_clean),
        NAs = sum(is.na(x)),
        Moyenne = round(mean(x_clean, na.rm = TRUE), 4),
        Médiane = round(median(x_clean, na.rm = TRUE), 4),
        Mode = as.numeric(names(sort(table(x_clean), decreasing = TRUE)[1])),
        Écart_type = round(sd(x_clean, na.rm = TRUE), 4),
        Variance = round(var(x_clean, na.rm = TRUE), 4),
        Min = round(min(x_clean, na.rm = TRUE), 4),
        Q1 = round(quantile(x_clean, 0.25, na.rm = TRUE), 4),
        Q3 = round(quantile(x_clean, 0.75, na.rm = TRUE), 4),
        Max = round(max(x_clean, na.rm = TRUE), 4),
        IQR = round(IQR(x_clean, na.rm = TRUE), 4),
        Skewness = round(skewness(x_clean, na.rm = TRUE), 4),
        Kurtosis = round(kurtosis(x_clean, na.rm = TRUE), 4),
        CV = round(sd(x_clean, na.rm = TRUE) / mean(x_clean, na.rm = TRUE) * 100, 2),
        stringsAsFactors = FALSE
      )
      
      stats_quant <- rbind(stats_quant, stats_row)
    }
    
    # Ajouter au workbook Excel
    addWorksheet(wb, "Stats_Quantitatives")
    writeData(wb, "Stats_Quantitatives", stats_quant)
    
    # Formatage
    style_header <- createStyle(
      fgFill = "#2E75B6", 
      fontColour = "white", 
      fontSize = 11,
      textDecoration = "bold",
      border = "TopBottom",
      halign = "center",
      valign = "center"
    )
    
    addStyle(wb, "Stats_Quantitatives", style_header, 
             rows = 1, cols = 1:ncol(stats_quant))
    
    # Largeur des colonnes
    setColWidths(wb, "Stats_Quantitatives", cols = 1:ncol(stats_quant), 
                 widths = rep(12, ncol(stats_quant)))
    
    cat("   ✓ Statistiques quantitatives calculées\n")
  }
  
  # ========================================================================
  # 2. STATISTIQUES DESCRIPTIVES DES VARIABLES CATÉGORIELLES
  # ========================================================================
  
  if (length(qual_vars) > 0) {
    cat("2️⃣ Calcul des statistiques descriptives catégorielles...\n")
    
    stats_qual_list <- list()
    
    for (var in qual_vars) {
      freq_table <- data.frame(table(df[[var]], useNA = "ifany"))
      colnames(freq_table) <- c(var, "Fréquence")
      freq_table$Pourcentage <- round(freq_table$Fréquence / sum(freq_table$Fréquence) * 100, 2)
      freq_table <- freq_table[order(freq_table$Fréquence, decreasing = TRUE), ]
      
      stats_qual_list[[var]] <- freq_table
    }
    
    # Ajouter chaque variable catégorique sur une feuille séparée
    for (var in qual_vars) {
      sheet_name <- paste0("Cat_", substr(var, 1, 20))
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, stats_qual_list[[var]])
      
      style_header <- createStyle(
        fgFill = "#70AD47", 
        fontColour = "white", 
        fontSize = 11,
        textDecoration = "bold",
        border = "TopBottom",
        halign = "center"
      )
      
      addStyle(wb, sheet_name, style_header, rows = 1, cols = 1:3)
      setColWidths(wb, sheet_name, cols = 1:3, widths = c(25, 12, 12))
    }
    
    cat("   ✓ Statistiques catégorielles calculées\n")
  }
  
  # ========================================================================
  # 3. MATRICE DE CORRÉLATION (pour variables quantitatives)
  # ========================================================================
  
  # POURQUOI CE CHUNK ?
  # La corrélation de Pearson n'est définie que pour les variables NUMÉRIQUES.
  # Les variables catégorielles sont donc exclues automatiquement.
  # On crée une feuille Excel dédiée avec un code couleur :
  #   - Rouge foncé  = corrélation négative forte (proche de -1)
  #   - Blanc        = absence de corrélation (proche de 0)
  #   - Bleu foncé   = corrélation positive forte (proche de +1)
  # Chaque cellule affiche la valeur arrondie à 2 décimales.
  if (length(quant_vars) >= 2) {
    cat("3️⃣ Création de la feuille Corrélations enrichie...\n")
    
    cor_matrix <- cor(df[, quant_vars], use = "complete.obs", method = "pearson")
    cor_df     <- data.frame(Variable = rownames(cor_matrix), as.data.frame(round(cor_matrix, 2)),
                             check.names = FALSE)
    
    addWorksheet(wb, "Corrélations")
    writeData(wb, "Corrélations", cor_df)
    
    n_vars <- length(quant_vars)
    
    # En-tête : fond bleu marine
    style_header_cor <- createStyle(
      fgFill = "#1F3864", fontColour = "white",
      fontSize = 11, textDecoration = "bold",
      halign = "center", border = "Bottom"
    )
    addStyle(wb, "Corrélations", style_header_cor,
             rows = 1, cols = 1:(n_vars + 1), stack = FALSE)
    
    # Colonne "Variable" : gris clair
    style_varname <- createStyle(
      fgFill = "#D6DCE4", fontColour = "#1F3864",
      fontSize = 10, textDecoration = "bold", halign = "left"
    )
    addStyle(wb, "Corrélations", style_varname,
             rows = 2:(n_vars + 1), cols = 1, stack = FALSE)
    
    # Coloriage conditionnel cellule par cellule
    for (i in seq_len(n_vars)) {
      for (j in seq_len(n_vars)) {
        val <- cor_matrix[i, j]
        
        # Interpolation couleur : bleu <-> blanc <-> rouge
        if (is.na(val)) {
          bg <- "#FFFFFF"
        } else if (val >= 0) {
          # blanc -> bleu foncé
          r_int <- round(255 - val * (255 - 31))
          g_int <- round(255 - val * (255 - 73))
          b_int <- round(255 - val * (255 - 125))
          bg <- sprintf("#%02X%02X%02X", r_int, g_int, b_int)
        } else {
          # blanc -> rouge foncé
          abs_val <- abs(val)
          r_int <- round(255 - abs_val * (255 - 192))
          g_int <- round(255 - abs_val * 255)
          b_int <- round(255 - abs_val * 255)
          bg <- sprintf("#%02X%02X%02X", r_int, g_int, b_int)
        }
        
        font_col <- if (abs(val) > 0.6) "white" else "black"
        
        style_cell <- createStyle(
          fgFill = bg, fontColour = font_col,
          fontSize = 10, halign = "center",
          border = "TopBottomLeftRight", borderColour = "#BFBFBF"
        )
        addStyle(wb, "Corrélations", style_cell,
                 rows = i + 1, cols = j + 1, stack = FALSE)
      }
    }
    
    # Diagonale : gris foncé (corrélation = 1, moins importante visuellement)
    for (k in seq_len(n_vars)) {
      style_diag <- createStyle(
        fgFill = "#595959", fontColour = "white",
        fontSize = 10, textDecoration = "bold", halign = "center"
      )
      addStyle(wb, "Corrélations", style_diag,
               rows = k + 1, cols = k + 1, stack = FALSE)
    }
    
    setColWidths(wb, "Corrélations",
                 cols = 1:(n_vars + 1),
                 widths = c(18, rep(11, n_vars)))
    
    cat("   ✓ Feuille Corrélations créée avec code couleur\n")
  }
  
  # ========================================================================
  # 4. RÉSUMÉ GLOBAL
  # ========================================================================
  
  cat("4️⃣ Création du résumé global...\n")
  
  resume_data <- data.frame(
    Métrique = c(
      "Nombre de lignes",
      "Nombre de colonnes",
      "Variables quantitatives",
      "Variables catégorielles",
      "Taux de données manquantes (%)",
      "Date de l'analyse"
    ),
    Valeur = c(
      nrow(df),
      ncol(df),
      length(quant_vars),
      length(qual_vars),
      round(sum(is.na(df)) / prod(dim(df)) * 100, 2),
      format(Sys.time(), "%d/%m/%Y %H:%M:%S")
    )
  )
  
  addWorksheet(wb, "Résumé")
  writeData(wb, "Résumé", resume_data)
  
  style_resume <- createStyle(
    fgFill = "#366092", 
    fontColour = "white", 
    fontSize = 12,
    textDecoration = "bold",
    halign = "left"
  )
  
  addStyle(wb, "Résumé", style_resume, rows = 1, cols = 1:2)
  setColWidths(wb, "Résumé", cols = 1:2, widths = c(30, 30))
  
  cat("   ✓ Résumé global créé\n")
  
  # ========================================================================
  # 5. SAUVEGARDER LE WORKBOOK EXCEL
  # ========================================================================
  
  cat("5️⃣ Sauvegarde du fichier Excel...\n")
  
  file_path <- paste0(output_dir, output_file)
  saveWorkbook(wb, file_path, overwrite = TRUE)
  
  cat(paste0("   ✓ Fichier Excel sauvegardé: ", file_path, "\n\n"))
  
  # ========================================================================
  # 6. VISUALISATIONS - VARIABLES QUANTITATIVES
  # ========================================================================
  
  if (length(quant_vars) > 0) {
    cat("6️⃣ Création des visualisations quantitatives...\n")
    
    # POURQUOI CE CHUNK ?
    # Chaque figure est enrichie avec les statistiques clés annotées directement
    # sur le graphique (moyenne, médiane, écart-type, N, skewness pour l'histo ;
    # Q1, médiane, Q3, min, max, nb valeurs aberrantes pour le boxplot).
    # Chaque variable génère un PNG individuel pour faciliter l'intégration.
    
    dir_hist <- paste0(output_dir, "histogrammes/")
    dir_box  <- paste0(output_dir, "boxplots/")
    if (!dir.exists(dir_hist)) dir.create(dir_hist, recursive = TRUE)
    if (!dir.exists(dir_box))  dir.create(dir_box,  recursive = TRUE)
    
    # --- Histogramme parlant : moyenne, médiane, écart-type, N, skewness ---
    for (var in quant_vars) {
      nom_fichier <- paste0(dir_hist, "hist_", var, ".png")
      png(nom_fichier, width = 1000, height = 720, res = 130)
      
      x     <- df[[var]][!is.na(df[[var]])]
      moy   <- mean(x)
      med   <- median(x)
      etyp  <- sd(x)
      skew  <- round(moments::skewness(x), 3)
      n_obs <- length(x)
      n_na  <- sum(is.na(df[[var]]))
      
      # Marges : haut agrandi pour la légende de stats
      par(mar = c(5, 4.5, 5.5, 2))
      
      h <- hist(x,
                main = "",
                xlab = var, ylab = "Fréquence",
                col  = "#4472C4", border = "white",
                breaks = "Sturges", las = 1)
      
      # Titre principal
      title(main = paste("Distribution de :", var), line = 3.5,
            cex.main = 1.2, font.main = 2, col.main = "#1F3864")
      
      # Ligne moyenne (rouge) et médiane (verte)
      abline(v = moy, col = "#C00000", lwd = 2, lty = 1)
      abline(v = med, col = "#375623", lwd = 2, lty = 2)
      
      # Rug plot
      rug(x, col = "#ED7D31", alpha = 0.4)
      
      # Légende des lignes
      legend("topright",
             legend = c(paste0("Moyenne = ", round(moy, 2)),
                        paste0("Médiane = ", round(med, 2))),
             col    = c("#C00000", "#375623"),
             lwd    = 2, lty = c(1, 2),
             bty    = "n", cex = 0.85)
      
      # Bandeau de stats en haut du graphique
      mtext(
        paste0("N = ", n_obs, "  |  NAs = ", n_na,
               "  |  Écart-type = ", round(etyp, 2),
               "  |  Skewness = ", skew,
               "  |  [Min = ", round(min(x), 2), "  ;  Max = ", round(max(x), 2), "]"),
        side = 3, line = 1.8, cex = 0.78, col = "#595959"
      )
      
      dev.off()
    }
    cat("   ✓ Histogrammes parlants créés (dans histogrammes/)\n")
    
    # --- Boxplot parlant : Q1, Q2, Q3, Min, Max, nb outliers ---
    for (var in quant_vars) {
      nom_fichier <- paste0(dir_box, "boxplot_", var, ".png")
      png(nom_fichier, width = 900, height = 720, res = 130)
      
      x       <- df[[var]]
      x_clean <- x[!is.na(x)]
      q1      <- quantile(x_clean, 0.25)
      q2      <- median(x_clean)
      q3      <- quantile(x_clean, 0.75)
      iqr_val <- IQR(x_clean)
      lo      <- q1 - 1.5 * iqr_val
      hi      <- q3 + 1.5 * iqr_val
      n_out   <- sum(x_clean < lo | x_clean > hi)
      
      par(mar = c(5, 5, 5.5, 2))
      
      bp <- boxplot(x,
                    main   = "",
                    ylab   = var,
                    col    = "#70AD47",
                    border = "#203864",
                    outcol = "#C55A11",
                    outpch = 16,
                    outcex = 0.9,
                    las    = 1,
                    whisklty = 1)
      
      title(main = paste("Boxplot de :", var), line = 3.5,
            cex.main = 1.2, font.main = 2, col.main = "#1F3864")
      
      # Annotations des quantiles sur le côté droit
      axis_x <- 1.42
      text(axis_x, q1, labels = paste0("Q1 = ", round(q1, 2)),
           cex = 0.75, col = "#203864", adj = 0)
      text(axis_x, q2, labels = paste0("Méd = ", round(q2, 2)),
           cex = 0.75, col = "#203864", adj = 0, font = 2)
      text(axis_x, q3, labels = paste0("Q3 = ", round(q3, 2)),
           cex = 0.75, col = "#203864", adj = 0)
      
      # Bandeau de stats
      mtext(
        paste0("N = ", length(x_clean), "  |  NAs = ", sum(is.na(x)),
               "  |  Moy = ", round(mean(x_clean), 2),
               "  |  IQR = ", round(iqr_val, 2),
               "  |  Outliers = ", n_out),
        side = 3, line = 1.8, cex = 0.78, col = "#595959"
      )
      
      dev.off()
    }
    cat("   ✓ Boxplots parlants créés (dans boxplots/)\n")
  }
  
  # ========================================================================
  # 7. VISUALISATIONS - HEATMAP DE CORRÉLATION
  # ========================================================================
  
  # POURQUOI CE CHUNK ?
  # La corrélation de Pearson ne s'applique qu'aux variables NUMÉRIQUES.
  # On filtre donc explicitement sur quant_vars (déjà identifiées plus haut).
  # On affiche les coefficients dans chaque cellule + une légende de couleur,
  # et on ajoute un titre et un sous-titre informatifs sur l'image.
  if (length(quant_vars) >= 2) {
    cat("7️⃣ Création de la heatmap de corrélation...\n")
    
    # Matrice de corrélation sur variables numériques uniquement
    cor_matrix <- cor(df[, quant_vars, drop = FALSE],
                      use = "complete.obs", method = "pearson")
    
    n_vars_cor <- length(quant_vars)
    # Taille adaptative selon le nombre de variables
    img_size <- max(1200, n_vars_cor * 120)
    
    png(paste0(output_dir, "heatmap_correlation.png"),
        width = img_size, height = img_size + 120, res = 150)
    
    par(mar = c(2, 2, 4, 2))
    
    corrplot(cor_matrix,
             method       = "color",
             type         = "upper",
             addCoef.col  = "black",
             number.cex   = max(0.5, 0.9 - n_vars_cor * 0.03),
             tl.cex       = max(0.6, 0.9 - n_vars_cor * 0.02),
             tl.col       = "#1F3864",
             tl.srt       = 45,
             cl.cex       = 0.75,
             col          = colorRampPalette(c("#C00000", "white", "#1F3864"))(200),
             is.corr      = TRUE,
             diag         = FALSE,
             mar          = c(0, 0, 3, 0))
    
    # Titre et sous-titre
    title(
      main = "Matrice de corrélation de Pearson (variables numériques)",
      line = 1.5, cex.main = 1.0, font.main = 2, col.main = "#1F3864"
    )
    mtext(
      paste0("N = ", nrow(df), " observations  |  ",
             n_vars_cor, " variables numériques  |  ",
             "Méthode : corrélation de Pearson (paires complètes)"),
      side = 3, line = 0.2, cex = 0.65, col = "#595959"
    )
    
    dev.off()
    cat("   ✓ Heatmap créée (heatmap_correlation.png)\n")
  }
  
  # ========================================================================
  # 8. VISUALISATIONS - VARIABLES CATÉGORIELLES
  # ========================================================================
  
  if (length(qual_vars) > 0) {
    cat("8️⃣ Création des visualisations catégorielles...\n")
    
    # POURQUOI CE CHUNK ?
    # Chaque barre affiche l'effectif (n) ET le pourcentage (%) directement
    # au-dessus de la barre, pour une lecture immédiate sans avoir à consulter
    # les axes. Un bandeau de stats (N total, nb modalités, mode) complète la figure.
    
    dir_bar <- paste0(output_dir, "diagrammes_barres/")
    if (!dir.exists(dir_bar)) dir.create(dir_bar, recursive = TRUE)
    
    for (var in qual_vars) {
      nom_fichier <- paste0(dir_bar, "barres_", var, ".png")
      png(nom_fichier, width = 1050, height = 720, res = 130)
      
      freq_raw  <- sort(table(df[[var]]), decreasing = TRUE)
      n_total   <- sum(freq_raw)
      n_modal   <- length(freq_raw)
      mode_cat  <- names(freq_raw)[1]
      
      # Limiter à 15 catégories pour la lisibilité
      if (length(freq_raw) > 15) freq_raw <- freq_raw[1:15]
      
      pcts <- round(freq_raw / n_total * 100, 1)
      
      # Palette de couleurs dégradée
      nb_bars <- length(freq_raw)
      couleurs <- colorRampPalette(c("#1F3864", "#4472C4", "#9DC3E6"))(nb_bars)
      
      par(mar = c(6.5, 5, 5.5, 2))
      
      bp <- barplot(freq_raw,
                    main   = "",
                    ylab   = "Effectif (n)",
                    col    = couleurs,
                    border = "white",
                    las    = 2,
                    ylim   = c(0, max(freq_raw) * 1.22),
                    names.arg = names(freq_raw),
                    cex.names = 0.82)
      
      title(main = paste("Distribution de :", var), line = 3.5,
            cex.main = 1.2, font.main = 2, col.main = "#1F3864")
      
      # Annotation au-dessus de chaque barre : effectif + pourcentage
      text(x      = bp,
           y      = as.numeric(freq_raw) + max(freq_raw) * 0.015,
           labels = paste0(freq_raw, "\n(", pcts, "%)"),
           cex    = 0.75, col = "#1F3864", font = 2, adj = c(0.5, 0))
      
      # Ligne de la moyenne des effectifs
      moy_eff <- mean(as.numeric(freq_raw))
      abline(h = moy_eff, col = "#C00000", lwd = 1.5, lty = 2)
      legend("topright",
             legend = paste0("Moy. effectif = ", round(moy_eff, 1)),
             col = "#C00000", lwd = 1.5, lty = 2, bty = "n", cex = 0.8)
      
      # Bandeau de stats
      mtext(
        paste0("N total = ", n_total,
               "  |  Modalités = ", n_modal,
               "  |  Mode = \"", mode_cat, "\"",
               "  |  NAs = ", sum(is.na(df[[var]]))),
        side = 3, line = 1.8, cex = 0.75, col = "#595959"
      )
      
      dev.off()
    }
    cat("   ✓ Diagrammes en barres parlants créés (dans diagrammes_barres/)\n")
  }
  
  # ========================================================================
  # 9. DONNÉES MANQUANTES
  # ========================================================================
  
  cat("9️⃣ Analyse des données manquantes...\n")
  
  missing_data <- data.frame(
    Variable = names(df),
    N_Missing = colSums(is.na(df)),
    Pct_Missing = round(colSums(is.na(df)) / nrow(df) * 100, 2)
  )
  
  missing_data <- missing_data[order(missing_data$N_Missing, decreasing = TRUE), ]
  
  wb_missing <- createWorkbook()
  addWorksheet(wb_missing, "Données_Manquantes")
  writeData(wb_missing, "Données_Manquantes", missing_data)
  
  style_header <- createStyle(
    fgFill = "#C65911", 
    fontColour = "white", 
    fontSize = 11,
    textDecoration = "bold"
  )
  
  addStyle(wb_missing, "Données_Manquantes", style_header, rows = 1, cols = 1:3)
  setColWidths(wb_missing, "Données_Manquantes", cols = 1:3, widths = c(25, 12, 12))
  
  saveWorkbook(wb_missing, paste0(output_dir, "Donnees_Manquantes.xlsx"), 
               overwrite = TRUE)
  
  cat("   ✓ Données manquantes analysées\n\n")
  
  # ========================================================================
  # RAPPORT FINAL
  # ========================================================================
  
  cat("✅ ANALYSE DESCRIPTIVE COMPLÉTÉE AVEC SUCCÈS !\n")
  cat("================================================\n")
  cat("📁 Fichiers créés:\n")
  cat("   1. Analyse_Descriptive.xlsx (Excel complet avec stats)\n")
  cat("   2. Donnees_Manquantes.xlsx (Analyse des NA)\n")
  cat("   3. histogrammes/hist_<variable>.png (un PNG par variable)\n")
  cat("   4. boxplots/boxplot_<variable>.png (un PNG par variable)\n")
  cat("   5. heatmap_correlation.png\n")
  cat("   6. diagrammes_barres/barres_<variable>.png (un PNG par variable)\n")
  cat("================================================\n")
  cat(paste0("📂 Dossier de sortie: ", output_dir, "\n\n"))
  
  # Retourner les statistiques pour consultation dans R
  return(list(
    stats_quantitatives = if (length(quant_vars) > 0) stats_quant else NULL,
    donnees_manquantes = missing_data,
    variables_quantitatives = quant_vars,
    variables_qualitatives = qual_vars,
    dossier_sortie = output_dir
  ))
}

# ============================================================================
# EXEMPLE D'UTILISATION
# ============================================================================

# Créer un dataset exemple
set.seed(123)
df_exemple <- data.frame(
  Age = rnorm(500, mean = 40, sd = 15),
  Salaire = rnorm(500, mean = 50000, sd = 20000),
  Expérience = rnorm(500, mean = 10, sd = 5),
  Score = rnorm(500, mean = 75, sd = 15),
  Département = sample(c("Ventes", "IT", "HR", "Finance", "Marketing"), 500, replace = TRUE),
  Ville = sample(c("Paris", "Lyon", "Marseille", "Toulouse", "Nice"), 500, replace = TRUE),
  Satisfaction = sample(c("Très insatisfait", "Insatisfait", "Neutre", "Satisfait", "Très satisfait"), 500, replace = TRUE)
)

# Ajouter quelques valeurs manquantes
df_exemple$Age[sample(1:500, 20)] <- NA
df_exemple$Salaire[sample(1:500, 15)] <- NA

# EXÉCUTER L'ANALYSE
# resultats <- analyse_descriptive_pro(df_exemple)

# Pour voir le résumé des statistiques
# print(resultats$stats_quantitatives)
# print(resultats$donnees_manquantes)
