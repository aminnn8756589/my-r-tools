# ==============================================================================
# FONCTION : traiter_outliers()
# SUITE DE : analyse_exploratoire()
# OBJET    : Traitement complet des valeurs aberrantes (winsorisation, imputation
#            médiane, suppression) avec export Excel multi-feuilles colorisé.
# ==============================================================================

traiter_outliers <- function(
    df,
    var_interet    = NULL,       # nom (chaîne) de la variable dépendante
    methode        = "winsorisation",  # "winsorisation" | "mediane" | "suppression"
    outlier_seuil  = 3,          # seuil Z-score pour outliers sévères
    output_dir     = "output",   # MÊME dossier que analyse_exploratoire()
    winsor_probs   = c(0.05, 0.95)  # percentiles de winsorisation
) {

  # ── 0. Packages ─────────────────────────────────────────────────────────────
  pkgs <- c("openxlsx", "dplyr", "moments")
  invisible(lapply(pkgs, function(p) {
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
    library(p, character.only = TRUE)
  }))

  # ── 1. Vérifications ────────────────────────────────────────────────────────
  if (!methode %in% c("winsorisation", "mediane", "suppression"))
    stop("methode doit être 'winsorisation', 'mediane' ou 'suppression'.")
  if (!is.null(var_interet) && !(var_interet %in% names(df)))
    stop("var_interet '", var_interet, "' introuvable dans df.")

  dir_data <- file.path(output_dir, "data")
  dir.create(dir_data, recursive = TRUE, showWarnings = FALSE)

  vars_num     <- names(df)[sapply(df, is.numeric)]
  vars_exp_num <- setdiff(vars_num, var_interet)

  # ── 2. Styles Excel (cohérents avec analyse_exploratoire) ───────────────────
  wb <- createWorkbook()

  st_header <- createStyle(fontName = "Arial", fontSize = 12, fontColour = "white",
                           fgFill = "#1F4E79", halign = "CENTER", valign = "CENTER",
                           textDecoration = "bold", border = "Bottom",
                           borderColour = "#FFFFFF")
  st_titre  <- createStyle(fontName = "Arial", fontSize = 14,
                           textDecoration = "bold", fontColour = "#1F4E79")
  st_sous   <- createStyle(fontName = "Arial", fontSize = 12,
                           textDecoration = "bold", fontColour = "#2E75B6")
  st_normal <- createStyle(fontName = "Arial", fontSize = 10)
  st_corps  <- createStyle(fontName = "Arial", fontSize = 10,
                           border = "TopBottomLeftRight", borderColour = "#D9D9D9")
  st_rouge  <- createStyle(fgFill = "#FF4444", fontColour = "white",
                           fontName = "Arial", fontSize = 10,
                           textDecoration = "bold", halign = "CENTER")
  st_orange <- createStyle(fgFill = "#FFA500", fontColour = "white",
                           fontName = "Arial", fontSize = 10, halign = "CENTER")
  st_vert   <- createStyle(fgFill = "#27AE60", fontColour = "white",
                           fontName = "Arial", fontSize = 10, halign = "CENTER")
  st_bleu   <- createStyle(fgFill = "#EBF3FB", fontName = "Arial", fontSize = 10)

  # ══════════════════════════════════════════════════════════════════════════════
  # FEUILLE 0 : RÉCAPITULATIF GÉNÉRAL (pédagogique + méthodologique)
  # ══════════════════════════════════════════════════════════════════════════════
  addWorksheet(wb, "0_Recapitulatif_General")

  # Titre principal
  writeData(wb, "0_Recapitulatif_General",
            "TRAITEMENT DES VALEURS ABERRANTES — RÉCAPITULATIF GÉNÉRAL",
            startRow = 1, startCol = 1)
  addStyle(wb, "0_Recapitulatif_General", st_titre, rows = 1, cols = 1)
  mergeCells(wb, "0_Recapitulatif_General", cols = 1:6, rows = 1)

  writeData(wb, "0_Recapitulatif_General",
            paste0("Méthode appliquée : ", toupper(methode),
                   "   |   Seuil Z-score : ±", outlier_seuil,
                   "   |   Date : ", format(Sys.Date(), "%d/%m/%Y")),
            startRow = 2, startCol = 1)
  addStyle(wb, "0_Recapitulatif_General",
           createStyle(fontName = "Arial", fontSize = 11, fontColour = "grey40"),
           rows = 2, cols = 1)

  # ── A. Qu'est-ce qu'un outlier ? ────────────────────────────────────────────
  r <- 4
  writeData(wb, "0_Recapitulatif_General",
            "A. QU'EST-CE QU'UNE VALEUR ABERRANTE (OUTLIER) ?",
            startRow = r, startCol = 1)
  addStyle(wb, "0_Recapitulatif_General", st_sous, rows = r, cols = 1)

  texte_def <- data.frame(
    Paragraphe = c(
      "Une valeur aberrante est une observation dont la valeur diffère significativement des autres observations d'un échantillon. Elle peut résulter d'une erreur de saisie, d'un cas extrême réel ou d'un phénomène exceptionnel.",
      "Les outliers peuvent biaiser les estimateurs (moyenne, écart-type, coefficients de régression), réduire la puissance des tests statistiques et fausser les prédictions des modèles économétriques.",
      "Dans un mémoire de fin d'études, le chercheur doit obligatoirement signaler la présence d'outliers, justifier la méthode de traitement choisie et comparer les résultats avant/après traitement."
    ), stringsAsFactors = FALSE)
  writeDataTable(wb, "0_Recapitulatif_General", texte_def,
                 startRow = r + 1, startCol = 1,
                 tableStyle = "TableStyleLight1", headerStyle = st_header)
  setColWidths(wb, "0_Recapitulatif_General", cols = 1, widths = 120)

  # ── B. Méthodes de détection ─────────────────────────────────────────────────
  r <- r + 1 + nrow(texte_def) + 2
  writeData(wb, "0_Recapitulatif_General",
            "B. MÉTHODES DE DÉTECTION UTILISÉES DANS CE RAPPORT",
            startRow = r, startCol = 1)
  addStyle(wb, "0_Recapitulatif_General", st_sous, rows = r, cols = 1)

  methodes_det <- data.frame(
    Méthode = c("Règle de Tukey (IQR)", "Z-score standardisé"),
    Formule = c(
      "Borne inf = Q1 − 1.5×IQR   |   Borne sup = Q3 + 1.5×IQR   (IQR = Q3 − Q1)",
      paste0("Z = (Xi − X̄) / σ   →   outlier sévère si |Z| > ", outlier_seuil)
    ),
    `Comment l'interpréter` = c(
      "Toute valeur en dehors des bornes est suspecte (outlier modéré). Non sensible à la présence d'outliers dans le calcul.",
      "Mesure l'éloignement en nombre d'écarts-types. Plus robuste sur distributions symétriques."
    ),
    `Référence académique` = c(
      "Tukey, J.W. (1977). Exploratory Data Analysis. Addison-Wesley.",
      "Grubbs, F.E. (1969). Procedures for detecting outlying observations. Technometrics, 11(1), 1–21."
    ),
    stringsAsFactors = FALSE
  )
  writeDataTable(wb, "0_Recapitulatif_General", methodes_det,
                 startRow = r + 1, startCol = 1,
                 tableStyle = "TableStyleMedium2", headerStyle = st_header)
  setColWidths(wb, "0_Recapitulatif_General", cols = 1:4, widths = c(28, 60, 50, 55))

  # ── C. Méthodes de traitement ────────────────────────────────────────────────
  r <- r + 1 + nrow(methodes_det) + 2
  writeData(wb, "0_Recapitulatif_General",
            "C. MÉTHODES DE TRAITEMENT — AVANTAGES ET LIMITES",
            startRow = r, startCol = 1)
  addStyle(wb, "0_Recapitulatif_General", st_sous, rows = r, cols = 1)

  methodes_trt <- data.frame(
    Méthode = c("Winsorisation", "Imputation par la médiane", "Suppression"),
    `Comment ça marche` = c(
      "Les valeurs inférieures au percentile p_bas sont remplacées par la valeur du p_bas-ième centile ; idem pour p_haut. On ne supprime aucune observation.",
      "Chaque outlier est remplacé par la médiane de sa variable. La médiane est robuste car insensible aux extrêmes.",
      "Les lignes contenant au moins un outlier sévère (|Z| > seuil) sont supprimées de l'échantillon."
    ),
    Avantages = c(
      "Conserve toutes les observations. Réduit l'influence des extrêmes sans éliminer d'information.",
      "Simple à mettre en œuvre. N'introduit pas de biais si les outliers sont rares.",
      "Élimine définitivement les valeurs problématiques. Utile si les outliers sont dus à des erreurs."
    ),
    Inconvénients = c(
      "Modifie artificiellement les distributions aux extrêmes. Doit être justifiée.",
      "Peut sous-estimer la variabilité réelle si de nombreux outliers sont remplacés.",
      "Réduit la taille de l'échantillon. Peut introduire un biais de sélection."
    ),
    `Quand l'utiliser` = c(
      "Données continues avec extrêmes dus au contexte (revenus, prix, volumes). Fréquent en économie et finance.",
      "Outliers rares et aléatoires. Données manquantes imputées par la même occasion.",
      "Outliers clairement identifiés comme erreurs de saisie ou cas impossibles."
    ),
    stringsAsFactors = FALSE
  )
  writeDataTable(wb, "0_Recapitulatif_General", methodes_trt,
                 startRow = r + 1, startCol = 1,
                 tableStyle = "TableStyleMedium9", headerStyle = st_header)
  setColWidths(wb, "0_Recapitulatif_General", cols = 1:5,
               widths = c(26, 52, 45, 45, 48))

  # Colorier la ligne de la méthode utilisée
  idx_methode <- switch(methode,
    "winsorisation" = 1,
    "mediane"       = 2,
    "suppression"   = 3
  )
  for (col_i in 1:5) {
    addStyle(wb, "0_Recapitulatif_General",
             createStyle(fgFill = "#D9F2E6", fontName = "Arial", fontSize = 10,
                         textDecoration = "bold", border = "TopBottomLeftRight",
                         borderColour = "#27AE60"),
             rows = r + 1 + idx_methode, cols = col_i, stack = FALSE)
  }

  # ── D. Ce qu'il faut écrire dans le mémoire ──────────────────────────────────
  r <- r + 1 + nrow(methodes_trt) + 2
  writeData(wb, "0_Recapitulatif_General",
            "D. CE QU'IL FAUT ÉCRIRE DANS LE MÉMOIRE DE FIN D'ÉTUDES",
            startRow = r, startCol = 1)
  addStyle(wb, "0_Recapitulatif_General", st_sous, rows = r, cols = 1)

  guide_mem <- data.frame(
    Section = c(
      "Présentation de l'échantillon",
      "Identification des outliers",
      "Choix de la méthode de traitement",
      "Résultats avant / après",
      "Analyse de robustesse",
      "Formulation type"
    ),
    `Ce qu'il faut écrire` = c(
      "Décrire la taille initiale de l'échantillon (N), la période d'étude et la source des données.",
      "Indiquer la méthode de détection (Tukey, Z-score), le seuil retenu et le nombre d'outliers identifiés par variable.",
      paste0("Justifier le choix de la méthode '", methode, "' par rapport aux alternatives. Citer au moins une référence méthodologique (ex. : Tukey, 1977 ; Grubbs, 1969)."),
      "Comparer les statistiques descriptives (moyenne, écart-type, asymétrie) avant et après traitement dans un tableau.",
      "Ré-estimer le modèle principal avec et sans traitement des outliers pour s'assurer de la stabilité des résultats.",
      paste0("Exemple : « L'analyse préliminaire des données a révélé la présence de ", "XX", " valeurs aberrantes identifiées par la méthode combinée de Tukey (1977) et du Z-score (seuil : ±", outlier_seuil, "). Ces observations ont été traitées par ", methode, " afin de préserver la représentativité de l'échantillon tout en réduisant l'influence des extrêmes sur les estimateurs. »")
    ),
    stringsAsFactors = FALSE
  )
  writeDataTable(wb, "0_Recapitulatif_General", guide_mem,
                 startRow = r + 1, startCol = 1,
                 tableStyle = "TableStyleLight9", headerStyle = st_header)
  setColWidths(wb, "0_Recapitulatif_General", cols = 1:2, widths = c(35, 100))

  # ── E. Légende des couleurs ───────────────────────────────────────────────────
  r <- r + 1 + nrow(guide_mem) + 2
  writeData(wb, "0_Recapitulatif_General",
            "E. LÉGENDE DES COULEURS UTILISÉES DANS CE RAPPORT",
            startRow = r, startCol = 1)
  addStyle(wb, "0_Recapitulatif_General", st_sous, rows = r, cols = 1)

  legende_df <- data.frame(
    Couleur      = c("ROUGE",   "ORANGE",       "VERT",          "BLEU CLAIR"),
    Signification = c(
      paste0("Outlier sévère — |Z-score| > ", outlier_seuil, " → à traiter en priorité"),
      "Outlier modéré — hors bornes Tukey mais |Z| ≤ seuil → à surveiller",
      "Corrélation forte (|r| ≥ 0.7) ou résultat satisfaisant",
      "Ligne paire ou information secondaire"
    ),
    stringsAsFactors = FALSE
  )
  writeDataTable(wb, "0_Recapitulatif_General", legende_df,
                 startRow = r + 1, startCol = 1,
                 tableStyle = "TableStyleLight1", headerStyle = st_header)

  for (i in 1:nrow(legende_df)) {
    coul_fond <- switch(i, "#FF4444", "#FFA500", "#27AE60", "#EBF3FB")
    fc        <- if (i < 4) "white" else "black"
    addStyle(wb, "0_Recapitulatif_General",
             createStyle(fgFill = coul_fond, fontColour = fc,
                         fontName = "Arial", fontSize = 10, textDecoration = "bold"),
             rows = r + 1 + i, cols = 1, stack = FALSE)
  }
  setColWidths(wb, "0_Recapitulatif_General", cols = 1:2, widths = c(22, 80))

  # ══════════════════════════════════════════════════════════════════════════════
  # FEUILLE 1 : Diagnostic avant traitement
  # ══════════════════════════════════════════════════════════════════════════════
  addWorksheet(wb, "1_Diagnostic_Avant")
  writeData(wb, "1_Diagnostic_Avant",
            "DIAGNOSTIC DES OUTLIERS AVANT TRAITEMENT",
            startRow = 1, startCol = 1)
  addStyle(wb, "1_Diagnostic_Avant", st_titre, rows = 1, cols = 1)

  diag_list <- lapply(vars_num, function(v) {
    x  <- df[[v]]
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iq <- IQR(x, na.rm = TRUE)
    mu <- mean(x, na.rm = TRUE)
    sg <- sd(x, na.rm = TRUE)

    borne_inf  <- q1 - 1.5 * iq
    borne_sup  <- q3 + 1.5 * iq
    z_scores   <- (x - mu) / sg

    n_tukey  <- sum(!is.na(x) & (x < borne_inf | x > borne_sup))
    n_severe <- sum(!is.na(x) & abs(z_scores) > outlier_seuil)

    data.frame(
      Variable           = v,
      N_Total            = sum(!is.na(x)),
      N_Outliers_Tukey   = n_tukey,
      Pct_Tukey          = round(n_tukey / sum(!is.na(x)) * 100, 2),
      N_Outliers_Severes = n_severe,
      Pct_Severes        = round(n_severe / sum(!is.na(x)) * 100, 2),
      Min_Avant          = round(min(x, na.rm = TRUE), 4),
      Max_Avant          = round(max(x, na.rm = TRUE), 4),
      Moyenne_Avant      = round(mu, 4),
      EcartType_Avant    = round(sg, 4),
      Asymetrie_Avant    = round(moments::skewness(x, na.rm = TRUE), 4),
      Borne_Inf_Tukey    = round(borne_inf, 4),
      Borne_Sup_Tukey    = round(borne_sup, 4),
      stringsAsFactors   = FALSE
    )
  })
  diag_df <- do.call(rbind, diag_list)

  writeDataTable(wb, "1_Diagnostic_Avant", diag_df,
                 startRow = 3, startCol = 1,
                 tableStyle = "TableStyleMedium2", headerStyle = st_header)

  col_sev <- which(names(diag_df) == "N_Outliers_Severes")
  col_tuk <- which(names(diag_df) == "N_Outliers_Tukey")
  for (i in seq_len(nrow(diag_df))) {
    if (diag_df$N_Outliers_Severes[i] > 0) {
      addStyle(wb, "1_Diagnostic_Avant", st_rouge,
               rows = i + 3, cols = col_sev, stack = FALSE)
    }
    if (diag_df$N_Outliers_Tukey[i] > 0 && diag_df$N_Outliers_Severes[i] == 0) {
      addStyle(wb, "1_Diagnostic_Avant", st_orange,
               rows = i + 3, cols = col_tuk, stack = FALSE)
    }
  }
  setColWidths(wb, "1_Diagnostic_Avant", cols = 1:ncol(diag_df),
               widths = c(22, 10, 18, 12, 20, 14, 12, 12, 14, 16, 16, 18, 18))

  # ══════════════════════════════════════════════════════════════════════════════
  # FEUILLE 2 : Détail individuel des outliers
  # ══════════════════════════════════════════════════════════════════════════════
  addWorksheet(wb, "2_Detail_Outliers")
  writeData(wb, "2_Detail_Outliers",
            "LISTE DÉTAILLÉE DES OBSERVATIONS ABERRANTES",
            startRow = 1, startCol = 1)
  addStyle(wb, "2_Detail_Outliers", st_titre, rows = 1, cols = 1)

  detail_list <- lapply(vars_num, function(v) {
    x  <- df[[v]]
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iq <- IQR(x, na.rm = TRUE)
    mu <- mean(x, na.rm = TRUE)
    sg <- sd(x, na.rm = TRUE)

    borne_inf <- q1 - 1.5 * iq
    borne_sup <- q3 + 1.5 * iq
    z_scores  <- (x - mu) / sg

    idx <- which(!is.na(x) & (x < borne_inf | x > borne_sup))
    if (length(idx) == 0) return(NULL)

    data.frame(
      Observation      = idx,
      Variable         = v,
      Valeur_Originale = round(x[idx], 4),
      Z_score          = round(z_scores[idx], 4),
      Direction        = ifelse(x[idx] < borne_inf, "← Inférieur", "→ Supérieur"),
      Ecart_Borne_pct  = round(ifelse(x[idx] < borne_inf,
                                      (borne_inf - x[idx]) / abs(borne_inf) * 100,
                                      (x[idx] - borne_sup) / abs(borne_sup) * 100), 2),
      Borne_Inf_Tukey  = round(borne_inf, 4),
      Borne_Sup_Tukey  = round(borne_sup, 4),
      Gravite          = ifelse(abs(z_scores[idx]) > outlier_seuil,
                                "SÉVÈRE", "Modéré"),
      stringsAsFactors = FALSE
    )
  })
  detail_df <- do.call(rbind, detail_list)

  if (!is.null(detail_df) && nrow(detail_df) > 0) {
    detail_df <- detail_df %>% arrange(desc(abs(Z_score)))

    writeDataTable(wb, "2_Detail_Outliers", detail_df,
                   startRow = 3, startCol = 1,
                   tableStyle = "TableStyleMedium2", headerStyle = st_header)

    col_grav <- which(names(detail_df) == "Gravite")
    col_z2   <- which(names(detail_df) == "Z_score")
    for (i in seq_len(nrow(detail_df))) {
      if (detail_df$Gravite[i] == "SÉVÈRE") {
        addStyle(wb, "2_Detail_Outliers", st_rouge,
                 rows = i + 3, cols = col_grav, stack = FALSE)
        addStyle(wb, "2_Detail_Outliers",
                 createStyle(fgFill = "#FFD7D7", fontName = "Arial",
                             fontSize = 10, numFmt = "#,##0.0000"),
                 rows = i + 3, cols = col_z2, stack = FALSE)
      } else {
        addStyle(wb, "2_Detail_Outliers", st_orange,
                 rows = i + 3, cols = col_grav, stack = FALSE)
      }
    }
    setColWidths(wb, "2_Detail_Outliers", cols = 1:ncol(detail_df),
                 widths = c(14, 22, 18, 12, 15, 18, 18, 18, 12))
  } else {
    writeData(wb, "2_Detail_Outliers",
              "Aucun outlier détecté par la méthode Tukey.", startRow = 3)
  }

  # ══════════════════════════════════════════════════════════════════════════════
  # FEUILLE 3 : Application du traitement
  # ══════════════════════════════════════════════════════════════════════════════
  addWorksheet(wb, "3_Traitement_Applique")
  writeData(wb, "3_Traitement_Applique",
            paste0("TRAITEMENT APPLIQUÉ — MÉTHODE : ", toupper(methode)),
            startRow = 1, startCol = 1)
  addStyle(wb, "3_Traitement_Applique", st_titre, rows = 1, cols = 1)

  df_traite <- df

  trt_list <- lapply(vars_num, function(v) {
    x    <- df[[v]]
    mu   <- mean(x, na.rm = TRUE)
    sg   <- sd(x, na.rm = TRUE)
    med  <- median(x, na.rm = TRUE)
    q1   <- quantile(x, 0.25, na.rm = TRUE)
    q3   <- quantile(x, 0.75, na.rm = TRUE)
    iq   <- IQR(x, na.rm = TRUE)
    z    <- (x - mu) / sg

    borne_inf <- q1 - 1.5 * iq
    borne_sup <- q3 + 1.5 * iq
    idx_sev   <- which(!is.na(x) & abs(z) > outlier_seuil)

    x_new <- x
    valeur_remplacement <- NA_character_

    if (methode == "winsorisation") {
      p_bas <- quantile(x, winsor_probs[1], na.rm = TRUE)
      p_hau <- quantile(x, winsor_probs[2], na.rm = TRUE)
      x_new <- pmax(pmin(x, p_hau), p_bas)
      valeur_remplacement <- paste0("P", winsor_probs[1]*100, " = ", round(p_bas, 4),
                                    " / P", winsor_probs[2]*100, " = ", round(p_hau, 4))
    } else if (methode == "mediane") {
      x_new[idx_sev] <- med
      valeur_remplacement <- paste0("Médiane = ", round(med, 4))
    } else if (methode == "suppression") {
      # marqué ici, suppression faite sur df_traite après
      valeur_remplacement <- paste0(length(idx_sev), " lignes supprimées")
    }

    if (methode != "suppression") {
      df_traite[[v]] <<- x_new
    }

    n_modif <- if (methode == "winsorisation") {
      sum(!is.na(x) & (x < quantile(x, winsor_probs[1], na.rm = TRUE) |
                        x > quantile(x, winsor_probs[2], na.rm = TRUE)))
    } else {
      length(idx_sev)
    }

    data.frame(
      Variable              = v,
      N_Valeurs_Modifiees   = n_modif,
      Methode_Appliquee     = methode,
      Valeur_Remplacement   = valeur_remplacement,
      Moyenne_Avant         = round(mu, 4),
      Moyenne_Apres         = round(mean(x_new, na.rm = TRUE), 4),
      EcartType_Avant       = round(sg, 4),
      EcartType_Apres       = round(sd(x_new, na.rm = TRUE), 4),
      Asymetrie_Avant       = round(moments::skewness(x, na.rm = TRUE), 4),
      Asymetrie_Apres       = round(moments::skewness(x_new, na.rm = TRUE), 4),
      stringsAsFactors = FALSE
    )
  })
  trt_df <- do.call(rbind, trt_list)

  # Suppression : retirer lignes outliers sévères
  if (methode == "suppression") {
    idx_suppr <- unique(unlist(lapply(vars_num, function(v) {
      x <- df[[v]]
      mu <- mean(x, na.rm = TRUE); sg <- sd(x, na.rm = TRUE)
      which(!is.na(x) & abs((x - mu) / sg) > outlier_seuil)
    })))
    if (length(idx_suppr) > 0) df_traite <- df_traite[-idx_suppr, ]
  }

  writeDataTable(wb, "3_Traitement_Applique", trt_df,
                 startRow = 3, startCol = 1,
                 tableStyle = "TableStyleMedium2", headerStyle = st_header)

  col_n <- which(names(trt_df) == "N_Valeurs_Modifiees")
  for (i in seq_len(nrow(trt_df))) {
    if (trt_df$N_Valeurs_Modifiees[i] > 0) {
      addStyle(wb, "3_Traitement_Applique", st_orange,
               rows = i + 3, cols = col_n, stack = FALSE)
    }
  }
  setColWidths(wb, "3_Traitement_Applique", cols = 1:ncol(trt_df),
               widths = c(22, 20, 20, 35, 14, 14, 16, 16, 16, 16))

  # ══════════════════════════════════════════════════════════════════════════════
  # FEUILLE 4 : Comparaison avant / après (tableau de bord)
  # ══════════════════════════════════════════════════════════════════════════════
  addWorksheet(wb, "4_Comparaison_Avant_Apres")
  writeData(wb, "4_Comparaison_Avant_Apres",
            "COMPARAISON STATISTIQUE AVANT / APRÈS TRAITEMENT",
            startRow = 1, startCol = 1)
  addStyle(wb, "4_Comparaison_Avant_Apres", st_titre, rows = 1, cols = 1)

  comp_list <- lapply(vars_num, function(v) {
    x_av  <- df[[v]]
    x_ap  <- df_traite[[v]]
    if (is.null(x_ap)) x_ap <- x_av   # suppression : variable inchangée

    delta_moy <- round(mean(x_ap, na.rm = TRUE) - mean(x_av, na.rm = TRUE), 4)
    delta_sd  <- round(sd(x_ap, na.rm = TRUE)   - sd(x_av, na.rm = TRUE), 4)

    data.frame(
      Variable           = v,
      N_Avant            = sum(!is.na(x_av)),
      N_Apres            = sum(!is.na(x_ap)),
      Diff_N             = sum(!is.na(x_ap)) - sum(!is.na(x_av)),
      Moyenne_Avant      = round(mean(x_av, na.rm = TRUE), 4),
      Moyenne_Apres      = round(mean(x_ap, na.rm = TRUE), 4),
      Delta_Moyenne      = delta_moy,
      EcartType_Avant    = round(sd(x_av, na.rm = TRUE), 4),
      EcartType_Apres    = round(sd(x_ap, na.rm = TRUE), 4),
      Delta_EcartType    = delta_sd,
      Asymetrie_Avant    = round(moments::skewness(x_av, na.rm = TRUE), 4),
      Asymetrie_Apres    = round(moments::skewness(x_ap, na.rm = TRUE), 4),
      Min_Avant          = round(min(x_av, na.rm = TRUE), 4),
      Min_Apres          = round(min(x_ap, na.rm = TRUE), 4),
      Max_Avant          = round(max(x_av, na.rm = TRUE), 4),
      Max_Apres          = round(max(x_ap, na.rm = TRUE), 4),
      stringsAsFactors   = FALSE
    )
  })
  comp_df <- do.call(rbind, comp_list)

  writeDataTable(wb, "4_Comparaison_Avant_Apres", comp_df,
                 startRow = 3, startCol = 1,
                 tableStyle = "TableStyleMedium2", headerStyle = st_header)

  col_dM <- which(names(comp_df) == "Delta_Moyenne")
  col_dS <- which(names(comp_df) == "Delta_EcartType")
  for (i in seq_len(nrow(comp_df))) {
    # Variation importante de la moyenne (> 5 %)
    rel_chg <- abs(comp_df$Delta_Moyenne[i]) /
               (abs(comp_df$Moyenne_Avant[i]) + 1e-10) * 100
    if (rel_chg > 5) {
      addStyle(wb, "4_Comparaison_Avant_Apres", st_orange,
               rows = i + 3, cols = col_dM, stack = FALSE)
    } else {
      addStyle(wb, "4_Comparaison_Avant_Apres", st_vert,
               rows = i + 3, cols = col_dM, stack = FALSE)
    }
  }
  setColWidths(wb, "4_Comparaison_Avant_Apres", cols = 1:ncol(comp_df),
               widths = c(22, 10, 10, 10, 14, 14, 14, 16, 16, 16, 16, 16, 12, 12, 12, 12))

  # ══════════════════════════════════════════════════════════════════════════════
  # FEUILLE 5 : Données nettoyées
  # ══════════════════════════════════════════════════════════════════════════════
  addWorksheet(wb, "5_Donnees_Nettoyees")
  writeData(wb, "5_Donnees_Nettoyees",
            paste0("DONNÉES APRÈS TRAITEMENT (", methode, ") — N = ", nrow(df_traite),
                   " observations sur ", nrow(df), " initiales"),
            startRow = 1, startCol = 1)
  addStyle(wb, "5_Donnees_Nettoyees", st_titre, rows = 1, cols = 1)

  writeDataTable(wb, "5_Donnees_Nettoyees", df_traite,
                 startRow = 3, startCol = 1,
                 tableStyle = "TableStyleMedium9", headerStyle = st_header)

  setColWidths(wb, "5_Donnees_Nettoyees",
               cols = seq_along(df_traite),
               widths = pmax(nchar(names(df_traite)) + 4, 12))

  # ── 7. Sauvegarde ────────────────────────────────────────────────────────────
  chemin_excel <- file.path(dir_data, "traitement_outliers.xlsx")
  saveWorkbook(wb, chemin_excel, overwrite = TRUE)
  message("\n✔ Fichier Excel sauvegardé : ", chemin_excel)
  message("✔ Traitement terminé — méthode : ", methode,
          " — N avant : ", nrow(df), " — N après : ", nrow(df_traite))

  # ── 8. Retour invisible ──────────────────────────────────────────────────────
  invisible(list(
    df_original = df,
    df_traite   = df_traite,
    diagnostic  = diag_df,
    comparaison = comp_df,
    excel_path  = chemin_excel
  ))
}

