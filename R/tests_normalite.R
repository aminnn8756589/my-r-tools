# =============================================================================
# FONCTION : tests_normalite()
# Objectif : Tester la normalité de toutes les variables numériques d'un
#            dataframe (Shapiro-Wilk + Kolmogorov-Smirnov) et exporter les
#            résultats dans un fichier Excel coloré (une feuille par test).
# =============================================================================

# ---- Installation automatique des packages nécessaires ----
packages_requis <- c("openxlsx", "nortest")
for (pkg in packages_requis) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(openxlsx)
library(nortest) # pour le test K-S (lillie.test)


# =============================================================================
# FONCTION PRINCIPALE
# =============================================================================

tests_normalite <- function(df,
                            nom_fichier = "tests_normalite",
                            dossier_output = "output-data",
                            seuil = 0.05) {

  # POURQUOI CE BLOC ?
  # On crée le dossier output-data s'il n'existe pas encore,
  # pour éviter une erreur lors de la sauvegarde du fichier Excel.
  if (!dir.exists(dossier_output)) {
    dir.create(dossier_output, recursive = TRUE)
    message("Dossier créé : ", dossier_output)
  }

  # POURQUOI CE BLOC ?
  # On sélectionne uniquement les colonnes numériques du dataframe,
  # car les tests de normalité ne s'appliquent qu'à des variables quantitatives.
  vars_num <- names(df)[sapply(df, is.numeric)]

  if (length(vars_num) == 0) {
    stop("Aucune variable numérique trouvée dans le dataframe.")
  }

  message("Variables testées : ", paste(vars_num, collapse = ", "))

  # ---- Styles de couleurs ----
  # Vert  = normalité non rejetée (p >= seuil)
  # Rouge = normalité rejetée     (p <  seuil)
  # Gris  = en-tête
  style_header <- createStyle(
    fontName   = "Arial",
    fontSize   = 11,
    fontColour = "white",
    fgFill     = "#4472C4",
    halign     = "center",
    textDecoration = "bold",
    border     = "Bottom"
  )
  style_normal <- createStyle(
    fontName = "Arial",
    fontSize = 11,
    fgFill   = "#C6EFCE",   # vert clair
    fontColour = "#276221", # vert foncé
    halign   = "center"
  )
  style_non_normal <- createStyle(
    fontName = "Arial",
    fontSize = 11,
    fgFill   = "#FFC7CE",   # rouge clair
    fontColour = "#9C0006", # rouge foncé
    halign   = "center"
  )
  style_texte <- createStyle(
    fontName = "Arial",
    fontSize = 11,
    halign   = "left"
  )

  # =============================================================================
  # POURQUOI CE BLOC ?
  # On crée le classeur Excel et on y ajoute une feuille de synthèse
  # pour que l'utilisateur comprenne la logique de lecture du fichier.
  # =============================================================================
  wb <- createWorkbook()

  # ---- Feuille 0 : Légende / Synthèse ----
  addWorksheet(wb, "Légende")
  legende <- data.frame(
    Couleur      = c("Vert", "Rouge"),
    Signification = c(
      paste0("Normalité NON rejetée (p >= ", seuil, ") → distribution compatible avec la normalité"),
      paste0("Normalité REJETÉE     (p <  ", seuil, ") → distribution non normale")
    )
  )
  writeData(wb, "Légende", legende, startRow = 2, startCol = 1)
  addStyle(wb, "Légende", style_header,
           rows = 2, cols = 1:2, gridExpand = TRUE)
  addStyle(wb, "Légende", style_normal,
           rows = 3, cols = 1:2, gridExpand = TRUE)
  addStyle(wb, "Légende", style_non_normal,
           rows = 4, cols = 1:2, gridExpand = TRUE)
  writeData(wb, "Légende",
            paste0("Seuil de décision utilisé : α = ", seuil),
            startRow = 1, startCol = 1)
  setColWidths(wb, "Légende", cols = 1:2, widths = c(15, 70))


  # =============================================================================
  # POURQUOI CE BLOC ?
  # On exécute le test de Shapiro-Wilk sur chaque variable numérique.
  # Ce test est adapté pour des petits échantillons (n ≤ 5000).
  # H0 : la variable suit une loi normale.
  # =============================================================================

  # ---- Feuille 1 : Shapiro-Wilk ----
  addWorksheet(wb, "Shapiro-Wilk")

  resultats_sw <- data.frame(
    Variable   = character(),
    N          = integer(),
    Statistique_W = numeric(),
    p_valeur   = numeric(),
    Decision   = character(),
    stringsAsFactors = FALSE
  )

  for (var in vars_num) {
    x <- na.omit(df[[var]])
    n <- length(x)

    # Shapiro-Wilk exige 3 ≤ n ≤ 5000
    if (n < 3) {
      resultats_sw <- rbind(resultats_sw, data.frame(
        Variable = var, N = n,
        Statistique_W = NA, p_valeur = NA,
        Decision = "N insuffisant (< 3)"
      ))
      next
    }
    if (n > 5000) {
      resultats_sw <- rbind(resultats_sw, data.frame(
        Variable = var, N = n,
        Statistique_W = NA, p_valeur = NA,
        Decision = "N trop grand pour SW (> 5000), utiliser K-S"
      ))
      next
    }

    test <- shapiro.test(x)
    decision <- ifelse(test$p.value < seuil,
                       "Normalité REJETÉE",
                       "Normalité non rejetée")
    resultats_sw <- rbind(resultats_sw, data.frame(
      Variable      = var,
      N             = n,
      Statistique_W = round(test$statistic, 5),
      p_valeur      = round(test$p.value, 5),
      Decision      = decision
    ))
  }

  writeData(wb, "Shapiro-Wilk", resultats_sw, startRow = 1, startCol = 1)

  # En-tête
  addStyle(wb, "Shapiro-Wilk", style_header,
           rows = 1, cols = 1:5, gridExpand = TRUE)

  # Coloration ligne par ligne selon la décision
  for (i in seq_len(nrow(resultats_sw))) {
    ligne_excel <- i + 1
    if (grepl("REJETÉE", resultats_sw$Decision[i])) {
      addStyle(wb, "Shapiro-Wilk", style_non_normal,
               rows = ligne_excel, cols = 1:5, gridExpand = TRUE)
    } else if (grepl("non rejetée", resultats_sw$Decision[i])) {
      addStyle(wb, "Shapiro-Wilk", style_normal,
               rows = ligne_excel, cols = 1:5, gridExpand = TRUE)
    } else {
      addStyle(wb, "Shapiro-Wilk", style_texte,
               rows = ligne_excel, cols = 1:5, gridExpand = TRUE)
    }
  }
  setColWidths(wb, "Shapiro-Wilk", cols = 1:5,
               widths = c(25, 8, 18, 12, 30))


  # =============================================================================
  # POURQUOI CE BLOC ?
  # On exécute le test de Lilliefors (variante du K-S adaptée quand les
  # paramètres de la loi normale sont estimés sur l'échantillon, ce qui
  # est toujours le cas en pratique).
  # Ce test est plus adapté pour les grands échantillons (n > 50).
  # H0 : la variable suit une loi normale.
  # =============================================================================

  # ---- Feuille 2 : Kolmogorov-Smirnov (Lilliefors) ----
  addWorksheet(wb, "Kolmogorov-Smirnov")

  resultats_ks <- data.frame(
    Variable      = character(),
    N             = integer(),
    Statistique_D = numeric(),
    p_valeur      = numeric(),
    Decision      = character(),
    stringsAsFactors = FALSE
  )

  for (var in vars_num) {
    x <- na.omit(df[[var]])
    n <- length(x)

    if (n < 5) {
      resultats_ks <- rbind(resultats_ks, data.frame(
        Variable = var, N = n,
        Statistique_D = NA, p_valeur = NA,
        Decision = "N insuffisant (< 5)"
      ))
      next
    }

    test <- lillie.test(x)
    decision <- ifelse(test$p.value < seuil,
                       "Normalité REJETÉE",
                       "Normalité non rejetée")
    resultats_ks <- rbind(resultats_ks, data.frame(
      Variable      = var,
      N             = n,
      Statistique_D = round(test$statistic, 5),
      p_valeur      = round(test$p.value, 5),
      Decision      = decision
    ))
  }

  writeData(wb, "Kolmogorov-Smirnov", resultats_ks, startRow = 1, startCol = 1)

  addStyle(wb, "Kolmogorov-Smirnov", style_header,
           rows = 1, cols = 1:5, gridExpand = TRUE)

  for (i in seq_len(nrow(resultats_ks))) {
    ligne_excel <- i + 1
    if (grepl("REJETÉE", resultats_ks$Decision[i])) {
      addStyle(wb, "Kolmogorov-Smirnov", style_non_normal,
               rows = ligne_excel, cols = 1:5, gridExpand = TRUE)
    } else if (grepl("non rejetée", resultats_ks$Decision[i])) {
      addStyle(wb, "Kolmogorov-Smirnov", style_normal,
               rows = ligne_excel, cols = 1:5, gridExpand = TRUE)
    } else {
      addStyle(wb, "Kolmogorov-Smirnov", style_texte,
               rows = ligne_excel, cols = 1:5, gridExpand = TRUE)
    }
  }
  setColWidths(wb, "Kolmogorov-Smirnov", cols = 1:5,
               widths = c(25, 8, 18, 12, 30))


  # =============================================================================
  # POURQUOI CE BLOC ?
  # On crée une feuille de synthèse comparative qui regroupe les conclusions
  # des deux tests côte à côte, pour faciliter la prise de décision finale.
  # =============================================================================

  # ---- Feuille 3 : Synthèse comparative ----
  addWorksheet(wb, "Synthèse")

  synthese <- data.frame(
    Variable        = resultats_sw$Variable,
    N               = resultats_sw$N,
    Decision_SW     = resultats_sw$Decision,
    p_valeur_SW     = resultats_sw$p_valeur,
    Decision_KS     = resultats_ks$Decision[match(resultats_sw$Variable,
                                                   resultats_ks$Variable)],
    p_valeur_KS     = resultats_ks$p_valeur[match(resultats_sw$Variable,
                                                   resultats_ks$Variable)],
    stringsAsFactors = FALSE
  )

  # Conclusion globale : normale seulement si les DEUX tests ne rejettent pas
  synthese$Conclusion_globale <- ifelse(
    grepl("REJETÉE", synthese$Decision_SW) | grepl("REJETÉE", synthese$Decision_KS),
    "Non normale",
    "Probablement normale"
  )

  writeData(wb, "Synthèse", synthese, startRow = 1, startCol = 1)

  addStyle(wb, "Synthèse", style_header,
           rows = 1, cols = 1:7, gridExpand = TRUE)

  for (i in seq_len(nrow(synthese))) {
    ligne_excel <- i + 1
    if (synthese$Conclusion_globale[i] == "Non normale") {
      addStyle(wb, "Synthèse", style_non_normal,
               rows = ligne_excel, cols = 1:7, gridExpand = TRUE)
    } else {
      addStyle(wb, "Synthèse", style_normal,
               rows = ligne_excel, cols = 1:7, gridExpand = TRUE)
    }
  }
  setColWidths(wb, "Synthèse", cols = 1:7,
               widths = c(25, 8, 25, 14, 25, 14, 22))


  # =============================================================================
  # POURQUOI CE BLOC ?
  # On sauvegarde le fichier Excel dans le dossier output-data,
  # avec un nom horodaté pour éviter d'écraser les fichiers précédents.
  # =============================================================================
  horodatage  <- format(Sys.time(), "%Y%m%d_%H%M%S")
  chemin_final <- file.path(dossier_output,
                            paste0(nom_fichier, "_", horodatage, ".xlsx"))

  saveWorkbook(wb, chemin_final, overwrite = TRUE)
  message("Fichier exporté avec succès : ", chemin_final)

  # On retourne aussi les résultats sous forme de liste pour usage en R
  invisible(list(
    shapiro_wilk         = resultats_sw,
    kolmogorov_smirnov   = resultats_ks,
    synthese             = synthese,
    chemin_fichier       = chemin_final
  ))
}


# =============================================================================
# EXEMPLE D'UTILISATION
# =============================================================================

# Remplace "mon_dataframe" par le nom de ton dataframe chargé en R :
#
#   resultats <- tests_normalite(mon_dataframe)
#
# Options disponibles :
#   nom_fichier    : nom du fichier Excel (sans extension)
#   dossier_output : chemin du dossier de sortie
#   seuil          : seuil de significativité (défaut = 0.05)
#
# Exemple avec options :
#   resultats <- tests_normalite(
#     df             = mon_dataframe,
#     nom_fichier    = "normalite_criteres",
#     dossier_output = "output-data",
#     seuil          = 0.05
#   )
#
# Accéder aux résultats en R :
#   resultats$synthese
#   resultats$shapiro_wilk
#   resultats$kolmogorov_smirnov
