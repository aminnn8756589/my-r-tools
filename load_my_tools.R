# =====================================================
# load_my_tools.R — Chargeur expert ULTIME 2026
# =====================================================

load_my_tools <- function(fichiers = c("smart_impute.R", "expert_outlier_handler_2026.R", "expert_multilinear.R", "inspection.R")) {
  
  base_url <- "https://raw.githubusercontent.com/aminnn8756589/my-r-tools/main/R/"
  
  cat("🚀 Chargement des outils R EXPERT 2026 depuis GitHub...\n")
  cat("📍 Repo :", "aminnn8756589/my-r-tools\n")
  cat("📂 Fichiers :", paste(fichiers, collapse = ", "), "\n\n")
  
  for (f in fichiers) {
    url <- paste0(base_url, f)
    tryCatch({
      source(url, encoding = "UTF-8")
      cat("✅ Chargé →", f, "\n")
    }, error = function(e) {
      cat("❌ Erreur sur", f, "→ Vérifie le nom du fichier dans le dossier R/\n")
    })
  }
  
  cat("\n🌍 Tout est prêt  !\n")
  cat("📅", format(Sys.Date(), "%d %B %Y"), "\n")
  cat("============================================================\n\n")
  
  invisible(TRUE)
}

cat("✅ load_my_tools() prêt !\n")
