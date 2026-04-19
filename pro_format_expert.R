# =====================================================
# pro_format_expert.R — LA MEILLEURE VERSION 2026
# Formatage français pro + gt tables + auto-format
# =====================================================

# Packages pro
if (!requireNamespace("gt", quietly = TRUE)) {
  install.packages("gt", quiet = TRUE)
}
library(gt)

# Options globales expertes
options(scipen = 999)      # ← PLUS JAMAIS de notation e
options(digits = 4)        # précision par défaut

# Fonction FMT() ultra-rapide (pour inline et tout le reste)
fmt <- function(x, digits = 2) {
  if (is.null(x)) return("")
  sapply(x, function(val) {
    if (is.na(val) || is.nan(val)) return("NA")
    if (!is.numeric(val)) return(as.character(val))
    formatC(val, format = "f", digits = digits,
            big.mark = "\u202F",      # espace fine insécable
            decimal.mark = ",",
            zero.print = "0")
  })
}

# Formatage AUTOMATIQUE des nombres dans le texte Quarto/Rmd
knit_print.numeric <- function(x, ...) {
  knitr::knit_print(fmt(x, digits = 2), ...)
}
registerS3method("knit_print", "numeric", knit_print.numeric, envir = asNamespace("knitr"))

knit_print.integer <- function(x, ...) {
  knitr::knit_print(fmt(x, digits = 0), ...)
}
registerS3method("knit_print", "integer", knit_print.integer, envir = asNamespace("knitr"))

# Table PRO en 1 ligne (la plus belle possible)
pro_table <- function(data, 
                      title = NULL, 
                      subtitle = NULL,
                      decimals = 2,
                      style = 6) {   # 1 à 6 = différents styles élégants
  
  tbl <- data %>%
    gt() %>%
    fmt_number(columns = where(is.numeric), 
               decimals = decimals,
               locale = "fr") %>%          # ← format français parfait
    opt_stylize(style = style, color = "gray") %>%
    tab_options(table.font.size = px(15),
                heading.align = "left",
                table.width = pct(100))
  
  if (!is.null(title))    tbl <- tbl |> tab_header(title = title)
  if (!is.null(subtitle)) tbl <- tbl |> tab_header(subtitle = subtitle)
  
  tbl
}

# Message de bienvenue expert
cat("✅ pro_format_expert.R chargé → VERSION ULTIME 2026 !\n")
cat("   fmt(1234567.89) →", fmt(1234567.89), "\n")
cat("   pro_table() disponible pour des tables magnifiques\n")
cat("   Aucun 'e' + format français automatique partout\n\n")
