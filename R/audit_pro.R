audit_pro <- function(df, export_html = FALSE, file = "audit_report.html") {
  if (!is.data.frame(df)) stop("df must be a data.frame")
  message("Initialisation du diagnostic...")
  pkgs <- c("ggplot2","knitr","kableExtra","dplyr")
  invisible(lapply(pkgs, function(p) {
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }))
  
  n_obs <- nrow(df); n_vars <- ncol(df)
  na_global <- mean(is.na(df))*100
  dup <- sum(duplicated(df))
  num_vars <- names(df)[sapply(df, is.numeric)]
  cat("\n=== AUDIT DATASET PRO ===\n")
  cat("Observations:", n_obs, "\n")
  cat("Variables:", n_vars, "\n")
  cat("NA global (%):", round(na_global,2), "\n")
  cat("Doublons:", dup, "\n\n")
  
  meta <- data.frame(
    Variable = names(df),
    Classe = sapply(df, function(x) class(x)[1]),
    NA_pct = round(sapply(df, function(x) mean(is.na(x))*100),2),
    Uniques = sapply(df, function(x) length(unique(na.omit(x))))
  )
  print(knitr::kable(meta))
  
  if (length(num_vars) > 0) {
    for (v in num_vars) {
      x <- df[[v]]
      cat("\n---", v, "---\n")
      print(summary(x))
    }
  }
  
  invisible(list(summary = meta, n = n_obs, p = n_vars))
}

smart_impute_ultimate <- function(data,
                                  method = c("median","regression","knn"),
                                  k = 5,
                                  missing_threshold = 0.60,
                                  seed = 123,
                                  verbose = TRUE) {
  method <- match.arg(method)
  set.seed(seed)
  df <- data
  if (!is.data.frame(df)) stop("data must be a data.frame")
  
  # character -> factor
  for (j in seq_along(df)) if (is.character(df[[j]])) df[[j]] <- as.factor(df[[j]])
  
  report <- data.frame(
    Variable = names(df),
    Missing_N = sapply(df, function(x) sum(is.na(x))),
    Missing_Pct = round(sapply(df, function(x) mean(is.na(x))*100),2),
    Method = NA_character_,
    stringsAsFactors = FALSE
  )
  
  Mode <- function(x) {
    ux <- na.omit(unique(x)); ux[which.max(tabulate(match(x, ux)))]
  }
  
  num_cols <- names(df)[sapply(df, is.numeric)]
  
  for (nm in names(df)) {
    x <- df[[nm]]
    miss <- which(is.na(x))
    pct <- mean(is.na(x))
    if (pct > missing_threshold) {
      report$Method[report$Variable == nm] <- "Excluded"
      next
    }
    if (!length(miss)) {
      report$Method[report$Variable == nm] <- "No missing"
      next
    }
    
    if (is.numeric(x)) {
      if (method == "median") {
        x[miss] <- median(x, na.rm = TRUE)
        report$Method[report$Variable == nm] <- "Median"
      }
      if (method == "regression") {
        preds <- setdiff(num_cols, nm)
        if (length(preds) >= 1) {
          train <- df[!is.na(x), c(nm, preds), drop = FALSE]
          for (cc in preds) train[[cc]][is.na(train[[cc]])] <- median(train[[cc]], na.rm=TRUE)
          fit <- try(lm(as.formula(paste(nm, "~", paste(preds, collapse="+"))), data=train), silent=TRUE)
          if (!inherits(fit, "try-error")) {
            newd <- df[miss, preds, drop = FALSE]
            for (cc in preds) newd[[cc]][is.na(newd[[cc]])] <- median(train[[cc]], na.rm=TRUE)
            x[miss] <- predict(fit, newdata = newd)
            report$Method[report$Variable == nm] <- "Regression"
          } else {
            x[miss] <- median(x, na.rm = TRUE)
            report$Method[report$Variable == nm] <- "Fallback median"
          }
        }
      }
      if (method == "knn") {
        x[miss] <- median(x, na.rm = TRUE)
        report$Method[report$Variable == nm] <- "Pseudo-KNN median fallback"
      }
      df[[nm]] <- x
    } else {
      x[miss] <- Mode(x)
      df[[nm]] <- x
      report$Method[report$Variable == nm] <- "Mode"
    }
  }
  
  keep <- report$Missing_Pct <= (missing_threshold*100)
  df2 <- df[, keep, drop = FALSE]
  
  before_na <- sum(is.na(data))
  after_na <- sum(is.na(df2))
  score <- round(100 * (1 - after_na / max(before_na,1)),2)
  
  if (verbose) {
    cat("==============================
")
    cat("SMART IMPUTE ULTIMATE REPORT
")
    cat("Rows:", nrow(df2), " Cols:", ncol(df2), "
")
    cat("NA before:", before_na, " | NA after:", after_na, "
")
    cat("Improvement score:", score, "/100
")
    cat("==============================
")
    print(report, row.names = FALSE)
  }
  
  list(data = df2, report = report, score = score)
}

res <- smart_impute_ultimate(df)
df_clean <- res$data
