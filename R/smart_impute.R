# ==============================================================================
# FONCTION : smart_impute()
# Objet :
# Imputation robuste, reproductible et polyvalente des valeurs manquantes
# Compatible : variables numériques, facteurs, caractères, binaires
# Sans dépendances problématiques
# Méthode :
#   - Numérique : médiane robuste + option régression + bruit contrôlé
#   - Catégorielle : modalité dominante
#   - Colonnes très manquantes : exclusion optionnelle
#   - Rapport complet
# Auteur : version académique robuste
# ==============================================================================

smart_impute <- function(data,
                         method_num = c("median", "regression"),
                         missing_threshold = 0.60,
                         add_noise = TRUE,
                         noise_sd = 0.05,
                         seed = 123,
                         verbose = TRUE) {
  
  method_num <- match.arg(method_num)
  set.seed(seed)
  
  df <- data
  n <- nrow(df)
  p <- ncol(df)
  
  report <- data.frame(
    Variable = names(df),
    Type = NA,
    Missing_N = NA,
    Missing_Pct = NA,
    Method = NA,
    stringsAsFactors = FALSE
  )
  
  # --------------------------------------------------------------------------
  # Conversion caractères -> facteur
  # --------------------------------------------------------------------------
  for(j in seq_along(df)) {
    if(is.character(df[[j]])) df[[j]] <- as.factor(df[[j]])
  }
  
  # --------------------------------------------------------------------------
  # Fonction mode statistique
  # --------------------------------------------------------------------------
  Mode <- function(x) {
    ux <- na.omit(unique(x))
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # --------------------------------------------------------------------------
  # Boucle principale
  # --------------------------------------------------------------------------
  for(j in seq_along(df)) {
    
    x <- df[[j]]
    miss_id <- which(is.na(x))
    miss_n <- length(miss_id)
    miss_pct <- miss_n / n
    
    report$Missing_N[j] <- miss_n
    report$Missing_Pct[j] <- round(100 * miss_pct, 2)
    
    # Si trop manquante
    if(miss_pct > missing_threshold) {
      report$Method[j] <- "Excluded (too many missing)"
      report$Type[j] <- class(x)[1]
      next
    }
    
    # ----------------------------------------------------------------------
    # NUMERIQUE
    # ----------------------------------------------------------------------
    if(is.numeric(x) || is.integer(x)) {
      
      report$Type[j] <- "numeric"
      
      if(miss_n > 0) {
        
        # ----- Méthode médiane robuste -----
        if(method_num == "median") {
          
          med <- median(x, na.rm = TRUE)
          x[miss_id] <- med
          
          # léger bruit pour éviter variance écrasée
          if(add_noise) {
            sdx <- sd(x, na.rm = TRUE)
            if(!is.na(sdx) && sdx > 0) {
              x[miss_id] <- x[miss_id] + rnorm(miss_n, 0, noise_sd * sdx)
            }
          }
          
          report$Method[j] <- "Median robust"
        }
        
        # ----- Régression multivariée simple -----
        if(method_num == "regression") {
          
          obs <- which(!is.na(x))
          
          # prédicteurs numériques disponibles
          preds <- df[, sapply(df, is.numeric), drop = FALSE]
          preds <- preds[, colnames(preds) != names(df)[j], drop = FALSE]
          
          if(ncol(preds) >= 1 && length(obs) > 10) {
            
            temp <- data.frame(y = x, preds)
            temp_obs <- temp[obs, , drop = FALSE]
            
            # Remplir provisoirement autres NA par médiane
            for(k in seq_along(temp_obs)) {
              if(is.numeric(temp_obs[[k]])) {
                temp_obs[[k]][is.na(temp_obs[[k]])] <- median(temp_obs[[k]], na.rm=TRUE)
              }
            }
            
            fit <- try(lm(y ~ ., data = temp_obs), silent = TRUE)
            
            if(!inherits(fit, "try-error")) {
              
              newdat <- temp[miss_id, -1, drop = FALSE]
              
              for(k in seq_along(newdat)) {
                if(is.numeric(newdat[[k]])) {
                  newdat[[k]][is.na(newdat[[k]])] <- median(temp_obs[[k+1]], na.rm=TRUE)
                }
              }
              
              pred <- predict(fit, newdata = newdat)
              x[miss_id] <- pred
              report$Method[j] <- "Regression imputation"
              
            } else {
              x[miss_id] <- median(x, na.rm = TRUE)
              report$Method[j] <- "Fallback median"
            }
            
          } else {
            x[miss_id] <- median(x, na.rm = TRUE)
            report$Method[j] <- "Fallback median"
          }
        }
      } else {
        report$Method[j] <- "No missing"
      }
      
      df[[j]] <- x
    }
    
    # ----------------------------------------------------------------------
    # FACTEUR / CATEGORIEL
    # ----------------------------------------------------------------------
    else if(is.factor(x) || is.logical(x)) {
      
      report$Type[j] <- "categorical"
      
      if(miss_n > 0) {
        m <- Mode(x)
        x[miss_id] <- m
        report$Method[j] <- "Mode"
      } else {
        report$Method[j] <- "No missing"
      }
      
      df[[j]] <- x
    }
    
    else {
      report$Type[j] <- class(x)[1]
      report$Method[j] <- "Untreated"
    }
  }
  
  # --------------------------------------------------------------------------
  # Retirer colonnes trop manquantes
  # --------------------------------------------------------------------------
  keep <- report$Missing_Pct <= 100 * missing_threshold
  df_final <- df[, keep, drop = FALSE]
  report_final <- report
  
  if(verbose) {
    cat("=====================================================\n")
    cat("SMART IMPUTATION REPORT\n")
    cat("Rows :", nrow(df_final), "\n")
    cat("Cols :", ncol(df_final), "\n")
    cat("Seed :", seed, "\n")
    cat("=====================================================\n")
    print(report_final, row.names = FALSE)
  }
  
  return(list(
    data = df_final,
    report = report_final
  ))
}
# df_imputed <- smart_impute(df)$data
