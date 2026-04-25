audit <- function(df) { pkgs <- c("ggplot2","knitr","kableExtra","gridExtra","RColorBrewer","dplyr")
invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly=TRUE)) install.packages(p, quiet=TRUE)
  suppressPackageStartupMessages(library(p, character.only=TRUE))
}))

stopifnot(exists("df") && is.data.frame(df))

# ══════════════════════════════════════════════════════════════════════════════
#  FONCTIONS UTILITAIRES
# ══════════════════════════════════════════════════════════════════════════════

detect_type <- function(x) {
  if (is.ordered(x))                              return("Ordinale")
  if (inherits(x, c("Date","POSIXct","POSIXlt"))) return("Date/Temps")
  if (is.logical(x))                              return("Binaire")
  if (is.factor(x) || is.character(x)) {
    nu <- length(unique(na.omit(x)))
    if (nu <= 2)  return("Binaire")
    if (nu <= 20) return("Categorielle")
    return("Texte libre")
  }
  if (is.numeric(x) || is.integer(x)) {
    nu <- length(unique(na.omit(x)))
    if (nu <= 10) return("Discrete")
    return("Continue")
  }
  return("Autre")
}

detect_outliers_df <- function(data, vars_num) {
  res <- lapply(vars_num, function(v) {
    xn <- suppressWarnings(as.numeric(data[[v]]))
    xc <- xn[!is.na(xn)]
    if (length(xc) < 4) return(NULL)
    Q1 <- quantile(xc, .25); Q3 <- quantile(xc, .75); IQR_v <- Q3 - Q1
    lo1 <- Q1 - 1.5*IQR_v; hi1 <- Q3 + 1.5*IQR_v
    lo3 <- Q1 - 3.0*IQR_v; hi3 <- Q3 + 3.0*IQR_v
    idx <- which(xn < lo1 | xn > hi1)
    if (!length(idx)) return(NULL)
    data.frame(
      Variable  = v,
      Index     = idx,
      Valeur    = round(xn[idx], 4),
      Severite  = ifelse(xn[idx] < lo3 | xn[idx] > hi3,
                         "Extreme (>3xIQR)", "Modere (1.5xIQR)"),
      Borne_inf = round(lo1, 4),
      Borne_sup = round(hi1, 4),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, res)
}

h1 <- function(t) cat(sprintf("\n\n# %s\n\n", t))
h2 <- function(t) cat(sprintf("\n\n## %s\n\n", t))
h3 <- function(t) cat(sprintf("\n\n### %s\n\n", t))
hr <- function()  cat("\n\n---\n\n")

pal_type <- c(
  Continue     = "#378ADD",
  Discrete     = "#5DCAA5",
  Categorielle = "#D4537E",
  Ordinale     = "#EF9F27",
  Binaire      = "#7F77DD",
  `Date/Temps` = "#1D9E75",
  `Texte libre`= "#888780",
  Autre        = "#B4B2A9"
)

th <- theme_minimal(base_size=12) +
  theme(plot.title=element_text(size=13, face="bold", color="#2C2C2A"),
        plot.subtitle=element_text(size=10, color="#5F5E5A"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="#D3D1C7", linewidth=.3),
        axis.text=element_text(size=9, color="#444441"))

# ══════════════════════════════════════════════════════════════════════════════
#  PREPARATION
# ══════════════════════════════════════════════════════════════════════════════
n_obs     <- nrow(df)
n_vars    <- ncol(df)
types     <- setNames(sapply(df, detect_type), names(df))
na_glob   <- round(mean(is.na(df)) * 100, 1)
n_dup     <- sum(duplicated(df))
n_dup_all <- sum(duplicated(df) | duplicated(df, fromLast=TRUE))

vars_num  <- names(types)[types %in% c("Continue","Discrete")]
vars_cat  <- names(types)[types %in% c("Categorielle","Binaire","Ordinale","Texte libre")]
vars_date <- names(types)[types == "Date/Temps"]

# ══════════════════════════════════════════════════════════════════════════════
#  1. VUE D'ENSEMBLE
# ══════════════════════════════════════════════════════════════════════════════
h1("1. Vue d'ensemble")

cat(sprintf(
  "> **%d observations** &nbsp;|&nbsp; **%d variables** &nbsp;|&nbsp; **%.1f%% de NA globaux** &nbsp;|&nbsp; **%d doublon(s)**\n\n",
  n_obs, n_vars, na_glob, n_dup))

# Tableau inventaire
pct_na_vals <- round(sapply(df, function(x) mean(is.na(x))*100), 1)
couleur_na  <- ifelse(pct_na_vals == 0, "#3B6D11",
               ifelse(pct_na_vals <  5, "#BA7517",
               ifelse(pct_na_vals < 30, "#D85A30", "#A32D2D")))

meta <- data.frame(
  Num       = seq_len(n_vars),
  Variable  = names(df),
  Classe_R  = sapply(df, function(x) paste(class(x), collapse="/")),
  Type_stat = as.character(types),
  N_valides = as.integer(sapply(df, function(x) sum(!is.na(x)))),
  N_NA      = as.integer(sapply(df, function(x) sum(is.na(x)))),
  Pct_NA    = pct_na_vals,
  N_uniques = as.integer(sapply(df, function(x) length(unique(na.omit(x))))),
  stringsAsFactors = FALSE
)

# colorisation sans pipe, en base R
meta$Pct_NA_fmt <- vapply(seq_len(nrow(meta)), function(i)
  cell_spec(meta$Pct_NA[i], "html",
            color = couleur_na[i],
            bold  = meta$Pct_NA[i] > 20),
  character(1))

meta$Type_fmt <- vapply(meta$Type_stat, function(t) {
  bg <- if (t %in% names(pal_type)) pal_type[t] else "#888780"
  cell_spec(t, "html", color="white", background=bg)
}, character(1))

meta_show <- data.frame(
  `#`         = meta$Num,
  Variable    = meta$Variable,
  `Classe R`  = meta$Classe_R,
  `Type stat` = meta$Type_fmt,
  `N valides` = meta$N_valides,
  `N NA`      = meta$N_NA,
  `% NA`      = meta$Pct_NA_fmt,
  `N uniques` = meta$N_uniques,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

print(
  kbl(meta_show, escape=FALSE) |>
    kable_styling(bootstrap_options=c("striped","hover","condensed","responsive"),
                  full_width=FALSE) |>
    column_spec(2, bold=TRUE)
)

# Barplot types
td <- as.data.frame(table(types), stringsAsFactors=FALSE)
names(td) <- c("Type","N")
td$Pct    <- round(td$N / sum(td$N) * 100, 1)
td$Couleur<- ifelse(td$Type %in% names(pal_type), pal_type[td$Type], "#888780")

p_types <- ggplot(td, aes(x=reorder(Type, N), y=N, fill=Type)) +
  geom_col(width=.7, show.legend=FALSE) +
  geom_text(aes(label=paste0(N," (",Pct,"%)")), hjust=-.1, size=3.5) +
  scale_fill_manual(values=setNames(td$Couleur, td$Type)) +
  coord_flip() + expand_limits(y=max(td$N)*1.35) +
  labs(title="Composition par type de variable", x=NULL, y="Nb variables") + th
print(p_types)

hr()

# ══════════════════════════════════════════════════════════════════════════════
#  2. VALEURS MANQUANTES
# ══════════════════════════════════════════════════════════════════════════════
h1("2. Valeurs manquantes")

# Carte NA
n_s   <- min(n_obs, 400)
idx_s <- sort(sample(seq_len(n_obs), n_s))
df_s  <- df[idx_s, , drop=FALSE]

na_mat  <- is.na(df_s)
na_long <- data.frame(
  obs      = rep(seq_len(n_s), times=n_vars),
  variable = rep(names(df), each=n_s),
  manquant = as.vector(na_mat),
  stringsAsFactors = FALSE
)
na_long$variable <- factor(na_long$variable, levels=names(df))

p_carte <- ggplot(na_long, aes(x=obs, y=variable, fill=manquant)) +
  geom_tile() +
  scale_fill_manual(values=c("FALSE"="#EAF3DE","TRUE"="#E24B4A"),
                    labels=c("Present","Manquant"), name=NULL) +
  labs(
    title    = "Carte des valeurs manquantes",
    subtitle = if (n_obs > 400)
                 sprintf("Echantillon aleatoire : %d obs. sur %d", n_s, n_obs)
               else "",
    x="Observations", y=NULL) +
  th + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
             legend.position="top")
print(p_carte)

# Barplot NA
na_df <- data.frame(
  Variable = names(df),
  NA_pct   = pct_na_vals,
  stringsAsFactors = FALSE
)
na_df <- na_df[order(-na_df$NA_pct), ]
na_df$Couleur <- ifelse(na_df$NA_pct == 0,  "#3B6D11",
                 ifelse(na_df$NA_pct <  5,  "#639922",
                 ifelse(na_df$NA_pct < 15,  "#BA7517",
                 ifelse(na_df$NA_pct < 30,  "#D85A30", "#A32D2D"))))

p_na <- ggplot(na_df, aes(x=reorder(Variable, NA_pct), y=NA_pct, fill=Couleur)) +
  geom_col(width=.7, show.legend=FALSE) +
  geom_text(aes(label=paste0(NA_pct,"%")), hjust=-.1, size=3.5) +
  geom_hline(yintercept=c(5,15,30), linetype="dashed",
             color=c("#639922","#BA7517","#A32D2D"), linewidth=.5) +
  scale_fill_identity() +
  coord_flip() + expand_limits(y=max(na_df$NA_pct, 6)*1.25) +
  labs(title="Taux de NA par variable", subtitle="Seuils : 5% | 15% | 30%",
       x=NULL, y="% NA") + th
print(p_na)

# Tableau statut NA — tout en base R, zéro pipe
na_df$Statut <- ifelse(na_df$NA_pct == 0,  "Complet",
                ifelse(na_df$NA_pct <  5,  "Acceptable",
                ifelse(na_df$NA_pct < 15,  "A surveiller",
                ifelse(na_df$NA_pct < 30,  "Problematique", "Critique"))))

na_show <- data.frame(
  Variable = na_df$Variable,
  Pct_NA   = na_df$NA_pct,
  Statut   = na_df$Statut,
  stringsAsFactors = FALSE
)
names(na_show)[2] <- "% NA"

idx_crit  <- which(na_show$Statut == "Critique")
idx_prob  <- which(na_show$Statut == "Problematique")
idx_surv  <- which(na_show$Statut == "A surveiller")
idx_ok    <- which(na_show$Statut == "Complet")

tbl_na <- kbl(na_show, row.names=FALSE) |>
  kable_styling(bootstrap_options=c("striped","hover","condensed"), full_width=FALSE)
if (length(idx_crit)) tbl_na <- row_spec(tbl_na, idx_crit, bold=TRUE,  color="#A32D2D")
if (length(idx_prob)) tbl_na <- row_spec(tbl_na, idx_prob, color="#D85A30")
if (length(idx_surv)) tbl_na <- row_spec(tbl_na, idx_surv, color="#BA7517")
if (length(idx_ok))   tbl_na <- row_spec(tbl_na, idx_ok,   color="#3B6D11")
print(tbl_na)

hr()

# ══════════════════════════════════════════════════════════════════════════════
#  3. DOUBLONS
# ══════════════════════════════════════════════════════════════════════════════
h1("3. Doublons")

if (n_dup == 0) {
  cat("**Aucun doublon detecte.**\n\n")
} else {
  cat(sprintf(
    "**%d ligne(s) dupliquee(s)** — **%d lignes** au total concernees (originales + copies).\n\n",
    n_dup, n_dup_all))

  dup_idx <- which(duplicated(df) | duplicated(df, fromLast=TRUE))
  df_dup  <- df[dup_idx, , drop=FALSE]
  # trier pour grouper les identiques ensemble
  ord <- do.call(order, lapply(names(df_dup), function(v) df_dup[[v]]))
  df_dup <- df_dup[ord, , drop=FALSE]

  cat(sprintf("Apercu (max 30 lignes) :\n\n"))
  print(
    kbl(head(df_dup, 30), row.names=TRUE) |>
      kable_styling(bootstrap_options=c("striped","hover","condensed"),
                    full_width=FALSE) |>
      scroll_box(width="100%", height="350px")
  )
}

hr()

# ══════════════════════════════════════════════════════════════════════════════
#  4. VARIABLES NUMERIQUES — BOXPLOTS & OUTLIERS
# ══════════════════════════════════════════════════════════════════════════════
if (length(vars_num) > 0) {

  h1("4. Variables numeriques")

  h2("Boxplots")
  pb <- lapply(vars_num, function(v) {
    xn <- suppressWarnings(as.numeric(df[[v]]))
    d  <- data.frame(x = xn[!is.na(xn)])
    Q1 <- quantile(d$x,.25); Q3 <- quantile(d$x,.75); IQR_v <- Q3-Q1
    d$Sev <- ifelse(d$x < Q1-3*IQR_v | d$x > Q3+3*IQR_v, "Extreme",
              ifelse(d$x < Q1-1.5*IQR_v | d$x > Q3+1.5*IQR_v, "Modere", "Normal"))
    ggplot(d, aes(x="", y=x)) +
      geom_boxplot(fill="#E6F1FB", color="#185FA5", outlier.shape=NA,
                   width=.4, linewidth=.6) +
      geom_jitter(data=d[d$Sev!="Normal",,drop=FALSE],
                  aes(color=Sev), width=.15, size=1.8, alpha=.85) +
      scale_color_manual(values=c(Modere="#EF9F27", Extreme="#E24B4A"), name=NULL) +
      labs(title=v, x=NULL, y=NULL) + th +
      theme(axis.text.x=element_blank(), legend.position="top")
  })
  do.call(gridExtra::grid.arrange, c(pb, ncol=min(2, length(pb))))

  h2("Liste des valeurs extremes (IQR)")
  all_out <- detect_outliers_df(df, vars_num)

  if (is.null(all_out) || nrow(all_out) == 0) {
    cat("Aucune valeur extreme detectee.\n\n")
  } else {
    cat(sprintf("**%d valeur(s) extreme(s)** detectee(s) :\n\n", nrow(all_out)))
    # colorisation severite — base R, sans risque NA
    sev_col <- ifelse(grepl("Extreme", all_out$Severite), "#A32D2D", "#BA7517")
    sev_bold <- grepl("Extreme", all_out$Severite)
    all_out$Severite <- vapply(seq_len(nrow(all_out)), function(i)
      cell_spec(all_out$Severite[i], "html",
                color = sev_col[i],
                bold  = sev_bold[i]),
      character(1))
    print(
      kbl(all_out, escape=FALSE, row.names=FALSE) |>
        kable_styling(bootstrap_options=c("striped","hover","condensed"),
                      full_width=FALSE) |>
        scroll_box(height="400px")
    )
  }

  h2("Resume par variable numerique")
  resume_num <- data.frame(
    Variable = vars_num,
    N_valides= as.integer(sapply(vars_num, function(v) sum(!is.na(df[[v]])))),
    N_NA     = as.integer(sapply(vars_num, function(v) sum(is.na(df[[v]])))),
    Pct_NA   = round(sapply(vars_num, function(v) mean(is.na(df[[v]]))*100), 1),
    N_uniques= as.integer(sapply(vars_num, function(v) length(unique(na.omit(df[[v]]))))),
    N_out_mod= as.integer(sapply(vars_num, function(v) {
      o <- detect_outliers_df(df[,v,drop=FALSE], v)
      if (is.null(o)) 0L else sum(o$Severite == "Modere (1.5xIQR)") })),
    N_out_ext= as.integer(sapply(vars_num, function(v) {
      o <- detect_outliers_df(df[,v,drop=FALSE], v)
      if (is.null(o)) 0L else sum(grepl("Extreme", o$Severite)) })),
    stringsAsFactors = FALSE
  )

  tbl_num <- kbl(resume_num, row.names=FALSE,
                 col.names=c("Variable","N valides","N NA","% NA",
                             "N uniques","Outliers moderes","Outliers extremes")) |>
    kable_styling(bootstrap_options=c("striped","hover","condensed"), full_width=FALSE)
  idx_ext_rouge <- which(resume_num$N_out_ext > 0)
  if (length(idx_ext_rouge))
    tbl_num <- column_spec(tbl_num, 7, bold=TRUE, color="#A32D2D")
  print(tbl_num)

  hr()
}

# ══════════════════════════════════════════════════════════════════════════════
#  5. VARIABLES QUALITATIVES
# ══════════════════════════════════════════════════════════════════════════════
if (length(vars_cat) > 0) {

  h1("5. Variables qualitatives")

  h2("Distributions des modalites")
  pals <- list(RColorBrewer::brewer.pal(8,"Set2"),
               RColorBrewer::brewer.pal(8,"Pastel1"),
               RColorBrewer::brewer.pal(8,"Dark2"))

  pq <- lapply(seq_along(vars_cat), function(i) {
    v  <- vars_cat[i]
    xc <- na.omit(df[[v]])
    fd <- as.data.frame(sort(table(xc), decreasing=TRUE), stringsAsFactors=FALSE)
    names(fd) <- c("Modalite","N")
    fd <- head(fd, 15)
    fd$Pct <- round(fd$N / sum(fd$N) * 100, 1)
    pal <- pals[[(i-1)%%3+1]]
    ggplot(fd, aes(x=reorder(Modalite, N), y=N, fill=Modalite)) +
      geom_col(width=.7, show.legend=FALSE) +
      geom_text(aes(label=paste0(Pct,"%")), hjust=-.1, size=3) +
      scale_fill_manual(values=rep(pal, length.out=nrow(fd))) +
      coord_flip() + expand_limits(y=max(fd$N)*1.25) +
      labs(title=v, x=NULL, y="Effectif") + th +
      theme(axis.text.y=element_text(size=9))
  })
  do.call(gridExtra::grid.arrange, c(pq, ncol=min(2, length(pq))))

  h2("Resume par variable qualitative")
  resume_cat <- data.frame(
    Variable    = vars_cat,
    Type        = as.character(types[vars_cat]),
    N_valides   = as.integer(sapply(vars_cat, function(v) sum(!is.na(df[[v]])))),
    N_NA        = as.integer(sapply(vars_cat, function(v) sum(is.na(df[[v]])))),
    Pct_NA      = round(sapply(vars_cat, function(v) mean(is.na(df[[v]]))*100), 1),
    N_modalites = as.integer(sapply(vars_cat, function(v) length(unique(na.omit(df[[v]]))))),
    Mod_dom     = sapply(vars_cat, function(v) {
      fr <- sort(table(na.omit(df[[v]])), decreasing=TRUE); as.character(names(fr)[1]) }),
    Freq_dom    = round(sapply(vars_cat, function(v) {
      xc <- na.omit(df[[v]]); fr <- sort(table(xc), decreasing=TRUE)
      as.numeric(fr[1])/length(xc)*100 }), 1),
    stringsAsFactors = FALSE
  )

  print(
    kbl(resume_cat, row.names=FALSE,
        col.names=c("Variable","Type","N valides","N NA","% NA",
                    "N modalites","Modalite dominante","% dominant")) |>
      kable_styling(bootstrap_options=c("striped","hover","condensed"),
                    full_width=FALSE) |>
      scroll_box(width="100%")
  )
  hr()
}

# ══════════════════════════════════════════════════════════════════════════════
#  6. VARIABLES TEMPORELLES
# ══════════════════════════════════════════════════════════════════════════════
if (length(vars_date) > 0) {

  h1("6. Variables temporelles")

  for (v in vars_date) {
    h3(v)
    xd    <- as.Date(df[[v]])
    xd_ok <- xd[!is.na(xd)]
    cat(sprintf(
      "- N valides : **%d** | N NA : **%d** | De **%s** a **%s** (%d jours)\n\n",
      length(xd_ok), sum(is.na(xd)),
      format(min(xd_ok),"%d/%m/%Y"), format(max(xd_ok),"%d/%m/%Y"),
      as.integer(diff(range(xd_ok)))))
    p_date <- ggplot(data.frame(date=xd_ok), aes(x=date)) +
      geom_histogram(bins=40, fill="#9FE1CB", color="white") +
      labs(title=paste("Distribution :", v), x=NULL, y="Frequence") + th
    print(p_date)
  }
  hr()
}

# ══════════════════════════════════════════════════════════════════════════════
#  7. ALERTES
# ══════════════════════════════════════════════════════════════════════════════
h1("7. Alertes et recommandations")

alertes <- character(0)

v_na_crit <- names(df)[sapply(df, function(x) mean(is.na(x))) > .30]
if (length(v_na_crit))
  alertes <- c(alertes, paste0("**NA > 30%** sur : ", paste(v_na_crit, collapse=", "),
                               ". Imputation ou suppression a envisager."))

if (n_dup > 0)
  alertes <- c(alertes, paste0("**", n_dup, " doublon(s)** detecte(s). Verifier la collecte."))

v_const <- names(df)[sapply(df, function(x) length(unique(na.omit(x)))) == 1]
if (length(v_const))
  alertes <- c(alertes, paste0("**Variable(s) constante(s)** : ",
                               paste(v_const, collapse=", "), ". A supprimer."))

if (length(vars_num) > 0) {
  o_all <- detect_outliers_df(df, vars_num)
  if (!is.null(o_all) && nrow(o_all) > 0) {
    nb_ext <- sum(grepl("Extreme", o_all$Severite))
    if (nb_ext > 0)
      alertes <- c(alertes, paste0("**", nb_ext, " valeur(s) extreme(s)** (>3xIQR). Verifier les saisies."))
  }
}

v_texte <- names(types)[types == "Texte libre"]
if (length(v_texte))
  alertes <- c(alertes, paste0("**Texte libre** : ",
                               paste(v_texte, collapse=", "), ". Encoder avant modelisation."))

v_quasi <- names(df)[sapply(df, function(x) {
  xc <- na.omit(x); length(xc) > 0 && max(table(xc))/length(xc) > .95
})]
v_quasi <- setdiff(v_quasi, v_const)
if (length(v_quasi))
  alertes <- c(alertes, paste0("**Quasi-constante(s)** (>95% meme valeur) : ",
                               paste(v_quasi, collapse=", "), "."))

if (!length(alertes)) {
  cat("> Aucune alerte majeure. Le jeu de donnees semble de bonne qualite.\n\n")
} else {
  for (i in seq_along(alertes)) cat(sprintf("%d. %s\n\n", i, alertes[i]))
}

hr()

# ══════════════════════════════════════════════════════════════════════════════
#  8. RESUME EXECUTIF
# ══════════════════════════════════════════════════════════════════════════════
h1("8. Resume executif")

n_out_ext <- 0L
if (length(vars_num) > 0) {
  o_final <- detect_outliers_df(df, vars_num)
  if (!is.null(o_final)) n_out_ext <- sum(grepl("Extreme", o_final$Severite))
}

cat(sprintf(
"| Indicateur | Valeur |
|:---|---:|
| Observations | **%d** |
| Variables | **%d** |
| Variables numeriques | **%d** |
| Variables qualitatives | **%d** |
| Variables temporelles | **%d** |
| Taux NA global | **%.1f%%** |
| Variables sans NA | **%d** |
| Doublons | **%d** |
| Valeurs extremes (>3xIQR) | **%d** |
| Alertes | **%d** |

> Rapport genere le %s
",
n_obs, n_vars,
length(vars_num), length(vars_cat), length(vars_date),
na_glob,
sum(sapply(df, function(x) sum(is.na(x))) == 0),
n_dup, n_out_ext, length(alertes),
format(Sys.Date(), "%d/%m/%Y")))
}
