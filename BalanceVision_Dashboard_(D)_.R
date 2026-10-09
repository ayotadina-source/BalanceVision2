# ==============================================================================
# Balance Vision — R Shiny Analytics Dashboard
# ------------------------------------------------------------------------------
# Reads the live BalanceVision Google Sheet (public) and provides:
#   0. Pelvic Stability by Sex  (default tab) every person drawn as a female/male
#                           symbol: pelvic variability + mean pelvic tilt, with medians & CIs.
#      Sex Differences (Intervals)  effect sizes (Cliff's delta) + 95% CIs, women vs men.
#   1. Scatter Explorer   – X/Y continuous, colour by category, size + opacity,
#                           brush-to-table, hover tooltip, selection statistics,
#                           optional lm/loess fit and age-reference overlay.
#   2. Distributions      – histogram/density + normality on any continuous field.
#   3. Group Comparisons  – box/violin by category + per-group summary + ANOVA/KW.
#   4. Correlations       – Pearson/Spearman heatmap + matrix table.
#   5. Left–Right Asymmetry – paired best-duration per participant + paired test.
#   6. Data Explorer      – full filterable live table + CSV download.
#
# RUN:  install.packages(c("shiny","ggplot2","shinythemes","dplyr","DT","ragg"))
#       (ragg is recommended: it draws the female/male symbols reliably on every OS)
#       shiny::runApp("BalanceVision_Dashboard.R")
#   (requires internet; the sheet must remain "Anyone with the link can view")
# ==============================================================================

library(shiny)
library(ggplot2)
library(tools)
library(shinythemes)
library(dplyr)
library(DT)

# Crisp graphics device that handles the female/male symbol glyphs on any OS
if (requireNamespace("ragg", quietly = TRUE)) options(shiny.useragg = TRUE)

# ---- Data source -------------------------------------------------------------
SHEET_ID        <- "1Ly_4NAyrZkuKwGuYUwez3qEGAnNIMqvMMrOOABXbgLE"
SHEET_CSV       <- paste0("https://docs.google.com/spreadsheets/d/",
                          SHEET_ID, "/gviz/tq?tqx=out:csv")
AUTO_REFRESH_MS <- 120000   # re-poll the sheet every 2 minutes

NUM_COLS <- c("age", "trialNumber", "duration",
              "meanTorsoTilt", "maxTorsoTilt", "sdTorsoTilt",
              "meanLateralSway", "maxLateralSway", "rmsLateralSway", "sdLateralSway",
              "meanAngularVelocity", "peakAngularVelocity", "rmsAngularVelocity",
              "meanKneeAngle", "kneeVariability", "maxKneeDeviation",
              "meanPelvicTilt", "maxPelvicTilt", "pelvicVariability",
              "raisedLegVariability", "correctiveMovements", "stabilityScore")

# ---- Selectable variable menus (raw field -> readable label) -----------------
# raisedLegVariability is omitted: it is unpopulated in the current sheet.
# pelvicVariability is flagged (*) because atan2 hip-line wrap inflates its SD.
CONT_CHOICES <- c(
  "Age (years)"                   = "age",
  "Balance duration (s)"          = "duration",
  "Stability score (0-100)"       = "stabilityScore",
  "Corrective movements (count)"  = "correctiveMovements",
  "Mean torso tilt (deg)"         = "meanTorsoTilt",
  "Max torso tilt (deg)"          = "maxTorsoTilt",
  "SD torso tilt (deg)"           = "sdTorsoTilt",
  "Mean lateral sway (norm.)"     = "meanLateralSway",
  "Max lateral sway (norm.)"      = "maxLateralSway",
  "RMS lateral sway (norm.)"      = "rmsLateralSway",
  "SD lateral sway (norm.)"       = "sdLateralSway",
  "Mean angular velocity (deg/s)" = "meanAngularVelocity",
  "Peak angular velocity (deg/s)" = "peakAngularVelocity",
  "RMS angular velocity (deg/s)"  = "rmsAngularVelocity",
  "Mean knee angle (deg)"         = "meanKneeAngle",
  "Knee variability (deg)"        = "kneeVariability",
  "Max knee deviation (deg)"      = "maxKneeDeviation",
  "Mean pelvic tilt (deg)"        = "meanPelvicTilt",
  "Max pelvic tilt (deg)"         = "maxPelvicTilt",
  "Pelvic variability (deg)*"     = "pelvicVariability",
  "Trial number"                  = "trialNumber"
)

CAT_CHOICES <- c(
  "Participant type"    = "participantType",
  "Sex"                 = "sex",
  "Support leg"         = "supportLeg",
  "Age group"           = "ageGroup",
  "Performance band"    = "performanceBand",
  "Sway classification" = "swayClassification",
  "Test outcome"        = "completedFactor"
)

ALL_LABELS <- c(CONT_CHOICES, CAT_CHOICES)
pretty_lab <- function(v) {
  nm <- names(ALL_LABELS)[match(v, ALL_LABELS)]
  ifelse(is.na(nm), v, nm)
}

# ---- Load + clean ------------------------------------------------------------
load_raw <- function() {
  tryCatch(
    utils::read.csv(SHEET_CSV, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
}

clean_data <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  names(df) <- trimws(names(df))

  # De-duplicate on unique record id (guards against no-cors double-POSTs)
  if ("id" %in% names(df)) df <- df[!duplicated(df$id), , drop = FALSE]

  # Coerce numeric fields
  for (nm in intersect(NUM_COLS, names(df))) {
    df[[nm]] <- suppressWarnings(as.numeric(gsub(",", "", trimws(as.character(df[[nm]])))))
  }
  # Trim remaining character fields
  for (nm in setdiff(names(df), NUM_COLS)) {
    if (is.character(df[[nm]])) df[[nm]] <- trimws(df[[nm]])
  }

  # Derived fields
  df$completed       <- grepl("^Test completed", df$terminationReason)
  df$completedFactor <- factor(ifelse(df$completed, "Completed to max", "Assessor-stopped"),
                               levels = c("Completed to max", "Assessor-stopped"))
  df$tenSecondPass   <- ifelse(!is.na(df$duration), df$duration >= 10, NA)
  df$ageGroup        <- cut(df$age,
                            breaks = c(-Inf, 17, 39, 49, 59, 69, 79, Inf),
                            labels = c("<18","18-39","40-49","50-59","60-69","70-79","80+"))

  # Prettify categoricals
  if ("participantType" %in% names(df))
    df$participantType <- factor(df$participantType,
                                 levels = c("child", "adult", "older_adult"),
                                 labels = c("Child", "Adult", "Older adult"))
  if ("sex" %in% names(df)) {
    df$sex[is.na(df$sex) | df$sex == ""] <- "unspecified"
    df$sex <- droplevels(factor(df$sex,
                                levels = c("female","male","other","unspecified"),
                                labels = c("Female","Male","Other","Unspecified")))
  }
  if ("supportLeg" %in% names(df))
    df$supportLeg <- factor(df$supportLeg, levels = c("left","right"),
                            labels = c("Left","Right"))
  if ("swayClassification" %in% names(df))
    df$swayClassification <- factor(df$swayClassification,
                                    levels = c("Low Sway","Moderate Sway","High Sway"))
  if ("performanceLabel" %in% names(df))
    df$performanceBand <- factor(
      ifelse(grepl("^Strong", df$performanceLabel), "Strong",
      ifelse(grepl("^Moderate", df$performanceLabel), "Moderate",
      ifelse(grepl("^Limited", df$performanceLabel), "Limited", NA))),
      levels = c("Strong","Moderate","Limited"))

  # Tracking-artefact flag (optional exclusion; not dropped by default)
  df$artefact <- (!is.na(df$maxTorsoTilt)        & df$maxTorsoTilt > 90) |
                 (!is.na(df$peakAngularVelocity) & df$peakAngularVelocity > 500) |
                 (!is.na(df$meanAngularVelocity) & df$meanAngularVelocity > 60)
  df
}

fmt <- function(v, d = 2) ifelse(is.na(v), "-", formatC(v, format = "f", digits = d))

# ==============================================================================
# Sex-comparison helpers
# Pink (women) and blue (men): clearly different hues that sit well side by side. Distinct on their
# own, balanced when they sit side by side. Palette was colour-blind validated.
# ==============================================================================
# Make sure R can draw the female/male glyphs (needs a UTF-8 text locale)
if (!isTRUE(l10n_info()[["UTF-8"]]))
  try(suppressWarnings(Sys.setlocale("LC_CTYPE", "en_US.UTF-8")), silent = TRUE)

SEX_COL  <- c(Female = "#e0418f", Male = "#2a6fdb", Other = "#18a999", Unspecified = "#9aa0a6")
SEX_DARK <- c(Female = "#a8135f", Male = "#1a4fb0")          # text-safe shades
SEX_SYM  <- c(Female = "\u2640", Male = "\u2642", Other = "\u26A5", Unspecified = "?")
SEX_LAB  <- c(Female = "Women",  Male = "Men")
INK      <- "#1b1840"          # deep indigo: all headline / body text
INK2     <- "#5b5878"          # softer indigo: secondary text
GRID_COL <- "#e6e1f2"
VIOLET   <- "#8a63e6"          # the colour where pink and blue meet
# Categorical palette for non-sex groupings (pink / blue / violet / teal / amber ...)
PALETTE  <- c("#e0418f", "#2a6fdb", "#8a63e6", "#18a999", "#f2a33a", "#5b5878",
              "#c24bd1", "#4fb0ff", "#d96b3c", "#7aa83a", "#b04a7a", "#3b5998")
PLOT_FONT <- ""                # set e.g. "Nunito" if that font is installed locally
MIN_COMPLETE_S <- 30          # a trial counts as "completed" at >= 30 s
SYM_FONT <- ""                # set e.g. "Segoe UI Symbol" if the symbols do not draw

# Every tab shares one palette: sex uses pink / blue, other groups use PALETTE
sex_aware_colour <- function(z) {
  if (identical(z, "sex")) scale_colour_manual(values = SEX_COL, na.value = "grey70")
  else scale_colour_manual(values = rep_len(PALETTE, 12), na.value = "grey70")
}
sex_aware_fill <- function(z) {
  if (identical(z, "sex")) scale_fill_manual(values = SEX_COL, na.value = "grey70")
  else scale_fill_manual(values = rep_len(PALETTE, 12), na.value = "grey70")
}

fmt_p <- function(p) {
  ifelse(is.na(p), "-", ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 3)))
}

# One row per participant (first trial), women and men only, optional completers
prep_sex_data <- function(df, completers = TRUE, first_trial = TRUE) {
  d <- df[df$sex %in% c("Female", "Male") & !is.na(df$participantId), , drop = FALSE]
  d$sex <- factor(as.character(d$sex), levels = c("Female", "Male"))
  if (isTRUE(first_trial) && "timestamp" %in% names(d)) {
    d <- d[order(d$timestamp), , drop = FALSE]
    d <- d[!duplicated(d$participantId), , drop = FALSE]
  }
  if (isTRUE(completers))
    d <- d[!is.na(d$duration) & d$duration >= MIN_COMPLETE_S, , drop = FALSE]
  d
}

boot_median_ci <- function(x, B = 2000) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(c(NA_real_, NA_real_))
  stats::quantile(replicate(B, stats::median(sample(x, length(x), replace = TRUE))),
                  c(0.025, 0.975), names = FALSE)
}

# Cliff's delta: P(woman > man) - P(man > woman); 0 = complete overlap, +/-1 = no overlap
cliff_delta <- function(a, b) {
  a <- a[is.finite(a)]; b <- b[is.finite(b)]
  if (length(a) < 2 || length(b) < 2) return(NA_real_)
  mean(sign(outer(a, b, "-")))
}
cliff_ci <- function(a, b, B = 2000) {
  a <- a[is.finite(a)]; b <- b[is.finite(b)]
  if (length(a) < 3 || length(b) < 3) return(c(NA_real_, NA_real_))
  bs <- replicate(B, cliff_delta(sample(a, length(a), TRUE), sample(b, length(b), TRUE)))
  stats::quantile(bs, c(0.025, 0.975), names = FALSE)
}

EFFECT_MEASURES <- c("Pelvic variability*"  = "pelvicVariability",
                     "Mean pelvic tilt"     = "meanPelvicTilt",
                     "Max pelvic tilt"      = "maxPelvicTilt",
                     "Lateral sway (RMS)"   = "rmsLateralSway",
                     "Max torso tilt"       = "maxTorsoTilt")

sex_effects <- function(d) {
  f <- d[d$sex == "Female", , drop = FALSE]
  m <- d[d$sex == "Male",   , drop = FALSE]
  set.seed(42)
  out <- do.call(rbind, lapply(seq_along(EFFECT_MEASURES), function(i) {
    v <- EFFECT_MEASURES[[i]]; a <- f[[v]]; b <- m[[v]]
    ci <- cliff_ci(a, b)
    p  <- tryCatch(suppressWarnings(stats::wilcox.test(a, b, exact = FALSE)$p.value),
                   error = function(e) NA_real_)
    data.frame(measure = names(EFFECT_MEASURES)[i], var = v,
               delta = cliff_delta(a, b), lo = ci[1], hi = ci[2], p = p,
               nF = sum(is.finite(a)), nM = sum(is.finite(b)),
               stringsAsFactors = FALSE)
  }))
  out$verdict <- ifelse(is.na(out$lo), "Not enough data",
                 ifelse(out$lo > 0, "Women higher",
                 ifelse(out$hi < 0, "Men higher", "No detectable difference")))
  out
}

theme_sex <- function(base = 13, grid_x = FALSE) {
  theme_minimal(base_size = base, base_family = PLOT_FONT) +
    theme(plot.background   = element_rect(fill = "white", colour = NA),
          panel.grid.minor  = element_blank(),
          panel.grid.major.x = if (grid_x) element_line(colour = GRID_COL, linewidth = 0.4) else element_blank(),
          panel.grid.major.y = element_line(colour = GRID_COL, linewidth = 0.4),
          plot.title        = element_text(face = "bold", colour = INK, size = base + 2),
          plot.subtitle     = element_text(colour = INK2),
          plot.caption      = element_text(colour = INK2, hjust = 0, size = base - 3),
          plot.title.position   = "plot",
          plot.caption.position = "plot",
          axis.text         = element_text(colour = INK2),
          axis.text.x       = element_text(colour = INK, lineheight = 1.15),
          axis.title        = element_text(colour = INK2),
          plot.margin       = margin(10, 14, 8, 10),
          legend.title      = element_text(colour = INK, face = "bold"),
          legend.text       = element_text(colour = INK2))
}

# ---- Figure 1: every person as a female / male symbol -------------------------
plot_sex_dots <- function(d, var, title, ylab, log_y = FALSE, zoom = FALSE) {
  d <- d[is.finite(d[[var]]), , drop = FALSE]
  if (log_y) d <- d[d[[var]] > 0, , drop = FALSE]
  d$sex <- droplevels(d$sex)
  lv <- levels(d$sex)
  set.seed(7)
  d$x   <- match(as.character(d$sex), lv) + stats::runif(nrow(d), -0.18, 0.18)
  d$y   <- d[[var]]
  d$sym <- unname(SEX_SYM[as.character(d$sex)])

  st <- do.call(rbind, lapply(seq_along(lv), function(i) {
    y  <- d$y[d$sex == lv[i]]
    ci <- boot_median_ci(y)
    data.frame(sex = lv[i], pos = i, med = stats::median(y),
               lo = ci[1], hi = ci[2], n = length(y), stringsAsFactors = FALSE)
  }))

  pv <- if (length(lv) == 2 && all(st$n >= 2))
    tryCatch(suppressWarnings(stats::wilcox.test(y ~ sex, data = d, exact = FALSE)$p.value),
             error = function(e) NA_real_) else NA_real_

  med_txt <- ifelse(st$med < 10, formatC(st$med, format = "f", digits = 2),
                                 formatC(st$med, format = "f", digits = 1))
  tick <- sprintf("%s\nn = %d\nmedian %s", unname(SEX_LAB[st$sex]), st$n, med_txt)

  # tint behind each group (-Inf cannot be log-transformed, so use finite bounds there)
  st$ylo <- if (log_y) min(d$y) / 1.6 else -Inf
  st$yhi <- if (log_y) max(d$y) * 1.6 else  Inf

  p <- ggplot(d, aes(x = x, y = y)) +
    geom_rect(data = st, aes(xmin = pos - 0.5, xmax = pos + 0.62,
                             ymin = ylo, ymax = yhi, fill = sex),
              inherit.aes = FALSE, alpha = 0.13) +
    geom_segment(data = st, aes(x = pos - 0.30, xend = pos + 0.30, y = med, yend = med),
                 inherit.aes = FALSE, colour = INK, linewidth = 1.1) +
    geom_linerange(data = st, aes(x = pos + 0.40, ymin = lo, ymax = hi),
                   inherit.aes = FALSE, colour = INK, linewidth = 0.9, na.rm = TRUE) +
    geom_text(aes(label = sym, colour = sex), size = 6, family = SYM_FONT,
              alpha = 0.92, show.legend = FALSE) +
    scale_colour_manual(values = SEX_COL, guide = "none") +
    scale_fill_manual(values = SEX_COL, guide = "none") +
    scale_x_continuous(breaks = st$pos, labels = tick,
                       limits = c(0.45, length(lv) + 0.65), expand = c(0, 0))

  p <- p + if (log_y)
    scale_y_log10(labels = function(v) format(v, scientific = FALSE, trim = TRUE,
                                              drop0trailing = TRUE))
  else scale_y_continuous()

  out_n <- 0
  if (zoom && nrow(d) > 3) {
    q   <- stats::quantile(d$y, c(0.25, 0.75), names = FALSE); iqr <- q[2] - q[1]
    lo  <- max(min(d$y), q[1] - 2 * iqr); hi <- min(max(d$y), q[2] + 2 * iqr)
    pad <- 0.06 * (hi - lo); if (!is.finite(pad) || pad == 0) pad <- 1
    out_n <- sum(d$y < lo | d$y > hi)
    p <- p + coord_cartesian(ylim = c(lo - pad, hi + pad))
  }

  cap <- "Each symbol = one person. Bar = median; line = 95% bootstrap CI of the median."
  if (out_n > 0)
    cap <- paste0(cap, "\n", out_n, " point(s) lie outside this view but are included in the statistics.")

  p <- p + theme_sex() +
    theme(axis.text.x = element_text(colour = unname(SEX_DARK[st$sex]), face = "bold",
                                     lineheight = 1.15))
  p + labs(title = title,
           subtitle = paste0("Mann\u2013Whitney p = ", fmt_p(pv)),
           x = NULL, y = ylab, caption = cap)
}

# ---- Figure 2: effect sizes with confidence intervals ---------------------------
plot_effects <- function(eff) {
  eff$measure <- factor(eff$measure, levels = rev(eff$measure))
  eff$ypos <- as.numeric(eff$measure)
  eff$dir  <- ifelse(is.na(eff$delta) | eff$delta >= 0, "Female", "Male")
  eff$sym  <- unname(SEX_SYM[eff$dir])
  eff$lab  <- sprintf("%+.2f   [%+.2f, %+.2f]", eff$delta, eff$lo, eff$hi)
  n <- nrow(eff)

  ggplot(eff) +
    annotate("rect", xmin = -1, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = SEX_COL[["Male"]],   alpha = 0.13) +
    annotate("rect", xmin = 0,  xmax = 1, ymin = -Inf, ymax = Inf,
             fill = SEX_COL[["Female"]], alpha = 0.13) +
    annotate("text", x = -0.5, y = (n + 1) / 2, label = SEX_SYM[["Male"]],
             size = 44, family = SYM_FONT, colour = SEX_COL[["Male"]],   alpha = 0.17) +
    annotate("text", x =  0.5, y = (n + 1) / 2, label = SEX_SYM[["Female"]],
             size = 44, family = SYM_FONT, colour = SEX_COL[["Female"]], alpha = 0.17) +
    geom_vline(xintercept = 0, colour = INK, linewidth = 0.8) +
    geom_segment(aes(x = lo, xend = hi, y = ypos, yend = ypos),
                 colour = INK2, linewidth = 1.7, lineend = "round", na.rm = TRUE) +
    geom_text(aes(x = delta, y = ypos, label = sym, colour = dir),
              size = 9, family = SYM_FONT, na.rm = TRUE) +
    geom_text(aes(x = ifelse(hi > 0.45, lo - 0.03, hi + 0.03), y = ypos, label = lab,
                  hjust = ifelse(hi > 0.45, 1, 0)),
              size = 3.5, colour = INK2, na.rm = TRUE) +
    annotate("text", x = -0.975, y = 0.45, hjust = 0, size = 6, family = SYM_FONT,
             colour = SEX_COL[["Male"]],   label = SEX_SYM[["Male"]]) +
    annotate("text", x = -0.915, y = 0.45, hjust = 0, size = 4.2, fontface = "bold",
             colour = SEX_DARK[["Male"]],  label = "men higher") +
    annotate("text", x =  0.975, y = 0.45, hjust = 1, size = 6, family = SYM_FONT,
             colour = SEX_COL[["Female"]], label = SEX_SYM[["Female"]]) +
    annotate("text", x =  0.915, y = 0.45, hjust = 1, size = 4.2, fontface = "bold",
             colour = SEX_DARK[["Female"]], label = "women higher") +
    scale_colour_manual(values = SEX_COL, guide = "none") +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5), expand = c(0, 0)) +
    scale_y_continuous(breaks = eff$ypos, labels = as.character(eff$measure),
                       limits = c(0.2, n + 0.7), expand = c(0, 0)) +
    labs(x = "Cliff's delta (women vs men):  0 = the two groups overlap completely",
         y = NULL,
         caption = "Symbol = effect size, line = 95% bootstrap CI. A line that crosses 0 means no detectable difference.") +
    theme_sex() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "white", linewidth = 0.5),
          axis.text.y = element_text(colour = INK, size = 13))
}

# ---- Page styling -------------------------------------------------------------
APP_CSS <- "
  :root{--f:#e0418f;--m:#2a6fdb;--fd:#a8135f;--md:#1a4fb0;--v:#8a63e6;
        --ink:#1b1840;--ink2:#5b5878;--line:#e4dff0;
        --head:'Fredoka','Nunito',system-ui,sans-serif;
        --body:'Nunito',system-ui,-apple-system,'Segoe UI',Roboto,sans-serif;}
  body{color:var(--ink);min-height:100vh;font-family:var(--body);font-size:15px;
       background:
         radial-gradient(1000px 650px at -5% 18%,rgba(224,65,143,.26),transparent 62%),
         radial-gradient(1000px 650px at 105% 55%,rgba(42,111,219,.28),transparent 62%),
         radial-gradient(900px 500px at 50% 110%,rgba(150,110,230,.18),transparent 65%),
         linear-gradient(180deg,#fff0f8 0%,#f2ecfc 48%,#e6f0ff 100%);
       background-attachment:fixed;}
  .container-fluid{max-width:1440px;}
  h1,h2,h3,h4,h5,h6{font-family:var(--head)!important;color:var(--ink);font-weight:600;letter-spacing:.2px;}
  label,.control-label{font-family:var(--body);font-weight:700;color:var(--ink);}
  .checkbox label,.radio label,.radio-inline,.checkbox-inline{font-weight:600;color:var(--ink);}
  input[type=checkbox],input[type=radio]{accent-color:var(--f);}
  hr{border-top:1px solid rgba(27,24,64,.10);}
  .shiny-output-error-validation{color:var(--ink2);font-weight:600;}

  /* section headings inside tabs get a pink-to-blue marker */
  .tab-content h4{display:flex;align-items:center;gap:10px;margin:18px 0 10px;font-size:20px;}
  .tab-content h4:before{content:'';width:9px;height:22px;border-radius:6px;
                         background:linear-gradient(180deg,var(--f),var(--m));}

  /* ---------------- hero banner ---------------- */
  .bv-hero{position:relative;margin:0 -15px 22px -15px;padding:30px 40px 30px;overflow:hidden;color:#fff;
    background:radial-gradient(900px 300px at 6% -30%,rgba(224,65,143,.80),transparent 62%),
               radial-gradient(900px 300px at 96% 130%,rgba(42,111,219,.85),transparent 62%),#17123a;}
  .bv-hero-inner{position:relative;z-index:2;display:flex;align-items:center;gap:24px;}
  .bv-mark{display:flex;font-size:64px;line-height:1;font-family:var(--body);}
  .bv-mark span{display:inline-block;transform-origin:50% 95%;
                animation:bvSway 3.4s ease-in-out infinite;text-shadow:0 4px 18px rgba(0,0,0,.25);}
  .bv-mark .f{color:#ff9fcd;margin-right:-8px;}
  .bv-mark .m{color:#9cc6ff;animation-delay:-1.7s;}
  .bv-title{font-family:var(--head)!important;font-weight:700;font-size:clamp(34px,5vw,54px);line-height:1.05;
            margin:0;color:#fff;letter-spacing:1px;text-shadow:0 4px 22px rgba(224,65,143,.45);}
  .bv-title .ltr{display:inline-block;color:#fff;
    animation:bvDrop .85s cubic-bezier(.2,1.4,.4,1) both,
              bvWave 3s ease-in-out infinite,
              bvTint 7s ease-in-out infinite;
    animation-delay:calc(var(--i) * .07s),calc(1.5s + var(--i) * .12s),calc(var(--i) * .3s);}
  .bv-title .sp{display:inline-block;width:.32em;}
  .bv-sub{margin:10px 0 0;color:rgba(255,255,255,.92);font-size:16px;font-weight:600;max-width:820px;
          line-height:1.45;animation:bvRise .9s ease .9s both;}
  .bv-sub .kick{font-family:var(--head);font-weight:500;color:#ffc4e2;letter-spacing:.3px;}
  .bv-bar{position:absolute;left:0;right:0;bottom:0;height:6px;z-index:3;background-size:200% 100%;
          background-image:linear-gradient(90deg,#e0418f,#c9a8f0,#2a6fdb,#c9a8f0,#e0418f,#c9a8f0,#2a6fdb,#c9a8f0,#e0418f);
          animation:bvSlide 7s linear infinite;}
  .bv-orb{position:absolute;border-radius:50%;filter:blur(34px);opacity:.38;z-index:1;
          animation:bvFloat 9s ease-in-out infinite;}
  .bv-orb.o1{width:190px;height:190px;left:34%;top:-70px;background:#e0418f;}
  .bv-orb.o2{width:230px;height:230px;right:6%;top:-40px;background:#2a6fdb;animation-delay:-3s;}
  .bv-orb.o3{width:150px;height:150px;left:62%;bottom:-70px;background:#8a63e6;animation-delay:-6s;}

  @keyframes bvDrop{0%{opacity:0;transform:translateY(-42px) rotate(-14deg) scale(.55);}
                    60%{opacity:1;transform:translateY(7px) rotate(3deg) scale(1.06);}
                    100%{opacity:1;transform:none;}}
  @keyframes bvWave{0%,100%{transform:translateY(0);}50%{transform:translateY(-7px);}}
  @keyframes bvTint{0%,100%{color:#fff;}33%{color:#ffc4e2;}66%{color:#c4dcff;}}
  @keyframes bvSway{0%,100%{transform:rotate(-9deg);}50%{transform:rotate(9deg);}}
  @keyframes bvRise{from{opacity:0;transform:translateY(14px);}to{opacity:1;transform:none;}}
  @keyframes bvSlide{from{background-position:0% 0;}to{background-position:100% 0;}}
  @keyframes bvFloat{0%,100%{transform:translate(0,0);}50%{transform:translate(22px,-16px);}}
  @keyframes bvFade{from{opacity:0;transform:translateY(8px);}to{opacity:1;transform:none;}}
  @media (prefers-reduced-motion:reduce){.bv-hero *,.tab-content>.tab-pane{animation:none!important;}}
  @media (max-width:640px){.bv-hero{padding:22px 20px;}.bv-mark{font-size:44px;}.bv-sub{font-size:14px;}}

  /* ---------------- tabs ---------------- */
  .nav-tabs{border-bottom:1px solid rgba(27,24,64,.12);margin-bottom:18px;}
  .nav-tabs>li>a{font-family:var(--head);font-size:16px;font-weight:500;color:var(--ink2);border:none!important;
                 background:transparent!important;border-radius:0;padding:10px 16px;margin-right:2px;
                 transition:color .2s ease,transform .2s ease;}
  .nav-tabs>li>a:hover{color:var(--fd);transform:translateY(-2px);}
  .nav-tabs>li.active>a,.nav-tabs>li.active>a:hover,.nav-tabs>li.active>a:focus{
    color:var(--ink)!important;font-weight:600;border:none!important;border-bottom:4px solid transparent!important;
    border-image:linear-gradient(90deg,var(--f),var(--m)) 1!important;}
  .tab-content>.tab-pane.active{animation:bvFade .5s ease both;}

  /* ---------------- panels, filters, inputs, buttons ---------------- */
  .well{background:rgba(255,255,255,.88);border:1px solid var(--line);border-radius:14px;
        box-shadow:0 6px 18px rgba(27,24,64,.07);}
  .form-control,.selectize-input{border-radius:10px;border:1px solid var(--line);box-shadow:none;
        color:var(--ink);font-family:var(--body);font-weight:600;}
  .form-control:focus,.selectize-input.focus{border-color:var(--f);box-shadow:0 0 0 3px rgba(224,65,143,.16);}
  .selectize-dropdown{border-radius:10px;border:1px solid var(--line);font-family:var(--body);}
  .selectize-dropdown .active{background:rgba(224,65,143,.12);color:var(--ink);}
  .irs--shiny .irs-line{background:#ece7f6;border-color:#ece7f6;}
  .irs--shiny .irs-bar{background:linear-gradient(90deg,var(--f),var(--m));border-color:transparent;}
  .irs--shiny .irs-handle{border:2px solid var(--f);background:#fff;box-shadow:0 2px 6px rgba(27,24,64,.2);}
  .irs--shiny .irs-single,.irs--shiny .irs-from,.irs--shiny .irs-to{background:var(--v);font-family:var(--body);}
  .irs--shiny .irs-min,.irs--shiny .irs-max{color:var(--ink2);background:transparent;}
  .irs--shiny .irs-grid-text{color:var(--ink2);}
  .btn{font-family:var(--head);font-weight:500;font-size:15px;border-radius:10px;
       transition:transform .15s ease,box-shadow .15s ease,filter .15s ease;}
  .btn:hover{transform:translateY(-2px);box-shadow:0 8px 18px rgba(27,24,64,.16);}
  .btn-default{background:#fff;border:1px solid var(--line);color:var(--ink);}
  .btn-primary{background:linear-gradient(90deg,var(--f),var(--m));border:none;color:#fff;}
  .btn-primary:hover,.btn-primary:focus{filter:brightness(1.07);color:#fff;}
  .small-note{font-size:12.5px;color:var(--ink2);}
  .kpi{background:#fff;border:1px solid var(--line);border-top:5px solid transparent;
       border-image:linear-gradient(90deg,var(--f),var(--m)) 1;border-radius:0;padding:12px;text-align:center;
       box-shadow:0 6px 18px rgba(27,24,64,.07);transition:transform .2s ease;}
  .kpi:hover{transform:translateY(-3px);}
  .kpi .v{font-family:var(--head);font-size:28px;font-weight:600;color:var(--ink);}
  .kpi .l{font-size:12.5px;font-weight:700;color:var(--ink2);}

  /* ---------------- data tables (all tabs) ---------------- */
  .dataTables_wrapper{background:#fff;border:1px solid var(--line);border-radius:14px;padding:12px 16px;
                      box-shadow:0 8px 22px rgba(27,24,64,.09);color:var(--ink);font-family:var(--body);}
  table.dataTable thead th{font-family:var(--head);font-weight:500;color:var(--ink);
       border-bottom:3px solid transparent!important;border-image:linear-gradient(90deg,var(--f),var(--m)) 1;}
  table.dataTable tbody td{color:var(--ink);}
  table.dataTable tbody tr:hover td{background:#faf1fb!important;}
  .dataTables_filter input,.dataTables_length select{border:1px solid var(--line);border-radius:8px;padding:2px 8px;}
  .dataTables_wrapper .dataTables_paginate .paginate_button{border-radius:8px!important;color:var(--ink)!important;}
  .dataTables_wrapper .dataTables_paginate .paginate_button.current,
  .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover{
       background:linear-gradient(90deg,var(--f),var(--m))!important;color:#fff!important;border:none!important;}
  .dataTables_wrapper .dataTables_info{color:var(--ink2);font-size:12.5px;}

  /* ---------------- sex-comparison options ---------------- */
  .sx-opts{background:rgba(255,255,255,.88);border:1px solid var(--line);border-radius:12px;padding:10px 18px;
           margin-bottom:14px;display:flex;gap:28px;align-items:center;flex-wrap:wrap;
           box-shadow:0 4px 14px rgba(27,24,64,.06);}
  .sx-opts .form-group{margin:0;} .sx-opts .checkbox label{white-space:nowrap;}
  .sx-opts .lead-in{font-family:var(--head);font-weight:500;font-size:17px;color:var(--ink);}

  .sex-card{position:relative;overflow:hidden;border:1px solid var(--line);border-radius:16px;
            padding:16px 20px;margin-bottom:14px;min-height:178px;box-shadow:0 8px 22px rgba(27,24,64,.09);
            transition:transform .2s ease;}
  .sex-card:hover,.sex-bridge:hover{transform:translateY(-3px);}
  .sex-card.f{border-top:7px solid var(--f);background:linear-gradient(150deg,#fff 35%,#ffdcec 100%);}
  .sex-card.m{border-top:7px solid var(--m);background:linear-gradient(210deg,#fff 35%,#d6e6ff 100%);}
  .sex-card .sym{position:absolute;right:14px;top:-10px;font-size:100px;line-height:1;opacity:.20;}
  .sex-card.f .sym{color:var(--f);} .sex-card.m .sym{color:var(--m);}
  .sex-card .who{font-family:var(--head);font-size:27px;font-weight:600;}
  .sex-card.f .who{color:var(--fd);} .sex-card.m .who{color:var(--md);}
  .sex-card .n{color:var(--ink2);font-size:12.5px;font-weight:600;margin-bottom:8px;}
  .sex-card .stat{display:flex;justify-content:space-between;font-size:13.5px;padding:5px 0;
                  border-top:1px dashed rgba(27,24,64,.18);}
  .sex-card.f .stat .v{color:var(--fd);} .sex-card.m .stat .v{color:var(--md);}
  .sex-card .stat .v{font-weight:800;}
  .sex-bridge{background:linear-gradient(90deg,rgba(224,65,143,.20),rgba(190,160,240,.22),rgba(42,111,219,.20)),#fff;
              border:1px solid var(--line);border-radius:16px;padding:16px 20px;margin-bottom:14px;min-height:178px;
              box-shadow:0 8px 22px rgba(27,24,64,.09);transition:transform .2s ease;}
  .sex-bridge .who{font-family:var(--head);font-size:22px;font-weight:600;color:var(--ink);}
  .sex-bridge .sub{color:var(--ink2);font-size:12.5px;font-weight:600;margin-bottom:8px;}
  .sex-bridge .row2{font-size:13.5px;padding:5px 0;border-top:1px dashed rgba(27,24,64,.2);}
  .sex-bridge .row2 b{color:var(--ink);}

  /* ---------------- graph cards and insight boxes (every tab) ---------------- */
  .plot-card{background:#fff;border:1px solid var(--line);border-top:6px solid transparent;
             border-image:linear-gradient(90deg,var(--f),var(--m)) 1;border-radius:0;
             padding:10px;margin-bottom:16px;box-shadow:0 12px 30px rgba(27,24,64,.14);}
  .insight{background:rgba(255,255,255,.94);border:1px solid var(--line);border-left:6px solid transparent;
           border-image:linear-gradient(180deg,var(--f),var(--m)) 1;border-image-width:0 0 0 6px;
           padding:12px 18px;margin:4px 0 14px;font-size:14px;line-height:1.55;color:var(--ink);
           box-shadow:0 6px 18px rgba(27,24,64,.07);}
  .insight b{color:var(--ink);}
  .insight table th{font-family:var(--head);font-weight:500;color:var(--ink);}

  /* ---------------- hero scene: high-five, then single-leg balance (12 s loop) ---------------- */
  .bvs{position:absolute;right:34px;bottom:12px;width:270px;height:auto;z-index:2;pointer-events:none;
       filter:drop-shadow(0 4px 12px rgba(0,0,0,.30));}
  .bvs .w{--c:#ff9fcd;} .bvs .m{--c:#9cc6ff;}
  .bvs line,.bvs path{stroke:var(--c);stroke-width:6;stroke-linecap:round;stroke-linejoin:round;fill:none;}
  .bvs .arm line{stroke-width:5;}
  .bvs path.dress{fill:var(--c);stroke-width:3;}
  .bvs circle,.bvs rect{fill:var(--c);}
  .bvs line.ground{stroke:rgba(255,255,255,.35);stroke-width:3;}
  .bvs .spark line{stroke:#ffe9a8;stroke-width:2.6;}
  .bvs .spark{opacity:0;transform-origin:0px 0px;animation:bvfSpark 12s ease-out infinite;}
  .bvs .fig{animation:bvfBody 12s ease-in-out infinite;}
  .bvs .arm.inn{animation:bvfArmIn 12s ease-in-out infinite;}
  .bvs .arm.out{animation:bvfArmOut 12s ease-in-out infinite;}
  .bvs .thigh{animation:bvfThigh 12s ease-in-out infinite;}
  .bvs .shin{animation:bvfShin 12s ease-in-out infinite;}

  @keyframes bvfBody{
    0%{opacity:0;transform:translateX(calc(var(--d) * 30px)) rotate(0deg);}
    5%{opacity:1;}
    12%{transform:translateX(0px) rotate(0deg);}
    22%{transform:translateX(0px) rotate(0deg);}
    26%,34%{transform:translateX(0px) rotate(calc(var(--d) * -4deg));}
    42%{transform:translateX(0px) rotate(0deg);}
    48%{transform:translateX(0px) rotate(calc(var(--w) * 3deg));}
    52%{transform:translateX(0px) rotate(calc(var(--w) * -7deg));}
    56%{transform:translateX(0px) rotate(calc(var(--w) * 8deg));}
    60%{transform:translateX(0px) rotate(calc(var(--w) * -9deg));}
    64%{transform:translateX(0px) rotate(calc(var(--w) * 7deg));}
    68%{transform:translateX(0px) rotate(calc(var(--w) * -5deg));}
    72%{transform:translateX(0px) rotate(calc(var(--w) * 4deg));}
    76%{transform:translateX(0px) rotate(calc(var(--w) * -2deg));}
    80%{transform:translateX(0px) rotate(0deg);}
    94%{opacity:1;transform:translateX(0px) rotate(0deg);}
    100%{opacity:0;transform:translateX(calc(var(--d) * 30px)) rotate(0deg);}}
  @keyframes bvfArmIn{
    0%,14%{transform:rotate(0deg);}
    24%{transform:rotate(calc(var(--s) * 135deg));}
    27%{transform:rotate(calc(var(--s) * 124deg));}
    30%{transform:rotate(calc(var(--s) * 138deg));}
    38%{transform:rotate(calc(var(--s) * 130deg));}
    46%{transform:rotate(calc(var(--s) * 80deg));}
    52%{transform:rotate(calc(var(--s) * 92deg));}
    56%{transform:rotate(calc(var(--s) * 70deg));}
    60%{transform:rotate(calc(var(--s) * 95deg));}
    64%{transform:rotate(calc(var(--s) * 72deg));}
    68%{transform:rotate(calc(var(--s) * 88deg));}
    72%{transform:rotate(calc(var(--s) * 78deg));}
    80%{transform:rotate(calc(var(--s) * 80deg));}
    88%,100%{transform:rotate(0deg);}}
  @keyframes bvfArmOut{
    0%,40%{transform:rotate(0deg);}
    46%{transform:rotate(calc(var(--s) * 80deg));}
    52%{transform:rotate(calc(var(--s) * 68deg));}
    56%{transform:rotate(calc(var(--s) * 95deg));}
    60%{transform:rotate(calc(var(--s) * 70deg));}
    64%{transform:rotate(calc(var(--s) * 92deg));}
    68%{transform:rotate(calc(var(--s) * 76deg));}
    72%{transform:rotate(calc(var(--s) * 86deg));}
    80%{transform:rotate(calc(var(--s) * 80deg));}
    88%,100%{transform:rotate(0deg);}}
  @keyframes bvfThigh{
    0%,40%{transform:rotate(0deg);}
    48%,78%{transform:rotate(calc(var(--t) * 60deg));}
    86%,100%{transform:rotate(0deg);}}
  @keyframes bvfShin{
    0%,40%{transform:rotate(0deg);}
    48%,78%{transform:rotate(calc(var(--t) * -100deg));}
    86%,100%{transform:rotate(0deg);}}
  @keyframes bvfSpark{
    0%,24%{opacity:0;transform:scale(.2);}
    26%{opacity:1;transform:scale(.6);}
    28%{opacity:1;transform:scale(1.1);}
    30%{opacity:.9;transform:scale(.8);}
    32%{opacity:1;transform:scale(1.3);}
    38%,100%{opacity:0;transform:scale(1.9);}}

  @media (min-width:901px){.bv-hero-inner{padding-right:300px;}.bv-sub{max-width:620px;}}
  @media (max-width:900px){.bvs{display:none;}}
"

# Animated restroom-sign silhouettes: walk in, high-five, then balance on one leg (12 s loop)
BV_SCENE <- HTML(paste0(
  "<svg class='bvs' viewBox='0 0 300 130' aria-hidden='true' xmlns='http://www.w3.org/2000/svg'>",
  "<line class='ground' x1='70' y1='123' x2='232' y2='123'/>",

  # ---- woman (pink, dress silhouette) ----
  "<g transform='translate(118 122)'>",
  "<g class='fig w' style='--d:-1;--w:1;transform-origin:5px 0px'>",
  "<g class='arm out' style='--s:1;transform-origin:-9px -78px'>",
  "<line x1='-9' y1='-78' x2='-9' y2='-49'/><circle cx='-9' cy='-46' r='3.6'/></g>",
  "<g class='arm inn' style='--s:-1;transform-origin:9px -78px'>",
  "<line x1='9' y1='-78' x2='9' y2='-49'/><circle cx='9' cy='-46' r='3.6'/></g>",
  "<path class='dress' d='M-9 -80 Q0 -84 9 -80 L20 -40 L-20 -40 Z'/>",
  "<circle cx='0' cy='-92' r='8.5'/>",
  "<line x1='5' y1='-40' x2='5' y2='0'/>",
  "<g class='thigh' style='--t:1;transform-origin:-5px -40px'>",
  "<line x1='-5' y1='-40' x2='-5' y2='-20'/>",
  "<g class='shin' style='--t:1;transform-origin:-5px -20px'>",
  "<line x1='-5' y1='-20' x2='-5' y2='0'/></g></g>",
  "</g></g>",

  # ---- man (blue, trousers silhouette) ----
  "<g transform='translate(184 122)'>",
  "<g class='fig m' style='--d:1;--w:-1;transform-origin:-5px 0px'>",
  "<g class='arm out' style='--s:-1;transform-origin:13px -78px'>",
  "<line x1='13' y1='-78' x2='13' y2='-47'/><circle cx='13' cy='-44' r='3.6'/></g>",
  "<g class='arm inn' style='--s:1;transform-origin:-13px -78px'>",
  "<line x1='-13' y1='-78' x2='-13' y2='-47'/><circle cx='-13' cy='-44' r='3.6'/></g>",
  "<rect x='-11' y='-82' width='22' height='38' rx='6'/>",
  "<circle cx='0' cy='-93' r='8.5'/>",
  "<line x1='-5' y1='-44' x2='-5' y2='0'/>",
  "<g class='thigh' style='--t:-1;transform-origin:5px -44px'>",
  "<line x1='5' y1='-44' x2='5' y2='-22'/>",
  "<g class='shin' style='--t:-1;transform-origin:5px -22px'>",
  "<line x1='5' y1='-22' x2='5' y2='0'/></g></g>",
  "</g></g>",

  # ---- high-five burst ----
  "<g transform='translate(148 22)'><g class='spark'>",
  "<line x1='0' y1='-7' x2='0' y2='-15'/>",
  "<line x1='0' y1='-7' x2='0' y2='-15' transform='rotate(45)'/>",
  "<line x1='0' y1='-7' x2='0' y2='-15' transform='rotate(90)'/>",
  "<line x1='0' y1='-7' x2='0' y2='-15' transform='rotate(135)'/>",
  "<line x1='0' y1='-7' x2='0' y2='-15' transform='rotate(180)'/>",
  "<line x1='0' y1='-7' x2='0' y2='-15' transform='rotate(225)'/>",
  "<line x1='0' y1='-7' x2='0' y2='-15' transform='rotate(270)'/>",
  "<line x1='0' y1='-7' x2='0' y2='-15' transform='rotate(315)'/>",
  "</g></g>",
  "</svg>"))

# Splits a string into animated letters (each <span> gets its own animation delay)
title_letters <- function(txt) {
  ch <- strsplit(txt, "")[[1]]
  tagList(lapply(seq_along(ch), function(i) {
    if (ch[i] == " ") span(class = "sp")
    else span(class = "ltr", style = paste0("--i:", i, ";"), ch[i])
  }))
}

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  title = "Balance Vision",
  theme = shinytheme("flatly"),
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Fredoka:wght@400;500;600;700&family=Nunito:wght@400;600;700;800&display=swap"),
    tags$style(HTML(APP_CSS))
  ),

  # ---- Hero banner -------------------------------------------------------------
  div(class = "bv-hero",
      div(class = "bv-orb o1"), div(class = "bv-orb o2"), div(class = "bv-orb o3"),
      div(class = "bv-hero-inner",
          div(class = "bv-mark", span(class = "f", "\u2640"), span(class = "m", "\u2642")),
          div(tags$h1(class = "bv-title", `aria-label` = "Balance Vision",
                      title_letters("Balance Vision")),
              p(class = "bv-sub",
                span(class = "kick", "Single leg balance analytics:"),
                " Pelvic stability in Single-leg Stance: Women and Men compared"))),
      BV_SCENE,
      div(class = "bv-bar")),

  # ---- Global filter + data status bar --------------------------------------
  wellPanel(
    fluidRow(
      column(3, checkboxGroupInput("f_ptype", "Participant type:",
                                   choices  = c("Child","Adult","Older adult"),
                                   selected = c("Child","Adult","Older adult"),
                                   inline = TRUE)),
      column(3, checkboxGroupInput("f_sex", "Sex:",
                                   choices  = c("Female","Male","Other","Unspecified"),
                                   selected = c("Female","Male","Other","Unspecified"),
                                   inline = TRUE)),
      column(3,
             checkboxInput("f_completed", "Completed-to-max tests only", FALSE),
             checkboxInput("f_artefact",  "Exclude tracking artefacts",  FALSE)),
      column(3,
             actionButton("refresh", "Refresh data", icon = icon("rotate"),
                          class = "btn-primary"),
             br(), br(),
             uiOutput("status"))
    ),
    div(class = "small-note",
        "* Pelvic variability is affected by hip-line angle wrap; interpret with care. ",
        "Artefact filter removes rows with implausible torso tilt / angular velocity.")
  ),

  # ---- Options for the two sex-comparison tabs (shown only on those tabs) ------
  conditionalPanel(
    condition = "input.tabs == 'Pelvic Stability by Sex' || input.tabs == 'Sex Differences (Intervals)'",
    div(class = "sx-opts",
        span(class = "lead-in", "Sex-comparison options"),
        checkboxInput("sx_completers", "Completed tests only (\u2265 30 s)", TRUE),
        checkboxInput("sx_first", "First trial per participant", TRUE))
  ),

  tabsetPanel(
    id = "tabs",

    # ---- TAB 0 (default): Pelvic stability by sex ---------------------------------
    tabPanel("Pelvic Stability by Sex",
      uiOutput("sex_cards"),
      fluidRow(
        column(6, div(class = "plot-card", plotOutput("sex_plot_var",  height = "440px"))),
        column(6, div(class = "plot-card", plotOutput("sex_plot_tilt", height = "440px")))
      ),
      uiOutput("sex_insight"),
      div(class = "small-note",
          "Pelvic variability is shown on a log scale because it is strongly skewed. ",
          "Default view: first trial per participant, tests completed to at least 30 s ",
          "(change this in the options bar above).")
    ),

    # ---- TAB 0b: Effect-size intervals -------------------------------------------
    tabPanel("Sex Differences (Intervals)",
      uiOutput("effect_headline"),
      div(class = "plot-card", plotOutput("effect_plot", height = "470px")),
      h4("Numbers behind the plot"),
      DT::dataTableOutput("effect_table"),
      div(class = "small-note", style = "margin-top:8px;",
          "Cliff's delta is the probability that a randomly chosen woman scores higher than a randomly ",
          "chosen man, minus the reverse. Five measures are compared, so p-values are uncorrected and ",
          "one nominal hit in five would be unsurprising by chance.")
    ),


    # ---- TAB 1: Scatter Explorer --------------------------------------------
    tabPanel("Scatter Explorer",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("sy", "Y-axis:", choices = CONT_CHOICES, selected = "duration"),
          selectInput("sx", "X-axis:", choices = CONT_CHOICES, selected = "age"),
          selectInput("sz", "Colour by:", choices = CAT_CHOICES, selected = "participantType"),
          sliderInput("salpha", "Opacity (alpha):", min = 0.1, max = 1, value = 0.7, step = 0.05),
          sliderInput("ssize",  "Point size:",      min = 0.5, max = 6, value = 2.5, step = 0.5),
          radioButtons("smooth", "Trend line:",
                       choices = c("None" = "none", "Linear (lm)" = "lm", "LOESS" = "loess"),
                       selected = "lm", inline = TRUE),
          checkboxInput("ageref", "Overlay age-reference norms (Age vs Duration only)", FALSE),
          textInput("plot_title", "Plot title (optional):", placeholder = "Enter a title"),
          actionButton("update_title", "Update title", class = "btn-primary"),
          hr(),
          div(class = "small-note",
              "Hover a point for its ID and values. ",
              "Drag a rectangle to list and summarise the selected points below.")
        ),
        mainPanel(width = 9,
          div(class = "plot-card",
              div(style = "position:relative;",
                  plotOutput("scatter", height = "460px",
                             hover = hoverOpts("scatter_hover", delay = 60, delayType = "debounce"),
                             brush = brushOpts("scatter_brush")),
                  uiOutput("hover_info"))),
          uiOutput("fit_summary"),
          fluidRow(
            column(8,
              h4("Selected points"),
              DT::dataTableOutput("sel_table")),
            column(4,
              h4("Selection statistics"),
              div(class = "insight", uiOutput("sel_stats")))
          )
        )
      )
    ),

    # ---- TAB 2: Distributions -----------------------------------------------
    tabPanel("Distributions",
      br(),
      fluidRow(
        column(3, div(class = "kpi", div(class = "v", textOutput("kpi_n")),       div(class = "l", "Assessments"))),
        column(3, div(class = "kpi", div(class = "v", textOutput("kpi_part")),    div(class = "l", "Participants"))),
        column(3, div(class = "kpi", div(class = "v", textOutput("kpi_dur")),     div(class = "l", "Mean duration (s)"))),
        column(3, div(class = "kpi", div(class = "v", textOutput("kpi_score")),   div(class = "l", "Mean stability score")))
      ),
      br(),
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("dvar", "Variable:", choices = CONT_CHOICES, selected = "duration"),
          sliderInput("dbins", "Bins:", min = 5, max = 60, value = 20),
          checkboxInput("ddens", "Density curve", TRUE),
          checkboxInput("dnorm", "Normal overlay", TRUE)
        ),
        mainPanel(width = 9,
          div(class = "plot-card", plotOutput("hist", height = "420px")),
          h4("Summary & normality"),
          div(class = "insight", uiOutput("dist_stats"))
        )
      )
    ),

    # ---- TAB 3: Group Comparisons -------------------------------------------
    tabPanel("Group Comparisons",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("gnum", "Outcome (Y):", choices = CONT_CHOICES, selected = "duration"),
          selectInput("gcat", "Group (X):",   choices = CAT_CHOICES,  selected = "participantType"),
          radioButtons("gtype", "Plot:", choices = c("Box", "Violin"), inline = TRUE)
        ),
        mainPanel(width = 9,
          div(class = "plot-card", plotOutput("box", height = "420px")),
          h4("Per-group summary (five-number + mean/SD)"),
          DT::dataTableOutput("group_stats"),
          h4("Omnibus test"),
          div(class = "insight", uiOutput("group_test"))
        )
      )
    ),

    # ---- TAB 4: Correlations -------------------------------------------------
    tabPanel("Correlations",
      sidebarLayout(
        sidebarPanel(width = 3,
          checkboxGroupInput("cvars", "Metrics:",
            choices  = CONT_CHOICES,
            selected = c("age","duration","stabilityScore","rmsLateralSway",
                         "meanAngularVelocity","meanTorsoTilt","kneeVariability",
                         "correctiveMovements")),
          radioButtons("cmethod", "Method:",
                       choices = c("pearson", "spearman"), selected = "spearman", inline = TRUE)
        ),
        mainPanel(width = 9,
          div(class = "plot-card", plotOutput("corr", height = "480px")),
          h4("Correlation matrix"),
          DT::dataTableOutput("corr_table")
        )
      )
    ),

    # ---- TAB 5: Left–Right Asymmetry ----------------------------------------
    tabPanel("Left-Right Asymmetry",
      br(),
      p(class = "small-note",
        "Uses each participant's best (maximum) duration per support leg. ",
        "Only participants with both a left and a right record are included. ",
        "Repeated records per participant are non-independent by design."),
      fluidRow(
        column(6, div(class = "plot-card", plotOutput("asym_plot", height = "440px"))),
        column(6,
          h4("Paired comparison"),
          div(class = "insight", uiOutput("asym_test")),
          DT::dataTableOutput("asym_table"))
      )
    ),

    # ---- TAB 6: Data Explorer ------------------------------------------------
    tabPanel("Data Explorer",
      br(),
      downloadButton("download", "Download filtered data (CSV)", class = "btn-primary"),
      br(), br(),
      DT::dataTableOutput("explorer")
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  load_time <- reactiveVal(Sys.time())
  cache     <- reactiveVal(NULL)

  raw <- reactive({
    input$refresh                         # manual refresh dependency
    invalidateLater(AUTO_REFRESH_MS, session)
    d <- load_raw()
    if (!is.null(d) && nrow(d) > 0) { cache(d); load_time(Sys.time()) }
    cache()
  })

  clean <- reactive({ clean_data(raw()) })

  # Global filters applied everywhere
  fdata <- reactive({
    df <- clean()
    validate(need(!is.null(df) && nrow(df) > 0,
                  "Could not load data from the Google Sheet. Check the internet connection and that the sheet is shared as 'Anyone with the link can view', then press Refresh."))
    if (!is.null(input$f_ptype)) df <- df[df$participantType %in% input$f_ptype, , drop = FALSE]
    if (!is.null(input$f_sex))   df <- df[df$sex %in% input$f_sex, , drop = FALSE]
    if (isTRUE(input$f_completed)) df <- df[df$completed %in% TRUE, , drop = FALSE]
    if (isTRUE(input$f_artefact))  df <- df[!df$artefact, , drop = FALSE]
    validate(need(nrow(df) > 0, "No rows match the current filters."))
    df
  })

  output$status <- renderUI({
    df <- clean()
    if (is.null(df) || nrow(df) == 0)
      return(HTML("<span style='color:#c0392b'>No data loaded.</span>"))
    HTML(paste0("Loaded <b>", nrow(df), "</b> records &middot; <b>",
                length(unique(df$participantId)), "</b> participants &middot; last refresh ",
                format(load_time(), "%H:%M:%S")))
  })

  # ----- TAB 0 / 0b: Sex comparison -------------------------------------------
  opt <- function(x, default = TRUE) if (is.null(x)) default else isTRUE(x)

  sex_data <- reactive({
    d <- prep_sex_data(fdata(),
                       completers  = opt(input$sx_completers),
                       first_trial = opt(input$sx_first))
    validate(need(sum(d$sex == "Female") >= 2 && sum(d$sex == "Male") >= 2,
                  "Need at least two women and two men after filtering. Check the sex and completer options above."))
    d
  })
  sex_eff <- reactive({ sex_effects(sex_data()) })

  output$sex_cards <- renderUI({
    d <- sex_data(); e <- sex_eff()
    card <- function(s) {
      x <- d[d$sex == s, , drop = FALSE]
      div(class = paste("sex-card", if (s == "Female") "f" else "m"),
          div(class = "sym", SEX_SYM[[s]]),
          div(class = "who", SEX_LAB[[s]]),
          div(class = "n", paste0("n = ", nrow(x), " people")),
          div(class = "stat", span("Median pelvic tilt"),
              span(class = "v", paste0(fmt(stats::median(x$meanPelvicTilt, na.rm = TRUE), 1), "\u00B0"))),
          div(class = "stat", span("Median pelvic variability*"),
              span(class = "v", fmt(stats::median(x$pelvicVariability, na.rm = TRUE), 2))),
          div(class = "stat", span("Median stability score"),
              span(class = "v", fmt(stats::median(x$stabilityScore, na.rm = TRUE), 0))))
    }
    tilt <- e[e$var == "meanPelvicTilt", ]; pvr <- e[e$var == "pelvicVariability", ]
    fluidRow(
      column(4, card("Female")),
      column(4, div(class = "sex-bridge",
        div(class = "who", "Where they meet"),
        div(class = "sub", "Distinct groups, one overlapping picture"),
        div(class = "row2", HTML(paste0("<b>Pelvic tilt</b> \u2013 effect \u03B4 = ", sprintf("%+.2f", tilt$delta),
                                        ", p = ", fmt_p(tilt$p)))),
        div(class = "row2", HTML(paste0("<b>Pelvic variability</b> \u2013 effect \u03B4 = ", sprintf("%+.2f", pvr$delta),
                                        ", p = ", fmt_p(pvr$p)))),
        div(class = "row2", HTML(paste0("<b>", sum(e$verdict %in% c("Women higher", "Men higher")),
                                        " of ", nrow(e), "</b> measures differ detectably")))
      )),
      column(4, card("Male"))
    )
  })

  output$sex_plot_var <- renderPlot({
    plot_sex_dots(sex_data(), "pelvicVariability", "Pelvic variability*",
                  "Pelvic variability (log scale)", log_y = TRUE)
  }, res = 110)

  output$sex_plot_tilt <- renderPlot({
    plot_sex_dots(sex_data(), "meanPelvicTilt", "Mean pelvic tilt",
                  "Mean pelvic tilt (degrees)", zoom = TRUE)
  }, res = 110)

  output$sex_insight <- renderUI({
    d <- sex_data(); e <- sex_eff()
    f <- d[d$sex == "Female", ]; m <- d[d$sex == "Male", ]
    n_diff <- sum(e$verdict %in% c("Women higher", "Men higher"))
    tilt <- e[e$var == "meanPelvicTilt", ]; pvr <- e[e$var == "pelvicVariability", ]
    dm <- stats::median(f$meanPelvicTilt, na.rm = TRUE) - stats::median(m$meanPelvicTilt, na.rm = TRUE)
    lead <- if (n_diff == 0)
      "<b>Distinct, yet overlapping.</b> Women and men form recognisable groups, but their distributions overlap heavily and no pelvic or balance measure shows a detectable difference between them."
    else
      paste0("<b>", n_diff, " of ", nrow(e), " measures</b> differ detectably between women and men (see the interval tab). ",
             "Treat this as exploratory: with ", nrow(e), " comparisons, some hits arise by chance.")
    tilt_txt <- paste0("Mean pelvic tilt: the women's median is ", fmt(abs(dm), 1), "\u00B0 ",
                       if (dm >= 0) "higher" else "lower", " than the men's (Mann\u2013Whitney p = ", fmt_p(tilt$p), ")",
                       if (!is.na(tilt$p) && tilt$p >= 0.05) " \u2013 a trend, not a confirmed difference." else ".")
    var_txt <- paste0("Pelvic variability*: medians of ", fmt(stats::median(f$pelvicVariability, na.rm = TRUE), 2),
                      " (women) and ", fmt(stats::median(m$pelvicVariability, na.rm = TRUE), 2),
                      " (men), p = ", fmt_p(pvr$p), ".")
    div(class = "insight", HTML(paste0(lead, "<br/>", tilt_txt, "<br/>", var_txt)))
  })

  output$effect_headline <- renderUI({
    d <- sex_data(); e <- sex_eff()
    n_diff <- sum(e$verdict %in% c("Women higher", "Men higher"))
    h <- if (n_diff == 0) "Every interval crosses zero: no detectable sex difference"
         else paste0(n_diff, " of ", nrow(e), " measures show a detectable sex difference")
    div(class = "insight",
        HTML(paste0("<b style='font-size:16px;'>", h, "</b><br/>",
                    "Women n = ", sum(d$sex == "Female"), " &middot; Men n = ", sum(d$sex == "Male"),
                    ". The two sexes are shown as complementary halves: left of zero men score higher, ",
                    "right of zero women score higher; the closer to zero, the more the groups overlap.")))
  })

  output$effect_plot <- renderPlot({ plot_effects(sex_eff()) }, res = 110)

  output$effect_table <- DT::renderDataTable({
    e <- sex_eff()
    out <- data.frame(Measure = e$measure, `Women n` = e$nF, `Men n` = e$nM,
                      `Cliff's delta` = round(e$delta, 2),
                      `95% CI` = sprintf("[%+.2f, %+.2f]", e$lo, e$hi),
                      `p (uncorrected)` = fmt_p(e$p), Verdict = e$verdict,
                      check.names = FALSE, stringsAsFactors = FALSE)
    DT::datatable(out, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  # ----- TAB 1: Scatter -------------------------------------------------------
  plot_title <- eventReactive(input$update_title, toTitleCase(input$plot_title),
                              ignoreNULL = FALSE)

  scatter_df <- reactive({
    df <- fdata(); x <- input$sx; y <- input$sy
    df[!is.na(df[[x]]) & !is.na(df[[y]]), , drop = FALSE]
  })

  output$scatter <- renderPlot({
    d <- scatter_df(); x <- input$sx; y <- input$sy; z <- input$sz
    validate(need(nrow(d) > 0, "No non-missing points for the chosen axes."))
    p <- ggplot(d, aes(x = .data[[x]], y = .data[[y]], colour = .data[[z]])) +
      geom_point(alpha = input$salpha, size = input$ssize) +
      sex_aware_colour(z) +
      labs(x = pretty_lab(x), y = pretty_lab(y), colour = pretty_lab(z),
           title = if (isTRUE(nzchar(plot_title()))) plot_title() else NULL) +
      theme_sex(grid_x = TRUE)

    if (input$smooth == "lm")
      p <- p + geom_smooth(aes(group = 1), method = "lm", formula = y ~ x,
                           se = TRUE, colour = INK, linewidth = 0.7)
    if (input$smooth == "loess")
      p <- p + geom_smooth(aes(group = 1), method = "loess", formula = y ~ x,
                           se = TRUE, colour = INK, linewidth = 0.7)

    if (isTRUE(input$ageref) && x == "age" && y == "duration") {
      ref <- data.frame(age = c(28, 44, 54, 64, 74, 84),
                        ref = c(44.7, 41.9, 41.2, 32.1, 21.5, 9.4))
      p <- p +
        geom_step(data = ref, aes(x = age, y = ref), inherit.aes = FALSE,
                  linetype = "dashed", colour = "#f08a24") +
        geom_point(data = ref, aes(x = age, y = ref), inherit.aes = FALSE,
                   colour = "#f08a24", shape = 4, size = 2.5)
    }
    p
  })

  output$hover_info <- renderUI({
    hv <- input$scatter_hover; if (is.null(hv)) return(NULL)
    d <- scatter_df(); x <- input$sx; y <- input$sy; z <- input$sz
    if (nrow(d) == 0) return(NULL)
    pt <- nearPoints(d, hv, xvar = x, yvar = y, threshold = 12, maxpoints = 1)
    if (nrow(pt) == 0) return(NULL)
    style <- paste0(
      "position:absolute; z-index:200; pointer-events:none; ",
      "background-color: rgba(255,255,255,0.96); border:1px solid #e4dff0; ",
      "border-left:5px solid #e0418f; box-shadow:0 6px 16px rgba(27,24,64,.18); color:#1b1840; ",
      "border-radius:10px; padding:6px 10px; font-size:12px; font-weight:600; ",
      "left:", hv$coords_css$x + 12, "px; top:", hv$coords_css$y + 12, "px;")
    div(style = style, HTML(paste0(
      "<b>", htmltools::htmlEscape(as.character(pt$participantId)), "</b><br/>",
      pretty_lab(x), ": ", signif(pt[[x]], 3), "<br/>",
      pretty_lab(y), ": ", signif(pt[[y]], 3), "<br/>",
      pretty_lab(z), ": ", htmltools::htmlEscape(as.character(pt[[z]]))
    )))
  })

  selected_pts <- reactive({
    d <- scatter_df()
    brushedPoints(d, input$scatter_brush, xvar = input$sx, yvar = input$sy)
  })

  output$sel_table <- DT::renderDataTable({
    sel <- selected_pts()
    validate(need(nrow(sel) > 0,
                  "Drag a rectangle over the plot to list the selected points here."))
    cols <- unique(c("participantId","age","sex","participantType","supportLeg",
                     "trialNumber","duration","stabilityScore", input$sx, input$sy))
    cols <- intersect(cols, names(sel))
    DT::datatable(sel[, cols, drop = FALSE], rownames = FALSE,
                  options = list(pageLength = 5, scrollX = TRUE, dom = "tip"))
  })

  output$sel_stats <- renderUI({
    sel <- selected_pts()
    if (nrow(sel) == 0) return(HTML("<em>No points selected.</em>"))
    x <- input$sx; y <- input$sy; xv <- sel[[x]]; yv <- sel[[y]]
    desc <- function(v) c(mean(v, na.rm = TRUE), sd(v, na.rm = TRUE),
                          median(v, na.rm = TRUE), min(v, na.rm = TRUE), max(v, na.rm = TRUE))
    dx <- desc(xv); dy <- desc(yv)
    ok <- sum(!is.na(xv) & !is.na(yv))
    corr <- if (ok >= 3 && sd(xv, na.rm = TRUE) > 0 && sd(yv, na.rm = TRUE) > 0) {
      pr <- suppressWarnings(cor(xv, yv, use = "complete.obs", method = "pearson"))
      sp <- suppressWarnings(cor(xv, yv, use = "complete.obs", method = "spearman"))
      paste0("Pearson r = ", fmt(pr), " &nbsp;|&nbsp; Spearman &rho; = ", fmt(sp),
             " (n=", ok, ")")
    } else "Correlation needs &ge;3 complete pairs with variance."
    HTML(paste0(
      "<b>Selected: n = ", nrow(sel), "</b>",
      "<table style='width:100%;font-size:13px;margin-top:6px;'>",
      "<tr><th></th><th>", pretty_lab(x), "</th><th>", pretty_lab(y), "</th></tr>",
      "<tr><td>Mean</td><td>",   fmt(dx[1]), "</td><td>", fmt(dy[1]), "</td></tr>",
      "<tr><td>SD</td><td>",     fmt(dx[2]), "</td><td>", fmt(dy[2]), "</td></tr>",
      "<tr><td>Median</td><td>", fmt(dx[3]), "</td><td>", fmt(dy[3]), "</td></tr>",
      "<tr><td>Min</td><td>",    fmt(dx[4]), "</td><td>", fmt(dy[4]), "</td></tr>",
      "<tr><td>Max</td><td>",    fmt(dx[5]), "</td><td>", fmt(dy[5]), "</td></tr>",
      "</table><br/>", corr))
  })

  output$fit_summary <- renderUI({
    if (input$smooth != "lm") return(NULL)
    d <- scatter_df(); x <- input$sx; y <- input$sy
    if (nrow(d) < 3 || sd(d[[x]], na.rm = TRUE) == 0) return(NULL)
    m <- lm(d[[y]] ~ d[[x]]); s <- summary(m); co <- coef(s)
    HTML(paste0(
      "<div class='insight'><b>Linear fit (all shown points):</b> ",
      pretty_lab(y), " = ", fmt(co[1,1]), " + ", fmt(co[2,1], 3), " &times; ", pretty_lab(x),
      " &nbsp;|&nbsp; R&sup2; = ", fmt(s$r.squared, 3),
      " &nbsp;|&nbsp; slope p = ", fmt(co[2,4], 4),
      " &nbsp;|&nbsp; n = ", nrow(d), "</div>"))
  })

  # ----- TAB 2: Distributions -------------------------------------------------
  output$kpi_n     <- renderText({ nrow(fdata()) })
  output$kpi_part  <- renderText({ length(unique(fdata()$participantId)) })
  output$kpi_dur   <- renderText({ fmt(mean(fdata()$duration, na.rm = TRUE), 1) })
  output$kpi_score <- renderText({ fmt(mean(fdata()$stabilityScore, na.rm = TRUE), 1) })

  output$hist <- renderPlot({
    df <- fdata(); v <- input$dvar
    d <- df[!is.na(df[[v]]), , drop = FALSE]
    validate(need(nrow(d) > 0, "No data for this variable."))
    p <- ggplot(d, aes(x = .data[[v]])) +
      geom_histogram(aes(y = after_stat(density)), bins = input$dbins,
                     fill = VIOLET, colour = "white", alpha = 0.80) +
      labs(x = pretty_lab(v), y = "Density",
           title = paste("Distribution of", pretty_lab(v))) +
      theme_sex() +
      geom_vline(xintercept = mean(d[[v]]), colour = INK, linetype = "dotted", linewidth = 0.8)
    if (isTRUE(input$ddens)) p <- p + geom_density(colour = SEX_COL[["Female"]], linewidth = 1.1)
    if (isTRUE(input$dnorm)) {
      m <- mean(d[[v]]); s <- sd(d[[v]])
      if (is.finite(s) && s > 0)
        p <- p + stat_function(fun = dnorm, args = list(mean = m, sd = s),
                               colour = SEX_COL[["Male"]], linetype = "dashed", linewidth = 0.9)
    }
    p
  })

  output$dist_stats <- renderUI({
    df <- fdata(); v <- input$dvar; x <- df[[v]]; x <- x[!is.na(x)]
    if (length(x) < 1) return(HTML("<em>No data.</em>"))
    sk <- if (sd(x) > 0) mean((x - mean(x))^3) / sd(x)^3 else NA
    sh <- if (length(x) >= 3 && length(x) <= 5000 && sd(x) > 0)
      tryCatch(shapiro.test(x)$p.value, error = function(e) NA) else NA
    HTML(paste0(
      "<table style='font-size:13px;'>",
      "<tr><td style='padding-right:16px;'>n</td><td>", length(x), "</td>",
      "<td style='padding-left:24px;padding-right:16px;'>Median</td><td>", fmt(median(x)), "</td></tr>",
      "<tr><td>Mean</td><td>", fmt(mean(x)), "</td>",
      "<td style='padding-left:24px;'>IQR</td><td>", fmt(IQR(x)), "</td></tr>",
      "<tr><td>SD</td><td>", fmt(sd(x)), "</td>",
      "<td style='padding-left:24px;'>Min / Max</td><td>", fmt(min(x)), " / ", fmt(max(x)), "</td></tr>",
      "<tr><td>Skewness</td><td>", fmt(sk), "</td>",
      "<td style='padding-left:24px;'>Shapiro-Wilk p</td><td>", fmt(sh, 4), "</td></tr>",
      "</table>",
      "<div class='small-note'>Shapiro-Wilk p &lt; 0.05 suggests non-normality &rarr; prefer rank-based tests.</div>"))
  })

  # ----- TAB 3: Group Comparisons ---------------------------------------------
  group_df <- reactive({
    df <- fdata(); g <- input$gcat; y <- input$gnum
    df[!is.na(df[[y]]) & !is.na(df[[g]]), , drop = FALSE]
  })

  output$box <- renderPlot({
    d <- group_df(); g <- input$gcat; y <- input$gnum
    validate(need(nrow(d) > 0, "No data for this combination."))
    p <- ggplot(d, aes(x = .data[[g]], y = .data[[y]], fill = .data[[g]]))
    if (input$gtype == "Box")
      p <- p + geom_boxplot(alpha = 0.7, outlier.alpha = 0.5)
    else
      p <- p + geom_violin(alpha = 0.6, scale = "width") +
               geom_boxplot(width = 0.12, alpha = 0.85, outlier.shape = NA)
    p + geom_jitter(width = 0.15, alpha = 0.35, size = 1.2, colour = INK) +
      sex_aware_fill(g) +
      labs(x = pretty_lab(g), y = pretty_lab(y),
           title = paste(pretty_lab(y), "by", pretty_lab(g))) +
      theme_sex() +
      theme(legend.position = "none")
  })

  output$group_stats <- DT::renderDataTable({
    d <- group_df(); g <- input$gcat; y <- input$gnum
    validate(need(nrow(d) > 0, "No data."))
    parts <- split(d[[y]], droplevels(factor(d[[g]])))
    agg <- do.call(rbind, lapply(names(parts), function(k) {
      v <- parts[[k]]; v <- v[!is.na(v)]
      data.frame(Group = k, n = length(v),
                 Mean = mean(v), SD = sd(v), Min = min(v),
                 Q1 = quantile(v, .25), Median = median(v),
                 Q3 = quantile(v, .75), Max = max(v), IQR = IQR(v),
                 row.names = NULL)
    }))
    num <- sapply(agg, is.numeric); agg[num] <- lapply(agg[num], round, 2)
    DT::datatable(agg, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$group_test <- renderUI({
    d <- group_df(); g <- droplevels(factor(d[[input$gcat]])); yv <- d[[input$gnum]]
    if (nlevels(g) < 2) return(HTML("<em>Need &ge;2 groups.</em>"))
    aov_p <- tryCatch(summary(aov(yv ~ g))[[1]][["Pr(>F)"]][1], error = function(e) NA)
    kw_p  <- tryCatch(kruskal.test(yv ~ g)$p.value, error = function(e) NA)
    HTML(paste0(
      "One-way ANOVA p = <b>", fmt(aov_p, 4), "</b> (assumes normality + equal variance)<br/>",
      "Kruskal-Wallis p = <b>", fmt(kw_p, 4), "</b> (rank-based, no normality assumption)",
      "<div class='small-note'>With repeated participants, treat these as exploratory; ",
      "use mixed models for confirmatory work.</div>"))
  })

  # ----- TAB 4: Correlations --------------------------------------------------
  cor_mat <- reactive({
    df <- fdata(); vars <- input$cvars
    validate(need(length(vars) >= 2, "Select at least two metrics."))
    vars <- intersect(vars, names(df))
    cor(df[, vars, drop = FALSE], use = "pairwise.complete.obs", method = input$cmethod)
  })

  output$corr <- renderPlot({
    m <- cor_mat()
    cm <- as.data.frame(as.table(m)); names(cm) <- c("V1", "V2", "r")
    ord <- rownames(m)
    cm$V1 <- factor(cm$V1, levels = ord); cm$V2 <- factor(cm$V2, levels = rev(ord))
    ggplot(cm, aes(V1, V2, fill = r)) +
      geom_tile(colour = "white", linewidth = 1) +
      geom_text(aes(label = formatC(r, format = "f", digits = 2)), size = 3.2, colour = INK) +
      scale_fill_gradient2(low = SEX_COL[["Male"]], mid = "white", high = SEX_COL[["Female"]],
                           midpoint = 0, limits = c(-1, 1)) +
      scale_x_discrete(labels = function(z) vapply(z, pretty_lab, character(1))) +
      scale_y_discrete(labels = function(z) vapply(z, pretty_lab, character(1))) +
      labs(x = NULL, y = NULL, fill = paste0(toTitleCase(input$cmethod), " r"),
           title = "Correlation matrix (pairwise complete)") +
      theme_sex(12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major.y = element_blank())
  })

  output$corr_table <- DT::renderDataTable({
    m <- round(cor_mat(), 3)
    out <- data.frame(Metric = vapply(rownames(m), pretty_lab, character(1)), m,
                      check.names = FALSE, row.names = NULL)
    DT::datatable(out, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  # ----- TAB 5: Left–Right Asymmetry ------------------------------------------
  asym <- reactive({
    df <- fdata()
    d <- df[!is.na(df$duration) & df$supportLeg %in% c("Left","Right"), , drop = FALSE]
    validate(need(nrow(d) > 0, "No duration data available."))
    agg <- aggregate(duration ~ participantId + supportLeg, data = d, FUN = max)
    L <- agg[agg$supportLeg == "Left",  c("participantId","duration")]; names(L)[2] <- "Left"
    R <- agg[agg$supportLeg == "Right", c("participantId","duration")]; names(R)[2] <- "Right"
    w <- merge(L, R, by = "participantId")
    if (nrow(w) > 0) {
      w$Diff   <- round(w$Left - w$Right, 1)
      w$AbsPct <- round(100 * abs(w$Diff) / ((w$Left + w$Right) / 2), 1)
    }
    w
  })

  output$asym_plot <- renderPlot({
    w <- asym()
    validate(need(nrow(w) > 0,
                  "No participant has both a left and a right record after filtering."))
    lim <- range(c(w$Left, w$Right), na.rm = TRUE)
    ggplot(w, aes(Left, Right)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = INK2) +
      geom_point(aes(colour = AbsPct), size = 3, alpha = 0.85) +
      scale_colour_gradient(low = SEX_COL[["Male"]], high = SEX_COL[["Female"]], name = "|asym| %") +
      coord_equal(xlim = lim, ylim = lim) +
      labs(x = "Best left-support duration (s)",
           y = "Best right-support duration (s)",
           title = "Left vs right best single-leg duration") +
      theme_sex(grid_x = TRUE)
  })

  output$asym_table <- DT::renderDataTable({
    w <- asym()
    validate(need(nrow(w) > 0, "No paired participants."))
    DT::datatable(w, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  output$asym_test <- renderUI({
    w <- asym()
    if (nrow(w) < 2) return(HTML("<em>Need &ge;2 paired participants for a test.</em>"))
    wt <- tryCatch(wilcox.test(w$Left, w$Right, paired = TRUE), error = function(e) NULL)
    tt <- tryCatch(t.test(w$Left, w$Right, paired = TRUE), error = function(e) NULL)
    HTML(paste0(
      "Paired participants: <b>", nrow(w), "</b><br/>",
      "Mean L-R difference: <b>", fmt(mean(w$Diff)), " s</b><br/>",
      "Mean |asymmetry|: <b>", fmt(mean(w$AbsPct), 1), " %</b><br/>",
      "Wilcoxon signed-rank p = <b>", if (is.null(wt)) "-" else fmt(wt$p.value, 4), "</b><br/>",
      "Paired t-test p = <b>", if (is.null(tt)) "-" else fmt(tt$p.value, 4), "</b>",
      "<div class='small-note'>Duration is capped at the chosen max, so ceiling effects ",
      "can mask true asymmetry; favour the rank-based test.</div>"))
  })

  # ----- TAB 6: Data Explorer -------------------------------------------------
  output$explorer <- DT::renderDataTable({
    DT::datatable(fdata(), rownames = FALSE, filter = "top",
                  options = list(pageLength = 15, scrollX = TRUE))
  })

  output$download <- downloadHandler(
    filename = function() paste0("balancevision_", Sys.Date(), ".csv"),
    content  = function(file) utils::write.csv(fdata(), file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)
