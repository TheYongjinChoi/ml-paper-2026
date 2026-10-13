# ==============================================================
# 11_main_figures.R
# ==============================================================

source("code/01_config.R")
library(dplyr); library(tidyr); library(plotly)

df.imms <- df.imms |>
  mutate(
    region1 = ifelse(
      ctryTxt == "USA",
      paste0("USA_", as.character(haven::as_factor(REGION_US_STATES))),
      paste0(ctryAbb, "_", as.character(haven::as_factor(ADM2)))
    ),
    region1 = ifelse(grepl("_NA$|^NA_|_$", region1), NA_character_, region1)
  )

outcome_models <- readRDS(file.path(DIR_OUT, "outcome_models.rds"))
res_all <- outcome_models$results
res <- res_all |> filter(domain == "confidence")
res_vx <- res_all |> filter(domain == "uptake")
sc  <- readRDS(F_SCORES)
ext <- readRDS(file.path(DIR_OUT, "extension_results.rds"))
stopifnot(exists("df.imms"))

OUTCOMES_FIG <- outcome_models$meta$outcomes_conf
if (is.null(OUTCOMES_FIG)) OUTCOMES_FIG <- OUTCOMES

TERM_COL <- c(Mf = "#D85A30", Mr = "#1D9E75")
OUT_ORDER <- unname(OUTCOMES_FIG)

PLOT_FONT_SIZE  <- 13
AXIS_FONT_SIZE  <- 14
PANEL_FONT_SIZE <- 16
LEGEND_FONT_SIZE <- 13
MARKER_SIZE     <- 11


# ══════════════════════════════════════════════════════════════
# Fig 1. Association between headline detection ability and vaccine confidence, by country
# ══════════════════════════════════════════════════════════════

fp <- res |>
  filter(spec == "mundlak", term %in% c("Mf", "Mr")) |>
  mutate(
    outcome_label = factor(outcome_label, levels = rev(OUT_ORDER)),
    term_lab = ifelse(
      term == "Mf",
      "Fake news detection",
      "Real news detection"
    )
  )


plot_forest <- function(cty, show_legend = FALSE) {

  dd <- fp |>
    filter(country == cty) |>
    mutate(
      y_base = as.numeric(outcome_label),

      y_pos = y_base + ifelse(term == "Mf", 0.11, -0.11)
    )

  p <- plot_ly()

  for (tr in c("Mf", "Mr")) {

    di <- dd |> filter(term == tr)

    col <- TERM_COL[[tr]]
    lab <- unique(di$term_lab)[1]

    # 95% CI
    p <- p |>
      add_segments(
        data = di,
        x = ~or_lo,
        xend = ~or_hi,
        y = ~y_pos,
        yend = ~y_pos,
        line = list(
          color = "#B8B8B8",
          width = 2.5
        ),
        showlegend = FALSE,
        hoverinfo = "skip"
      )

    # Left CI spike
    p <- p |>
      add_segments(
        data = di,
        x = ~or_lo,
        xend = ~or_lo,
        y = ~y_pos - 0.045,
        yend = ~y_pos + 0.045,
        line = list(
          color = "#B8B8B8",
          width = 2
        ),
        showlegend = FALSE,
        hoverinfo = "skip"
      )

    # Right CI spike
    p <- p |>
      add_segments(
        data = di,
        x = ~or_hi,
        xend = ~or_hi,
        y = ~y_pos - 0.045,
        yend = ~y_pos + 0.045,
        line = list(
          color = "#B8B8B8",
          width = 2
        ),
        showlegend = FALSE,
        hoverinfo = "skip"
      )

    # OR point estimate
    p <- p |>
      add_markers(
        data = di,
        x = ~or,
        y = ~y_pos,
        name = lab,
        legendgroup = tr,
        showlegend = show_legend,
        marker = list(
          color = col,
          size = MARKER_SIZE,
          line = list(
            color = "white",
            width = 1
          )
        ),
        hovertemplate = ~paste0(
          "<b>", term_lab, "</b><br>",
          outcome_label,
          "<br>OR = ", sprintf("%.2f", or),
          " [", sprintf("%.2f", or_lo),
          ", ", sprintf("%.2f", or_hi), "]",
          "<br>n = ", n,
          "<extra></extra>"
        )
      )
  }

  p |>
    layout(
      font = list(size = PLOT_FONT_SIZE),

      xaxis = list(
        title = "Odds ratio per SD",
        type = "log",
        tickvals = c(0.8, 1, 1.25, 1.6, 2, 2.5, 3),
        ticktext = c("0.8", "1", "1.25", "1.6", "2", "2.5", "3"),
        tickfont = list(size = PLOT_FONT_SIZE),
        gridcolor = "#EEEEEE",
        zeroline = FALSE
      ),

      yaxis = list(
        title = "",
        tickvals = seq_along(levels(fp$outcome_label)),
        ticktext = levels(fp$outcome_label),
        tickfont = list(size = PLOT_FONT_SIZE),
        range = c(
          0.5,
          length(levels(fp$outcome_label)) + 0.5
        ),
        gridcolor = "#F3F3F3",
        zeroline = FALSE
      ),

      shapes = list(
        list(
          type = "line",
          x0 = 1,
          x1 = 1,
          y0 = 0.5,
          y1 = length(levels(fp$outcome_label)) + 0.5,
          line = list(
            dash = "dot",
            color = "#999999",
            width = 1.5
          )
        )
      )
    )
}


fig_forest <- plotly::subplot(
  plot_forest("USA", TRUE),
  plot_forest("UK"),
  plot_forest("Canada"),
  nrows = 1,
  shareY = TRUE,
  shareX = TRUE,
  titleX = TRUE,
  margin = 0.025
) |>
  layout(
    font = list(size = PLOT_FONT_SIZE),

    legend = list(
      orientation = "h",
      x = 0,
      y = -0.18,
      font = list(size = LEGEND_FONT_SIZE)
    ),

    annotations = list(
      list(
        text = "<b>USA</b>",
        x = .16, y = 1.04,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = PANEL_FONT_SIZE)
      ),
      list(
        text = "<b>UK</b>",
        x = .50, y = 1.04,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = PANEL_FONT_SIZE)
      ),
      list(
        text = "<b>Canada</b>",
        x = .84, y = 1.04,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = PANEL_FONT_SIZE)
      )
    )
  )

fig_forest


# ══════════════════════════════════════════════════════════════
# Fig 2. Predicted probability of vaccine confidence across misinformation detection ability
# ══════════════════════════════════════════════════════════════

MF_GRID  <- seq(2, 10, by = 0.5)
N_DRAWS_FIG <- 100

covars <- setdiff(
  intersect(COVARS_BASE, names(df.imms)),
  c("bAccess", "bAfford")
)

pred_curve <- function(cty, outcome) {

  s   <- sc$scores[[cty]]
  M   <- min(N_DRAWS_FIG, nrow(s$Mf))
  idx <- round(seq(1, nrow(s$Mf), length.out = M))

  base <- df.imms |>
    filter(ctryTxt == cty, LINE_NO %in% s$person_id) |>
    arrange(match(LINE_NO, s$person_id))
  stopifnot(identical(base$LINE_NO, s$person_id))

  ok <- complete.cases(base[c("dWEIGHT", "region1", covars)]) &
        !is.na(base[[outcome]])
  if (sum(ok) < 100) return(NULL)

  dat0 <- base[ok, ]
  dat0$region1 <- factor(dat0$region1)

  f <- as.formula(paste(outcome, "~ Mf + Mr + Mf_bar + Mr_bar +",
                        paste(covars, collapse = " + ")))

  curves <- matrix(NA_real_, M, length(MF_GRID))
  for (i in seq_len(M)) {
    dat <- dat0
    dat$Mf <- s$Mf[idx[i], ok]
    dat$Mr <- s$Mr[idx[i], ok]
    dat$Mf_bar <- ave(dat$Mf, dat$region1)
    dat$Mr_bar <- ave(dat$Mr, dat$region1)

    m  <- glm(f, data = dat, family = binomial, weights = dat$dWEIGHT)
    X  <- model.matrix(m)
    bt <- coef(m)
    jf <- which(colnames(X) == "Mf")
    for (g in seq_along(MF_GRID)) {
      X[, jf] <- MF_GRID[g]
      curves[i, g] <- weighted.mean(plogis(as.vector(X %*% bt)), dat$dWEIGHT)
    }
  }

  data.frame(country = cty, outcome = outcome,
             outcome_label = unname(OUTCOMES_FIG[outcome]),
             Mf = MF_GRID,
             p = colMeans(curves),
             lo = apply(curves, 2, quantile, 0.025),
             hi = apply(curves, 2, quantile, 0.975),
             row.names = NULL)
}

message("Calculating predicted probability curves")
curves <- do.call(rbind, lapply(CTRY, function(cty) {
  message(sprintf("  [%s]", cty))
  do.call(rbind, lapply(names(OUTCOMES_FIG), function(o) pred_curve(cty, o)))
}))

OUT_COL <- setNames(
  c("#D85A30", "#1D9E75", "#7F77DD", "#C99A2E", "#4A9BC4"),
  OUT_ORDER
)

plot_curve <- function(cty, show_legend = FALSE) {
  dd <- curves |> filter(country == cty)
  p <- plot_ly()
  for (ol in OUT_ORDER) {
    di <- dd |> filter(outcome_label == ol)
    if (nrow(di) == 0) next
    col <- OUT_COL[[ol]]
    p <- p |>
      add_ribbons(data = di, x = ~Mf, ymin = ~lo, ymax = ~hi,
                  fillcolor = plotly::toRGB(col, 0.15),
                  line = list(width = 0),
                  showlegend = FALSE, hoverinfo = "skip",
                  name = ol, legendgroup = ol) |>
      add_lines(data = di, x = ~Mf, y = ~p,
                line = list(color = col, width = 2),
                name = ol, legendgroup = ol, showlegend = show_legend,
                hovertemplate = ~paste0(
                  "<b>", outcome_label, "</b><br>Mf = ", sprintf("%.1f", Mf),
                  "<br>P = ", sprintf("%.3f", p), "<extra></extra>"))
  }
  p |> layout(
    xaxis = list(title = "Fake news detection score (0–10)"),
    yaxis = list(title = "P(agree)", tickformat = ".0%"))
}

fig_curve <- plotly::subplot(
  plot_curve("USA", TRUE), plot_curve("UK"), plot_curve("Canada"),
  nrows = 1, shareY = TRUE, shareX = TRUE, titleX = TRUE, titleY = TRUE,
  margin = 0.02
) |>
  layout(
    legend = list(orientation = "h", x = 0, y = -0.18),
    annotations = list(
      list(text = "USA", x = .16, y = 1.05, xref = "paper", yref = "paper",
           showarrow = FALSE, font = list(size = 18)),
      list(text = "UK", x = .50, y = 1.05, xref = "paper", yref = "paper",
           showarrow = FALSE, font = list(size = 18)),
      list(text = "Canada", x = .84, y = 1.05, xref = "paper", yref = "paper",
           showarrow = FALSE, font = list(size = 18))))

fig_curve

delta_tab <- curves |>
  group_by(country, outcome_label) |>
  summarise(
    p_at_5 = p[which.min(abs(Mf - 5))],
    p_at_6 = p[which.min(abs(Mf - 6))],
    pp_gain = 100 * (p_at_6 - p_at_5),
    p_at_2 = p[which.min(abs(Mf - 2))],
    p_at_10 = p[which.min(abs(Mf - 10))],
    pp_range = 100 * (p_at_10 - p_at_2),
    .groups = "drop")

message("\nProbability change from identifying one additional headline (5 -> 6 points)")
print(delta_tab |> dplyr::select(country, outcome_label, p_at_5, p_at_6, pp_gain,
                          pp_range),
      digits = 3, row.names = FALSE)

# ══════════════════════════════════════════════════════════════
# Fig X. IRT vs sum score
# ══════════════════════════════════════════════════════════════

cmp <- ext$sumscore |>
  filter(term == "Mf") |>
  dplyr::select(country, outcome, est_sum = est) |>
  left_join(
    res |> filter(spec == "mundlak", term == "Mf") |>
      dplyr::select(country, outcome, outcome_label, est_irt = est, lambda),
    by = c("country", "outcome")
  ) |>
  mutate(attenuation_pct = 100 * (1 - est_sum / est_irt),
         outcome_label = factor(outcome_label, levels = rev(OUT_ORDER)))

plot_atten <- function(cty, show_legend = FALSE) {
  dd <- cmp |> filter(country == cty)
  plot_ly(dd) |>
    add_segments(x = ~est_sum, xend = ~est_irt,
                 y = ~outcome_label, yend = ~outcome_label,
                 line = list(color = "#CCC", width = 3),
                 showlegend = FALSE, hoverinfo = "skip") |>
    add_markers(x = ~est_sum, y = ~outcome_label, name = "Sum score",
                marker = list(color = "#888780", size = 9),
                legendgroup = "sum", showlegend = show_legend,
                hovertemplate = ~paste0("Sum score: ", sprintf("%.3f", est_sum),
                                        "<extra></extra>")) |>
    add_markers(x = ~est_irt, y = ~outcome_label, name = "IRT expected score",
                marker = list(color = "#D85A30", size = 9),
                legendgroup = "irt", showlegend = show_legend,
                hovertemplate = ~paste0("IRT: ", sprintf("%.3f", est_irt),
                                        "<br>attenuation: ",
                                        sprintf("%.1f%%", attenuation_pct),
                                        "<br>lambda = ", sprintf("%.2f", lambda),
                                        "<extra></extra>")) |>
    layout(xaxis = list(title = "Log-odds per SD"),
           yaxis = list(title = ""))
}

fig_atten <- plotly::subplot(
  plot_atten("USA", TRUE), plot_atten("UK"), plot_atten("Canada"),
  nrows = 1, shareY = TRUE, shareX = TRUE, titleX = TRUE, margin = 0.02
) |>
  layout(
    title = "Coefficient on fake news detection: IRT vs sum score",
    legend = list(orientation = "h", x = 0, y = -0.18),
    annotations = list(
      list(text = "USA", x = .16, y = 1.05, xref = "paper", yref = "paper",
           showarrow = FALSE, font = list(size = 14)),
      list(text = "UK", x = .50, y = 1.05, xref = "paper", yref = "paper",
           showarrow = FALSE, font = list(size = 14)),
      list(text = "Canada", x = .84, y = 1.05, xref = "paper", yref = "paper",
           showarrow = FALSE, font = list(size = 14))))

fig_atten

message("\nAttenuation summary")
print(cmp |> dplyr::select(country, outcome_label, est_sum, est_irt,
                    attenuation_pct, lambda),
      digits = 3, row.names = FALSE)

# ══════════════════════════════════════════════════════════════
# Fig X. Association between detection ability and MCV uptake
# ══════════════════════════════════════════════════════════════

fp_vx <- res_vx |>
  filter(spec == "mundlak", term %in% c("Mf", "Mr")) |>
  mutate(
    country = factor(country, levels = rev(CTRY)),
    term_lab = ifelse(
      term == "Mf",
      "Fake news detection",
      "Real news detection"
    ),
    y_pos = as.numeric(country) + ifelse(term == "Mf", 0.11, -0.11)
  )

fig_vxmmr <- {

  p <- plot_ly()

  for (tr in c("Mf", "Mr")) {

    di  <- fp_vx |> filter(term == tr)
    col <- TERM_COL[[tr]]
    lab <- unique(di$term_lab)[1]

    p <- p |>
      add_segments(
        data = di,
        x = ~or_lo, xend = ~or_hi,
        y = ~y_pos, yend = ~y_pos,
        line = list(color = "#B8B8B8", width = 2.5),
        showlegend = FALSE, hoverinfo = "skip"
      ) |>
      add_segments(
        data = di,
        x = ~or_lo, xend = ~or_lo,
        y = ~y_pos - 0.045, yend = ~y_pos + 0.045,
        line = list(color = "#B8B8B8", width = 2),
        showlegend = FALSE, hoverinfo = "skip"
      ) |>
      add_segments(
        data = di,
        x = ~or_hi, xend = ~or_hi,
        y = ~y_pos - 0.045, yend = ~y_pos + 0.045,
        line = list(color = "#B8B8B8", width = 2),
        showlegend = FALSE, hoverinfo = "skip"
      ) |>
      add_markers(
        data = di,
        x = ~or, y = ~y_pos,
        name = lab, legendgroup = tr, showlegend = TRUE,
        marker = list(
          color = col, size = MARKER_SIZE,
          line = list(color = "white", width = 1)
        ),
        hovertemplate = ~paste0(
          "<b>", term_lab, "</b><br>", country,
          "<br>OR = ", sprintf("%.2f", or),
          " [", sprintf("%.2f", or_lo), ", ", sprintf("%.2f", or_hi), "]",
          "<br>n = ", n, " (unvaccinated ", n_neg, ")",
          "<extra></extra>"
        )
      )
  }

  p |>
    layout(
      font = list(size = PLOT_FONT_SIZE),
      xaxis = list(
        title = "Odds ratio per SD (MCV vaccination)",
        type = "log",
        tickvals = c(0.8, 1, 1.25, 1.6, 2, 2.5, 3),
        ticktext = c("0.8", "1", "1.25", "1.6", "2", "2.5", "3"),
        tickfont = list(size = PLOT_FONT_SIZE),
        gridcolor = "#EEEEEE", zeroline = FALSE
      ),
      yaxis = list(
        title = "",
        tickvals = seq_along(levels(fp_vx$country)),
        ticktext = levels(fp_vx$country),
        tickfont = list(size = AXIS_FONT_SIZE),
        range = c(0.5, length(levels(fp_vx$country)) + 0.5),
        gridcolor = "#F3F3F3", zeroline = FALSE
      ),
      legend = list(
        orientation = "h", x = 0, y = -0.22,
        font = list(size = LEGEND_FONT_SIZE)
      ),
      shapes = list(
        list(
          type = "line", x0 = 1, x1 = 1,
          y0 = 0.5, y1 = length(levels(fp_vx$country)) + 0.5,
          line = list(dash = "dot", color = "#999999", width = 1.5)
        )
      )
    )
}

fig_vxmmr

message("\nMMR vaccination — odds ratios (OR > 1 supports the hypothesis)")
print(
  fp_vx |>
    dplyr::select(country, term_lab, or, or_lo, or_hi, lambda, n, n_neg, n_region),
  digits = 3, row.names = FALSE
)

# ── Save ──────────────────────────────────────────────────────

plotly::save_image(
  fig_forest,
  "output/fig_forest.svg",
  width = 1200,
  height = 700
)

plotly::save_image(
  fig_curve,
  "output/fig_curve.svg",
  width = 1200,
  height = 700
)

plotly::save_image(
  fig_vxmmr,
  "output/fig_vxmmr.svg",
  width = 1200,
  height = 700
)

F_FIG <- file.path(DIR_OUT, "main_figures.rds")
saveRDS(list(fig_forest = fig_forest, fig_curve = fig_curve,
             fig_atten = fig_atten, fig_vxmmr = fig_vxmmr,
             curves = curves, delta_tab = delta_tab, cmp = cmp,
             meta = list(date = Sys.time(), n_draws_fig = N_DRAWS_FIG,
                         mf_grid = MF_GRID)),
        F_FIG)