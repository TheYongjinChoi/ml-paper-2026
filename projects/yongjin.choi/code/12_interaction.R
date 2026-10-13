# ==============================================================
# 12_h4_interaction.R
# ==============================================================

source("code/01_config.R")

library(dplyr)
library(tidyr)
library(lme4)
library(future.apply)
library(plotly)

if (!exists("N_CORES")) N_CORES <- max(1, parallel::detectCores() - 1)
plan(multisession, workers = N_CORES)

N_DRAWS_H4 <- min(100, N_DRAWS_MI)
PO_GRID    <- 1:6
F_H4       <- file.path(DIR_OUT, "h4_interaction.rds")

sc     <- readRDS(F_SCORES)
scores <- sc$scores
stopifnot(exists("df.imms"))

z <- function(x) as.vector(scale(x))

# ── 1. Regional variable (same as in 05) ──────────────────────

if (!"region1" %in% names(df.imms)) {
  df.imms <- df.imms |>
    mutate(
      region1 = ifelse(
        ctryTxt == "USA",
        paste0("USA_", as.character(haven::as_factor(REGION_US_STATES))),
        paste0(ctryAbb, "_", as.character(haven::as_factor(ADM2)))
      ),
      region1 = ifelse(
        grepl("_NA$|^NA_|_$", region1),
        NA_character_,
        region1
      )
    )
}

# Exclude poRight6 because it is the moderator. bAccess and bAfford are
# uptake-specific covariates in 05 and should not enter confidence models.
covars_h4 <- setdiff(
  intersect(COVARS_BASE, names(df.imms)),
  c("poRight6", "bAccess", "bAfford")
)

d <- df.imms |>
  select(
    LINE_NO,
    ctryTxt,
    region1,
    dWEIGHT,
    poRight6,
    all_of(names(OUTCOMES)),
    all_of(covars_h4)
  )

# ══════════════════════════════════════════════════════════════
# 2. Estimation
# ══════════════════════════════════════════════════════════════

# Run each country × outcome combination sequentially and parallelise
# across posterior draws within each combination.
# Failed combinations do not stop the script; NULL is retained in res_list,
# and failed combinations are reported at the end.

grid <- expand.grid(
  country = CTRY,
  outcome = names(OUTCOMES),
  stringsAsFactors = FALSE
)

message(sprintf(
  "H4 interaction: %d combinations x %d draws, workers = %d",
  nrow(grid),
  N_DRAWS_H4,
  N_CORES
))

t0 <- Sys.time()

res_list <- vector("list", nrow(grid))
fail_log <- character(0)

for (k in seq_len(nrow(grid))) {

  cty <- grid$country[k]
  oc  <- grid$outcome[k]
  tk  <- Sys.time()

  s   <- scores[[cty]]
  M   <- min(N_DRAWS_H4, nrow(s$Mf))
  idx <- round(seq(1, nrow(s$Mf), length.out = M))

  base <- d |>
    filter(
      ctryTxt == cty,
      LINE_NO %in% s$person_id
    ) |>
    arrange(match(LINE_NO, s$person_id))

  # Verify row alignment. Misalignment would silently produce incorrect results.

  stopifnot(identical(base$LINE_NO, s$person_id))

  ok <- complete.cases(
    base[c("dWEIGHT", "region1", "poRight6", covars_h4)]
  ) &
    !is.na(base[[oc]])

  if (sum(ok) < 200) {
    fail_log <- c(
      fail_log,
      sprintf(
        "%s/%s: sample size %d (<200)",
        cty,
        oc,
        sum(ok)
      )
    )
    next
  }

  dat0 <- base[ok, ]
  dat0$region1 <- factor(dat0$region1)

  mu_po <- mean(dat0$poRight6)
  dat0$poR <- dat0$poRight6 - mu_po

  f <- as.formula(
    paste(
      oc,
      "~ Mf * poR + Mr + Mf_bar + Mr_bar +",
      paste(covars_h4, collapse = " + "),
      "+ (1 | region1)"
    )
  )

  # Marginal association for each posterior draw.
  # Failed draws return an error message string so the cause is retained.

  out <- future_lapply(
    seq_len(M),
    function(i) {

      dat <- dat0

      dat$Mf <- z(s$Mf[idx[i], ok])
      dat$Mr <- z(s$Mr[idx[i], ok])

      dat$Mf_bar <- ave(dat$Mf, dat$region1)
      dat$Mr_bar <- ave(dat$Mr, dat$region1)

      tryCatch({

        m <- lme4::glmer(
          f,
          data = dat,
          family = binomial,
          weights = dWEIGHT,
          control = lme4::glmerControl(
            optimizer = "bobyqa",
            calc.derivs = FALSE
          )
        )

        cf <- lme4::fixef(m)

        cf[["Mf"]] +
          cf[["Mf:poR"]] * (PO_GRID - mu_po)

      }, error = function(e) conditionMessage(e))
    },
    future.seed = TRUE
  )

  bad <- !vapply(out, is.numeric, logical(1))

  if (any(bad)) {
    message(sprintf(
      "  %d failed draws. Example: %s",
      sum(bad),
      out[[which(bad)[1]]]
    ))
  }

  out <- out[!bad]

  if (length(out) < 20) {
    fail_log <- c(
      fail_log,
      sprintf(
        "%s/%s: only %d successful draws",
        cty,
        oc,
        length(out)
      )
    )
    next
  }

  D    <- do.call(rbind, out)  # draws x 6
  n_po <- as.integer(
    table(
      factor(
        dat0$poRight6,
        levels = PO_GRID
      )
    )
  )

  res_list[[k]] <- data.frame(
    country = cty,
    outcome = oc,
    outcome_label = unname(OUTCOMES[oc]),
    poRight6 = PO_GRID,
    est = colMeans(D),
    lo = apply(D, 2, quantile, 0.025),
    hi = apply(D, 2, quantile, 0.975),
    or = exp(colMeans(D)),
    or_lo = exp(apply(D, 2, quantile, 0.025)),
    or_hi = exp(apply(D, 2, quantile, 0.975)),
    n_at_point = n_po,
    n = sum(ok),
    M = nrow(D),
    M_failed = sum(bad),
    row.names = NULL
  )

  message(sprintf(
    "[%2d/%d] %-7s %-8s  n = %5d, draws = %3d, %.1f min",
    k,
    nrow(grid),
    cty,
    oc,
    sum(ok),
    nrow(D),
    as.numeric(
      difftime(
        Sys.time(),
        tk,
        units = "mins"
      )
    )
  ))
}

marg_h4 <- do.call(rbind, res_list)

message(sprintf(
  "\nElapsed %.1f min, %d/%d combinations completed successfully",
  as.numeric(
    difftime(
      Sys.time(),
      t0,
      units = "mins"
    )
  ),
  sum(!vapply(res_list, is.null, logical(1))),
  nrow(grid)
))

if (length(fail_log) > 0) {

  message("Failed combinations")

  for (x in fail_log) {
    message("  ", x)
  }
}

stopifnot(
  !is.null(marg_h4),
  nrow(marg_h4) > 0
)

# ══════════════════════════════════════════════════════════════
# 3. Figure
# ══════════════════════════════════════════════════════════════

OUT_ORDER <- unname(OUTCOMES)

OUT_COL <- setNames(
  c(
    "#D85A30",
    "#1D9E75",
    "#7F77DD",
    "#C99A2E",
    "#4A9BC4"
  ),
  OUT_ORDER
)

XAXIS <- list(
  title = "Political orientation (1 = left, 6 = right)",
  tickvals = PO_GRID
)

YAXIS <- list(
  title = "Odds ratio per SD of fake news detection",
  type = "log"
)

HLINE <- list(
  list(
    type = "line",
    x0 = 1,
    x1 = 6,
    y0 = 1,
    y1 = 1,
    line = list(
      dash = "dot",
      color = "#999"
    )
  )
)

# ── USA ───────────────────────────────────────────────────────

dd <- marg_h4 |>
  filter(country == "USA")

p_usa <- plot_ly()

for (ol in OUT_ORDER) {

  di <- dd |>
    filter(outcome_label == ol)

  if (nrow(di) == 0) next

  col <- OUT_COL[[ol]]

  p_usa <- p_usa |>
    add_ribbons(
      data = di,
      x = ~poRight6,
      ymin = ~or_lo,
      ymax = ~or_hi,
      fillcolor = toRGB(col, 0.15),
      line = list(width = 0),
      showlegend = FALSE,
      hoverinfo = "skip",
      name = ol,
      legendgroup = ol
    ) |>
    add_lines(
      data = di,
      x = ~poRight6,
      y = ~or,
      line = list(
        color = col,
        width = 2
      ),
      marker = list(
        color = col,
        size = 5
      ),
      mode = "lines+markers",
      name = ol,
      legendgroup = ol,
      showlegend = TRUE,
      hovertemplate = ~paste0(
        "<b>", outcome_label, "</b>",
        "<br>orientation = ", poRight6,
        "<br>OR = ", sprintf("%.2f", or),
        " [", sprintf("%.2f", or_lo),
        ", ", sprintf("%.2f", or_hi), "]",
        "<br>n at this point = ", n_at_point,
        "<extra></extra>"
      )
    )
}

p_usa <- p_usa |>
  layout(
    xaxis = XAXIS,
    yaxis = YAXIS,
    shapes = HLINE
  )

p_usa

# ── UK ────────────────────────────────────────────────────────

dd <- marg_h4 |>
  filter(country == "UK")

p_uk <- plot_ly()

for (ol in OUT_ORDER) {

  di <- dd |>
    filter(outcome_label == ol)

  if (nrow(di) == 0) next

  col <- OUT_COL[[ol]]

  p_uk <- p_uk |>
    add_ribbons(
      data = di,
      x = ~poRight6,
      ymin = ~or_lo,
      ymax = ~or_hi,
      fillcolor = toRGB(col, 0.15),
      line = list(width = 0),
      showlegend = FALSE,
      hoverinfo = "skip",
      name = ol,
      legendgroup = ol
    ) |>
    add_lines(
      data = di,
      x = ~poRight6,
      y = ~or,
      line = list(
        color = col,
        width = 2
      ),
      marker = list(
        color = col,
        size = 5
      ),
      mode = "lines+markers",
      name = ol,
      legendgroup = ol,
      showlegend = FALSE,
      hovertemplate = ~paste0(
        "<b>", outcome_label, "</b>",
        "<br>orientation = ", poRight6,
        "<br>OR = ", sprintf("%.2f", or),
        " [", sprintf("%.2f", or_lo),
        ", ", sprintf("%.2f", or_hi), "]",
        "<br>n at this point = ", n_at_point,
        "<extra></extra>"
      )
    )
}

p_uk <- p_uk |>
  layout(
    xaxis = XAXIS,
    yaxis = YAXIS,
    shapes = HLINE
  )

p_uk

# ── Canada ────────────────────────────────────────────────────

dd <- marg_h4 |>
  filter(country == "Canada")

p_can <- plot_ly()

for (ol in OUT_ORDER) {

  di <- dd |>
    filter(outcome_label == ol)

  if (nrow(di) == 0) next

  col <- OUT_COL[[ol]]

  p_can <- p_can |>
    add_ribbons(
      data = di,
      x = ~poRight6,
      ymin = ~or_lo,
      ymax = ~or_hi,
      fillcolor = toRGB(col, 0.15),
      line = list(width = 0),
      showlegend = FALSE,
      hoverinfo = "skip",
      name = ol,
      legendgroup = ol
    ) |>
    add_lines(
      data = di,
      x = ~poRight6,
      y = ~or,
      line = list(
        color = col,
        width = 2
      ),
      marker = list(
        color = col,
        size = 5
      ),
      mode = "lines+markers",
      name = ol,
      legendgroup = ol,
      showlegend = FALSE,
      hovertemplate = ~paste0(
        "<b>", outcome_label, "</b>",
        "<br>orientation = ", poRight6,
        "<br>OR = ", sprintf("%.2f", or),
        " [", sprintf("%.2f", or_lo),
        ", ", sprintf("%.2f", or_hi), "]",
        "<br>n at this point = ", n_at_point,
        "<extra></extra>"
      )
    )
}

p_can <- p_can |>
  layout(
    xaxis = XAXIS,
    yaxis = YAXIS,
    shapes = HLINE
  )

p_can

# ── Combine panels ─────────────────────────────────────────────

fig_h4 <- plotly::subplot(
  p_usa,
  p_uk,
  p_can,
  nrows = 1,
  shareY = TRUE,
  shareX = TRUE,
  titleX = TRUE,
  titleY = TRUE,
  margin = 0.02
) |>
  layout(
    title = " ",
    legend = list(
      orientation = "h",
      x = 0,
      y = -0.20
    ),
    annotations = list(
      list(
        text = "USA",
        x = .1455,
        y = 1.05,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 18)
      ),
      list(
        text = "UK",
        x = .50,
        y = 1.05,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 18)
      ),
      list(
        text = "Canada",
        x = .868,
        y = 1.05,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 18)
      )
    )
  )

fig_h4

# ══════════════════════════════════════════════════════════════
# 4. Summary and save
# ══════════════════════════════════════════════════════════════

slope_tab <- marg_h4 |>
  filter(poRight6 %in% c(1, 6)) |>
  select(
    country,
    outcome_label,
    poRight6,
    or
  ) |>
  pivot_wider(
    names_from = poRight6,
    values_from = or,
    names_prefix = "or_at_"
  ) |>
  mutate(
    ratio = or_at_6 / or_at_1
  )

plotly::save_image(
  fig_h4,
  "output/fig_h4.svg",
  width = 1200,
  height = 700
)

message("\nOdds ratios at the ideological extremes and their ratio")

print(
  slope_tab,
  digits = 3,
  row.names = FALSE
)

message(
  "A ratio < 1 indicates that the association becomes weaker toward the political right."
)

message("\nSample size by ideological position")

print(
  marg_h4 |>
    filter(outcome == names(OUTCOMES)[1]) |>
    select(
      country,
      poRight6,
      n_at_point
    ) |>
    pivot_wider(
      names_from = poRight6,
      values_from = n_at_point
    ),
  row.names = FALSE
)

message(
  "Positions with smaller samples have wider confidence intervals and should be interpreted accordingly."
)

saveRDS(
  list(
    marg_h4 = marg_h4,
    slope_tab = slope_tab,
    fig_h4 = fig_h4,
    meta = list(
      date = Sys.time(),
      n_draws = N_DRAWS_H4,
      covars = covars_h4,
      workers = N_CORES,
      note = "Mundlak CRE with Mf x poRight6 interaction"
    )
  ),
  F_H4
)

plan(sequential)

message("\n", strrep("=", 62))
message("saved: ", normalizePath(F_H4))
message(sprintf(
  "  size: %.2f MB",
  file.size(F_H4) / 1024^2
))
message("  marg_h4   : marginal associations by ideological position (est, lo, hi, or, n_at_point)")
message("  slope_tab : comparison between the ideological extremes")
message("  fig_h4    : plotly object")
message(strrep("-", 62))
message("in the global environment")
message("  marg_h4, slope_tab, fig_h4, p_usa, p_uk, p_can")
message(strrep("=", 62))