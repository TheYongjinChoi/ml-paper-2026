# ==============================================================
# 14_ability_by_ideology.R
# ==============================================================

source("code/01_config.R")

library(dplyr)
library(tidyr)
library(plotly)

F_ABIL <- file.path(DIR_OUT, "ability_by_ideology.rds")
PO_GRID <- 1:6

sc <- readRDS(F_SCORES)
stopifnot(exists("df.imms"))

# ── 1. Data ────────────────────────────────────────────────────

dat <- df.imms |>
  filter(ctryTxt %in% CTRY, !is.na(poRight6), !is.na(dWEIGHT)) |>
  dplyr::select(LINE_NO, ctryTxt, poRight6, dWEIGHT)

# ── 2. Adjusted means by ideological position ──────────────────

covars <- setdiff(
  intersect(COVARS_BASE, names(df.imms)),
  c("poRight6", "bAccess", "bAfford")
)
N_DRAWS_FIG <- 200

dat <- df.imms |>
  filter(ctryTxt %in% CTRY, !is.na(poRight6), !is.na(dWEIGHT)) |>
  dplyr::select(LINE_NO, ctryTxt, poRight6, dWEIGHT, all_of(covars)) |>
  filter(complete.cases(across(all_of(covars))))

abil <- list()

for (cty in CTRY) {

  s  <- sc$scores[[cty]]
  dd <- dat |>
    filter(ctryTxt == cty, LINE_NO %in% s$person_id) |>
    arrange(match(LINE_NO, s$person_id))

  idx_person <- match(dd$LINE_NO, s$person_id)
  stopifnot(!anyNA(idx_person))

  D  <- nrow(s$Mf)
  M  <- min(N_DRAWS_FIG, D)
  it <- round(seq(1, D, length.out = M))

  f <- as.formula(
    paste(
      "y ~ factor(poRight6) +",
      paste(covars, collapse = " + ")
    )
  )

  for (score in c("Mf", "Mr")) {

    curves <- matrix(NA_real_, M, length(PO_GRID))

    for (i in seq_len(M)) {

      d0 <- dd
      d0$y <- s[[score]][it[i], idx_person]

      m <- lm(
        f,
        data = d0,
        weights = d0$dWEIGHT
      )

      for (g in seq_along(PO_GRID)) {

        d1 <- d0
        d1$poRight6 <- PO_GRID[g]

        curves[i, g] <- weighted.mean(
          predict(m, newdata = d1),
          d0$dWEIGHT
        )
      }
    }

    abil[[length(abil) + 1]] <- data.frame(
      country = cty,
      score = ifelse(
        score == "Mf",
        "Fake news detection",
        "Real news detection"
      ),
      poRight6 = PO_GRID,
      mean = colMeans(curves),
      lo = apply(curves, 2, quantile, 0.025),
      hi = apply(curves, 2, quantile, 0.975),
      n = as.integer(
        table(
          factor(
            dd$poRight6,
            levels = PO_GRID
          )
        )
      ),
      row.names = NULL
    )
  }
}

abil <- do.call(rbind, abil)

# ── 3. Figure ──────────────────────────────────────────────────

SCORE_COL <- c(
  "Fake news detection" = "#D85A30",
  "Real news detection" = "#1D9E75"
)

XAXIS <- list(
  title = "Political orientation (1 = left, 6 = right)",
  tickvals = PO_GRID
)

YAXIS <- list(
  title = "Expected detection score (0-10)"
)

# ── USA ───────────────────────────────────────────────────────

dd <- abil |>
  filter(country == "USA")

p_usa <- plot_ly()

for (sn in names(SCORE_COL)) {

  di <- dd |>
    filter(score == sn)

  if (nrow(di) == 0) next

  col <- SCORE_COL[[sn]]

  p_usa <- p_usa |>
    add_ribbons(
      data = di,
      x = ~poRight6,
      ymin = ~lo,
      ymax = ~hi,
      fillcolor = toRGB(col, 0.15),
      line = list(width = 0),
      showlegend = FALSE,
      hoverinfo = "skip",
      name = sn,
      legendgroup = sn
    ) |>
    add_lines(
      data = di,
      x = ~poRight6,
      y = ~mean,
      line = list(
        color = col,
        width = 2
      ),
      marker = list(
        color = col,
        size = 5
      ),
      mode = "lines+markers",
      name = sn,
      legendgroup = sn,
      showlegend = TRUE,
      hovertemplate = ~paste0(
        "<b>", score, "</b><br>",
        "orientation = ", poRight6,
        "<br>score = ", sprintf("%.2f", mean),
        " [", sprintf("%.2f", lo),
        ", ", sprintf("%.2f", hi), "]",
        "<br>n = ", n,
        "<extra></extra>"
      )
    )
}

p_usa <- p_usa |>
  layout(
    xaxis = XAXIS,
    yaxis = YAXIS
  )

p_usa

# ── UK ────────────────────────────────────────────────────────

dd <- abil |>
  filter(country == "UK")

p_uk <- plot_ly()

for (sn in names(SCORE_COL)) {

  di <- dd |>
    filter(score == sn)

  if (nrow(di) == 0) next

  col <- SCORE_COL[[sn]]

  p_uk <- p_uk |>
    add_ribbons(
      data = di,
      x = ~poRight6,
      ymin = ~lo,
      ymax = ~hi,
      fillcolor = toRGB(col, 0.15),
      line = list(width = 0),
      showlegend = FALSE,
      hoverinfo = "skip",
      name = sn,
      legendgroup = sn
    ) |>
    add_lines(
      data = di,
      x = ~poRight6,
      y = ~mean,
      line = list(
        color = col,
        width = 2
      ),
      marker = list(
        color = col,
        size = 5
      ),
      mode = "lines+markers",
      name = sn,
      legendgroup = sn,
      showlegend = FALSE,
      hovertemplate = ~paste0(
        "<b>", score, "</b><br>",
        "orientation = ", poRight6,
        "<br>score = ", sprintf("%.2f", mean),
        " [", sprintf("%.2f", lo),
        ", ", sprintf("%.2f", hi), "]",
        "<br>n = ", n,
        "<extra></extra>"
      )
    )
}

p_uk <- p_uk |>
  layout(
    xaxis = XAXIS,
    yaxis = YAXIS
  )

p_uk

# ── Canada ────────────────────────────────────────────────────

dd <- abil |>
  filter(country == "Canada")

p_can <- plot_ly()

for (sn in names(SCORE_COL)) {

  di <- dd |>
    filter(score == sn)

  if (nrow(di) == 0) next

  col <- SCORE_COL[[sn]]

  p_can <- p_can |>
    add_ribbons(
      data = di,
      x = ~poRight6,
      ymin = ~lo,
      ymax = ~hi,
      fillcolor = toRGB(col, 0.15),
      line = list(width = 0),
      showlegend = FALSE,
      hoverinfo = "skip",
      name = sn,
      legendgroup = sn
    ) |>
    add_lines(
      data = di,
      x = ~poRight6,
      y = ~mean,
      line = list(
        color = col,
        width = 2
      ),
      marker = list(
        color = col,
        size = 5
      ),
      mode = "lines+markers",
      name = sn,
      legendgroup = sn,
      showlegend = FALSE,
      hovertemplate = ~paste0(
        "<b>", score, "</b><br>",
        "orientation = ", poRight6,
        "<br>score = ", sprintf("%.2f", mean),
        " [", sprintf("%.2f", lo),
        ", ", sprintf("%.2f", hi), "]",
        "<br>n = ", n,
        "<extra></extra>"
      )
    )
}

p_can <- p_can |>
  layout(
    xaxis = XAXIS,
    yaxis = YAXIS
  )

p_can

# ── Combine panels ─────────────────────────────────────────────

fig_abil <- plotly::subplot(
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

fig_abil

plotly::save_image(
  fig_abil,
  "output/fig_abil.svg",
  width = 1200,
  height = 700
)

# ── 4. Summary ─────────────────────────────────────────────────

message("\nDetection ability at the ideological extremes")

print(
  as.data.frame(
    abil |>
      filter(poRight6 %in% c(1, 6)) |>
      dplyr::select(
        country,
        score,
        poRight6,
        mean,
        lo,
        hi,
        n
      ) |>
      pivot_wider(
        names_from = poRight6,
        values_from = c(mean, lo, hi, n),
        names_sep = "_"
      ) |>
      mutate(
        diff = mean_6 - mean_1
      )
  ),
  row.names = FALSE
)

message(
  "A negative diff indicates that detection ability decreases toward the political right."
)

saveRDS(
  list(
    abil = abil,
    fig_abil = fig_abil,
    meta = list(
      date = Sys.time(),
      note = "design-weighted mean of expected scores by ideology point, computed per posterior draw"
    )
  ),
  F_ABIL
)