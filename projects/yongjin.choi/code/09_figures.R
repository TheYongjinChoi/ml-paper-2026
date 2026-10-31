# ==============================================================
# 09_figures.R
# ==============================================================

source("R/01_config.R")
library(dplyr); library(tidyr); library(plotly)

irt  <- readRDS(F_IRT)
fits <- irt$fits
sc   <- readRDS(F_SCORES)

get_ab <- function(f) {
  if (is.matrix(f$a)) return(list(a = f$a, b = f$b))
  S <- as.matrix(f$item); J <- length(f$items)
  list(
    a = S[, sprintf("a[%d]", seq_len(J)), drop = FALSE],
    b = S[, sprintf("b[%d]", seq_len(J)), drop = FALSE]
  )
}

TH_GRID   <- seq(-4, 4, length.out = 121)
BLOCK_COL <- c(fake = "#D85A30", real = "#1D9E75")

# ══════════════════════════════════════════════════════════════
# 1. Item characteristic curves (ICC)
# ══════════════════════════════════════════════════════════════

icc_data <- function(cty, blk, n_draws = 300) {
  f  <- fits[[cty]][[blk]]
  AB <- get_ab(f)
  D  <- nrow(AB$a)
  idx <- round(seq(1, D, length.out = min(n_draws, D)))
  items <- sub("^mist_", "", f$items)

  do.call(rbind, lapply(seq_along(items), function(j) {

    # draws x grid

    M <- plogis(
      outer(
        AB$a[idx, j],
        TH_GRID,
        function(a, th) a * (th - 0)
      ) +
        outer(
          -AB$a[idx, j] * AB$b[idx, j],
          rep(1, length(TH_GRID))
        )
    )

    data.frame(
      country = cty,
      block = blk,
      item = items[j],
      theta = TH_GRID,
      p = colMeans(M),
      lo = apply(M, 2, quantile, 0.025),
      hi = apply(M, 2, quantile, 0.975),
      row.names = NULL
    )
  }))
}

message("Calculating ICCs")

icc <- do.call(
  rbind,
  lapply(CTRY, function(cty) {
    do.call(
      rbind,
      lapply(
        c("fake", "real"),
        function(blk) icc_data(cty, blk)
      )
    )
  })
)

plot_icc <- function(cty) {

  dd <- icc |> filter(country == cty)
  p <- plot_ly()

  for (blk in c("fake", "real")) {
    for (it in unique(dd$item[dd$block == blk])) {

      di <- dd |> filter(block == blk, item == it)

      p <- p |>
        add_lines(
          data = di,
          x = ~theta,
          y = ~p,
          line = list(
            color = BLOCK_COL[[blk]],
            width = 1.2
          ),
          opacity = 0.7,
          showlegend = FALSE,
          hovertemplate = ~paste0(
            "",
            item,
            " (", block, ")",
            "theta = ", sprintf("%.2f", theta),
            "P = ", sprintf("%.3f", p),
            ""
          )
        )
    }
  }

  p |>
    layout(
      title = paste0("Item characteristic curves — ", cty),
      xaxis = list(title = "Latent ability (theta)"),
      yaxis = list(
        title = "P(correct)",
        range = c(0, 1),
        tickformat = ".0%"
      ),
      shapes = list(
        list(
          type = "line",
          x0 = -4,
          x1 = 4,
          y0 = .5,
          y1 = .5,
          line = list(
            dash = "dot",
            width = 1,
            color = "#999"
          )
        )
      )
    )
}

fig_icc <- plot_icc("USA")
fig_icc

plot_icc_band <- function(cty, blk, items_show) {

  dd <- icc |>
    filter(
      country == cty,
      block == blk,
      item %in% items_show
    )

  plot_ly(
    dd,
    x = ~theta,
    color = ~item
  ) |>
    add_ribbons(
      ymin = ~lo,
      ymax = ~hi,
      opacity = 0.25,
      line = list(width = 0),
      showlegend = FALSE
    ) |>
    add_lines(y = ~p) |>
    layout(
      title = paste0(
        "ICC with 95% credible bands — ",
        cty, " / ", blk
      ),
      xaxis = list(title = "Latent ability (theta)"),
      yaxis = list(
        title = "P(correct)",
        range = c(0, 1),
        tickformat = ".0%"
      )
    )
}

# ══════════════════════════════════════════════════════════════
# 2. Test information function (TIF)
# ══════════════════════════════════════════════════════════════

# I(theta) = sum_j a_j^2 P_j (1 - P_j)

tif_data <- function(cty, blk, n_draws = 300) {

  f <- fits[[cty]][[blk]]
  AB <- get_ab(f)

  idx <- round(
    seq(
      1,
      nrow(AB$a),
      length.out = min(n_draws, nrow(AB$a))
    )
  )

  I <- matrix(
    0,
    length(idx),
    length(TH_GRID)
  )

  for (j in seq_len(ncol(AB$a))) {

    P <- plogis(
      outer(AB$a[idx, j], TH_GRID) -
        matrix(
          AB$a[idx, j] * AB$b[idx, j],
          length(idx),
          length(TH_GRID)
        )
    )

    I <- I +
      (AB$a[idx, j]^2) *
      P *
      (1 - P)
  }

  data.frame(
    country = cty,
    block = blk,
    theta = TH_GRID,
    info = colMeans(I),
    lo = apply(I, 2, quantile, 0.025),
    hi = apply(I, 2, quantile, 0.975),
    se = 1 / sqrt(colMeans(I)),
    row.names = NULL
  )
}

message("Calculating test information functions")

tif <- do.call(
  rbind,
  lapply(CTRY, function(cty) {
    do.call(
      rbind,
      lapply(
        c("fake", "real"),
        function(blk) tif_data(cty, blk)
      )
    )
  })
)

plot_tif <- function(cty, show_legend = FALSE) {

  dd <- tif |> filter(country == cty)

  plot_ly(
    dd,
    x = ~theta,
    color = ~block,
    colors = BLOCK_COL,
    legendgroup = ~block,
    showlegend = show_legend
  ) |>
    add_ribbons(
      ymin = ~lo,
      ymax = ~hi,
      opacity = 0.2,
      line = list(width = 0),
      showlegend = FALSE
    ) |>
    add_lines(y = ~info) |>
    layout(
      xaxis = list(title = "Latent ability (theta)"),
      yaxis = list(title = "Test information")
    )
}

fig_tif <- plotly::subplot(
  plot_tif("USA", TRUE),
  plot_tif("UK"),
  plot_tif("Canada"),
  nrows = 1,
  shareY = TRUE,
  titleY = TRUE,
  margin = 0.03
) |>
  layout(
    title = "Test information function by block",
    legend = list(
      orientation = "h",
      x = 0,
      y = -0.12
    ),
    annotations = list(
      list(
        text = "USA",
        x = .16,
        y = 1.04,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 14)
      ),
      list(
        text = "UK",
        x = .50,
        y = 1.04,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 14)
      ),
      list(
        text = "Canada",
        x = .84,
        y = 1.04,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 14)
      )
    )
  )

fig_tif

# ══════════════════════════════════════════════════════════════
# 3. Mf–Mr scatterplot
# ══════════════════════════════════════════════════════════════

plot_scatter <- function(cty, show_legend = FALSE) {

  dd <- sc$score_df |>
    filter(ctryTxt == cty)

  r <- cor(dd$Mf, dd$Mr)

  plot_ly(
    dd,
    x = ~Mf,
    y = ~Mr,
    type = "scatter",
    mode = "markers",
    marker = list(
      size = 4,
      opacity = 0.35,
      color = "#7F77DD"
    ),
    showlegend = show_legend,
    name = "respondent",
    hovertemplate = ~paste0(
      "Mf = ", sprintf("%.2f", Mf),
      "Mr = ", sprintf("%.2f", Mr),
      ""
    )
  ) |>
    layout(
      xaxis = list(
        title = "Fake news detection (0–10)",
        range = c(0, 10)
      ),
      yaxis = list(
        title = "Real news detection (0–10)",
        range = c(0, 10)
      ),
      annotations = list(
        list(
          text = sprintf("r = %.2f", r),
          x = 0.05,
          y = 0.95,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 12)
        )
      ),
      shapes = list(
        list(
          type = "line",
          x0 = 0,
          x1 = 10,
          y0 = 0,
          y1 = 10,
          line = list(
            dash = "dot",
            width = 1,
            color = "#999"
          )
        )
      )
    )
}

fig_scatter <- plotly::subplot(
  plot_scatter("USA"),
  plot_scatter("UK"),
  plot_scatter("Canada"),
  nrows = 1,
  shareY = TRUE,
  titleX = TRUE,
  titleY = TRUE,
  margin = 0.03
) |>
  layout(
    title = "Fake vs real news detection (posterior means)",
    annotations = list(
      list(
        text = "USA",
        x = .16,
        y = 1.04,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 14)
      ),
      list(
        text = "UK",
        x = .50,
        y = 1.04,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 14)
      ),
      list(
        text = "Canada",
        x = .84,
        y = 1.04,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 14)
      )
    )
  )

fig_scatter

plotly::save_image(
  fig_scatter,
  "output/fig_scatter.svg",
  width = 1200,
  height = 500
)

saveRDS(
  list(
    icc = icc,
    tif = tif,
    fig_icc = fig_icc,
    fig_tif = fig_tif,
    fig_scatter = fig_scatter,
    meta = list(date = Sys.time())
  ),
  file.path(DIR_OUT, "figures.rds")
)