# ==============================================================
# 08_ppc_prior.R
# ==============================================================

source("code/01_config.R")
library(rjags); library(coda); library(dplyr); library(tidyr)

F_PPC      <- file.path(DIR_OUT, "ppc_prior.rds")
JAGS_PRIOR <- "R/2pl_prior.jags"
N_PPC      <- 300

stopifnot(file.exists(JAGS_PRIOR))



irt  <- readRDS(F_IRT)
fits <- irt$fits

MIST_BLK <- list(fake = MIST_SCORED_FAKE, real = MIST_SCORED_REAL)

# ── 1. Posterior predictive checks

ppc <- NULL

for (cty in CTRY) {
  for (blk in c("fake", "real")) {

    f <- fits[[cty]][[blk]]

    ids <- fits[[cty]][[paste0("id_", blk)]]
    if (is.null(ids)) ids <- fits[[cty]]$person_id
    if (is.null(ids)) stop(cty, ": no id vector for block ", blk)

    dd <- df.imms |>
      filter(ctryTxt == cty, LINE_NO %in% ids) |>
      arrange(match(LINE_NO, ids))

    if (!identical(dd$LINE_NO, ids)) {
      stop(sprintf("%s / %s: %d of %d fitted ids absent from df.imms",
                   cty, blk, length(setdiff(ids, dd$LINE_NO)), length(ids)))
    }

    Yobs <- as.matrix(dd[MIST_BLK[[blk]]])
    stopifnot(identical(colnames(Yobs), f$items),
              nrow(Yobs) == ncol(f$theta),
              ncol(Yobs) == ncol(f$a))

    N <- nrow(Yobs)
    J <- ncol(Yobs)
    D <- min(N_PPC, nrow(f$a))

    # a, b and theta share a row order, so one index for all three
    idx <- if (nrow(f$a) <= D) seq_len(nrow(f$a)) else
      round(seq(1, nrow(f$a), length.out = D))
    D <- length(idx)

    obs_cor <- cor(Yobs, use = "pairwise")
    prs     <- which(upper.tri(obs_cor), arr.ind = TRUE)
    rep_cor <- matrix(NA_real_, D, nrow(prs))

    pb <- txtProgressBar(min = 0, max = D, style = 3)

    for (t in seq_len(D)) {

      r <- idx[t]

      P <- plogis(
        outer(f$theta[r, ], f$b[r, ], "-") * rep(f$a[r, ], each = N)
      )

      Yrep <- matrix(rbinom(N * J, 1, P), N, J)
      # Same missingness as the observed data, so both correlation
      # matrices rest on the same cells
      Yrep[is.na(Yobs)] <- NA

      cr <- cor(Yrep, use = "pairwise")
      rep_cor[t, ] <- cr[upper.tri(cr)]

      setTxtProgressBar(pb, t)
    }

    close(pb)

    ppc <- rbind(ppc, data.frame(
      country  = cty,
      block    = blk,
      item1    = sub("^mist_", "", colnames(Yobs)[prs[, 1]]),
      item2    = sub("^mist_", "", colnames(Yobs)[prs[, 2]]),
      obs      = obs_cor[upper.tri(obs_cor)],
      rep_mean = colMeans(rep_cor, na.rm = TRUE),
      rep_lo   = apply(rep_cor, 2, quantile, 0.025, na.rm = TRUE),
      rep_hi   = apply(rep_cor, 2, quantile, 0.975, na.rm = TRUE),
      ppp      = colMeans(sweep(rep_cor, 2, obs_cor[upper.tri(obs_cor)],
                                ">="), na.rm = TRUE),
      row.names = NULL
    ) |>
      mutate(resid = obs - rep_mean,
             flag  = ppp < 0.025 | ppp > 0.975))
  }
}

ppc_summary <- ppc |>
  group_by(country, block) |>
  summarise(n_pairs = n(), n_flag = sum(flag), pct_flag = 100 * mean(flag),
            max_abs_resid = max(abs(resid)), .groups = "drop")

message("\nPPC: item pairs outside the 95% replicated interval")
print(ppc_summary, digits = 3, row.names = FALSE)

message("\nLargest residuals")
print(
  ppc |>
    arrange(desc(abs(resid))) |>
    head(10) |>
    dplyr::select(country, block, item1, item2, obs, rep_mean, resid, ppp),
  digits = 3, row.names = FALSE
)

# ── 2. Prior sensitivity (USA)

PRIOR_GRID <- data.frame(
  setting = c("baseline", "tight_a", "loose_a", "tight_b", "loose_b"),
  tau_a   = c(1, 4, 0.25, 1, 1),
  tau_b   = c(0.25, 0.25, 0.25, 1, 0.0625)
)

prior_res <- NULL

for (blk in c("fake", "real")) {

  ids <- fits[["USA"]][[paste0("id_", blk)]]
  if (is.null(ids)) ids <- fits[["USA"]]$person_id

  dd <- df.imms |>
    filter(ctryTxt == "USA", LINE_NO %in% ids) |>
    arrange(match(LINE_NO, ids))

  Y <- as.matrix(dd[MIST_BLK[[blk]]])
  storage.mode(Y) <- "integer"
  items <- sub("^mist_", "", colnames(Y))

  for (i in seq_len(nrow(PRIOR_GRID))) {

    g  <- PRIOR_GRID[i, ]
    jd <- list(Y = Y, N = nrow(Y), J = ncol(Y),
               tau_a = g$tau_a, tau_b = g$tau_b)

    inits <- vector("list", 2)
    for (ch in 1:2) {
      inits[[ch]] <- list(
        theta     = rnorm(jd$N, 0, 2),
        a         = runif(jd$J, 0.5, 2),
        b         = rnorm(jd$J, 0, 2),
        .RNG.name = "base::Mersenne-Twister",
        .RNG.seed = 7000 + i * 10 + ch
      )
    }

    message(sprintf("  [%s / %-8s] tau_a = %.2f, tau_b = %.4f",
                    blk, g$setting, g$tau_a, g$tau_b))

    m <- jags.model(JAGS_PRIOR, data = jd, inits = inits,
                    n.chains = 2, n.adapt = 1000, quiet = TRUE)
    update(m, n.iter = 3000, progress.bar = "none")

    S <- as.matrix(coda.samples(m, c("a", "b"), n.iter = 5000,
                                thin = 5, progress.bar = "none"))

    prior_res <- rbind(prior_res, data.frame(
      block   = blk,
      setting = g$setting,
      item    = items,
      a       = colMeans(S[, sprintf("a[%d]", seq_len(jd$J)), drop = FALSE]),
      b       = colMeans(S[, sprintf("b[%d]", seq_len(jd$J)), drop = FALSE]),
      row.names = NULL
    ))
  }
}

prior_wide <- prior_res |>
  dplyr::select(block, item, setting, a) |>
  pivot_wider(names_from = setting, values_from = a)

message("\nDiscrimination by prior setting (USA, posterior means)")
print(prior_wide, digits = 3, row.names = FALSE)

prior_shift <- prior_res |>
  group_by(setting) |>
  summarise(mean_a = mean(a), .groups = "drop") |>
  mutate(pct_vs_baseline = 100 * (mean_a / mean_a[setting == "baseline"] - 1))

message("\nMean discrimination shift vs baseline")
print(prior_shift, digits = 3, row.names = FALSE)

# ── 3. Save

saveRDS(
  list(ppc = ppc, ppc_summary = ppc_summary,
       prior_res = prior_res, prior_wide = prior_wide,
       prior_shift = prior_shift, prior_grid = PRIOR_GRID,
       meta = list(date = Sys.time(), n_ppc = N_PPC)),
  F_PPC
)