# ==============================================================
# 04_mist_scores.R
# ==============================================================

source("code/01_config.R")

irt <- readRDS(F_IRT)
fits <- irt$fits

# ── 1. Expected ability scores

# M^(t)_i = sum_j logit^-1[ a_j^(t) ( theta_i^(t) - b_j^(t) ) ]
# Returns: draws x N

expected_score <- function(f, n_draws = NULL) {
  D <- nrow(f$a)
  keep <- if (is.null(n_draws) || n_draws >= D) seq_len(D) else
    round(seq(1, D, length.out = n_draws))

  out <- matrix(0, length(keep), f$N)
  for (j in seq_len(f$J)) {
    out <- out + plogis(
      f$a[keep, j] *
        (f$theta[keep, , drop = FALSE] - f$b[keep, j])
    )
  }
  out
}

# ── 2. Poisson-binomial probability mass function

pb_pmf <- function(P) {
  N <- nrow(P); J <- ncol(P)
  pmf <- matrix(0, N, J + 1)
  pmf[, 1] <- 1
  for (j in seq_len(J)) {
    p <- P[, j]
    pmf[, 2:(j + 1)] <- pmf[, 2:(j + 1), drop = FALSE] * (1 - p) +
      pmf[, 1:j, drop = FALSE] * p
    pmf[, 1] <- pmf[, 1] * (1 - p)
  }
  pmf
}

# ── 3. Expected bias scores
# E[Md] = sum_a sum_b P(Mr=a) P(Mf=b) max(b-a, 0)

W_D <- outer(0:10, 0:10, function(a, b) pmax(b - a, 0))
W_N <- outer(0:10, 0:10, function(a, b) pmax(a - b, 0))

item_prob <- function(f, t) {

  plogis(
    outer(f$theta[t, ], f$b[t, ], "-") *
      rep(f$a[t, ], each = f$N)
  )
}

bias_scores <- function(ff, fr, n_draws = NULL) {
  D <- nrow(ff$a)
  keep <- if (is.null(n_draws) || n_draws >= D) seq_len(D) else
    round(seq(1, D, length.out = n_draws))

  Md <- matrix(NA_real_, length(keep), ff$N)
  Mn <- matrix(NA_real_, length(keep), ff$N)

  pb <- txtProgressBar(min = 0, max = length(keep), style = 3)
  for (i in seq_along(keep)) {
    t  <- keep[i]
    pf <- pb_pmf(item_prob(ff, t))
    pr <- pb_pmf(item_prob(fr, t))
    Md[i, ] <- rowSums((pr %*% W_D) * pf)
    Mn[i, ] <- rowSums((pr %*% W_N) * pf)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  list(Md = Md, Mn = Mn, keep = keep)
}

# ── 4. Calculate scores by country

scores <- list()

for (cty in CTRY) {
  message(sprintf("[%s] expected scores ...", cty))
  ff <- fits[[cty]]$fake
  fr <- fits[[cty]]$real

  Mf <- expected_score(ff, N_DRAWS_SCORE)
  Mr <- expected_score(fr, N_DRAWS_SCORE)
  Mv <- Mf + Mr

  message(sprintf("[%s] bias scores (Poisson binomial) ...", cty))
  bs <- bias_scores(ff, fr, N_DRAWS_SCORE)

  scores[[cty]] <- list(
    country    = cty,
    person_id = fits[[cty]]$person_id,
    Mf = Mf, Mr = Mr, Mv = Mv, Md = bs$Md, Mn = bs$Mn
  )
}

# ── 5. Posterior-mean data frame

score_df <- do.call(rbind, lapply(CTRY, function(cty) {
  s <- scores[[cty]]
  data.frame(
    LINE_NO = s$person_id, ctryTxt = cty,
    Mf = colMeans(s$Mf), Mr = colMeans(s$Mr), Mv = colMeans(s$Mv),
    Md = colMeans(s$Md), Mn = colMeans(s$Mn),
    Mf_sd = apply(s$Mf, 2, sd), Mr_sd = apply(s$Mr, 2, sd),
    row.names = NULL
  )
}))

# ── 6. Save output

saveRDS(
  list(
    scores = scores,
    score_df = score_df,
    meta = list(date = Sys.time(), n_draws = N_DRAWS_SCORE)
  ),
  F_SCORES
)