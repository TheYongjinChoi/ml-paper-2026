# ==============================================================
# 03_irt_2pl.R
# ==============================================================

source("code/01_config.R")

N_THETA_DIAG <- 500


# ── 1. Build the task list (one task per country x block x chain)

set.seed(20260808)

tasks <- list()
k     <- 0

for (cty in CTRY) {

  dd <- df.imms |> filter(ctryTxt == cty)

  if (!is.null(N_SUB) && nrow(dd) > N_SUB) {
    dd <- dd[sample(nrow(dd), N_SUB), ]
  }

  Yf <- as.matrix(dd[MIST_SCORED_FAKE]); storage.mode(Yf) <- "integer"
  Yr <- as.matrix(dd[MIST_SCORED_REAL]); storage.mode(Yr) <- "integer"

  keep_f <- rowSums(!is.na(Yf)) > 0
  keep_r <- rowSums(!is.na(Yr)) > 0

  message(sprintf("[%s] n = %d (fake %d, real %d)",
                  cty, nrow(dd), sum(keep_f), sum(keep_r)))

  k <- k + 1

  for (blk in c("fake", "real")) {

    if (blk == "fake") {
      Y <- Yf[keep_f, , drop = FALSE]
      ids <- dd$LINE_NO[keep_f]
      seed_offset <- 2 * k - 1
    } else {
      Y <- Yr[keep_r, , drop = FALSE]
      ids <- dd$LINE_NO[keep_r]
      seed_offset <- 2 * k
    }

    for (ch in seq_len(N_CHAINS)) {

      init <- list(
        theta     = rnorm(nrow(Y), 0, 2),
        a         = runif(ncol(Y), 0.5, 2),
        b         = rnorm(ncol(Y), 0, 2),
        .RNG.name = "base::Mersenne-Twister",
        .RNG.seed = 3000 + seed_offset * 10 + ch
      )

      tasks[[length(tasks) + 1]] <- list(
        country = cty,
        block   = blk,
        chain   = ch,
        label   = sprintf("%s_%s_c%d", cty, blk, ch),
        job     = paste0(cty, "_", blk),
        ids     = ids,
        Y       = Y,
        init    = init,
        size    = nrow(Y)
      )
    }
  }
}

tasks <- tasks[order(vapply(tasks, `[[`, numeric(1), "size"),
                     decreasing = TRUE)]


# ── 2. Fit in parallel

n_cores <- parallel::detectCores(logical = FALSE)
if (is.na(n_cores)) n_cores <- parallel::detectCores()
n_workers <- min(length(tasks), max(1, n_cores - 1))

message(sprintf("\nfitting %d chains (%d jobs x %d chains) on %d workers",
                length(tasks), length(tasks) / N_CHAINS, N_CHAINS, n_workers))

cl <- parallel::makeCluster(n_workers, outfile = "")
registerDoParallel(cl)

t_all <- Sys.time()

res <- foreach(
  task = tasks,
  .packages = c("rjags", "coda"),
  .export = c("JAGS_FILE", "N_ADAPT", "N_BURN", "N_ITER", "THIN_KEEP"),
  .options.snow = list(preschedule = FALSE)
) %dopar% {

  Y  <- task$Y
  jd <- list(Y = Y, N = nrow(Y), J = ncol(Y))

  t0 <- Sys.time()
  message(sprintf("  [%-18s] start  N = %5d", task$label, jd$N))

  m <- jags.model(JAGS_FILE, data = jd, inits = task$init,
                  n.chains = 1, n.adapt = N_ADAPT, quiet = TRUE)
  update(m, n.iter = N_BURN, progress.bar = "none")

  s <- coda.samples(m, c("a", "b", "theta"), n.iter = N_ITER,
                    thin = THIN_KEEP, progress.bar = "none")

  message(sprintf("  [%-18s] done   (%.1f min)", task$label,
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))

  list(country = task$country, block = task$block, chain = task$chain,
       job = task$job, ids = task$ids, items = colnames(Y),
       N = jd$N, J = jd$J,
       minutes = as.numeric(difftime(Sys.time(), t0, units = "mins")),
       draws = s[[1]])
}

parallel::stopCluster(cl)
registerDoSEQ()

message(sprintf("\nall chains complete (%.1f min wall clock, %.1f min of CPU work)",
                as.numeric(difftime(Sys.time(), t_all, units = "mins")),
                sum(vapply(res, `[[`, numeric(1), "minutes"))))


# ── 3. Reassemble chains and compute diagnostics

set.seed(11)

fits <- list()

for (cty in CTRY) {
  fits[[cty]] <- list(country = cty)

  for (blk in c("fake", "real")) {

    idx <- which(vapply(res, `[[`, character(1), "job") ==
                   paste0(cty, "_", blk))
    idx <- idx[order(vapply(res[idx], `[[`, numeric(1), "chain"))]

    r1 <- res[[idx[1]]]
    ml <- as.mcmc.list(lapply(res[idx], `[[`, "draws"))

    i_th <- grep("^theta\\[", varnames(ml))
    s_ab <- ml[, -i_th, drop = FALSE]

    # Subsample of persons for the theta diagnostic
    i_chk <- if (length(i_th) > N_THETA_DIAG) {
      sort(sample(i_th, N_THETA_DIAG))
    } else {
      i_th
    }
    s_th <- ml[, i_chk, drop = FALSE]

    rhat_ab <- gelman.diag(s_ab, multivariate = FALSE)$psrf
    rhat_th <- gelman.diag(s_th, multivariate = FALSE)$psrf
    ess_ab  <- effectiveSize(s_ab)
    ess_th  <- effectiveSize(s_th)

    # as.matrix() stacks the chains in order, as a single n.chains fit did
    S <- as.matrix(ml)

    if (blk == "fake") fits[[cty]]$id_fake <- r1$ids
    if (blk == "real") fits[[cty]]$id_real <- r1$ids

    fits[[cty]][[blk]] <- list(
      label = paste0(cty, "_", blk),
      items = r1$items,
      N     = r1$N,
      J     = r1$J,
      # Rows are draws; the row order is aligned across a, b and theta.
      a     = S[, sprintf("a[%d]",     seq_len(r1$J)), drop = FALSE],
      b     = S[, sprintf("b[%d]",     seq_len(r1$J)), drop = FALSE],
      theta = S[, sprintf("theta[%d]", seq_len(r1$N)), drop = FALSE],
      diag  = list(
        rhat           = rhat_ab,
        ess            = ess_ab,
        rhat_theta_max = max(rhat_th[, 1], na.rm = TRUE),
        ess_theta_min  = min(ess_th, na.rm = TRUE),
        n_theta_diag   = length(i_chk)
      )
    )

    rm(ml, S); gc()
  }
}

rm(res); gc()


# ── 4. Convergence diagnostics

# rhat < 1.01, ESS > 400

irt_diag <- NULL

for (cty in CTRY) {
  for (blk in c("fake", "real")) {

    f <- fits[[cty]][[blk]]

    max_rhat       <- max(f$diag$rhat[, 1], na.rm = TRUE)
    min_ess        <- min(f$diag$ess,       na.rm = TRUE)
    max_rhat_theta <- f$diag$rhat_theta_max
    min_ess_theta  <- f$diag$ess_theta_min

    irt_diag <- rbind(irt_diag, data.frame(
      country        = cty,
      block          = blk,
      n              = f$N,
      max_rhat       = max_rhat,
      min_ess        = min_ess,
      max_rhat_theta = max_rhat_theta,
      min_ess_theta  = min_ess_theta,
      n_theta_diag   = f$diag$n_theta_diag,
      draws_kept     = nrow(f$a),
      ok             = max_rhat < 1.01 & min_ess > 400 &
                       max_rhat_theta < 1.01 & min_ess_theta > 400,
      row.names      = NULL
    ))
  }
}

print(irt_diag, digits = 3)


# ── 5. Item parameters

irt_items <- NULL

for (cty in CTRY) {
  for (blk in c("fake", "real")) {

    f <- fits[[cty]][[blk]]

    irt_items <- rbind(irt_items, data.frame(
      country   = cty,
      block     = blk,
      item      = sub("^mist_", "", f$items),
      a         = colMeans(f$a),
      a_lo      = apply(f$a, 2, quantile, 0.025),
      a_hi      = apply(f$a, 2, quantile, 0.975),
      b         = colMeans(f$b),
      b_lo      = apply(f$b, 2, quantile, 0.025),
      b_hi      = apply(f$b, 2, quantile, 0.975),
      row.names = NULL
    ))
  }
}

print(irt_items, digits = 3)


# ── 6. Save output

out <- list(
  fits      = fits,
  irt_items = irt_items,
  irt_diag  = irt_diag,
  meta      = list(date = Sys.time(), jags_file = JAGS_FILE,
                   countries = CTRY, n_sub = N_SUB, workers = n_workers,
                   n_theta_diag = N_THETA_DIAG,
                   mcmc = c(chains = N_CHAINS, adapt = N_ADAPT,
                            burnin = N_BURN, iter = N_ITER,
                            thin_keep = THIN_KEEP))
)

saveRDS(out, F_IRT)