# ==============================================================
# 06_outcome_models.R
# ==============================================================

source("code/00_config.R")

library(dplyr)
library(tidyr)
library(lme4)
library(future.apply)

# ── 0. Settings

if (!exists("N_CORES"))    N_CORES    <- max(1, parallel::detectCores() - 1)
if (!exists("RUN_POOLED")) RUN_POOLED <- TRUE
if (!exists("RUN_NOBAR"))  RUN_NOBAR  <- TRUE
if (!exists("RUN_CONF"))   RUN_CONF   <- TRUE

MIN_EVENTS <- 25
MIN_N      <- 100
MIN_DRAWS  <- 20

plan(multisession, workers = N_CORES)
options(future.globals.maxSize = 4 * 1024^3)

F_RESULTS_ALL <- file.path(DIR_OUT, "outcome_models.rds")

sc     <- readRDS(F_SCORES)
scores <- sc$scores

for (cty in CTRY) {
  if (is.null(scores[[cty]]$person_id)) {
    stop("scores[['", cty, "']]$person_id is missing. ",
         "The scoring script must store the ids used to build Mf/Mr.")
  }
  if (ncol(scores[[cty]]$Mf) != length(scores[[cty]]$person_id)) {
    stop("scores[['", cty, "']]: Mf has ", ncol(scores[[cty]]$Mf),
         " columns but ", length(scores[[cty]]$person_id), " ids.")
  }
}

# ── 1. Outcomes

OUTCOMES_CONF <- c(
  vcChd2 = "Important for children",
  vcImp2 = "Vaccines are important",
  vcSaf2 = "Vaccines are safe",
  vcEff2 = "Vaccines are effective",
  vcRel2 = "Compatible with beliefs"
)
OUTCOMES_VX <- c(vxMMR = "Received MMR vaccination")
OUT_LABEL   <- c(OUTCOMES_CONF, OUTCOMES_VX)

CONF_MAIN <- "vcChd2"   # mediator in the uptake model

df.imms$vxMMR <- as.integer(df.imms$vxMMR)

# ── 2. First-level administrative regions

df.imms <- df.imms |>
  dplyr::mutate(
    region1 = ifelse(
      ctryTxt == "USA",
      paste0("USA_", as.character(haven::as_factor(REGION_US_STATES))),
      paste0(ctryAbb, "_", as.character(haven::as_factor(ADM2)))
    ),
    region1 = ifelse(grepl("_NA$|^NA_|_$", region1), NA_character_, region1)
  )

# region1 entering the completeness filter as all-NA is the one failure
# that empties every model without raising an error, so check it here.
reg_ok <- tapply(!is.na(df.imms$region1), df.imms$ctryTxt, sum)
print(data.frame(
  country    = names(reg_ok),
  n_region1  = as.integer(reg_ok),
  n_distinct = as.integer(tapply(
    df.imms$region1, df.imms$ctryTxt,
    function(x) length(unique(na.omit(x))))),
  row.names = NULL))
if (any(reg_ok[CTRY] == 0, na.rm = TRUE)) {
  stop("region1 is entirely missing in at least one country.")
}

# ── 3. Analysis sample

BARRIERS    <- intersect(c("bAccess", "bAfford"), names(df.imms))
FACTOR_COVS <- intersect(c("ethnicity", "religion"), names(df.imms))

df.imms <- df.imms |>
  dplyr::mutate(across(all_of(BARRIERS), factor))

covars_base <- intersect(COVARS_BASE, names(df.imms))
covars_vx   <- c(covars_base, BARRIERS)

if (length(covars_base) != length(COVARS_BASE)) {
  stop("Covariates not found in df.imms: ",
       paste(setdiff(COVARS_BASE, covars_base), collapse = ", "))
}

# Named dat_all rather than d: a one-letter name that fails to be created
# resolves to some package's function instead, and the error then surfaces
# much later as "'filter' applied to an object of class function".
dat_all <- df.imms |>
  dplyr::select(
    LINE_NO, ctryTxt, region1, dWEIGHT,
    all_of(names(OUTCOMES_CONF)), all_of(names(OUTCOMES_VX)),
    all_of(covars_vx)
  )

stopifnot(is.data.frame(dat_all), nrow(dat_all) > 0)

# ── 4. Specifications

SPECS <- list(
  list(key = "conf",        spec = "mundlak", tag = "mundlak",
       outcomes = names(OUTCOMES_CONF), covs = covars_base, run = TRUE),
  list(key = "vx",          spec = "mundlak", tag = "mundlak",
       outcomes = names(OUTCOMES_VX),   covs = covars_vx,   run = TRUE),
  list(key = "vx_nobar",    spec = "mundlak", tag = "no_barriers",
       outcomes = names(OUTCOMES_VX),   covs = covars_base, run = RUN_NOBAR),
  list(key = "vx_conf",     spec = "mundlak", tag = "with_confidence",
       outcomes = names(OUTCOMES_VX),   covs = c(covars_vx, CONF_MAIN),
       run = RUN_CONF),
  list(key = "conf_pooled", spec = "pooled",  tag = "pooled",
       outcomes = names(OUTCOMES_CONF), covs = covars_base, run = RUN_POOLED),
  list(key = "vx_pooled",   spec = "pooled",  tag = "pooled",
       outcomes = names(OUTCOMES_VX),   covs = covars_vx,   run = RUN_POOLED)
)

# ── 5. Estimation

results     <- NULL
results_all <- list()
skipped     <- NULL

for (sp in SPECS) {

  if (!sp$run) next

  spec <- sp$spec
  tag  <- sp$tag
  covs <- sp$covs
  rows <- NULL

  terms_keep <- if (spec == "mundlak") {
    c("Mf", "Mr", "Mf_bar", "Mr_bar")
  } else {
    c("Mf", "Mr")
  }

  message(sprintf("\n[%s / %s]", sp$key, tag))

  for (cty in CTRY) {
    for (outcome in sp$outcomes) {

      tk <- Sys.time()

      s   <- scores[[cty]]
      M   <- min(N_DRAWS_MI, nrow(s$Mf))
      idx <- round(seq(1, nrow(s$Mf), length.out = M))

      base <- dat_all |>
        dplyr::filter(ctryTxt == cty, LINE_NO %in% s$person_id) |>
        dplyr::arrange(match(LINE_NO, s$person_id))

      # Misalignment here would attach the wrong scores to the wrong
      # respondents without any error.
      if (!identical(base$LINE_NO, s$person_id)) {
        stop(sprintf(
          "%s: %d of %d scored ids are absent from dat_all, so the score columns cannot be matched to survey rows.",
          cty, length(setdiff(s$person_id, base$LINE_NO)),
          length(s$person_id)))
      }

      ok <- complete.cases(base[c("dWEIGHT", "region1", covs)]) &
        !is.na(base[[outcome]])

      if (sum(ok) < MIN_N) {
        # Which covariate is emptying the sample
        miss <- vapply(c("dWEIGHT", "region1", covs, outcome),
                       function(v) mean(is.na(base[[v]])), numeric(1))
        skipped <- rbind(skipped, data.frame(
          spec = tag, country = cty, outcome = outcome,
          reason = sprintf("n = %d < %d", sum(ok), MIN_N),
          detail = sprintf("%s %.0f%% NA", names(which.max(miss)),
                           100 * max(miss)),
          row.names = NULL))
        message(sprintf("  [%-7s %-8s] skipped: n = %d (worst: %s, %.0f%% NA)",
                        cty, outcome, sum(ok),
                        names(which.max(miss)), 100 * max(miss)))
        next
      }

      tab <- table(base[[outcome]][ok])
      if (length(tab) < 2 || min(tab) < MIN_EVENTS) {
        skipped <- rbind(skipped, data.frame(
          spec = tag, country = cty, outcome = outcome,
          reason = sprintf("minority category = %d < %d",
                           if (length(tab) < 2) 0 else min(tab), MIN_EVENTS),
          detail = NA_character_, row.names = NULL))
        message(sprintf("  [%-7s %-8s] skipped: minority category = %d",
                        cty, outcome,
                        if (length(tab) < 2) 0 else min(tab)))
        next
      }

      dat0 <- base[ok, ]
      dat0$region1 <- factor(dat0$region1)
      R <- nlevels(dat0$region1)
      dat0 <- droplevels(dat0)

      Mf_sub <- s$Mf[idx, ok, drop = FALSE]
      Mr_sub <- s$Mr[idx, ok, drop = FALSE]

      f <- if (spec == "mundlak") {
        as.formula(paste(outcome, "~ Mf + Mr + Mf_bar + Mr_bar +",
                         paste(covs, collapse = " + "), "+ (1 | region1)"))
      } else {
        as.formula(paste(outcome, "~ Mf + Mr +",
                         paste(covs, collapse = " + ")))
      }

      # The only function in the script: future_lapply needs one to send
      # each posterior draw to a worker. Set plan(sequential) to step
      # through it with the usual debugging tools.
      mi <- future_lapply(seq_len(M), function(i) {

        dat <- dat0
        dat$Mf <- as.vector(scale(Mf_sub[i, ]))
        dat$Mr <- as.vector(scale(Mr_sub[i, ]))

        if (spec == "mundlak") {
          dat$Mf_bar <- ave(dat$Mf, dat$region1)
          dat$Mr_bar <- ave(dat$Mr, dat$region1)
        }

        # f was built in the loop, so its environment is the loop frame,
        # where `dat` does not exist. glmer() resolves `weights` in the
        # formula environment, so it is repointed here. Each worker edits
        # its own copy of f.
        environment(f) <- environment()

        tryCatch({
          if (spec == "mundlak") {
            m <- lme4::glmer(
              f, data = dat, family = binomial, weights = dWEIGHT,
              control = lme4::glmerControl(optimizer = "bobyqa",
                                           calc.derivs = FALSE)
            )
            sg <- as.numeric(attr(lme4::VarCorr(m)$region1, "stddev"))
          } else {
            m <- glm(f, data = dat, family = binomial, weights = dWEIGHT)
            sg <- NA_real_
          }
          cf <- summary(m)$coefficients
          if (!all(terms_keep %in% rownames(cf))) {
            stop("terms dropped: ",
                 paste(setdiff(terms_keep, rownames(cf)), collapse = ", "))
          }
          list(est     = cf[terms_keep, "Estimate"],
               var     = cf[terms_keep, "Std. Error"]^2,
               sigma_u = sg,
               err     = NA_character_)
        }, error = function(e) list(err = conditionMessage(e)))

      }, future.seed = TRUE, future.packages = "lme4")

      bad  <- vapply(mi, function(x) is.null(x$est), logical(1))
      errs <- unique(na.omit(vapply(mi[bad], function(x) x$err,
                                    character(1))))
      mi   <- mi[!bad]

      if (length(mi) < MIN_DRAWS) {
        skipped <- rbind(skipped, data.frame(
          spec = tag, country = cty, outcome = outcome,
          reason = sprintf("only %d of %d draws fitted", length(mi), M),
          detail = if (length(errs)) errs[1] else NA_character_,
          row.names = NULL))
        message(sprintf("  [%-7s %-8s] skipped: %d/%d draws fitted. %s",
                        cty, outcome, length(mi), M,
                        if (length(errs)) errs[1] else ""))
        next
      }

      est  <- do.call(rbind, lapply(mi, `[[`, "est"))
      vr   <- do.call(rbind, lapply(mi, `[[`, "var"))
      su   <- vapply(mi, `[[`, numeric(1), "sigma_u")
      Meff <- nrow(est)

      # Rubin's rules
      for (tm in terms_keep) {

        Qbar   <- mean(est[, tm])
        Ubar   <- mean(vr[, tm])
        B      <- var(est[, tm])
        Tvar   <- Ubar + (1 + 1 / Meff) * B
        lambda <- (1 + 1 / Meff) * B / Tvar
        dfr    <- (Meff - 1) / max(lambda, 1e-8)^2
        crit   <- qt(0.975, dfr) * sqrt(Tvar)

        rows <- rbind(rows, data.frame(
          country       = cty,
          domain        = if (outcome %in% names(OUTCOMES_VX)) "uptake"
                          else "confidence",
          spec          = tag,
          outcome       = outcome,
          outcome_label = unname(OUT_LABEL[outcome]),
          term          = tm,
          est           = Qbar,
          se            = sqrt(Tvar),
          lo            = Qbar - crit,
          hi            = Qbar + crit,
          or            = exp(Qbar),
          or_lo         = exp(Qbar - crit),
          or_hi         = exp(Qbar + crit),
          lambda        = lambda,
          sigma_u       = mean(su),
          n             = sum(ok),
          n_event       = sum(dat0[[outcome]] == 1),
          n_neg         = sum(dat0[[outcome]] == 0),
          n_region      = R,
          M             = Meff,
          M_failed      = sum(bad),
          row.names     = NULL
        ))
      }

      message(sprintf("  [%-7s %-8s] %.1f min", cty, outcome,
                      as.numeric(difftime(Sys.time(), tk, units = "mins"))))
    }
  }

  results_all[[sp$key]] <- rows
  results <- rbind(results, rows)
}

plan(sequential)

if (!is.null(skipped)) {
  message("\nSkipped model cells")
  print(skipped, row.names = FALSE)
}

# Everything below assumes at least one fitted model. Stopping here names
# the cause instead of raising the same NULL error in every print block.
if (is.null(results) || nrow(results) == 0) {
  stop("No model produced estimates. See the skipped table above.")
}

# ── 6. Output

conf_tab <- results |>
  dplyr::filter(domain == "confidence", spec == "mundlak",
                term %in% c("Mf", "Mr"))

if (nrow(conf_tab) > 0) {
  message("\nConfidence outcomes — within-region estimates (OR, 95% CI)")
  message(sprintf("Primary outcome: %s; the rest are sensitivity analyses.",
                  CONF_MAIN))
  print(
    conf_tab |>
      dplyr::arrange(country, outcome != CONF_MAIN, outcome_label,
                     desc(term)) |>
      dplyr::select(country, outcome_label, term, or, or_lo, or_hi,
                    lambda, n),
    digits = 3, row.names = FALSE
  )
}

vx_tab <- results |>
  dplyr::filter(domain == "uptake", spec == "mundlak",
                term %in% c("Mf", "Mr"))

if (nrow(vx_tab) > 0) {
  message("\nUptake — within-region estimates (OR, 95% CI)")
  message("Outcome coded 1 = vaccinated, so OR > 1 supports the hypothesis")
  print(
    vx_tab |>
      dplyr::select(country, term, or, or_lo, or_hi, lambda, sigma_u,
                    n, n_neg, n_region),
    digits = 3, row.names = FALSE
  )
}

message("\nlambda: share of total uncertainty from measurement error")
print(aggregate(lambda ~ domain + spec, data = results, FUN = mean),
      digits = 3, row.names = FALSE)

sig_tab <- results |>
  dplyr::filter(spec == "mundlak", term == "Mf")

if (nrow(sig_tab) > 0) {
  message("\nsigma_u: SD of the baseline outcome across regions (log odds)")
  print(
    sig_tab |> dplyr::select(country, outcome_label, sigma_u, n_region),
    digits = 3, row.names = FALSE
  )
}

# Each comparison needs both specifications present, so build the wide
# table first and only print it when both columns exist.

nobar_tab <- results |>
  dplyr::filter(domain == "uptake", term == "Mf",
                spec %in% c("mundlak", "no_barriers")) |>
  dplyr::select(country, spec, est, n) |>
  pivot_wider(names_from = spec, values_from = c(est, n))

if (all(c("est_mundlak", "est_no_barriers") %in% names(nobar_tab))) {
  message("\nUptake: with and without barrier covariates (Mf)")
  print(
    nobar_tab |>
      dplyr::mutate(diff_pct = 100 * (est_mundlak / est_no_barriers - 1)),
    digits = 3, row.names = FALSE
  )
}

conf_adj_tab <- results |>
  dplyr::filter(domain == "uptake", term == "Mf",
                spec %in% c("mundlak", "with_confidence")) |>
  dplyr::select(country, spec, est, n) |>
  pivot_wider(names_from = spec, values_from = c(est, n))

if (all(c("est_mundlak", "est_with_confidence") %in% names(conf_adj_tab))) {
  message(sprintf("\nUptake: with and without %s (Mf)", CONF_MAIN))
  print(
    conf_adj_tab |>
      dplyr::mutate(pct_via_confidence =
                      100 * (1 - est_with_confidence / est_mundlak)),
    digits = 3, row.names = FALSE
  )
}

pooled_tab <- results |>
  dplyr::filter(term == "Mf", spec %in% c("mundlak", "pooled")) |>
  dplyr::select(country, outcome, spec, est) |>
  pivot_wider(names_from = spec, values_from = est)

if (all(c("mundlak", "pooled") %in% names(pooled_tab))) {
  message("\nWith and without regional effects (Mf)")
  print(
    pooled_tab |> dplyr::mutate(diff_pct = 100 * (mundlak / pooled - 1)),
    digits = 3, row.names = FALSE
  )
}

if (any(results$M_failed > 0)) {
  message("\nPosterior draws with convergence failures")
  print(
    results |>
      dplyr::filter(M_failed > 0) |>
      dplyr::select(country, outcome, spec, M, M_failed) |>
      dplyr::distinct(),
    row.names = FALSE
  )
}

# ── 7. Save

saveRDS(
  list(
    results = results,
    skipped = skipped,
    meta = list(
      date          = Sys.time(),
      n_draws_mi    = N_DRAWS_MI,
      covars_base   = covars_base,
      covars_vx     = covars_vx,
      factor_covs   = FACTOR_COVS,
      barriers      = BARRIERS,
      conf_main     = CONF_MAIN,
      outcomes_conf = OUTCOMES_CONF,
      outcomes_vx   = OUTCOMES_VX,
      n_cores       = N_CORES,
      min_events    = MIN_EVENTS,
      min_n         = MIN_N,
      min_draws     = MIN_DRAWS,
      note = paste(
        "Note: xxx"
      )
    )
  ),
  F_RESULTS_ALL
)