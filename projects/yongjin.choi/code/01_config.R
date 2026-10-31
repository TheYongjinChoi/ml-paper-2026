# ==============================================================
# 00_config.R
# ==============================================================

N_CORES    <- max(1, parallel::detectCores() - 1)
RUN_POOLED <- TRUE

CTRY <- c("USA", "UK", "Canada")

N_SUB <- NULL

# MCMC
N_CHAINS  <- 4
N_ADAPT   <- 2000
N_BURN    <- 5000
N_ITER    <- 10000
THIN_DIAG <- 1
THIN_KEEP <- 40

N_DRAWS_SCORE <- 1000

N_DRAWS_MI <- 200

JAGS_FILE  <- "code/2pl.jags"
DIR_OUT    <- "output"
F_IRT      <- file.path(DIR_OUT, "irt_2pl_blocks.rds")
F_SCORES   <- file.path(DIR_OUT, "mist_scores.rds")
F_RESULTS  <- file.path(DIR_OUT, "outcome_results.rds")

OUTCOMES <- c(
  vcImp2 = "Vaccines are important",
  vcChd2 = "Important for children",
  vcSaf2 = "Vaccines are safe",
  vcEff2 = "Vaccines are effective",
  vcRel2 = "Compatible with beliefs"
)

COVARS_BASE <- c("female", "age", "edu", "income", "job", "rural",
                 "ethnicity", "religion", "poRight6")
# COVARS_TRUST <- c("trHCP", "trInternet", "sns")

if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)
