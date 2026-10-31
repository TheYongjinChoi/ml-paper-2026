# ==============================================================
# 03_descriptive.R
# ==============================================================

source("code/01_config.R")
F_DOCX <- file.path(DIR_OUT, "eSupp_missing_descriptives.docx")

sc <- readRDS(F_SCORES)

nC <- length(CTRY)

# ── 1. Variables ──────────────────────────────────────────────

if (!"region1" %in% names(df.imms)) {
  df.imms <- df.imms |>
    mutate(
      region1 = ifelse(
        ctryTxt == "USA",
        paste0("USA_", as.character(haven::as_factor(REGION_US_STATES))),
        paste0(ctryAbb, "_", as.character(haven::as_factor(ADM2)))
      ),
      region1 = ifelse(grepl("_NA$|^NA_|_$", region1), NA_character_, region1)
    )
}

covars <- intersect(COVARS_BASE, names(df.imms))

if (length(covars) != length(COVARS_BASE)) {
  stop("Covariates not found in df.imms: ",
       paste(setdiff(COVARS_BASE, covars), collapse = ", "))
}

dat <- df.imms |>
  filter(ctryTxt %in% CTRY) |>
  left_join(sc$score_df |> dplyr::select(LINE_NO, Mf, Mr, Mv, Md, Mn),
            by = "LINE_NO") |>
  mutate(ctryTxt = factor(ctryTxt, levels = CTRY))

# Labels
VAR_LABEL <- c(
  dWEIGHT   = "Design weight",
  region1   = "First-level administrative region",
  female    = "Sex (female)",
  age       = "Age group",
  edu       = "Education",
  income    = "Household financial situation",
  job       = "Employment status",
  rural     = "Urban-rural residence",
  ethnicity = "Ethnicity",
  religion  = "Religion",
  poRight6  = "Political orientation (1 = left, 6 = right)",
  vcImp2    = "Vaccines are important",
  vcChd2    = "Vaccines are important for children",
  vcSaf2    = "Vaccines are safe",
  vcEff2    = "Vaccines are effective",
  vcRel2    = "Vaccines are compatible with beliefs",

  bAccess   = "Access to vaccination services",
  bAfford   = "Affordability of vaccines",

  Mf        = "Fake news detection (expected score)",
  Mr        = "Real news detection (expected score)",
  Md        = "Distrust bias",
  Mn        = "Naivety bias"
)

VAR_GROUP <- c(
  dWEIGHT   = "Design",
  region1   = "Design",
  female    = "Covariate",
  age       = "Covariate",
  edu       = "Covariate",
  income    = "Covariate",
  job       = "Covariate",
  rural     = "Covariate",
  ethnicity = "Covariate",
  religion  = "Covariate",
  poRight6  = "Moderator",
  vcImp2    = "Outcome",
  vcChd2    = "Outcome",
  vcSaf2    = "Outcome",
  vcEff2    = "Outcome",
  vcRel2    = "Outcome",
  bAccess   = "Vaccination access",
  bAfford   = "Vaccination access",
  Mf        = "MIST score",
  Mr        = "MIST score",
  Md        = "MIST score",
  Mn        = "MIST score"
)

VARS_REPORT <- names(VAR_LABEL)


# ══════════════════════════════════════════════════════════════
# Table 1. Missing records
# ══════════════════════════════════════════════════════════════

miss_long <- dat |>
  dplyr::select(ctryTxt, all_of(VARS_REPORT)) |>
  pivot_longer(-ctryTxt, names_to = "var", values_to = "val",
               values_transform = list(val = as.character)) |>
  group_by(ctryTxt, var) |>
  summarise(n_miss = sum(is.na(val)), n = n(), .groups = "drop") |>
  mutate(pct = 100 * n_miss / n)

tab1 <- miss_long |>
  dplyr::select(ctryTxt, var, n_miss, pct) |>
  pivot_wider(names_from = ctryTxt, values_from = c(n_miss, pct),
              names_sep = "_") |>
  left_join(miss_long |> group_by(var) |>
              summarise(overall = 100 * sum(n_miss) / sum(n), .groups = "drop"),
            by = "var") |>
  arrange(desc(overall)) |>
  mutate(
    Variable = unname(VAR_LABEL[var]),
    Group    = unname(VAR_GROUP[var]),
    Handling = "Listwise deletion"
  ) |>
  dplyr::select(Group, Variable, Handling,
         starts_with("n_miss_"), starts_with("pct_"), overall)

# MIST item-level nonresponse.
mist_miss <- dat |>
  dplyr::select(ctryTxt, all_of(MIST_SCORED)) |>
  pivot_longer(-ctryTxt, names_to = "item", values_to = "val") |>
  group_by(ctryTxt) |>
  summarise(n_miss = sum(is.na(val)), n = n(), .groups = "drop") |>
  mutate(pct = 100 * n_miss / n)

tab1_mist <- data.frame(
  Group = "MIST items",
  Variable = "MIST-20 (item responses; denominator = respondents x 20)",
  Handling = "Retained; imputed within the measurement model"
)
for (c0 in CTRY) {
  tab1_mist[[paste0("n_miss_", c0)]] <- mist_miss$n_miss[mist_miss$ctryTxt == c0]
  tab1_mist[[paste0("pct_", c0)]]    <- mist_miss$pct[mist_miss$ctryTxt == c0]
}
tab1_mist$overall <- 100 * sum(mist_miss$n_miss) / sum(mist_miss$n)

tab1 <- bind_rows(tab1[names(tab1_mist)], tab1_mist) |>
  dplyr::select(Group, Variable, Handling,
         starts_with("n_miss_"), starts_with("pct_"), overall)

stopifnot(ncol(tab1) == 3 + 2 * nC + 1)

ft1 <- tab1 |>
  mutate(across(starts_with("pct_"), ~ sprintf("%.1f", .x)),
         overall = sprintf("%.1f", overall)) |>
  flextable() |>
  set_header_labels(
    values = setNames(
      as.list(c("", "Variable", "Missing-data handling",
                CTRY, CTRY, "Overall %")),
      names(tab1)
    )
  ) |>
  add_header_row(
    values = c("", "", "", "Missing (n)", "Missing (%)", ""),
    colwidths = c(1, 1, 1, nC, nC, 1),
    top = TRUE) |>
  autofit() |>
  set_table_properties(layout = "autofit", width = 1)

# ══════════════════════════════════════════════════════════════
# Table 2. Samples by model
# ══════════════════════════════════════════════════════════════

n_tab <- lapply(CTRY, function(c0) {

  dd <- dat |> filter(ctryTxt == c0)

  n_total <- nrow(dd)
  n_meas  <- sum(rowSums(!is.na(dd[MIST_SCORED])) > 0)
  base_ok <- complete.cases(dd[c("dWEIGHT", "region1", covars)])

  rows <- data.frame(
    Country = c0,
    Model = c("Total respondents",
              "Measurement model (2PL IRT)",
              "Covariates and design complete"),
    n = c(n_total, n_meas, sum(base_ok))
  )

  for (o in names(OUTCOMES)) {
    rows <- rbind(rows, data.frame(
      Country = c0,
      Model = paste0("Outcome model: ", unname(OUTCOMES[o])),
      n = sum(base_ok & !is.na(dd[[o]]))))
  }

  rows <- rbind(rows, data.frame(
    Country = c0,
    Model = "Interaction model (vaccine safety, adds political orientation)",
    n = sum(base_ok & !is.na(dd$poRight6) & !is.na(dd$vcSaf2))))

  rows$pct_of_total <- sprintf("%.1f", 100 * rows$n / n_total)
  rows
}) |> bind_rows()

ft2 <- n_tab |>
  pivot_wider(id_cols = Model, names_from = Country,
              values_from = c(n, pct_of_total), names_sep = " ") |>
  flextable() |>
  autofit() |>
  set_table_properties(layout = "autofit", width = 1)

# ══════════════════════════════════════════════════════════════
# Table 3. Descriptive statistics
# ══════════════════════════════════════════════════════════════

ana <- dat |>
  filter(if_all(all_of(c("dWEIGHT", "region1", covars)), ~ !is.na(.x)))

CAT_LEVELS <- list(
  female  = c("0" = "Other", "1" = "Female"),
  age     = c("0" = "18-34", "1" = "35-44", "2" = "45+"),
  edu     = c("0" = "Primary or below", "1" = "Secondary or above"),
  income  = c("0" = "Basic needs or below", "1" = "Basic goods or above"),
  job     = c("0" = "Employed", "1" = "Other"),
  rural   = c("0" = "Urban", "1" = "Peri-urban", "2" = "Rural"),
  bAccess = c(
    "0" = "Not at all / A little easy",
    "1" = "Moderately easy",
    "2" = "Very easy"
  ),
  bAfford = c(
    "0" = "Not at all / A little affordable",
    "1" = "Moderately affordable",
    "2" = "Very affordable"
  )
)

FACTOR_VARS <- intersect(c("ethnicity", "religion"), names(ana))
for (v in FACTOR_VARS) {
  lv <- levels(ana[[v]])
  CAT_LEVELS[[v]] <- setNames(lv, lv)
}

# Keep the reporting order of Table 1
CAT_LEVELS <- CAT_LEVELS[intersect(VARS_REPORT, names(CAT_LEVELS))]

unmapped <- lapply(names(CAT_LEVELS), function(v) {
  setdiff(unique(as.character(ana[[v]])), c(names(CAT_LEVELS[[v]]), NA))
})
names(unmapped) <- names(CAT_LEVELS)
unmapped <- unmapped[lengths(unmapped) > 0]
if (length(unmapped)) {
  stop("Values with no entry in CAT_LEVELS:\n",
       paste(sprintf("  %s: %s", names(unmapped),
                     sapply(unmapped, paste, collapse = ", ")),
             collapse = "\n"))
}

desc_cat <- lapply(names(CAT_LEVELS), function(v) {
  ana |>
    filter(!is.na(.data[[v]])) |>
    group_by(ctryTxt, lev = as.character(.data[[v]])) |>
    summarise(n = n(), w = sum(dWEIGHT), .groups = "drop_last") |>
    mutate(pct = 100 * w / sum(w)) |>
    ungroup() |>
    mutate(Variable = unname(VAR_LABEL[v]),
           Level = unname(CAT_LEVELS[[v]][lev]),
           # Preserve the declared level order rather than alphabetical
           lev_i = match(lev, names(CAT_LEVELS[[v]])),
           value = sprintf("%d (%.1f)", n, pct)) |>
    arrange(lev_i) |>
    dplyr::select(ctryTxt, Variable, Level, value)
}) |> bind_rows()

desc_bin <- lapply(names(OUTCOMES), function(v) {
  ana |>
    filter(!is.na(.data[[v]])) |>
    group_by(ctryTxt) |>
    summarise(n = sum(.data[[v]] == 1),
              pct = 100 * weighted.mean(.data[[v]], dWEIGHT), .groups = "drop") |>
    mutate(Variable = unname(VAR_LABEL[v]), Level = "Agree",
           value = sprintf("%d (%.1f)", n, pct)) |>
    dplyr::select(ctryTxt, Variable, Level, value)
}) |> bind_rows()

desc_num <- lapply(c("poRight6", "Mf", "Mr", "Md", "Mn"), function(v) {
  ana |>
    filter(!is.na(.data[[v]])) |>
    group_by(ctryTxt) |>
    summarise(m = weighted.mean(.data[[v]], dWEIGHT),
              s = sd(.data[[v]]), .groups = "drop") |>
    mutate(Variable = unname(VAR_LABEL[v]), Level = "Mean (SD)",
           value = sprintf("%.2f (%.2f)", m, s)) |>
    dplyr::select(ctryTxt, Variable, Level, value)
}) |> bind_rows()

desc_n <- ana |>
  count(ctryTxt) |>
  mutate(Variable = "Analytic sample", Level = "n",
         value = as.character(n)) |>
  dplyr::select(ctryTxt, Variable, Level, value)

tab3 <- bind_rows(desc_n, desc_cat, desc_bin, desc_num) |>
  pivot_wider(names_from = ctryTxt, values_from = value)

ft3 <- flextable(tab3) |>
  autofit() |>
  set_table_properties(layout = "autofit", width = 1)

# ══════════════════════════════════════════════════════════════
# Table 4. Exclusion vs inclusion
# ══════════════════════════════════════════════════════════════

dat$included <- complete.cases(dat[c("dWEIGHT", "region1", covars)])

cmp_vars <- c("Mf", "Mr", "poRight6", "female", "edu", "income")
cmp_vars <- intersect(cmp_vars, names(dat))

tab4 <- lapply(cmp_vars, function(v) {
  dat |>
    filter(!is.na(.data[[v]])) |>
    group_by(ctryTxt, included) |>
    summarise(m = mean(.data[[v]]), n = n(), .groups = "drop") |>
    mutate(Variable = unname(VAR_LABEL[v]),
           grp = factor(ifelse(included, "Included", "Excluded"),
                        levels = c("Included", "Excluded")),
           value = sprintf("%.2f (n=%d)", m, n)) |>
    dplyr::select(ctryTxt, Variable, grp, value)
}) |> bind_rows() |>
  pivot_wider(names_from = c(ctryTxt, grp), values_from = value,
              names_sep = " ", names_expand = TRUE)

ft4 <- flextable(tab4) |>
  autofit() |>
  set_table_properties(layout = "autofit", width = 1)

# ── Table 5. Composition of excluded respondents on factor covariates ──

tab5 <- lapply(FACTOR_VARS, function(v) {
  dat |>
    filter(!is.na(.data[[v]])) |>
    group_by(ctryTxt, included, lev = as.character(.data[[v]])) |>
    summarise(n = n(), .groups = "drop_last") |>
    mutate(pct = 100 * n / sum(n)) |>
    ungroup() |>
    mutate(Variable = unname(VAR_LABEL[v]),
           Level = lev,
           lev_i = match(lev, levels(dat[[v]])),
           grp = factor(ifelse(included, "Included", "Excluded"),
                        levels = c("Included", "Excluded")),
           value = sprintf("%d (%.1f)", n, pct)) |>
    arrange(lev_i) |>
    dplyr::select(ctryTxt, Variable, Level, grp, value)
}) |> bind_rows() |>
  pivot_wider(names_from = c(ctryTxt, grp), values_from = value,
              names_sep = " ", names_expand = TRUE)

ft5 <- flextable(tab5) |>
  autofit() |>
  set_table_properties(layout = "autofit", width = 1)

# ══════════════════════════════════════════════════════════════
# Word
# ══════════════════════════════════════════════════════════════

doc <- read_docx() |>
  body_add_par("Appendix. Missing data and sample characteristics",
               style = "heading 1") |>

  body_add_par("Table A1. Missingness by variable and country",
               style = "heading 2") |>
  body_add_flextable(ft1) |>
  body_add_par("") |>

  body_add_par("Table A2. Analytic sample size by model",
               style = "heading 2") |>
  body_add_flextable(ft2) |>
  body_add_par("") |>

  body_add_par("Table A3. Descriptive statistics, analytic sample",
               style = "heading 2") |>
  body_add_flextable(ft3) |>
  body_add_par("") |>

  body_add_par("Table A4. Included versus excluded respondents",
               style = "heading 2") |>
  body_add_flextable(ft4) |>
  body_add_par("") |>

  body_add_par("Table A5. Ethnicity and religion, included versus excluded",
               style = "heading 2") |>
  body_add_flextable(ft5)

print(doc, target = F_DOCX)