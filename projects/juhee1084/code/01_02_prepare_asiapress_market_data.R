# =============================================================================
# 01_02_prepare_asiapress_market_data.R
# AsiaPress 북한 시장가격 자료 수집·정리 및 기술적 추세 확인
# Project: Drought Variability and Regional Rice Prices in North Korea
# Author: Ju Hee Jeung
#
# Input
# - AsiaPress 북한 시장가격 웹페이지
#
# Data output
# - 웹 수집 snapshot
# - 정제된 조사일 단위 시장자료
# - 월별 시장자료
# - 월별 시장자료 + 전년동월 대비 변화율
#
# Tables
# - 조사일 중복
# - 변수 범위
# - 관측자료 결측
# - 월별 결측
# - 무조사 월
# - 전체기간 기술적 추세
# - 2023-01~2026-07 기술적 추세
# - 전년동월 대비 변화율 요약
#
# Figures
# - 전체 시장변수 월별 추세
# - 쌀·옥수수 월별 가격 추세
# - 최근 쌀가격
# - 최근 쌀 전년동월 대비 변화율
# - 최근 옥수수가격
# - 최근 옥수수 전년동월 대비 변화율
#
# Text output
# - conclusion_ko.txt
# - conclusion_en.txt
#
# Important decisions
# - 월별 분석 종료일은 2026-07-31로 고정
# - 평양·신의주·혜산 쌀가격 및 기상자료 종료시점과 일치
# - 코드 변수명: *_change_12m_pct
# - 국문: 전년동월 대비 변화율
# - 영문: 12-month percentage change
# - YoY를 기본 변수명·표현으로 사용하지 않음
# - 임의 interpretation은 작성하지 않음
# - 실제 계산된 기술통계만 conclusion에 추가
#
# Next
# - 향후 최종 도시×월 패널 구축 시 보조 시장정보로 검토
# =============================================================================


# =============================================================================
# Workflow
# =============================================================================
# Step 0. Initial settings
# Step 1. Collect AsiaPress data
# Step 2. Clean and check observation-level data
# Step 3. Check missing values
# Step 4. Create and check monthly data
# Step 5. Calculate descriptive trend characteristics
# Step 6. Create and save figures
# Step 7. Save processed data and bilingual conclusions
# =============================================================================


# =============================================================================
# Step 0. Initial settings
# =============================================================================

source("code/00_00_initial.R")

# Current script
script_name <- "01_02_prepare_asiapress_market_data"
script_prefix <- "01_02_prepare_asiapress_market_data"

# Project paths
data_processed_dir <- file.path("data", "processed", script_name)
output_dir <- file.path("output", script_name)
figure_dir <- file.path(output_dir, "figures")
table_dir <- file.path(output_dir, "tables")

# Dates
collection_date <- Sys.Date()                   # 실제 웹 수집일
analysis_end_date <- as.Date("2026-07-31")     # 분석 종료일 고정
recent_start_date <- as.Date("2023-01-01")     # 최근기간 추세 시작
recent_end_date <- analysis_end_date

collection_tag <- format(collection_date, "%Y%m%d")
analysis_end_month <- format(analysis_end_date, "%Y-%m")
recent_start_tag <- format(recent_start_date, "%Y%m")
recent_end_tag <- format(recent_end_date, "%Y%m")
recent_period_label <- paste0(format(recent_start_date, "%Y-%m"), "–", format(recent_end_date, "%Y-%m"))

# Date checks
stopifnot(analysis_end_date <= collection_date)
stopifnot(analysis_end_date == lubridate::ceiling_date(analysis_end_date, "month") - lubridate::days(1))

# Folder check: 폴더 생성은 00_01_make_project_folders.R에서 담당
required_dirs <- c(data_processed_dir, figure_dir, table_dir)
missing_dirs <- required_dirs[!dir.exists(required_dirs)]

if (length(missing_dirs) > 0) {
  stop("필요한 프로젝트 폴더가 없습니다. 00_01_make_project_folders.R을 확인하세요:\n",
       paste(missing_dirs, collapse = "\n"))
}


# =============================================================================
# Step 1. Collect AsiaPress data
# =============================================================================

asiapress_url <- "https://www.asiapress.org/korean/nk-korea-prices/"

asiapress_page <- rvest::read_html(asiapress_url)
asiapress_tables <- asiapress_page |> rvest::html_elements("table") |> rvest::html_table(fill = TRUE)

if (length(asiapress_tables) == 0) stop("AsiaPress 페이지에서 표를 찾지 못했습니다.")

asiapress_raw <- asiapress_tables[[1]]

dplyr::glimpse(asiapress_raw)
names(asiapress_raw)
head(asiapress_raw)

# 웹 crawling으로 생성된 snapshot이므로 data/original이 아니라 processed에 저장
save_project_data(
  data = asiapress_raw,
  data_name = paste0("asiapress_market_snapshot_", collection_tag),
  script_name = script_name,
  script_prefix = script_prefix
)


# =============================================================================
# Step 2. Clean and check observation-level data
# =============================================================================


# -----------------------------------------------------------------------------
# 2-1. Clean survey dates and market variables
# -----------------------------------------------------------------------------

asiapress_data_clean <- asiapress_raw |>
  dplyr::transmute(
    date = lubridate::ymd(
      stringr::str_remove_all(
        stringr::str_extract(조사일, "\\d{4}\\s*/\\s*\\d{1,2}\\s*/\\s*\\d{1,2}"),
        "\\s"
      )
    ),
    gasoline_asiapress = readr::parse_number(휘발유, na = c("", "NA", "／", "/")),
    diesel_asiapress = readr::parse_number(디젤유, na = c("", "NA", "／", "/")),
    rice_asiapress = readr::parse_number(백미, na = c("", "NA", "／", "/")),
    corn_asiapress = readr::parse_number(옥수수, na = c("", "NA", "／", "/")),
    cny_asiapress = readr::parse_number(`중국元 환율`, na = c("", "NA", "／", "/")),
    usd_asiapress = readr::parse_number(`1USD 환율`, na = c("", "NA", "／", "/"))
  ) |>
  dplyr::arrange(date) |>
  dplyr::filter(date <= collection_date)

dplyr::glimpse(asiapress_data_clean)
head(asiapress_data_clean)
tail(asiapress_data_clean)

n_raw <- nrow(asiapress_raw)
n_clean_before_missing_drop <- nrow(asiapress_data_clean)

n_raw
n_clean_before_missing_drop
min(asiapress_data_clean$date, na.rm = TRUE)
max(asiapress_data_clean$date, na.rm = TRUE)
sum(is.na(asiapress_data_clean$date))


# -----------------------------------------------------------------------------
# 2-2. Check duplicate survey dates
# -----------------------------------------------------------------------------

asiapress_duplicate_dates <- asiapress_data_clean |>
  dplyr::count(date) |>
  dplyr::filter(n > 1)

print(asiapress_duplicate_dates, n = Inf)
if (interactive()) View(asiapress_duplicate_dates)

save_project_table(
  table = asiapress_duplicate_dates,
  table_name = "duplicate_survey_dates",
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 2-3. Check rice-price observation period
# -----------------------------------------------------------------------------

rice_dates <- asiapress_data_clean$date[!is.na(asiapress_data_clean$rice_asiapress)]

rice_observation_start <- if (length(rice_dates) == 0) as.Date(NA) else min(rice_dates)
rice_observation_end <- if (length(rice_dates) == 0) as.Date(NA) else max(rice_dates)

rice_observation_start
rice_observation_end


# -----------------------------------------------------------------------------
# 2-4. Check variable ranges
# -----------------------------------------------------------------------------

safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

asiapress_range_table <- asiapress_data_clean |>
  dplyr::summarise(
    dplyr::across(
      dplyr::where(is.numeric),
      list(min = safe_min, max = safe_max)
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(min|max)"
  )

print(asiapress_range_table, n = Inf)

save_project_table(
  table = asiapress_range_table,
  table_name = "market_variable_ranges",
  script_name = script_name,
  script_prefix = script_prefix
)


# =============================================================================
# Step 3. Check missing values
# =============================================================================


# -----------------------------------------------------------------------------
# 3-1. Remove observations with all market variables missing
# -----------------------------------------------------------------------------

asiapress_all_missing <- asiapress_data_clean |>
  dplyr::filter(dplyr::if_all(-date, is.na))

print(asiapress_all_missing, n = Inf)
if (interactive()) View(asiapress_all_missing)

save_project_table(
  table = asiapress_all_missing,
  table_name = "all_market_variables_missing_dates",
  script_name = script_name,
  script_prefix = script_prefix
)

asiapress_data_clean <- asiapress_data_clean |>
  dplyr::filter(!dplyr::if_all(-date, is.na))

n_clean_final <- nrow(asiapress_data_clean)
n_clean_final


# -----------------------------------------------------------------------------
# 3-2. Observation-level missing values
# -----------------------------------------------------------------------------

asiapress_missing_observation <- asiapress_data_clean |>
  dplyr::summarise(
    dplyr::across(
      dplyr::everything(),
      list(
        n_missing = ~ sum(is.na(.)),
        rate_missing = ~ round(mean(is.na(.)) * 100, 2)
      )
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(n_missing|rate_missing)"
  ) |>
  dplyr::mutate(rate_missing_percent = paste0(rate_missing, "%"))

print(asiapress_missing_observation, n = Inf)

save_project_table(
  table = asiapress_missing_observation,
  table_name = "observation_missing_values",
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 3-3. Dates with missing values
# -----------------------------------------------------------------------------

asiapress_missing_dates <- asiapress_data_clean |>
  dplyr::filter(dplyr::if_any(-date, is.na))

asiapress_missing_dates_table <- asiapress_missing_dates |>
  dplyr::mutate(dplyr::across(-date, ~ ifelse(is.na(.), "Missing", "")))

print(asiapress_missing_dates_table, n = Inf)
if (interactive()) View(asiapress_missing_dates_table)

save_project_table(
  table = asiapress_missing_dates_table,
  table_name = "missing_dates_and_variables",
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 3-4. Initial availability of USD exchange-rate data
# -----------------------------------------------------------------------------

usd_dates <- asiapress_data_clean$date[!is.na(asiapress_data_clean$usd_asiapress)]
first_usd_date <- if (length(usd_dates) == 0) as.Date(NA) else min(usd_dates)

if (is.na(first_usd_date)) {

  usd_availability_table <- tibble::tibble(
    period = "Entire observation period",
    date_or_period = paste0(
      format(min(asiapress_data_clean$date, na.rm = TRUE), "%Y-%m-%d"),
      " – ",
      format(max(asiapress_data_clean$date, na.rm = TRUE), "%Y-%m-%d")
    ),
    usd_data_status = "No USD exchange-rate observations"
  )

} else {

  usd_initial_missing_period <- asiapress_data_clean |>
    dplyr::filter(date < first_usd_date) |>
    dplyr::summarise(
      start_date = if (dplyr::n() == 0) as.Date(NA) else min(date),
      end_date = if (dplyr::n() == 0) as.Date(NA) else max(date),
      n_observations = dplyr::n(),
      n_usd_observed = sum(!is.na(usd_asiapress)),
      n_usd_missing = sum(is.na(usd_asiapress))
    )

  initial_period_label <- if (usd_initial_missing_period$n_observations == 0) {
    "No preceding observations"
  } else {
    paste0(
      format(usd_initial_missing_period$start_date, "%Y-%m-%d"),
      " – ",
      format(usd_initial_missing_period$end_date, "%Y-%m-%d")
    )
  }

  usd_availability_table <- tibble::tibble(
    period = c("Initial period", "First USD observation"),
    date_or_period = c(initial_period_label, format(first_usd_date, "%Y-%m-%d")),
    usd_data_status = c(
      paste0("No USD values reported across ", usd_initial_missing_period$n_observations, " observations"),
      "USD value first reported"
    )
  )
}

print(usd_availability_table, n = Inf)

save_project_table(
  table = usd_availability_table,
  table_name = "usd_initial_availability",
  script_name = script_name,
  script_prefix = script_prefix
)


# =============================================================================
# Step 4. Create and check monthly data
# =============================================================================


# -----------------------------------------------------------------------------
# 4-1. Create monthly data
# -----------------------------------------------------------------------------

mean_or_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

asiapress_monthly <- asiapress_data_clean |>
  dplyr::filter(date <= analysis_end_date) |>
  dplyr::mutate(month_date = lubridate::floor_date(date, "month")) |>
  dplyr::group_by(month_date) |>
  dplyr::summarise(
    gasoline_asiapress = mean_or_na(gasoline_asiapress),
    diesel_asiapress = mean_or_na(diesel_asiapress),
    rice_asiapress = mean_or_na(rice_asiapress),
    corn_asiapress = mean_or_na(corn_asiapress),
    cny_asiapress = mean_or_na(cny_asiapress),
    usd_asiapress = mean_or_na(usd_asiapress),
    n_obs = dplyr::n(),
    .groups = "drop"
  ) |>
  tidyr::complete(
    month_date = seq(
      min(month_date),
      lubridate::floor_date(analysis_end_date, "month"),
      by = "month"
    )
  ) |>
  dplyr::mutate(
    n_obs = tidyr::replace_na(n_obs, 0L),   # 조사 없음: n_obs만 0, 가격·환율은 NA
    month = format(month_date, "%Y-%m")
  ) |>
  dplyr::select(
    month,
    gasoline_asiapress,
    diesel_asiapress,
    rice_asiapress,
    corn_asiapress,
    cny_asiapress,
    usd_asiapress,
    n_obs
  )

dplyr::glimpse(asiapress_monthly)
print(asiapress_monthly, n = Inf)
if (interactive()) View(asiapress_monthly)


# -----------------------------------------------------------------------------
# 4-2. Save monthly data table
# -----------------------------------------------------------------------------

save_project_table(
  table = asiapress_monthly,
  table_name = "monthly_market_data",
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 4-3. Check missing monthly data
# -----------------------------------------------------------------------------

asiapress_monthly_missing <- asiapress_monthly |>
  dplyr::summarise(dplyr::across(-c(month, n_obs), ~ sum(is.na(.)))) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variable",
    values_to = "n_missing"
  )

print(asiapress_monthly_missing, n = Inf)

save_project_table(
  table = asiapress_monthly_missing,
  table_name = "monthly_missing_values",
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 4-4. Check months with no survey observations
# -----------------------------------------------------------------------------

asiapress_no_observation_months <- asiapress_monthly |>
  dplyr::filter(n_obs == 0)

print(asiapress_no_observation_months, n = Inf)
if (interactive()) View(asiapress_no_observation_months)

save_project_table(
  table = asiapress_no_observation_months,
  table_name = "months_with_no_survey_observations",
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 4-5. Check final monthly period
# -----------------------------------------------------------------------------

after_analysis_end <- asiapress_monthly |>
  dplyr::filter(month > analysis_end_month)

expected_months <- seq(
  lubridate::floor_date(min(asiapress_data_clean$date, na.rm = TRUE), "month"),
  lubridate::floor_date(analysis_end_date, "month"),
  by = "month"
)

print(after_analysis_end, n = Inf)

stopifnot(nrow(after_analysis_end) == 0)
stopifnot(max(asiapress_monthly$month) == analysis_end_month)
stopifnot(nrow(asiapress_monthly) == length(expected_months))


# =============================================================================
# Step 5. Calculate descriptive trend characteristics
# =============================================================================


# -----------------------------------------------------------------------------
# 5-1. Prepare long-format market data
# -----------------------------------------------------------------------------

market_item_labels <- tibble::tibble(
  item_order = 1:6,
  variable = c(
    "gasoline_asiapress",
    "diesel_asiapress",
    "rice_asiapress",
    "corn_asiapress",
    "cny_asiapress",
    "usd_asiapress"
  ),
  item_ko = c(
    "휘발유",
    "디젤유",
    "쌀",
    "옥수수",
    "중국 위안 환율",
    "미국 달러 환율"
  ),
  item_en = c(
    "Gasoline",
    "Diesel",
    "Rice",
    "Corn",
    "CNY exchange rate",
    "USD exchange rate"
  )
)

asiapress_monthly_base <- asiapress_monthly |>
  dplyr::mutate(
    month_date = as.Date(paste0(month, "-01")),
    month_index = lubridate::year(month_date) * 12 + lubridate::month(month_date)
  )

asiapress_monthly_long <- asiapress_monthly_base |>
  dplyr::select(
    month_date,
    month_index,
    gasoline_asiapress,
    diesel_asiapress,
    rice_asiapress,
    corn_asiapress,
    cny_asiapress,
    usd_asiapress
  ) |>
  tidyr::pivot_longer(
    cols = -c(month_date, month_index),
    names_to = "variable",
    values_to = "value"
  ) |>
  dplyr::left_join(market_item_labels, by = "variable") |>
  dplyr::arrange(item_order, month_date)


# -----------------------------------------------------------------------------
# 5-2. Function for descriptive trend statistics
# -----------------------------------------------------------------------------

summarise_market_trend <- function(data) {

  data |>
    dplyr::filter(!is.na(value)) |>
    dplyr::group_by(item_order, variable, item_ko, item_en) |>
    dplyr::summarise(
      n_observed_months = dplyr::n(),
      start_month = dplyr::first(month_date),
      end_month = dplyr::last(month_date),
      start_value = dplyr::first(value),
      end_value = dplyr::last(value),
      mean_value = mean(value, na.rm = TRUE),
      sd_value = stats::sd(value, na.rm = TRUE),
      first_to_last_change_pct = if (dplyr::first(value) == 0) {
        NA_real_
      } else {
        (dplyr::last(value) / dplyr::first(value) - 1) * 100
      },
      linear_trend_per_month = if (dplyr::n() >= 2) {
        unname(stats::coef(stats::lm(value ~ month_index))[2])
      } else {
        NA_real_
      },
      .groups = "drop"
    ) |>
    dplyr::arrange(item_order)
}


# -----------------------------------------------------------------------------
# 5-3. Full-period trend characteristics
# -----------------------------------------------------------------------------

market_trend_full_period <- summarise_market_trend(asiapress_monthly_long)

print(market_trend_full_period, n = Inf)

save_project_table(
  table = market_trend_full_period,
  table_name = "market_trend_full_period",
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 5-4. Recent-period trend characteristics: 2023-01~2026-07
# -----------------------------------------------------------------------------

market_trend_recent_period <- asiapress_monthly_long |>
  dplyr::filter(month_date >= recent_start_date, month_date <= recent_end_date) |>
  summarise_market_trend()

print(market_trend_recent_period, n = Inf)

save_project_table(
  table = market_trend_recent_period,
  table_name = paste0("market_trend_", recent_start_tag, "_", recent_end_tag),
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 5-5. Calculate 12-month percentage changes
# -----------------------------------------------------------------------------
# 국문: 전년동월 대비 변화율
# 영문: 12-month percentage change
# 달력월을 완성했으므로 lag(..., 12)는 정확히 12개월 전을 의미

asiapress_monthly_with_change_12m <- asiapress_monthly_base |>
  dplyr::arrange(month_date) |>
  dplyr::mutate(
    gasoline_change_12m_pct = ifelse(
      !is.na(gasoline_asiapress) &
        !is.na(dplyr::lag(gasoline_asiapress, 12)) &
        dplyr::lag(gasoline_asiapress, 12) != 0,
      (gasoline_asiapress / dplyr::lag(gasoline_asiapress, 12) - 1) * 100,
      NA_real_
    ),
    diesel_change_12m_pct = ifelse(
      !is.na(diesel_asiapress) &
        !is.na(dplyr::lag(diesel_asiapress, 12)) &
        dplyr::lag(diesel_asiapress, 12) != 0,
      (diesel_asiapress / dplyr::lag(diesel_asiapress, 12) - 1) * 100,
      NA_real_
    ),
    rice_change_12m_pct = ifelse(
      !is.na(rice_asiapress) &
        !is.na(dplyr::lag(rice_asiapress, 12)) &
        dplyr::lag(rice_asiapress, 12) != 0,
      (rice_asiapress / dplyr::lag(rice_asiapress, 12) - 1) * 100,
      NA_real_
    ),
    corn_change_12m_pct = ifelse(
      !is.na(corn_asiapress) &
        !is.na(dplyr::lag(corn_asiapress, 12)) &
        dplyr::lag(corn_asiapress, 12) != 0,
      (corn_asiapress / dplyr::lag(corn_asiapress, 12) - 1) * 100,
      NA_real_
    ),
    cny_change_12m_pct = ifelse(
      !is.na(cny_asiapress) &
        !is.na(dplyr::lag(cny_asiapress, 12)) &
        dplyr::lag(cny_asiapress, 12) != 0,
      (cny_asiapress / dplyr::lag(cny_asiapress, 12) - 1) * 100,
      NA_real_
    ),
    usd_change_12m_pct = ifelse(
      !is.na(usd_asiapress) &
        !is.na(dplyr::lag(usd_asiapress, 12)) &
        dplyr::lag(usd_asiapress, 12) != 0,
      (usd_asiapress / dplyr::lag(usd_asiapress, 12) - 1) * 100,
      NA_real_
    )
  )


# -----------------------------------------------------------------------------
# 5-6. Summarise 12-month percentage changes
# -----------------------------------------------------------------------------

market_change_12m_long <- asiapress_monthly_with_change_12m |>
  dplyr::select(
    month_date,
    gasoline_change_12m_pct,
    diesel_change_12m_pct,
    rice_change_12m_pct,
    corn_change_12m_pct,
    cny_change_12m_pct,
    usd_change_12m_pct
  ) |>
  tidyr::pivot_longer(
    cols = -month_date,
    names_to = "change_variable",
    values_to = "change_12m_pct"
  ) |>
  dplyr::mutate(
    variable = dplyr::recode(
      change_variable,
      gasoline_change_12m_pct = "gasoline_asiapress",
      diesel_change_12m_pct = "diesel_asiapress",
      rice_change_12m_pct = "rice_asiapress",
      corn_change_12m_pct = "corn_asiapress",
      cny_change_12m_pct = "cny_asiapress",
      usd_change_12m_pct = "usd_asiapress"
    )
  ) |>
  dplyr::left_join(market_item_labels, by = "variable") |>
  dplyr::arrange(item_order, month_date)


month_of_max <- function(month, x) {
  if (all(is.na(x))) return(as.Date(NA))
  month[which.max(replace(x, is.na(x), -Inf))]
}

month_of_min <- function(month, x) {
  if (all(is.na(x))) return(as.Date(NA))
  month[which.min(replace(x, is.na(x), Inf))]
}

latest_valid_month <- function(month, x) {
  idx <- which(!is.na(x))
  if (length(idx) == 0) as.Date(NA) else month[tail(idx, 1)]
}

latest_valid_value <- function(x) {
  idx <- which(!is.na(x))
  if (length(idx) == 0) NA_real_ else x[tail(idx, 1)]
}


market_change_12m_summary <- market_change_12m_long |>
  dplyr::group_by(item_order, variable, item_ko, item_en) |>
  dplyr::summarise(
    n_change_observed = sum(!is.na(change_12m_pct)),
    mean_change_12m_pct = if (all(is.na(change_12m_pct))) NA_real_ else mean(change_12m_pct, na.rm = TRUE),
    median_change_12m_pct = if (all(is.na(change_12m_pct))) NA_real_ else stats::median(change_12m_pct, na.rm = TRUE),
    max_change_12m_pct = if (all(is.na(change_12m_pct))) NA_real_ else max(change_12m_pct, na.rm = TRUE),
    max_change_month = month_of_max(month_date, change_12m_pct),
    min_change_12m_pct = if (all(is.na(change_12m_pct))) NA_real_ else min(change_12m_pct, na.rm = TRUE),
    min_change_month = month_of_min(month_date, change_12m_pct),
    positive_change_share_pct = if (all(is.na(change_12m_pct))) {
      NA_real_
    } else {
      mean(change_12m_pct[!is.na(change_12m_pct)] > 0) * 100
    },
    latest_change_month = latest_valid_month(month_date, change_12m_pct),
    latest_change_12m_pct = latest_valid_value(change_12m_pct),
    .groups = "drop"
  ) |>
  dplyr::arrange(item_order)

print(market_change_12m_summary, n = Inf)

save_project_table(
  table = market_change_12m_summary,
  table_name = "market_change_12m_summary",
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 5-7. Create bilingual data-based descriptive sentences
# -----------------------------------------------------------------------------
# 실제 계산값만 문장에 사용하며 경제적·인과적 해석은 추가하지 않음

format_number_1 <- function(x) if (is.na(x)) "NA" else format(round(x, 1), big.mark = ",", scientific = FALSE, trim = TRUE)
format_percent_1 <- function(x) if (is.na(x)) "NA" else paste0(format_number_1(x), "%")
format_month_ym <- function(x) if (is.na(x)) "NA" else format(as.Date(x), "%Y-%m")


change_direction_ko <- function(x) {
  if (is.na(x)) return("변화율을 계산할 수 없었다")
  if (x > 0) return(paste0(format_percent_1(abs(x)), " 증가하였다"))
  if (x < 0) return(paste0(format_percent_1(abs(x)), " 감소하였다"))
  "변화가 없었다"
}


change_direction_en <- function(x) {
  if (is.na(x)) return("could not be calculated")
  if (x > 0) return(paste0("increased by ", format_percent_1(abs(x))))
  if (x < 0) return(paste0("decreased by ", format_percent_1(abs(x))))
  "showed no change"
}


make_trend_sentence_ko <- function(
  item_ko,
  n_observed_months,
  start_month,
  end_month,
  start_value,
  end_value,
  mean_value,
  first_to_last_change_pct,
  linear_trend_per_month,
  period_name
) {

  paste0(
    "- ", item_ko, ": ", period_name, "의 유효 관측월은 ", n_observed_months, "개월이었다. ",
    "월평균 값은 ", format_number_1(mean_value), "였다. ",
    "최초 유효관측값은 ", format_month_ym(start_month), "의 ", format_number_1(start_value),
    "였으며, 최종 유효관측값은 ", format_month_ym(end_month), "의 ", format_number_1(end_value), "였다. ",
    "최초·최종 유효관측값 기준으로 ", change_direction_ko(first_to_last_change_pct), ". ",
    "월 단위 단순 OLS 선형추세계수는 ", format_number_1(linear_trend_per_month), "였다."
  )
}


make_trend_sentence_en <- function(
  item_en,
  n_observed_months,
  start_month,
  end_month,
  start_value,
  end_value,
  mean_value,
  first_to_last_change_pct,
  linear_trend_per_month,
  period_name
) {

  paste0(
    "- ", item_en, ": During ", period_name, ", there were ", n_observed_months, " valid monthly observations. ",
    "The mean monthly value was ", format_number_1(mean_value), ". ",
    "The first valid observation was ", format_number_1(start_value), " in ", format_month_ym(start_month),
    ", and the last valid observation was ", format_number_1(end_value), " in ", format_month_ym(end_month), ". ",
    "Based on the first and last valid observations, the value ", change_direction_en(first_to_last_change_pct), ". ",
    "The simple OLS linear trend coefficient was ", format_number_1(linear_trend_per_month), " per month."
  )
}


full_trend_sentences_ko <- purrr::pmap_chr(
  market_trend_full_period |>
    dplyr::select(
      item_ko,
      n_observed_months,
      start_month,
      end_month,
      start_value,
      end_value,
      mean_value,
      first_to_last_change_pct,
      linear_trend_per_month
    ),
  make_trend_sentence_ko,
  period_name = "전체 분석기간"
)


full_trend_sentences_en <- purrr::pmap_chr(
  market_trend_full_period |>
    dplyr::select(
      item_en,
      n_observed_months,
      start_month,
      end_month,
      start_value,
      end_value,
      mean_value,
      first_to_last_change_pct,
      linear_trend_per_month
    ),
  make_trend_sentence_en,
  period_name = "the full analysis period"
)


recent_trend_sentences_ko <- purrr::pmap_chr(
  market_trend_recent_period |>
    dplyr::select(
      item_ko,
      n_observed_months,
      start_month,
      end_month,
      start_value,
      end_value,
      mean_value,
      first_to_last_change_pct,
      linear_trend_per_month
    ),
  make_trend_sentence_ko,
  period_name = paste0("최근 분석기간(", recent_period_label, ")")
)


recent_trend_sentences_en <- purrr::pmap_chr(
  market_trend_recent_period |>
    dplyr::select(
      item_en,
      n_observed_months,
      start_month,
      end_month,
      start_value,
      end_value,
      mean_value,
      first_to_last_change_pct,
      linear_trend_per_month
    ),
  make_trend_sentence_en,
  period_name = paste0("the recent analysis period (", recent_period_label, ")")
)


change_12m_sentences_ko <- purrr::pmap_chr(
  market_change_12m_summary,
  function(
    item_order,
    variable,
    item_ko,
    item_en,
    n_change_observed,
    mean_change_12m_pct,
    median_change_12m_pct,
    max_change_12m_pct,
    max_change_month,
    min_change_12m_pct,
    min_change_month,
    positive_change_share_pct,
    latest_change_month,
    latest_change_12m_pct
  ) {

    paste0(
      "- ", item_ko, ": 전년동월 대비 변화율을 계산할 수 있는 관측월은 ", n_change_observed, "개월이었다. ",
      "평균은 ", format_percent_1(mean_change_12m_pct),
      ", 중앙값은 ", format_percent_1(median_change_12m_pct), "였다. ",
      "최대 변화율은 ", format_percent_1(max_change_12m_pct), " (", format_month_ym(max_change_month), "), ",
      "최소 변화율은 ", format_percent_1(min_change_12m_pct), " (", format_month_ym(min_change_month), ")였다. ",
      "양(+)의 변화율이 관측된 비중은 ", format_percent_1(positive_change_share_pct), "였다. ",
      "가장 최근 계산 가능한 변화율은 ", format_month_ym(latest_change_month), "의 ",
      format_percent_1(latest_change_12m_pct), "였다."
    )
  }
)


change_12m_sentences_en <- purrr::pmap_chr(
  market_change_12m_summary,
  function(
    item_order,
    variable,
    item_ko,
    item_en,
    n_change_observed,
    mean_change_12m_pct,
    median_change_12m_pct,
    max_change_12m_pct,
    max_change_month,
    min_change_12m_pct,
    min_change_month,
    positive_change_share_pct,
    latest_change_month,
    latest_change_12m_pct
  ) {

    paste0(
      "- ", item_en, ": The 12-month percentage change was available for ", n_change_observed, " months. ",
      "The mean was ", format_percent_1(mean_change_12m_pct),
      " and the median was ", format_percent_1(median_change_12m_pct), ". ",
      "The maximum was ", format_percent_1(max_change_12m_pct), " in ", format_month_ym(max_change_month),
      ", while the minimum was ", format_percent_1(min_change_12m_pct), " in ", format_month_ym(min_change_month), ". ",
      "Positive changes accounted for ", format_percent_1(positive_change_share_pct), " of valid observations. ",
      "The latest available 12-month percentage change was ", format_percent_1(latest_change_12m_pct),
      " in ", format_month_ym(latest_change_month), "."
    )
  }
)


# =============================================================================
# Step 6. Create and save figures
# =============================================================================

# -----------------------------------------------------------------------------
# 6-1. Full-period trends for all market variables
# -----------------------------------------------------------------------------
# 6개 시장변수를 각각의 panel에서 표시
# 선과 점은 파란색으로 통일
# 분석기간은 실제 월자료의 시작월과 종료월을 자동 표시

plot_start_date <- min(asiapress_monthly_long$month_date, na.rm = TRUE)
plot_end_date <- max(asiapress_monthly_long$month_date, na.rm = TRUE)

plot_start_label <- paste0(
  month.name[lubridate::month(plot_start_date)], " ",
  lubridate::year(plot_start_date)
)

plot_end_label <- paste0(
  month.name[lubridate::month(plot_end_date)], " ",
  lubridate::year(plot_end_date)
)

analysis_period_label <- paste0(
  "Analysis period: ",
  plot_start_label,
  "–",
  plot_end_label
)

asiapress_monthly_plot <- ggplot2::ggplot(
  asiapress_monthly_long,
  ggplot2::aes(x = month_date, y = value)
) +
  ggplot2::geom_line(color = "blue", linewidth = 0.8, na.rm = TRUE) +
  ggplot2::geom_point(color = "blue", size = 1.3, na.rm = TRUE) +
  ggplot2::facet_wrap(~ item_en, scales = "free_y", ncol = 2) +
  ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggplot2::scale_y_continuous(labels = scales::label_comma()) +
  ggplot2::labs(
    title = "Monthly Trends in AsiaPress Market Variables",
    subtitle = analysis_period_label,
    x = "Year",
    y = "Value"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

print(asiapress_monthly_plot)

save_project_plot(
  plot = asiapress_monthly_plot,
  plot_name = "monthly_market_variable_trends",
  script_name = script_name,
  script_prefix = script_prefix
)

# -----------------------------------------------------------------------------
# 6-2. Full-period rice and corn price trends
# -----------------------------------------------------------------------------

asiapress_rice_corn_plot <- asiapress_monthly_long |>
  dplyr::filter(variable %in% c("rice_asiapress", "corn_asiapress")) |>
  ggplot2::ggplot(
    ggplot2::aes(x = month_date, y = value, color = item_en)
  ) +
  ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
  ggplot2::geom_point(size = 1.5, na.rm = TRUE) +
  ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggplot2::scale_y_continuous(labels = scales::label_comma()) +
  ggplot2::labs(
    title = "Monthly Rice and Corn Prices in AsiaPress",
    subtitle = paste0("Analysis through ", format(analysis_end_date, "%Y-%m")),
    x = "Year",
    y = "Price",
    color = "Commodity"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

print(asiapress_rice_corn_plot)

save_project_plot(
  plot = asiapress_rice_corn_plot,
  plot_name = "rice_corn_monthly_price_trends",
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 6-3. Prepare recent rice and corn data: 2023-01~2026-07
# -----------------------------------------------------------------------------

recent_rice_corn_data <- asiapress_monthly_with_change_12m |>
  dplyr::filter(month_date >= recent_start_date, month_date <= recent_end_date)


# -----------------------------------------------------------------------------
# 6-4. Recent rice and corn price levels: continuous monthly axis
# -----------------------------------------------------------------------------
# 기존 Jan~Dec + 연도별 선 방식 대신
# 2023-01부터 2026-07까지 하나의 연속 월축으로 표시

recent_rice_corn_price_long <- recent_rice_corn_data |>
  dplyr::select(month_date, rice_asiapress, corn_asiapress) |>
  tidyr::pivot_longer(
    cols = c(rice_asiapress, corn_asiapress),
    names_to = "variable",
    values_to = "price"
  ) |>
  dplyr::mutate(
    item = dplyr::recode(
      variable,
      rice_asiapress = "Rice",
      corn_asiapress = "Corn"
    )
  )

recent_rice_corn_price_plot <- ggplot2::ggplot(
  recent_rice_corn_price_long,
  ggplot2::aes(x = month_date, y = price, color = item)
) +
  ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
  ggplot2::geom_point(size = 1.7, na.rm = TRUE) +
  ggplot2::scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  ggplot2::scale_y_continuous(labels = scales::label_comma()) +
  ggplot2::labs(
    title = paste0("Monthly Rice and Corn Prices: ", recent_period_label),
    x = "Month",
    y = "Price",
    color = "Commodity"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

print(recent_rice_corn_price_plot)

save_project_plot(
  plot = recent_rice_corn_price_plot,
  plot_name = "recent_rice_corn_price_levels",
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 6-5. Recent rice and corn 12-month percentage changes
# -----------------------------------------------------------------------------
# 국문: 전년동월 대비 변화율
# 영문: 12-month percentage change
# 2023-01부터 2026-07까지 연속 월축으로 표시

recent_rice_corn_change_long <- recent_rice_corn_data |>
  dplyr::select(month_date, rice_change_12m_pct, corn_change_12m_pct) |>
  tidyr::pivot_longer(
    cols = c(rice_change_12m_pct, corn_change_12m_pct),
    names_to = "variable",
    values_to = "change_12m_pct"
  ) |>
  dplyr::mutate(
    item = dplyr::recode(
      variable,
      rice_change_12m_pct = "Rice",
      corn_change_12m_pct = "Corn"
    )
  )

recent_rice_corn_change_plot <- ggplot2::ggplot(
  recent_rice_corn_change_long,
  ggplot2::aes(x = month_date, y = change_12m_pct, color = item)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
  ggplot2::geom_point(size = 1.7, na.rm = TRUE) +
  ggplot2::scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  ggplot2::labs(
    title = paste0(
      "12-Month Percentage Changes in Rice and Corn Prices: ",
      recent_period_label
    ),
    x = "Month",
    y = "12-Month Percentage Change (%)",
    color = "Commodity"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

print(recent_rice_corn_change_plot)

save_project_plot(
  plot = recent_rice_corn_change_plot,
  plot_name = "recent_rice_corn_change_12m_pct",
  script_name = script_name,
  script_prefix = script_prefix
)


# =============================================================================
# Step 7. Save processed data and bilingual conclusions
# =============================================================================


# -----------------------------------------------------------------------------
# 7-1. Create period tags
# -----------------------------------------------------------------------------

observation_start_tag <- format(min(asiapress_data_clean$date, na.rm = TRUE), "%Y%m")
observation_end_tag <- format(max(asiapress_data_clean$date, na.rm = TRUE), "%Y%m")
monthly_start_tag <- gsub("-", "", min(asiapress_monthly$month))
monthly_end_tag <- gsub("-", "", max(asiapress_monthly$month))

stopifnot(monthly_end_tag == "202607")


# -----------------------------------------------------------------------------
# 7-2. Save processed data
# -----------------------------------------------------------------------------

save_project_data(
  data = asiapress_data_clean,
  data_name = paste0("asiapress_market_observations_", observation_start_tag, "_", observation_end_tag),
  script_name = script_name,
  script_prefix = script_prefix
)

save_project_data(
  data = asiapress_monthly,
  data_name = paste0("asiapress_market_monthly_", monthly_start_tag, "_", monthly_end_tag),
  script_name = script_name,
  script_prefix = script_prefix
)

save_project_data(
  data = asiapress_monthly_with_change_12m,
  data_name = paste0("asiapress_market_monthly_change_12m_", monthly_start_tag, "_", monthly_end_tag),
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 7-3. Korean conclusion
# -----------------------------------------------------------------------------

script_conclusion_ko <- c(
  "01_02_prepare_asiapress_market_data.R 결론",
  "================================================",
  "",
  paste0("1. AsiaPress 웹자료 수집일: ", format(collection_date, "%Y-%m-%d"), "."),
  paste0("2. 분석기간: ", min(asiapress_monthly$month), " ~ ", max(asiapress_monthly$month), "."),
  paste0("3. 웹 수집 표 행 수: ", n_raw, "개, 최종 정제 관측치 수: ", n_clean_final, "개."),
  paste0("4. 조사 자체가 없었던 월 수: ", nrow(asiapress_no_observation_months), "개월."),
  paste0("5. USD 환율 최초 관측일: ", ifelse(is.na(first_usd_date), "NA", as.character(first_usd_date)), "."),
  "6. 조사 자체가 없었던 월은 행을 유지하고 시장변수는 NA, n_obs는 0으로 처리하였다.",
  "7. AsiaPress 자료는 조사 지역이 시점별로 달라 전국 평균이나 특정 지역의 대표가격으로 해석하지 않는다.",
  "8. 본 연구에서는 북한 시장상황을 확인하기 위한 보조적 시장정보로 사용한다.",
  "",
  "9. 데이터 기반 기술적 추세 요약",
  "------------------------------------",
  "아래 내용은 실제 월별자료에서 계산된 기술통계이며 인과관계 또는 통계적 유의성을 의미하지 않는다.",
  "",
  "9-1. 전체 분석기간",
  full_trend_sentences_ko,
  "",
  paste0("9-2. 최근 분석기간(", recent_period_label, ")"),
  recent_trend_sentences_ko,
  "",
  "9-3. 전년동월 대비 변화율",
  change_12m_sentences_ko
)

cat("\n", paste(script_conclusion_ko, collapse = "\n"), "\n")

save_project_text(
  text = script_conclusion_ko,
  text_name = "conclusion_ko",
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 7-4. English conclusion
# -----------------------------------------------------------------------------

script_conclusion_en <- c(
  "01_02_prepare_asiapress_market_data.R Conclusion",
  "================================================",
  "",
  paste0("1. AsiaPress web data collection date: ", format(collection_date, "%Y-%m-%d"), "."),
  paste0("2. Analysis period: ", min(asiapress_monthly$month), " to ", max(asiapress_monthly$month), "."),
  paste0("3. Number of rows collected from the web: ", n_raw,
         "; number of observations in the final cleaned dataset: ", n_clean_final, "."),
  paste0("4. Number of months with no survey observations: ", nrow(asiapress_no_observation_months), "."),
  paste0("5. First observed date for the USD exchange rate: ",
         ifelse(is.na(first_usd_date), "NA", as.character(first_usd_date)), "."),
  "6. Months with no survey observations were retained; market variables remain NA and n_obs is set to 0.",
  "7. Because AsiaPress survey locations vary over time, the data should not be interpreted as a national average or as prices representing one fixed region.",
  "8. In this study, AsiaPress data are used as supplementary information on market conditions in North Korea.",
  "",
  "9. Data-Based Descriptive Trend Summary",
  "----------------------------------------",
  "The following statements are descriptive statistics calculated directly from the monthly data and do not imply causality or statistical significance.",
  "",
  "9-1. Full analysis period",
  full_trend_sentences_en,
  "",
  paste0("9-2. Recent analysis period (", recent_period_label, ")"),
  recent_trend_sentences_en,
  "",
  "9-3. 12-month percentage changes",
  change_12m_sentences_en
)

cat("\n", paste(script_conclusion_en, collapse = "\n"), "\n")

save_project_text(
  text = script_conclusion_en,
  text_name = "conclusion_en",
  script_name = script_name,
  script_prefix = script_prefix
)


# =============================================================================
# Final check
# =============================================================================

cat("\nProcessed data files:\n")
print(list.files(data_processed_dir))

cat("\nTable files:\n")
print(list.files(table_dir))

cat("\nFigure files:\n")
print(list.files(figure_dir))

cat("\nConclusion files:\n")
print(list.files(output_dir, pattern = "conclusion"))

cat("\nAnalysis end month:\n")
print(max(asiapress_monthly$month))   # 반드시 "2026-07"