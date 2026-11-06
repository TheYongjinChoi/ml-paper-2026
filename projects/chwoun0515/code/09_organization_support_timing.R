# 09_organization_support_timing.R ----------------------------------------
# 2020년 조직 형태와 2021~2025년 처음 관찰된 지원중 연도를 연결합니다.
# 지원기간은 연도 단위입니다. 이 표들은 선정 효과나 인과효과를 추정하지 않습니다.
source("C:/Users/chwou/Desktop/graduate/04_analysis/workspace/ml-paper-2026/projects/chwoun0515/code/01_package.R")

market_timing <- analysis_data |>
  dplyr::filter(quarter == 1) |>
  dplyr::group_by(code) |>
  dplyr::summarise(
    name = dplyr::first(name),
    organization_type_2020 = dplyr::first(organization_type_2020),
    organization_presence_2020_0_1 = dplyr::first(organization_presence_2020_0_1),
    first_observed_support_year = if (any(support_active_0_1 == 1, na.rm = TRUE))
      min(year[support_active_0_1 == 1]) else NA_integer_,
    observed_support_years = sum(support_active_0_1 == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    organization_group_2020 = dplyr::case_when(
      organization_type_2020 == intToUtf8(c(48277,51201,45800,52404)) ~ "legal",
      organization_type_2020 == intToUtf8(c(51076,51032,45800,52404)) ~ "informal",
      organization_presence_2020_0_1 == 0 ~ "no_recorded_group",
      TRUE ~ "unmatched"
    ),
    first_support_label = ifelse(is.na(first_observed_support_year),
                                 "not_observed_2021_2025",
                                 as.character(first_observed_support_year))
  )
readr::write_csv(market_timing, file.path(result_dir, "09_market_first_support.csv"))

timing_table <- market_timing |>
  dplyr::count(organization_group_2020, first_support_label, name = "markets")
readr::write_csv(timing_table, file.path(result_dir, "09_timing_by_organization.csv"))
print(timing_table)

# 지원 시작 '전년도'와 '다음 연도'의 평균을 같은 시장에서 비교합니다.
# 지원 시작 연도는 실제 시작 분기를 알 수 없어 비교에서 제외합니다.
# 최초 관찰이 2021년인 시장은 관찰 시작 전에 이미 지원받았을 수 있어 제외합니다.
annual_outcomes <- analysis_data |>
  dplyr::group_by(code, year) |>
  dplyr::summarise(
    annual_sales_yoy_log = if (all(is.na(sales_yoy_log))) NA_real_
      else mean(sales_yoy_log, na.rm = TRUE),
    annual_net_entry_rate = if (all(is.na(net_entry_rate))) NA_real_
      else mean(net_entry_rate, na.rm = TRUE),
    .groups = "drop"
  )

before_after <- market_timing |>
  dplyr::filter(first_observed_support_year >= 2022,
                first_observed_support_year <= 2024) |>
  dplyr::select(code, name, organization_group_2020, first_observed_support_year) |>
  dplyr::left_join(annual_outcomes, by = "code") |>
  dplyr::filter(year == first_observed_support_year - 1L |
                year == first_observed_support_year + 1L) |>
  dplyr::mutate(relative_year = ifelse(year < first_observed_support_year,
                                       "before", "after")) |>
  dplyr::select(-year) |>
  tidyr::pivot_wider(names_from = relative_year,
                     values_from = c(annual_sales_yoy_log, annual_net_entry_rate)) |>
  dplyr::mutate(
    sales_change_after_minus_before = annual_sales_yoy_log_after - annual_sales_yoy_log_before,
    entry_change_after_minus_before = annual_net_entry_rate_after - annual_net_entry_rate_before
  )
readr::write_csv(before_after, file.path(result_dir, "09_before_after_market.csv"))

before_after_summary <- before_after |>
  dplyr::group_by(organization_group_2020, first_observed_support_year) |>
  dplyr::summarise(
    markets = dplyr::n(),
    markets_with_sales_pair = sum(!is.na(sales_change_after_minus_before)),
    mean_sales_change = if (all(is.na(sales_change_after_minus_before))) NA_real_
      else mean(sales_change_after_minus_before, na.rm = TRUE),
    markets_with_entry_pair = sum(!is.na(entry_change_after_minus_before)),
    mean_entry_change = if (all(is.na(entry_change_after_minus_before))) NA_real_
      else mean(entry_change_after_minus_before, na.rm = TRUE),
    .groups = "drop"
  )
readr::write_csv(before_after_summary, file.path(result_dir, "09_before_after_summary.csv"))
print(before_after_summary)

# 법적단체 시장 안에서 '첫 지원이 관찰된 다음 연도 이후'와 결과의 관계를 봅니다.
# 2021년에 이미 지원중인 시장은 시작 연도를 알 수 없어 제외합니다.
# 2025년 첫 지원은 후속 결과가 없으므로 비교군으로 사용하지 않습니다.
# 이 고정효과 계수는 선정 과정과 지원 이외 변화를 분리하지 못합니다.
legal_timing <- market_timing |>
  dplyr::filter(organization_group_2020 == "legal",
                is.na(first_observed_support_year) |
                  first_observed_support_year >= 2022) |>
  dplyr::select(code, first_observed_support_year)
legal_panel <- analysis_data |>
  dplyr::inner_join(legal_timing, by = "code") |>
  dplyr::mutate(
    post_first_support = as.integer(!is.na(first_observed_support_year) &
      year > first_observed_support_year),
    # 지원 개시 연도는 분기 내 정확한 노출을 알 수 없어 제외합니다.
    support_start_year = !is.na(first_observed_support_year) &
      year == first_observed_support_year
  ) |>
  dplyr::filter(!support_start_year, year >= 2022)

run_legal_timing_fe <- function(outcome, predictors, label) {
  predictors <- setdiff(predictors, c("x_support_sales", "x_support_entry"))
  d <- legal_panel |>
    dplyr::select(code, period, post_first_support,
                  dplyr::all_of(c(outcome, predictors))) |>
    tidyr::drop_na()
  fit <- fixest::feols(
    stats::as.formula(paste(outcome, "~",
      paste(c("post_first_support", predictors), collapse = " + "),
      "| code + period")), data = d, vcov = ~ code)
  capture.output(summary(fit),
    file = file.path(result_dir, paste0("09_legal_timing_FE_", label, ".txt")))
  tab <- as.data.frame(fixest::coeftable(fit))
  tab$variable <- rownames(tab)
  tab$outcome <- outcome
  tab$n <- stats::nobs(fit)
  tab$markets <- dplyr::n_distinct(d$code)
  tab$treated_markets <- dplyr::n_distinct(d$code[d$post_first_support == 1])
  tab
}
timing_fe <- dplyr::bind_rows(
  run_legal_timing_fe("sales_yoy_log", sales_x, "sales"),
  run_legal_timing_fe("net_entry_rate", entry_x, "entry"))
readr::write_csv(timing_fe, file.path(result_dir, "09_legal_timing_FE_coefficients.csv"))
print(timing_fe |>
  dplyr::filter(variable == "post_first_support") |>
  dplyr::select(outcome, Estimate, `Std. Error`, n, markets, treated_markets))

cat("주의: 2021년은 최초 '관찰' 연도이고, 2025년 신규 지원은 다음 연도 결과가 없어 전후 비교에서 제외합니다.\n")
cat("전후 차이는 공통 경기변화, 사업 선정 및 조직 변화를 분리하지 않은 기술통계입니다.\n")
