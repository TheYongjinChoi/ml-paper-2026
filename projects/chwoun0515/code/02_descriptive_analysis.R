# 02_descriptive_analysis.R -----------------------------------------------
# 목적: 표본의 구성, 결측, 시장별 경제적 변화를 먼저 확인합니다.
source("C:/Users/chwou/Desktop/graduate/04_analysis/workspace/ml-paper-2026/projects/chwoun0515/code/01_package.R")

cat("시장 수:", dplyr::n_distinct(analysis_data$code), "\n")
cat("시장-분기 행 수:", nrow(analysis_data), "\n")

variables <- c("sales_yoy_log", "net_entry_rate", "industry_entropy",
               "organization_presence_2020_0_1", "support_active_0_1")
missing_table <- data.frame(
  variable = variables,
  missing_n = vapply(analysis_data[variables], function(x) sum(is.na(x)), numeric(1)),
  missing_pct = round(100 * vapply(analysis_data[variables],
                                    function(x) mean(is.na(x)), numeric(1)), 1)
)
print(missing_table)
readr::write_csv(missing_table, file.path(result_dir, "02_missingness.csv"))

# 분기별 중앙값과 평균을 모두 봅니다. 결측은 계산에서 제외합니다.
quarter_table <- analysis_data |>
  dplyr::group_by(year, quarter) |>
  dplyr::summarise(
    n_sales = sum(!is.na(sales_yoy_log)),
    sales_mean = mean(sales_yoy_log, na.rm = TRUE),
    sales_median = median(sales_yoy_log, na.rm = TRUE),
    n_entry = sum(!is.na(net_entry_rate)),
    entry_mean = mean(net_entry_rate, na.rm = TRUE),
    entry_median = median(net_entry_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(dplyr::across(c(sales_mean, sales_median, entry_mean, entry_median),
                             ~ ifelse(is.nan(.x), NA_real_, .x)))
print(quarter_table)
readr::write_csv(quarter_table, file.path(result_dir, "02_quarter_summary.csv"))

organization_table <- analysis_data |>
  dplyr::filter(year == 2021, quarter == 1) |>
  dplyr::count(organization_presence_2020_0_1, name = "markets")
support_table <- analysis_data |>
  dplyr::filter(quarter == 1) |>
  dplyr::count(year, support_stage_observed, name = "markets")
readr::write_csv(organization_table, file.path(result_dir, "02_organization_2020.csv"))
readr::write_csv(support_table, file.path(result_dir, "02_support_by_year.csv"))
print(organization_table)
print(support_table)
