# 01_package.R ------------------------------------------------------------
# 모든 분석 스크립트가 공통으로 사용하는 패키지, 경로, 변수 생성입니다.
# 실행 순서: 02 -> 03 -> ... -> 08. 각 파일은 이 파일을 자동으로 불러옵니다.

packages <- c("readr", "dplyr", "tidyr", "ggplot2", "fixest", "lme4", "ranger", "ragg")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("먼저 설치하세요: install.packages(c(",
       paste(sprintf('"%s"', missing_packages), collapse = ", "), "))")
}
invisible(lapply(packages, library, character.only = TRUE))

set.seed(2026)
code_dir <- "C:/Users/chwou/Desktop/graduate/04_analysis/workspace/ml-paper-2026/projects/chwoun0515/code"
data_file <- file.path(code_dir, "panel_data_21-25.csv")
result_dir <- file.path(code_dir, "results")
dir.create(result_dir, showWarnings = FALSE)

panel <- readr::read_csv(data_file, locale = readr::locale(encoding = "UTF-8"),
                         show_col_types = FALSE)
if (nrow(readr::problems(panel)) > 0) stop("CSV 판독 오류가 있습니다. problems(panel)을 확인하세요.")
if (nrow(panel) != 6100 || anyDuplicated(panel[c("code", "period")])) {
  stop("시장-분기 행 수 또는 중복 키를 확인하세요.")
}

# 업종별 점포 수로 Shannon 엔트로피를 계산합니다.
industry_cols <- grep("^stores_by_industry_cs[0-9]+$", names(panel), value = TRUE)
if (length(industry_cols) == 0) stop("업종별 점포 열을 찾지 못했습니다.")
industry_matrix <- as.matrix(panel[industry_cols])
storage.mode(industry_matrix) <- "double"
industry_matrix[is.na(industry_matrix)] <- 0
industry_total <- rowSums(industry_matrix)
industry_share <- industry_matrix / pmax(industry_total, 1)
industry_share[industry_share == 0] <- NA_real_
panel$industry_entropy <- -rowSums(industry_share * log(industry_share), na.rm = TRUE)
panel$industry_entropy[industry_total == 0] <- NA_real_

# 한 시장의 분기를 시간순으로 정렬한 뒤 결과와 선행 특성을 만듭니다.
# 매출 원천 열의 정확한 집계기간은 원자료 정의서와 대조해야 합니다.
analysis_data <- panel |>
  dplyr::arrange(code, year, quarter) |>
  dplyr::group_by(code) |>
  dplyr::mutate(
    sales_amount = sales_total_current_month_sales_amount,
    store_count = stores_total_stores_count,
    open_count = stores_total_openings_stores_count,
    close_count = stores_total_closures_stores_count,
    sales_yoy_log = dplyr::if_else(
      sales_amount > 0 & dplyr::lag(sales_amount, 4) > 0,
      100 * (log(sales_amount) - log(dplyr::lag(sales_amount, 4))), NA_real_),
    net_entry_rate = dplyr::if_else(
      dplyr::lag(store_count, 1) > 0,
      100 * (open_count - close_count) / dplyr::lag(store_count, 1), NA_real_),
    sales_yoy_pct = dplyr::if_else(
      dplyr::lag(sales_amount, 4) > 0,
      100 * (sales_amount / dplyr::lag(sales_amount, 4) - 1), NA_real_),
    opening_rate = dplyr::if_else(dplyr::lag(store_count, 1) > 0,
      100 * open_count / dplyr::lag(store_count, 1), NA_real_),
    closure_rate = dplyr::if_else(dplyr::lag(store_count, 1) > 0,
      100 * close_count / dplyr::lag(store_count, 1), NA_real_),
    # t-4 특성은 매출 전년 동기 변화구간의 시작 시점입니다.
    x_entropy_sales = dplyr::lag(industry_entropy, 4),
    x_stores_sales = log1p(dplyr::lag(store_count, 4)),
    x_operating_sales = dplyr::lag(area_d_change_operation_operating_months_average, 4),
    x_foot_sales = log1p(dplyr::lag(area_street_total_foot_traffic_count, 4)),
    x_resident_sales = log1p(dplyr::lag(area_living_total_resident_count, 4)),
    x_support_sales = dplyr::lag(support_active_0_1, 4),
    # 순진입률의 시장 특성은 전분기, 지원은 연도별 자료이므로 전년 동기 값을 씁니다.
    x_entropy_entry = dplyr::lag(industry_entropy, 1),
    x_stores_entry = log1p(dplyr::lag(store_count, 1)),
    x_operating_entry = dplyr::lag(area_d_change_operation_operating_months_average, 1),
    x_foot_entry = log1p(dplyr::lag(area_street_total_foot_traffic_count, 1)),
    x_resident_entry = log1p(dplyr::lag(area_living_total_resident_count, 1)),
    x_support_entry = dplyr::lag(support_active_0_1, 4),
    x_support_history = dplyr::lag(support_recorded_before_year_0_1, 4)
  ) |>
  dplyr::ungroup()

sales_x <- c("x_entropy_sales", "x_stores_sales", "x_operating_sales",
             "x_foot_sales", "x_resident_sales", "x_support_sales")
entry_x <- c("x_entropy_entry", "x_stores_entry", "x_operating_entry",
             "x_foot_entry", "x_resident_entry", "x_support_entry")

# 가설과 달리 분석 결과를 본 뒤 분류 기준을 바꾸지 않도록 0을 고정 경계로 둡니다.
# 두 경제지표를 합산하지 않고, 각각 개선/악화의 네 조합을 표시합니다.
analysis_data <- analysis_data |>
  dplyr::mutate(economic_pattern = dplyr::case_when(
    is.na(sales_yoy_log) | is.na(net_entry_rate) ~ NA_character_,
    sales_yoy_log >= 0 & net_entry_rate >= 0 ~ "both_nonnegative",
    sales_yoy_log >= 0 & net_entry_rate < 0 ~ "sales_only",
    sales_yoy_log < 0 & net_entry_rate >= 0 ~ "entry_only",
    TRUE ~ "both_negative"
  ))

message("공통 자료 준비 완료: ", nrow(analysis_data), "행, ",
        dplyr::n_distinct(analysis_data$code), "개 시장")
