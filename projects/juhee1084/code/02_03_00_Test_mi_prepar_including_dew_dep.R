# =============================================================================
# 02_03_00_Test_mi_prepare_including_dew_dep.R
#
# TEST: Multiple Imputation preparation including Dew-Point Depression
#
# 목적:
# - dew_dep를 MICE 후보 predictor에 포함하여 초기 변수구조를 검토
# - 본 분석과 분리된 독립 TEST로 보존
#
# 원칙:
# - TEST 객체는 모두 test_ 로 시작
# - TEST 자료와 결과는 TEST 전용 폴더에만 저장
# - 본 분석의 data/, output/에는 저장하지 않음
#
# TEST root:
# test/02-03-00_Test_mi_prepar_including_dew_dep/
#
# Raw input:
# nk_weather_daily_precip_raw_test_197301_202607.rds
#
# Data output:
# data/test_nk_weather_daily_02_03_00_mi_prepare_including_dew_dep.rds
# =============================================================================



# =============================================================================
# TEST Workflow
# =============================================================================
#
# 02_03_00_Test_mi_prepare_including_dew_dep.R
#   Step 1. Raw 자료 불러오기
#   Step 2. 강수량 결측 확인
#   Step 3. MICE용 시간·관측소 변수 및 dew_dep 생성
#   Step 4. 후보 predictor 선정
#   Step 5. 강수 결측일에서 predictor 가용률 확인
#   Step 6-1. 연속형 predictor 간 상관관계 확인
#
# 이 TEST는 독립 검토자료이며 본 MICE 분석과 별도로 관리함.
# =============================================================================



# =============================================================================
# 0. Initial settings
# =============================================================================

# 필요한 패키지
library(tidyverse)
library(lubridate)
library(mice)


# 실제 TEST 폴더
test_root <- "test/02-03-00_Test_mi_prepar_including_dew_dep"


# TEST 내부 경로
test_data_dir <- file.path(test_root, "data")
test_output_dir <- file.path(test_root, "output")
test_figure_dir <- file.path(test_output_dir, "figures")
test_table_dir <- file.path(test_output_dir, "tables")


# Raw 자료
test_raw_file <- file.path(
  test_root,
  "nk_weather_daily_precip_raw_test_197301_202607.rds"
)


# TEST 준비자료 저장파일
test_output_file <- file.path(
  test_data_dir,
  "test_nk_weather_daily_02_03_00_mi_prepare_including_dew_dep.rds"
)


# 폴더 확인
if (!dir.exists(test_root)) stop("TEST root 폴더가 없습니다: ", test_root)
if (!dir.exists(test_data_dir)) stop("TEST data 폴더가 없습니다: ", test_data_dir)
if (!dir.exists(test_figure_dir)) stop("TEST figures 폴더가 없습니다: ", test_figure_dir)
if (!dir.exists(test_table_dir)) stop("TEST tables 폴더가 없습니다: ", test_table_dir)


# Raw 자료 확인
if (!file.exists(test_raw_file)) stop("TEST raw data를 찾을 수 없습니다: ", test_raw_file)



# =============================================================================
# Step 1. Raw 자료 불러오기
# =============================================================================

# TEST 일별 기상자료
test_nk_weather_daily_raw <- readRDS(test_raw_file)

# 자료 구조 확인
dplyr::glimpse(test_nk_weather_daily_raw)



# =============================================================================
# Step 2. 강수량 결측 확인
# =============================================================================

# 강수량 기본 통계
summary(test_nk_weather_daily_raw$precip_raw)

# 강수량 결측 개수
sum(is.na(test_nk_weather_daily_raw$precip_raw))   # 기존 확인값: 315,671

# 음수 강수량 확인
sum(test_nk_weather_daily_raw$precip_raw < 0, na.rm = TRUE)


# 전체 변수별 결측 개수와 결측률
test_missing_table <-
  test_nk_weather_daily_raw |>
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


# 결과 확인
print(test_missing_table, n = Inf, width = Inf)
if (interactive()) View(test_missing_table)


# 표 저장
readr::write_csv(
  test_missing_table,
  file.path(test_table_dir, "test_missing_table.csv")
)



# =============================================================================
# Step 3. MICE용 시간·관측소 변수 및 dew_dep 생성
# =============================================================================

# TEST에서만 dew_dep 생성
test_nk_weather_daily_mi <-
  test_nk_weather_daily_raw |>
  dplyr::mutate(
    date = as.Date(date),
    year = lubridate::year(date),
    month = factor(lubridate::month(date), levels = 1:12),
    station_factor = factor(station_name_en),

    # Dew-Point Depression = 평균기온 - 이슬점온도
    dew_dep = temperature_mean_day - dewpoint_mean_day
  )


# 확인
dplyr::glimpse(test_nk_weather_daily_mi)



# =============================================================================
# Step 4. 후보 predictor 선정
# =============================================================================

# TEST에서는 dew_dep 포함
test_mice_variables <- c(
  "precip_raw",
  "temperature_mean_day",
  "temperature_max_day",
  "temperature_min_day",
  "dewpoint_mean_day",
  "dew_dep",
  "humidity_mean_day",
  "cloud_mean_day",
  "wind_mean_day",
  "sea_pressure_mean_day",
  "year",
  "month",
  "station_factor"
)


# 후보 predictor 자료
test_mice_data <-
  test_nk_weather_daily_mi |>
  dplyr::select(dplyr::all_of(test_mice_variables))


# 확인
dplyr::glimpse(test_mice_data)



# =============================================================================
# Step 5. 강수 결측일에서 predictor 가용률 확인
# =============================================================================

# 강수 결측일에서 각 predictor의 사용 가능 비율 확인
test_mice_predictor_availability <-
  test_mice_data |>
  dplyr::filter(is.na(precip_raw)) |>
  dplyr::select(
    temperature_mean_day,
    temperature_max_day,
    temperature_min_day,
    dewpoint_mean_day,
    dew_dep,
    humidity_mean_day,
    cloud_mean_day,
    wind_mean_day,
    sea_pressure_mean_day
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variable",
    values_to = "value"
  ) |>
  dplyr::group_by(variable) |>
  dplyr::summarise(
    n_precip_missing = dplyr::n(),
    n_available = sum(!is.na(value)),
    n_unavailable = sum(is.na(value)),
    availability_rate = round(n_available / n_precip_missing * 100, 1),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(availability_rate))


# 결과 확인
print(test_mice_predictor_availability, n = Inf, width = Inf)
if (interactive()) View(test_mice_predictor_availability)


# 표 저장
readr::write_csv(
  test_mice_predictor_availability,
  file.path(test_table_dir, "test_mice_predictor_availability.csv")
)



# =============================================================================
# Step 6-1. 연속형 predictor 간 상관관계 확인
# =============================================================================

# 연속형 기상 predictor
test_continuous_predictors <- c(
  "temperature_mean_day",
  "temperature_max_day",
  "temperature_min_day",
  "dewpoint_mean_day",
  "dew_dep",
  "humidity_mean_day",
  "cloud_mean_day",
  "wind_mean_day",
  "sea_pressure_mean_day"
)



# -----------------------------------------------------------------------------
# Step 6-1-1. Pearson correlation
# -----------------------------------------------------------------------------

test_predictor_correlation_pearson <-
  test_mice_data |>
  dplyr::select(dplyr::all_of(test_continuous_predictors)) |>
  stats::cor(use = "pairwise.complete.obs", method = "pearson")


# 결과 확인
round(test_predictor_correlation_pearson, 2)
if (interactive()) View(test_predictor_correlation_pearson)



# -----------------------------------------------------------------------------
# Step 6-1-2. 참고용 Spearman correlation
# -----------------------------------------------------------------------------

# TEST 내부 비교용
test_predictor_correlation_spearman <-
  test_mice_data |>
  dplyr::select(dplyr::all_of(test_continuous_predictors)) |>
  stats::cor(use = "pairwise.complete.obs", method = "spearman")


# 결과 확인
round(test_predictor_correlation_spearman, 2)
if (interactive()) View(test_predictor_correlation_spearman)



# -----------------------------------------------------------------------------
# Step 6-1-3. 상관행렬 표 저장
# -----------------------------------------------------------------------------

# Pearson 표
test_pearson_table <-
  as.data.frame(test_predictor_correlation_pearson) |>
  tibble::rownames_to_column(var = "variable")

readr::write_csv(
  test_pearson_table,
  file.path(test_table_dir, "test_predictor_correlation_pearson.csv")
)


# Spearman 표
test_spearman_table <-
  as.data.frame(test_predictor_correlation_spearman) |>
  tibble::rownames_to_column(var = "variable")

readr::write_csv(
  test_spearman_table,
  file.path(test_table_dir, "test_predictor_correlation_spearman.csv")
)



# -----------------------------------------------------------------------------
# Step 6-1-4. 상관행렬 그림용 자료 준비
# -----------------------------------------------------------------------------

# 그림 변수 순서
test_correlation_variable_order <- c(
  "cloud_mean_day",
  "dew_dep",
  "dewpoint_mean_day",
  "humidity_mean_day",
  "temperature_max_day",
  "temperature_mean_day",
  "temperature_min_day",
  "sea_pressure_mean_day",
  "wind_mean_day"
)


# 그림용 변수명
test_correlation_variable_labels <- c(
  cloud_mean_day        = "Cloud",
  dew_dep               = "Dew-Point Depression",
  dewpoint_mean_day     = "Dew Point",
  humidity_mean_day     = "Humidity",
  temperature_max_day   = "Max Temp",
  temperature_mean_day  = "Mean Temp",
  temperature_min_day   = "Min Temp",
  sea_pressure_mean_day = "Sea-level Pressure",
  wind_mean_day         = "Wind"
)


# Pearson 행렬을 long format으로 변환
test_predictor_correlation_plot_data <-
  test_predictor_correlation_pearson |>
  as.data.frame() |>
  tibble::rownames_to_column(var = "variable_1") |>
  tidyr::pivot_longer(
    cols = -variable_1,
    names_to = "variable_2",
    values_to = "correlation"
  ) |>
  dplyr::mutate(
    row_number = match(variable_1, test_correlation_variable_order),
    column_number = match(variable_2, test_correlation_variable_order)
  ) |>
  dplyr::filter(row_number > column_number) |>
  dplyr::mutate(
    variable_1 = dplyr::recode(
      variable_1,
      !!!test_correlation_variable_labels
    ),
    variable_2 = dplyr::recode(
      variable_2,
      !!!test_correlation_variable_labels
    )
  )


# 축 순서
test_correlation_label_order <-
  unname(
    test_correlation_variable_labels[
      test_correlation_variable_order
    ]
  )


test_predictor_correlation_plot_data <-
  test_predictor_correlation_plot_data |>
  dplyr::mutate(
    variable_2 = factor(
      variable_2,
      levels = test_correlation_label_order
    ),
    variable_1 = factor(
      variable_1,
      levels = rev(test_correlation_label_order)
    )
  )



# -----------------------------------------------------------------------------
# Step 6-1-5. English heatmap
# -----------------------------------------------------------------------------

test_correlation_heatmap_plot_en <-
  ggplot2::ggplot(
    test_predictor_correlation_plot_data,
    ggplot2::aes(x = variable_2, y = variable_1, fill = correlation)
  ) +
  ggplot2::geom_tile(color = "white", linewidth = 0.5) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", correlation)),
    size = 3.4
  ) +
  ggplot2::scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  ggplot2::labs(
    title = "Correlation Matrix of Candidate Continuous Predictors",
    subtitle = "Pearson correlation coefficients",
    x = NULL,
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = ggplot2::element_text(size = 9, margin = ggplot2::margin(r = 6)),
    plot.title = ggplot2::element_text(face = "bold", size = 13),
    plot.subtitle = ggplot2::element_text(size = 10),
    legend.title = ggplot2::element_text(size = 9),
    legend.text = ggplot2::element_text(size = 8),
    plot.margin = ggplot2::margin(t = 10, r = 15, b = 15, l = 45)
  )


# 화면 확인
print(test_correlation_heatmap_plot_en)


# 영문 PNG 저장
ggplot2::ggsave(
  filename = file.path(test_figure_dir, "test_correlation_heatmap_pearson.png"),
  plot = test_correlation_heatmap_plot_en,
  width = 9,
  height = 7,
  dpi = 300
)


# 영문 PDF 저장
ggplot2::ggsave(
  filename = file.path(test_figure_dir, "test_correlation_heatmap_pearson.pdf"),
  plot = test_correlation_heatmap_plot_en,
  width = 9,
  height = 7
)



# -----------------------------------------------------------------------------
# Step 6-1-6. Korean heatmap
# -----------------------------------------------------------------------------

test_correlation_heatmap_plot_kor <-
  ggplot2::ggplot(
    test_predictor_correlation_plot_data,
    ggplot2::aes(x = variable_2, y = variable_1, fill = correlation)
  ) +
  ggplot2::geom_tile(color = "white", linewidth = 0.5) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", correlation)),
    size = 3.4
  ) +
  ggplot2::scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1),
    name = "상관계수"
  ) +
  ggplot2::labs(
    title = "강수량 결측 보정을 위한 연속형 후보 예측변수 간 상관행렬",
    subtitle = "Pearson 상관계수",
    x = NULL,
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = ggplot2::element_text(size = 9, margin = ggplot2::margin(r = 6)),
    plot.title = ggplot2::element_text(face = "bold", size = 13),
    plot.subtitle = ggplot2::element_text(size = 10),
    legend.title = ggplot2::element_text(size = 9),
    legend.text = ggplot2::element_text(size = 8),
    plot.margin = ggplot2::margin(t = 10, r = 15, b = 15, l = 45)
  )


# 화면 확인
print(test_correlation_heatmap_plot_kor)


# 국문 PNG 저장
ggplot2::ggsave(
  filename = file.path(test_figure_dir, "test_correlation_heatmap_pearson_kor.png"),
  plot = test_correlation_heatmap_plot_kor,
  width = 7.5,
  height = 6.5,
  dpi = 300
)



# -----------------------------------------------------------------------------
# Step 6-1-7. 결과 해석
# -----------------------------------------------------------------------------
#
# 1) 평균·최고·최저기온 간 상관관계가 매우 높음.
#
# 2) 기온과 이슬점 사이에도 높은 상관관계가 나타남.
#
# 3) Dew-Point Depression과 Humidity 사이에는
#    매우 강한 음(-)의 상관관계가 나타남.
#
# 4) dew_dep는 temperature_mean_day - dewpoint_mean_day로
#    직접 계산되는 파생변수임.
#
# 결론:
# - dew_dep를 초기 후보 predictor로 검토한 사실은 TEST로 보존함.
# - 이 TEST 결과는 본 분석 자료와 분리하여 관리함.
# -----------------------------------------------------------------------------



# =============================================================================
# Save TEST MICE preparation data
# =============================================================================

# TEST 전용 준비자료 저장
saveRDS(test_nk_weather_daily_mi, test_output_file)



# =============================================================================
# Final check: TEST output locations
# =============================================================================

cat(
  "\n------------------------------------------------------------\n",
  "02_03_00 TEST analysis completed.\n",
  "TEST root: ", test_root, "\n",
  "Data:      ", test_output_file, "\n",
  "Tables:    ", test_table_dir, "\n",
  "Figures:   ", test_figure_dir, "\n",
  "------------------------------------------------------------\n"
)