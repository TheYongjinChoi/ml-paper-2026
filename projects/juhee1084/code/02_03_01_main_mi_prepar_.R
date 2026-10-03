# =============================================================================
# 02_03_01_main_mi_prepar_.R
#
# Precipitation Data - Multiple Imputation Preparation
# 27 WMO stations, daily precipitation
#
# 목적:
# - MICE-PMM 강수량 결측 보정을 위한 후보 predictor 준비
# - predictor 가용률 및 변수 간 중복 가능성 확인
# - Pearson 및 Spearman 상관관계를 비교하여 후속 predictor 선정의 근거 마련
#
# 주의:
# - dew_dep는 02_03_00 TEST에서만 검토하고 본 분석에서는 제외
# - 현재 단계에서는 상관관계만으로 predictor를 최종 제외하지 않음
#
# Input:
# data/processed/precipitation/
# nk_weather_daily_precip_raw_197301_202607.rds
#
# Data output:
# data/processed/precipitation/
# nk_weather_daily_02_03_01_mi_prepare.rds
#
# Tables:
# output/02_03_01_main_mi_prepar_/tables/
#
# Figures:
# output/02_03_01_main_mi_prepar_/figures/
#
# Text outputs:
# output/02_03_01_main_mi_prepar_/
#   02_03_01_correlation_interpretation.txt
#   02_03_01_conclusion.txt
# =============================================================================



# =============================================================================
# Multiple Imputation (MICE-PMM) Workflow
# =============================================================================
#
# 02_03_01_main_mi_prepar_.R
#   Step 1. Raw 자료 불러오기
#   Step 2. 강수량 결측 확인
#   Step 3. MICE용 시간·관측소 변수 생성
#   Step 4. 후보 predictor 선정
#   Step 5. 강수 결측일에서 predictor 가용률 확인
#   Step 6-1. 연속형 predictor 간 상관관계 확인
#
# 02_03_02_mi_predictor_diagnostics.R
#   Step 6-2. Predictor와 관측 강수량의 관련성 확인
#   Step 6-3. Predictor 간 동시결측 및 결측패턴 확인
#
# 02_03_03_mi_spatial_predictors.R
#   Step 6-4. 인근 관측소 및 공간 predictor 검토
#
# 02_03_04_mi_predictor_matrix.R
#   Step 7. 최종 predictor 선정 및 predictor matrix 확정
#
# 02_03_05_mi_pilot.R
#   Step 8. Pilot MICE-PMM
#
# 02_03_06_mi_validation.R
#   Step 9. 수렴·분포·대치값 진단
#   Step 10. Masking validation
#
# 02_03_07_mi_final.R
#   Step 11. 최종 MICE-PMM 실행 및 결과 저장
#
# =============================================================================



# =============================================================================
# 0. Initial settings
# =============================================================================

# 공통 패키지 및 설정
source("code/00_00_initial.R")


# 입력 및 데이터 저장 경로
mi_raw_file <- "data/processed/precipitation/nk_weather_daily_precip_raw_197301_202607.rds"
mi_output_file <- "data/processed/precipitation/nk_weather_daily_02_03_01_mi_prepare.rds"


# 이 script의 결과 저장 경로
mi_output_dir <- "output/02_03_01_main_mi_prepar_"
mi_figure_dir <- file.path(mi_output_dir, "figures")
mi_table_dir <- file.path(mi_output_dir, "tables")


# 텍스트 결과 저장 경로
mi_correlation_text_file <- file.path(mi_output_dir, "02_03_01_correlation_interpretation.txt")
mi_conclusion_file <- file.path(mi_output_dir, "02_03_01_conclusion.txt")


# 입력자료 확인
if (!file.exists(mi_raw_file)) stop("Raw precipitation data를 찾을 수 없습니다: ", mi_raw_file)


# output 폴더 확인
if (!dir.exists(mi_output_dir)) stop("Output 폴더가 없습니다: ", mi_output_dir)
if (!dir.exists(mi_figure_dir)) stop("Figures 폴더가 없습니다: ", mi_figure_dir)
if (!dir.exists(mi_table_dir)) stop("Tables 폴더가 없습니다: ", mi_table_dir)



# =============================================================================
# Step 1. Raw 자료 불러오기
# =============================================================================

# 일별 기상자료
nk_weather_daily_raw <- readRDS(mi_raw_file)

# 자료 구조 확인
dplyr::glimpse(nk_weather_daily_raw)



# =============================================================================
# Step 2. 강수량 결측 확인
# =============================================================================

# 강수량 기본 통계
summary(nk_weather_daily_raw$precip_raw)

# 강수량 결측 개수
n_precip_missing <- sum(is.na(nk_weather_daily_raw$precip_raw))
n_precip_missing

# 강수량 결측률
rate_precip_missing <- round(mean(is.na(nk_weather_daily_raw$precip_raw)) * 100, 2)
rate_precip_missing

# 음수 강수량 확인
sum(nk_weather_daily_raw$precip_raw < 0, na.rm = TRUE)


# 전체 변수별 결측 현황
missing_table <-
  nk_weather_daily_raw |>
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
print(missing_table, n = Inf, width = Inf)
if (interactive()) View(missing_table)


# 표 저장
readr::write_csv(
  missing_table,
  file.path(mi_table_dir, "mi_missing_table.csv")
)



# =============================================================================
# Step 3. MICE용 시간·관측소 변수 생성
# =============================================================================

# 연도·월·관측소 변수 생성
nk_weather_daily_mi <-
  nk_weather_daily_raw |>
  dplyr::mutate(
    date = as.Date(date),
    year = lubridate::year(date),
    month = factor(lubridate::month(date), levels = 1:12),
    station_factor = factor(station_name_en)
  )


# 확인
dplyr::glimpse(nk_weather_daily_mi)



# =============================================================================
# Step 4. 후보 predictor 선정
# =============================================================================

# 현재는 최종 predictor가 아닌 후보군
# mean/max/min temperature는 상관관계 비교를 위해 모두 유지

mice_variables <- c(
  "precip_raw",
  "temperature_mean_day",
  "temperature_max_day",
  "temperature_min_day",
  "dewpoint_mean_day",
  "humidity_mean_day",
  "cloud_mean_day",
  "wind_mean_day",
  "sea_pressure_mean_day",
  "year",
  "month",
  "station_factor"
)


# 후보 predictor 자료
mice_data <- nk_weather_daily_mi |> dplyr::select(dplyr::all_of(mice_variables))


# 확인
dplyr::glimpse(mice_data)



# =============================================================================
# Step 5. 강수 결측일에서 predictor 가용률 확인
# =============================================================================

# 강수량 결측일에서 각 기상 predictor가 얼마나 관측되어 있는지 확인
mice_predictor_availability <-
  mice_data |>
  dplyr::filter(is.na(precip_raw)) |>
  dplyr::select(
    temperature_mean_day,
    temperature_max_day,
    temperature_min_day,
    dewpoint_mean_day,
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
print(mice_predictor_availability, n = Inf, width = Inf)
if (interactive()) View(mice_predictor_availability)


# 표 저장
readr::write_csv(
  mice_predictor_availability,
  file.path(mi_table_dir, "mi_predictor_availability.csv")
)



# =============================================================================
# Step 6-1. 연속형 predictor 간 상관관계 확인
# =============================================================================

# 연속형 기상 predictor
continuous_predictors <- c(
  "temperature_mean_day",
  "temperature_max_day",
  "temperature_min_day",
  "dewpoint_mean_day",
  "humidity_mean_day",
  "cloud_mean_day",
  "wind_mean_day",
  "sea_pressure_mean_day"
)



# -----------------------------------------------------------------------------
# Step 6-1-1. Pearson correlation
# -----------------------------------------------------------------------------

# 선형적 상관관계
predictor_correlation_pearson <-
  mice_data |>
  dplyr::select(dplyr::all_of(continuous_predictors)) |>
  stats::cor(use = "pairwise.complete.obs", method = "pearson")


# 결과 확인
round(predictor_correlation_pearson, 2)
if (interactive()) View(predictor_correlation_pearson)


# 표 저장
pearson_table <-
  as.data.frame(predictor_correlation_pearson) |>
  tibble::rownames_to_column(var = "variable")

readr::write_csv(
  pearson_table,
  file.path(mi_table_dir, "mi_predictor_correlation_pearson.csv")
)



# -----------------------------------------------------------------------------
# Step 6-1-2. Spearman correlation
# -----------------------------------------------------------------------------

# 순위 기반 단조 관계
predictor_correlation_spearman <-
  mice_data |>
  dplyr::select(dplyr::all_of(continuous_predictors)) |>
  stats::cor(use = "pairwise.complete.obs", method = "spearman")


# 결과 확인
round(predictor_correlation_spearman, 2)
if (interactive()) View(predictor_correlation_spearman)


# 표 저장
spearman_table <-
  as.data.frame(predictor_correlation_spearman) |>
  tibble::rownames_to_column(var = "variable")

readr::write_csv(
  spearman_table,
  file.path(mi_table_dir, "mi_predictor_correlation_spearman.csv")
)



# -----------------------------------------------------------------------------
# Step 6-1-3. Pearson과 Spearman 비교표
# -----------------------------------------------------------------------------

# 상관행렬을 비교 가능한 long format으로 변환
pearson_long <-
  predictor_correlation_pearson |>
  as.data.frame() |>
  tibble::rownames_to_column("variable_1") |>
  tidyr::pivot_longer(
    cols = -variable_1,
    names_to = "variable_2",
    values_to = "pearson"
  )


spearman_long <-
  predictor_correlation_spearman |>
  as.data.frame() |>
  tibble::rownames_to_column("variable_1") |>
  tidyr::pivot_longer(
    cols = -variable_1,
    names_to = "variable_2",
    values_to = "spearman"
  )


# 동일 변수쌍을 한 번만 남김
correlation_comparison_table <-
  dplyr::left_join(
    pearson_long,
    spearman_long,
    by = c("variable_1", "variable_2")
  ) |>
  dplyr::mutate(
    row_number = match(variable_1, continuous_predictors),
    column_number = match(variable_2, continuous_predictors)
  ) |>
  dplyr::filter(row_number < column_number) |>
  dplyr::select(variable_1, variable_2, pearson, spearman)


# 확인
print(correlation_comparison_table, n = Inf, width = Inf)
if (interactive()) View(correlation_comparison_table)


# 비교표 저장
readr::write_csv(
  correlation_comparison_table,
  file.path(mi_table_dir, "mi_predictor_correlation_pearson_spearman_comparison.csv")
)



# -----------------------------------------------------------------------------
# Step 6-1-4. 상관행렬 그림용 설정
# -----------------------------------------------------------------------------

# 그림 변수 순서
correlation_variable_order <- c(
  "cloud_mean_day",
  "dewpoint_mean_day",
  "humidity_mean_day",
  "temperature_max_day",
  "temperature_mean_day",
  "temperature_min_day",
  "sea_pressure_mean_day",
  "wind_mean_day"
)


# 그림용 변수명
correlation_variable_labels <- c(
  cloud_mean_day        = "Cloud",
  dewpoint_mean_day     = "Dew Point",
  humidity_mean_day     = "Humidity",
  temperature_max_day   = "Max Temp",
  temperature_mean_day  = "Mean Temp",
  temperature_min_day   = "Min Temp",
  sea_pressure_mean_day = "Sea-level Pressure",
  wind_mean_day         = "Wind"
)


correlation_label_order <-
  unname(correlation_variable_labels[correlation_variable_order])



# -----------------------------------------------------------------------------
# Step 6-1-5. Pearson heatmap
# -----------------------------------------------------------------------------

# Pearson 그림용 자료
pearson_plot_data <-
  pearson_long |>
  dplyr::mutate(
    row_number = match(variable_1, correlation_variable_order),
    column_number = match(variable_2, correlation_variable_order)
  ) |>
  dplyr::filter(row_number > column_number) |>
  dplyr::mutate(
    variable_1 = dplyr::recode(variable_1, !!!correlation_variable_labels),
    variable_2 = dplyr::recode(variable_2, !!!correlation_variable_labels),
    variable_1 = factor(variable_1, levels = rev(correlation_label_order)),
    variable_2 = factor(variable_2, levels = correlation_label_order)
  )


# Pearson heatmap
pearson_heatmap <-
  ggplot2::ggplot(
    pearson_plot_data,
    ggplot2::aes(x = variable_2, y = variable_1, fill = pearson)
  ) +
  ggplot2::geom_tile(color = "white", linewidth = 0.5) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", pearson)),
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
    axis.text.y = ggplot2::element_text(size = 9),
    plot.title = ggplot2::element_text(face = "bold", size = 13)
  )


# 화면 확인
print(pearson_heatmap)


# 그림 저장
ggplot2::ggsave(
  filename = file.path(mi_figure_dir, "mi_predictor_correlation_pearson.png"),
  plot = pearson_heatmap,
  width = 9,
  height = 7,
  dpi = 300
)



# -----------------------------------------------------------------------------
# Step 6-1-6. Spearman heatmap
# -----------------------------------------------------------------------------

# Spearman 그림용 자료
spearman_plot_data <-
  spearman_long |>
  dplyr::mutate(
    row_number = match(variable_1, correlation_variable_order),
    column_number = match(variable_2, correlation_variable_order)
  ) |>
  dplyr::filter(row_number > column_number) |>
  dplyr::mutate(
    variable_1 = dplyr::recode(variable_1, !!!correlation_variable_labels),
    variable_2 = dplyr::recode(variable_2, !!!correlation_variable_labels),
    variable_1 = factor(variable_1, levels = rev(correlation_label_order)),
    variable_2 = factor(variable_2, levels = correlation_label_order)
  )


# Spearman heatmap
spearman_heatmap <-
  ggplot2::ggplot(
    spearman_plot_data,
    ggplot2::aes(x = variable_2, y = variable_1, fill = spearman)
  ) +
  ggplot2::geom_tile(color = "white", linewidth = 0.5) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", spearman)),
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
    subtitle = "Spearman correlation coefficients",
    x = NULL,
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = ggplot2::element_text(size = 9),
    plot.title = ggplot2::element_text(face = "bold", size = 13)
  )


# 화면 확인
print(spearman_heatmap)


# 그림 저장
ggplot2::ggsave(
  filename = file.path(mi_figure_dir, "mi_predictor_correlation_spearman.png"),
  plot = spearman_heatmap,
  width = 9,
  height = 7,
  dpi = 300
)



# -----------------------------------------------------------------------------
# Step 6-1-7. Pearson 및 Spearman 결과 해석
# -----------------------------------------------------------------------------

# 주요 상관계수를 직접 가져와 결과문 작성
correlation_interpretation <- c(

  "02_03_01 Pearson and Spearman Correlation Interpretation",
  "=========================================================",
  "",

  "1. Temperature variables",
  sprintf(
    "- Mean Temp ↔ Max Temp: Pearson %.2f, Spearman %.2f",
    predictor_correlation_pearson["temperature_mean_day", "temperature_max_day"],
    predictor_correlation_spearman["temperature_mean_day", "temperature_max_day"]
  ),
  sprintf(
    "- Mean Temp ↔ Min Temp: Pearson %.2f, Spearman %.2f",
    predictor_correlation_pearson["temperature_mean_day", "temperature_min_day"],
    predictor_correlation_spearman["temperature_mean_day", "temperature_min_day"]
  ),
  sprintf(
    "- Max Temp ↔ Min Temp: Pearson %.2f, Spearman %.2f",
    predictor_correlation_pearson["temperature_max_day", "temperature_min_day"],
    predictor_correlation_spearman["temperature_max_day", "temperature_min_day"]
  ),
  "- Pearson과 Spearman 모두 매우 높은 상관을 보이면 세 기온변수의 정보 중복 가능성이 큼.",
  "- 따라서 후속 단계에서는 대표 기온변수를 선택하는 방향을 검토함.",
  "",

  "2. Temperature and Dew Point",
  sprintf(
    "- Mean Temp ↔ Dew Point: Pearson %.2f, Spearman %.2f",
    predictor_correlation_pearson["temperature_mean_day", "dewpoint_mean_day"],
    predictor_correlation_spearman["temperature_mean_day", "dewpoint_mean_day"]
  ),
  sprintf(
    "- Min Temp ↔ Dew Point: Pearson %.2f, Spearman %.2f",
    predictor_correlation_pearson["temperature_min_day", "dewpoint_mean_day"],
    predictor_correlation_spearman["temperature_min_day", "dewpoint_mean_day"]
  ),
  "- 기온과 이슬점 역시 높은 상관을 보이므로 정보 중복 가능성이 있음.",
  "- 그러나 강수량 예측에 제공하는 정보가 다를 수 있으므로 이 단계에서 제외하지 않음.",
  "",

  "3. Humidity and Cloud",
  sprintf(
    "- Humidity ↔ Cloud: Pearson %.2f, Spearman %.2f",
    predictor_correlation_pearson["humidity_mean_day", "cloud_mean_day"],
    predictor_correlation_spearman["humidity_mean_day", "cloud_mean_day"]
  ),
  "- 두 변수는 관련성이 있으나 기온변수들만큼 강한 중복은 아님.",
  "",

  "4. Sea-level Pressure",
  sprintf(
    "- Sea-level Pressure ↔ Mean Temp: Pearson %.2f, Spearman %.2f",
    predictor_correlation_pearson["sea_pressure_mean_day", "temperature_mean_day"],
    predictor_correlation_spearman["sea_pressure_mean_day", "temperature_mean_day"]
  ),
  sprintf(
    "- Sea-level Pressure ↔ Dew Point: Pearson %.2f, Spearman %.2f",
    predictor_correlation_pearson["sea_pressure_mean_day", "dewpoint_mean_day"],
    predictor_correlation_spearman["sea_pressure_mean_day", "dewpoint_mean_day"]
  ),
  "- Pearson보다 Spearman의 절대값이 크게 나타날 경우 선형관계보다 단조적 관계가 강할 가능성이 있음.",
  "- 이는 오류를 의미하지 않으며, Sea-level Pressure를 단순 상관계수만으로 제외하지 않음.",
  "",

  "5. Overall interpretation",
  "- Pearson은 선형관계, Spearman은 순위에 기반한 단조관계를 보여줌.",
  "- 두 결과를 함께 보되 상관계수만으로 최종 predictor를 결정하지 않음.",
  "- 다음 단계에서 강수량과의 관련성 및 동시결측 패턴을 추가 확인함."
)


# Console 확인
cat(paste(correlation_interpretation, collapse = "\n"), "\n")


# 결과 해석 저장
readr::write_lines(
  correlation_interpretation,
  mi_correlation_text_file
)



# =============================================================================
# Save 02_03_01 preparation data
# =============================================================================

# 후속 MICE 진단에 사용할 전체 준비자료 저장
saveRDS(nk_weather_daily_mi, mi_output_file)



# =============================================================================
# 02_03_01 Conclusion
# =============================================================================

# 이 script의 최종 결론
mi_prepare_conclusion <- c(

  "02_03_01_main_mi_prepar_.R Conclusion",
  "=======================================",
  "",

  sprintf(
    "1. 일별 강수량 결측치는 %s개이며 전체의 %.2f%%이다.",
    format(n_precip_missing, big.mark = ","),
    rate_precip_missing
  ),

  "2. 강수량 결측일에서 대부분의 기상 predictor는 높은 가용률을 보이므로 MICE 후보로 추가 검토할 수 있다.",

  "3. 평균·최고·최저기온은 Pearson과 Spearman 모두 매우 높은 상관관계를 보여 정보 중복 가능성이 크다.",

  "4. 기온과 이슬점 역시 높은 상관관계를 보이지만, 강수량과의 관련성을 확인하기 전에는 제외하지 않는다.",

  "5. Humidity와 Cloud는 중간 정도의 관련성을 보이며 서로 완전히 동일한 정보를 제공하는 수준은 아니다.",

  "6. Sea-level Pressure는 Pearson과 Spearman 결과가 상당히 다를 수 있으므로 단순 선형상관만으로 판단하지 않는다.",

  "7. dew_dep는 별도 TEST에서 검토하였으며 본 분석 predictor 후보에서는 제외하였다.",

  "8. 현재 단계에서는 상관관계만으로 최종 predictor를 제거하지 않는다.",

  "9. 다음 단계인 02_03_02_mi_predictor_diagnostics.R에서 predictor와 관측 강수량의 관련성 및 동시결측 패턴을 확인한다.",

  "10. 이후 공간 predictor 검토 결과까지 종합하여 02_03_04_mi_predictor_matrix.R에서 최종 predictor matrix를 확정한다."
)


# Console 확인
cat("\n", paste(mi_prepare_conclusion, collapse = "\n"), "\n")


# 결론 저장
readr::write_lines(
  mi_prepare_conclusion,
  mi_conclusion_file
)



# =============================================================================
# Final check
# =============================================================================

cat(
  "\n------------------------------------------------------------\n",
  "02_03_01 MICE preparation completed.\n",
  "Data:           ", mi_output_file, "\n",
  "Tables:         ", mi_table_dir, "\n",
  "Figures:        ", mi_figure_dir, "\n",
  "Interpretation: ", mi_correlation_text_file, "\n",
  "Conclusion:     ", mi_conclusion_file, "\n",
  "------------------------------------------------------------\n"
)