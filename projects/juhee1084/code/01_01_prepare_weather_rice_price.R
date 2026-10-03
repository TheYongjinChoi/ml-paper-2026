# =============================================================================
# 01_01_prepare_weather_rice_price.R
# Import, check, and prepare weather and rice price data
# Project: Drought Variability and Regional Rice Prices in North Korea
# Author: Ju Hee Jeung
#
# 목적
# - 북한 일별 기상 원자료의 구조·중복·결측 현황 확인
# - 평양·신의주·혜산 쌀가격 관측자료를 월평균 자료로 변환
# - 쌀가격 누락월 및 월 20% 이상 급등 확인
# - 후속 분석용 데이터와 표·그림·결론 저장
#
# Input
# - data/original/weather/nk_wmo_weather_daily_197301_202607_original.dta
# - data/original/rice_price/nk_rice_price_observations_201301_202607_original.dta
#
# Data output
# - data/processed/01_01_prepare_weather_rice_price/
#
# Results
# - output/01_01_prepare_weather_rice_price/figures/
# - output/01_01_prepare_weather_rice_price/tables/
# - output/01_01_prepare_weather_rice_price/*_conclusion.txt
#
# 중요
# - data/original/ 원자료는 수정하지 않음
# - 강수량 결측 보정은 후속 02_* script에서 수행
# - 중간 데이터는 RDS로 저장하고 DTA는 생성하지 않음
# =============================================================================


# =============================================================================
# Workflow
# =============================================================================
#
# 01_01_prepare_weather_rice_price.R
#   Step 1. Weather data
#     1-1. 기상 원자료 불러오기
#     1-2. 관측소·중복·결측 현황 확인
#
#   Step 2. Rice price data
#     2-1. 쌀가격 원자료 불러오기
#     2-2. 원자료 확인
#     2-3. 월별 쌀가격 생성
#     2-4. 월별 자료 확인
#     2-5. 누락월 확인
#     2-6. 관측·월별 가격 추세 그림
#     2-7. 월별 가격 변화율 계산
#     2-8. 월 20% 이상 상승 확인
#     2-9. 가격 급등 표 생성
#
#   Step 3. Save data
#     3-1. 기상 준비자료 저장
#     3-2. 월별 쌀가격 저장
#
#   Step 4. Conclusion
#
# 이후
#   02_* : 강수량 결측 보정
#   03_* : SPI·SPEI·SMI 등 가뭄지수 생성
#   04_* : 최종 분석 패널 구축
#   05_* : 기술통계·시각화
#   06_* : 회귀·계량분석
# =============================================================================


# =============================================================================
# 0. Initial settings
# =============================================================================

source("code/00_00_initial.R")   # 공통 package와 저장 함수 불러오기

script_name <- "01_01_prepare_weather_rice_price"      # 실제 R script 이름
script_prefix <- "01_01_prepare_weather_rice_price"    # 저장파일 공통 prefix


# 원자료 경로
weather_raw_file <- "data/original/weather/nk_wmo_weather_daily_197301_202607_original.dta"   # 북한 WMO 일별 기상 원자료
rice_raw_file <- "data/original/rice_price/nk_rice_price_observations_201301_202607_original.dta"   # 북한 쌀가격 원 관측자료

if (!file.exists(weather_raw_file)) stop("기상 원자료가 없습니다: ", weather_raw_file)   # 파일 존재 확인
if (!file.exists(rice_raw_file)) stop("쌀가격 원자료가 없습니다: ", rice_raw_file)


# =============================================================================
# Step 1. Weather data
# =============================================================================


# -----------------------------------------------------------------------------
# 1-1. Import weather data
# -----------------------------------------------------------------------------

weather_daily_raw <- haven::read_dta(weather_raw_file)   # 일별 기상 원자료 불러오기

dplyr::glimpse(weather_daily_raw)   # 자료의 행·열과 변수형식 확인
names(weather_daily_raw)            # 변수명 확인
summary(weather_daily_raw)          # 변수별 기본 요약통계 확인


# -----------------------------------------------------------------------------
# 1-2. Check weather data
# -----------------------------------------------------------------------------

n_weather_stations <- dplyr::n_distinct(weather_daily_raw$station)   # 전체 기상관측소 수

n_duplicate_station_date <- weather_daily_raw |>
  dplyr::count(station, date) |>    # 관측소-날짜별 관측 개수 계산
  dplyr::filter(n > 1) |>           # 같은 관측소·날짜가 2개 이상인 경우만 선택
  nrow()                            # 중복 station-date 조합 수

n_weather_stations
n_duplicate_station_date

colSums(is.na(weather_daily_raw))                    # 변수별 결측치 개수
round(colMeans(is.na(weather_daily_raw)) * 100, 2)  # 변수별 결측률(%)


# 변수별 결측 현황표
weather_missing_table <- weather_daily_raw |>
  dplyr::summarise(
    dplyr::across(
      dplyr::everything(),                           # 모든 변수에 같은 계산 적용
      list(
        n_missing = ~ sum(is.na(.)),                 # 변수별 결측치 개수
        rate_missing = ~ round(mean(is.na(.)) * 100, 2)   # 변수별 결측률(%)
      )
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c("variable", ".value"),             # 변수명과 계산결과를 분리
    names_pattern = "(.*)_(n_missing|rate_missing)"
  ) |>
  dplyr::mutate(
    rate_missing_percent = paste0(rate_missing, "%")   # 결측률에 % 기호 추가
  )

print(weather_missing_table, n = Inf, width = Inf)
if (interactive()) View(weather_missing_table)       # 직접 실행할 때만 View 창 표시

save_project_table(
  table = weather_missing_table,
  table_name = "weather_missing_table",
  script_name = script_name,
  script_prefix = script_prefix
)

# 강수량 결측 보정은 원자료를 변경하지 않고 후속 02_* script에서 수행


# =============================================================================
# Step 2. Rice price data
# =============================================================================


# -----------------------------------------------------------------------------
# 2-1. Import rice price data
# -----------------------------------------------------------------------------

rice_price_obs <- haven::read_dta(rice_raw_file)   # 쌀가격 관측자료 불러오기


# -----------------------------------------------------------------------------
# 2-2. Check rice price data
# -----------------------------------------------------------------------------

dplyr::glimpse(rice_price_obs)   # 자료구조와 변수형식 확인
names(rice_price_obs)            # 변수명 확인
summary(rice_price_obs)          # 기본 요약통계 확인


# -----------------------------------------------------------------------------
# 2-3. Transform rice price data into monthly data
# -----------------------------------------------------------------------------

rice_price_monthly <- rice_price_obs |>
  dplyr::mutate(
    month = format(as.Date(date), "%Y-%m")   # 관측일을 연-월 형식으로 변환
  ) |>
  dplyr::group_by(month) |>                  # 같은 월의 관측치를 묶음
  dplyr::summarise(

    # 해당 월의 평양 가격이 모두 결측이면 숫자형 NA를 유지
    # 하나라도 관측값이 있으면 결측치를 제외하고 월평균 계산
    rice_pyongyang = if (all(is.na(rice_pyongyang))) {
      NA_real_
    } else {
      mean(rice_pyongyang, na.rm = TRUE)
    },

    # 해당 월의 신의주 가격이 모두 결측이면 숫자형 NA를 유지
    # 하나라도 관측값이 있으면 결측치를 제외하고 월평균 계산
    rice_sinuiju = if (all(is.na(rice_sinuiju))) {
      NA_real_
    } else {
      mean(rice_sinuiju, na.rm = TRUE)
    },

    # 해당 월의 혜산 가격이 모두 결측이면 숫자형 NA를 유지
    # 하나라도 관측값이 있으면 결측치를 제외하고 월평균 계산
    rice_hyesan = if (all(is.na(rice_hyesan))) {
      NA_real_
    } else {
      mean(rice_hyesan, na.rm = TRUE)
    },

    n_obs = dplyr::n(),   # 해당 월의 전체 가격 조사 행 수
    .groups = "drop"      # 월별 계산 후 grouping 해제
  )

print(rice_price_monthly)
if (interactive()) View(rice_price_monthly)


# -----------------------------------------------------------------------------
# 2-4. Check monthly rice price data
# -----------------------------------------------------------------------------

dplyr::glimpse(rice_price_monthly)   # 월별 자료구조 확인
names(rice_price_monthly)            # 월별 변수명 확인
summary(rice_price_monthly)          # 월별 기본 통계 확인


# -----------------------------------------------------------------------------
# 2-5. Check missing months in rice price data
# -----------------------------------------------------------------------------

all_months <- tibble::tibble(
  month = format(
    seq(
      as.Date("2013-01-01"),        # 분석 시작월
      as.Date("2026-07-31"),        # 분석 종료월
      by = "month"
    ),
    "%Y-%m"
  )
)

missing_rice_months <- all_months |>
  dplyr::anti_join(
    rice_price_monthly,
    by = "month"
  )   # 전체 월 중 쌀가격 자료 자체가 존재하지 않는 월 선택

n_total_months <- nrow(all_months)                   # 전체 분석기간 월 수
n_observed_rice_months <- nrow(rice_price_monthly)  # 쌀가격 자료가 존재하는 월 수
n_missing_rice_months <- nrow(missing_rice_months)  # 완전히 누락된 월 수

n_total_months
n_observed_rice_months
n_missing_rice_months

print(missing_rice_months, n = Inf)
if (interactive()) View(missing_rice_months)

save_project_table(
  table = missing_rice_months,
  table_name = "rice_price_missing_months",
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 2-6. Plot observed and monthly rice price trends
# -----------------------------------------------------------------------------


# 관측자료를 그림용 long 형태로 변환
rice_price_obs_long <- rice_price_obs |>
  dplyr::select(date, rice_pyongyang, rice_sinuiju, rice_hyesan) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("rice_"),
    names_to = "region",
    values_to = "rice_price"
  ) |>
  dplyr::mutate(
    region = dplyr::recode(
      region,
      rice_pyongyang = "Pyongyang",
      rice_sinuiju = "Sinuiju",
      rice_hyesan = "Hyesan"
    )
  )

# 관측 쌀가격 추세 그림
rice_price_obs_plot <- ggplot2::ggplot(
  rice_price_obs_long,
  ggplot2::aes(x = date, y = rice_price, color = region)
) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_vline(
    xintercept = as.Date("2025-06-01"),   # 최근 가격 급등 시작점
    linetype = "dashed"
  ) +
  ggplot2::scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  ggplot2::labs(
    title = "Observed Rice Price Trends",
    subtitle = "Pyongyang, Sinuiju, and Hyesan",
    x = "Year",
    y = "Rice price",
    color = "Region"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

print(rice_price_obs_plot)

save_project_plot(
  plot = rice_price_obs_plot,
  plot_name = "rice_price_observed_trends",
  script_name = script_name,
  script_prefix = script_prefix
)


# 월별자료를 그림용 long 형태로 변환
rice_price_monthly_long <- rice_price_monthly |>
  dplyr::select(month, rice_pyongyang, rice_sinuiju, rice_hyesan) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("rice_"),
    names_to = "region",
    values_to = "rice_price"
  ) |>
  dplyr::mutate(
    region = dplyr::recode(
      region,
      rice_pyongyang = "Pyongyang",
      rice_sinuiju = "Sinuiju",
      rice_hyesan = "Hyesan"
    )
  )

# 월평균 쌀가격 추세 그림
rice_price_monthly_plot <- ggplot2::ggplot(
  rice_price_monthly_long,
  ggplot2::aes(
    x = as.Date(paste0(month, "-01")),
    y = rice_price,
    color = region
  )
) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_vline(
    xintercept = as.Date("2025-06-01"),   # 최근 가격 급등 시작점
    linetype = "dashed"
  ) +
  ggplot2::scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  ggplot2::labs(
    title = "Monthly Rice Price Trends",
    subtitle = "Pyongyang, Sinuiju, and Hyesan",
    x = "Year",
    y = "Rice price",
    color = "Region"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

print(rice_price_monthly_plot)

save_project_plot(
  plot = rice_price_monthly_plot,
  plot_name = "rice_price_monthly_trends",
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 2-7. Calculate monthly rice price changes
# -----------------------------------------------------------------------------

rice_price_change <- all_months |>
  dplyr::left_join(rice_price_monthly, by = "month") |>   # 누락월도 포함한 전체 월 기준으로 결합
  dplyr::select(month, rice_pyongyang, rice_sinuiju, rice_hyesan) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("rice_"),
    names_to = "region",
    values_to = "rice_price"
  ) |>
  dplyr::mutate(
    region = dplyr::recode(
      region,
      rice_pyongyang = "Pyongyang",
      rice_sinuiju = "Sinuiju",
      rice_hyesan = "Hyesan"
    )
  ) |>
  dplyr::group_by(region) |>
  dplyr::arrange(month, .by_group = TRUE) |>
  dplyr::mutate(
    rice_price_lag = dplyr::lag(rice_price),                    # 직전 월 쌀가격
    price_change = rice_price - rice_price_lag,                 # 전월 대비 가격 차이
    price_change_percent = (rice_price / rice_price_lag - 1) * 100   # 전월 대비 상승률(%)
  ) |>
  dplyr::ungroup()


# -----------------------------------------------------------------------------
# 2-8. Check monthly rice price increases of 20% or more
# -----------------------------------------------------------------------------

rice_price_increase_20percent <- rice_price_change |>
  dplyr::filter(price_change_percent >= 20) |>   # 전월 대비 20% 이상 상승한 경우만 선택
  dplyr::mutate(
    region = factor(
      region,
      levels = c("Pyongyang", "Sinuiju", "Hyesan")
    )
  ) |>
  dplyr::arrange(month, region) |>
  dplyr::select(
    month,
    region,
    rice_price_lag,
    rice_price,
    price_change,
    price_change_percent
  )

print(rice_price_increase_20percent, n = Inf)
if (interactive()) View(rice_price_increase_20percent)

save_project_table(
  table = rice_price_increase_20percent,
  table_name = "rice_price_increase_20percent_long",
  script_name = script_name,
  script_prefix = script_prefix
)


# -----------------------------------------------------------------------------
# 2-9. Create wide table of monthly rice price increases
# -----------------------------------------------------------------------------

rice_price_increase_table <- rice_price_increase_20percent |>
  dplyr::select(month, region, price_change_percent) |>
  tidyr::pivot_wider(
    names_from = region,
    values_from = price_change_percent
  ) |>
  dplyr::select(month, Pyongyang, Sinuiju, Hyesan) |>
  dplyr::mutate(
    Pyongyang = ifelse(is.na(Pyongyang), "–", paste0(round(Pyongyang, 1), "%")),
    Sinuiju = ifelse(is.na(Sinuiju), "–", paste0(round(Sinuiju, 1), "%")),
    Hyesan = ifelse(is.na(Hyesan), "–", paste0(round(Hyesan, 1), "%"))
  ) |>
  dplyr::arrange(month)

knitr::kable(
  rice_price_increase_table,
  col.names = c("Month", "Pyongyang", "Sinuiju", "Hyesan"),
  align = c("l", "c", "c", "c"),
  caption = "Monthly rice price increases of 20% or more"
)

if (interactive()) View(rice_price_increase_table)

save_project_table(
  table = rice_price_increase_table,
  table_name = "rice_price_increase_20percent",
  script_name = script_name,
  script_prefix = script_prefix,
  save_tex = TRUE,   # 논문·Overleaf용 TEX도 함께 저장
  caption = "Monthly rice price increases of 20% or more"
)


# =============================================================================
# Step 3. Save data
# =============================================================================


# -----------------------------------------------------------------------------
# 3-1. Save weather data
# -----------------------------------------------------------------------------

# 원본 DTA는 data/original/weather/에 그대로 보존
# 이 script에서 불러온 기상자료는 후속 분석용 RDS로 processed에 저장

save_project_data(
  data = weather_daily_raw,                              # 저장할 R 객체: 북한 WMO 일별 기상자료
  data_name = "nk_wmo_weather_daily_197301_202607",     # 파일 핵심 이름: 자료내용 + 빈도 + 기간
  script_name = script_name,                            # 저장 폴더 결정: data/processed/<script_name>/
  script_prefix = script_prefix                         # 저장파일 앞에 현재 script 이름을 자동으로 붙임
)


# -----------------------------------------------------------------------------
# 3-2. Save monthly rice price data
# -----------------------------------------------------------------------------

# 월별 쌀가격은 후속 분석용 중간자료이므로 RDS로 저장
save_project_data(
  data = rice_price_monthly,
  data_name = "nk_rice_price_monthly_201301_202607",
  script_name = script_name,
  script_prefix = script_prefix
)


# =============================================================================
# Step 4. Conclusion
# =============================================================================

script_conclusion <- c(
  "01_01_prepare_weather_rice_price.R Conclusion",
  "=====================================",
  "",
  paste0("1. 북한 WMO 기상 관측소 수: ", n_weather_stations, "개."),
  paste0("2. 중복 station-date 조합: ", n_duplicate_station_date, "개."),
  "3. 기상자료의 변수별 결측 현황을 확인하고 CSV로 저장하였다.",
  "4. 강수량 결측 보정은 원자료를 변경하지 않고 후속 02_* script에서 수행한다.",
  paste0("5. 쌀가격 전체 분석기간 월 수: ", n_total_months, "개월."),
  paste0("6. 쌀가격 관측 월 수: ", n_observed_rice_months, "개월."),
  paste0("7. 쌀가격 누락 월 수: ", n_missing_rice_months, "개월."),
  "8. 평양·신의주·혜산 쌀가격을 월평균으로 변환하였다.",
  "9. 한 달의 가격이 모두 결측인 경우 NaN이 아니라 숫자형 NA로 유지하였다.",
  "10. 월 20% 이상 가격상승 사례를 long 및 wide table로 정리하였다.",
  "11. 후속 분석용 중간 데이터는 RDS 형식으로 저장하였다."
)

cat("\n", paste(script_conclusion, collapse = "\n"), "\n")

save_project_text(
  text = script_conclusion,
  text_name = "conclusion",
  script_name = script_name,
  script_prefix = script_prefix
)


# =============================================================================
# Final check
# =============================================================================

cat("\nProcessed data files:\n")
print(list.files(file.path("data", "processed", script_name)))   # 저장된 중간 데이터 확인

cat("\nTable files:\n")
print(list.files(file.path("output", script_name, "tables")))   # 저장된 표 확인

cat("\nFigure files:\n")
print(list.files(file.path("output", script_name, "figures")))  # 저장된 그림 확인