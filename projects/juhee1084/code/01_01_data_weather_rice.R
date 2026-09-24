# ============================================================
# 01_01_data_weather_rice.R
# Import, check (missing values), and prepare data
# Project: Drought Variability and Regional Rice Prices
# Author: Ju Hee Jeung
# 
# Note
# - 기상자료: 일자료 확인(강수량 결측비율: 66.28%) → 02_00~02_07에서 강수량 결측 보정 → 월자료 변환
# - 쌀가격: 이 파일에서 관측data를 월평균으로 변환 
#
# Workflow
# - Weather: Daily data → Missing-value check → Missing-value treatment (02_00~02_07) → Monthly data
# - Rice price: Observation data → Monthly averages → Missing-month check → Save
# ============================================================

# 0. Initial settings -----------------------------------------------------

source("code/00_initial.R")


# 1. Weather data ---------------------------------------------------------

# 1-1. Import weather data
weather_daily_raw <- read_dta(
  "data/raw/weather/nk_weather_daily_1973to202607_original.dta"
)

# 1-2. Check weather data
glimpse(weather_daily_raw)
names(weather_daily_raw)
summary(weather_daily_raw)

# Number of weather stations
n_distinct(weather_daily_raw$station)   # 27개

# Duplicated station-date observations
weather_daily_raw |>                     # 기상 일자료를 사용
  count(station, date) |>                # 관측소(station)-날짜(date)별 관측 개수를 계산
  filter(n > 1) |>                       # 같은 관측소·날짜가 2개 이상인 중복 관측만 선택
  nrow()                                 # 중복된 station-date 조합의 개수를 계산

# Missing values by variable
colSums(is.na(weather_daily_raw))

# Missing rate (%) by variable
round(
  colMeans(is.na(weather_daily_raw)) * 100,
  2
)

# Missing-value table
missing_table <- weather_daily_raw |>                         # 기상 원자료를 이용해 결측치 표 생성
  summarise(                                                  # 데이터를 요약
    across(                                                   # 여러 열에 같은 계산을 적용
      everything(),                                           # 모든 변수를 대상으로
      list(                                                   # 두 가지 계산을 함께 수행
        n_missing = ~ sum(is.na(.)),                          # 변수별 결측치 개수
        rate_missing = ~ round(mean(is.na(.)) * 100, 2)       # 변수별 결측률(%), 소수점 둘째 자리까지
      )
    )
  ) |>
  pivot_longer(                                               # 가로로 넓은 결과를 세로 형태의 표로 변환
    everything(),                                             # 모든 열을 변환
    names_to = c("variable", ".value"),                       # 변수명과 계산결과 열 이름으로 분리
    names_pattern = "(.*)_(n_missing|rate_missing)"           # 변수명 / n_missing / rate_missing 구분 규칙
  ) |>
  mutate(                                                     # 새로운 열 생성
    rate_missing_percent = paste0(rate_missing, "%")          # 결측률 숫자 뒤에 % 기호를 붙여 표시
  )

missing_table                                                 # 완성된 변수별 결측치 표 출력

# 강수량 결측치(66.28%) 보정은 02_00~02_07에서 수행한 후 월 단위로 변환

# 2. Rice price data ------------------------------------------------------

# 2-1. Import rice price data
rice_price_obs <- read_dta(
  "data/raw/rice_price/nk_rice_price_obs_2013to202607.dta"
)

# 2-2. Check rice price data
glimpse(rice_price_obs)
names(rice_price_obs)
summary(rice_price_obs)


# 2-3. Transform rice price data into monthly data 
rice_price_monthly <- rice_price_obs |>                          # 쌀가격 원자료를 월별 자료로 변환
  dplyr::mutate(                                                 # 새로운 열 생성 또는 기존 열 수정
    month = format(date, "%Y-%m")                                # 날짜를 "연-월" 형식으로 변환
  ) |> 
  dplyr::group_by(month) |>                                      # 같은 연-월끼리 그룹화
  dplyr::summarise(                                              # 각 월을 1개 행으로 요약
    rice_pyongyang = mean(rice_pyongyang, na.rm = TRUE),         # 평양 쌀가격 월평균
    rice_sinuiju   = mean(rice_sinuiju, na.rm = TRUE),           # 신의주 쌀가격 월평균
    rice_hyesan    = mean(rice_hyesan, na.rm = TRUE),            # 혜산 쌀가격 월평균
    n_obs          = n(),                                        # 해당 월의 쌀가격 조사 횟수
    .groups = "drop"                                             # 월별 요약 후 그룹 상태 해제
  ) 


print(rice_price_monthly)

# 2-4. Check monthly rice price data 
glimpse(rice_price_monthly)                               # 월별 자료의 행·열, 변수형식 확인
names(rice_price_monthly)
summary(rice_price_monthly)  

# 2-5. Check missing months in rice price data

all_months <- tibble(                                     # 2013-01~2026-07의 전체 월 목록 생성
  month = format(                                         # 날짜를 "연-월" 형식으로 변환
    seq(                                                  # 시작일부터 종료일까지 월별 날짜 순서 생성
      as.Date("2013-01-01"),                              # 분석 시작일
      as.Date("2026-07-31"),                              # 분석 종료일
      by = "month"                                        # 1개월 간격으로 생성
    ),
    "%Y-%m"                                               # "연-월" 형식으로 표시 (예: 2013-01)
  )
)                                                         # 전체 월 수 확인 및 쌀가격 누락 월 탐색에 사용

missing_rice_months <- all_months |>
  anti_join(rice_price_monthly, by = "month")             # 쌀가격 자료에 없는 월만 선택

nrow(all_months)                                          # 전체 분석기간의 월 수 : 163
nrow(rice_price_monthly)                                  # 쌀가격이 관측된 월 수 : 152
nrow(missing_rice_months)                                 # 쌀가격이 없는 월 수   :  11

missing_rice_months                                       # 쌀가격이 없는 연-월 목록 출력


# 2-6. Plot observed and monthly rice price trends -------------------------------

# 2-6. Plot observed and monthly rice price trends ------------------------

# Observation Data
rice_price_obs_long <- rice_price_obs |>
  select(date, rice_pyongyang, rice_sinuiju, rice_hyesan) |>
  pivot_longer(
    cols = starts_with("rice_"),
    names_to = "region",
    values_to = "rice_price"
  ) |>
  mutate(
    region = recode(
      region,
      rice_pyongyang = "Pyongyang",
      rice_sinuiju   = "Sinuiju",
      rice_hyesan    = "Hyesan"
    )
  )

rice_price_obs_plot <- ggplot(
  rice_price_obs_long,
  aes(x = date, y = rice_price, color = region)
) +
  geom_line() +
  geom_point() +
  geom_vline(
    xintercept = as.Date("2025-06-01"),                 # 최근 가격 급등 시작점
    linetype = "dashed"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Observed Rice Price Trends",
    subtitle = "Pyongyang, Sinuiju, and Hyesan",
    x = "Year",
    y = "Rice price",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

rice_price_obs_plot


# Monthly Rice Price
rice_price_monthly_long <- rice_price_monthly |>
  select(month, rice_pyongyang, rice_sinuiju, rice_hyesan) |>
  pivot_longer(
    cols = starts_with("rice_"),
    names_to = "region",
    values_to = "rice_price"
  ) |>
  mutate(
    region = recode(
      region,
      rice_pyongyang = "Pyongyang",
      rice_sinuiju   = "Sinuiju",
      rice_hyesan    = "Hyesan"
    )
  )

rice_price_monthly_plot <- ggplot(
  rice_price_monthly_long,
  aes(
    x = as.Date(paste0(month, "-01")),
    y = rice_price,
    color = region
  )
) +
  geom_line() +
  geom_point() +
  geom_vline(
    xintercept = as.Date("2025-06-01"),                 # 최근 가격 급등 시작점
    linetype = "dashed"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Monthly Rice Price Trends",
    subtitle = "Pyongyang, Sinuiju, and Hyesan",
    x = "Year",
    y = "Rice price",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

rice_price_monthly_plot


# 2-7. Calculate monthly rice price changes -------------------------------

rice_price_change <- all_months |>
  left_join(rice_price_monthly, by = "month") |>
  select(
    month,
    rice_pyongyang,
    rice_sinuiju,
    rice_hyesan
  ) |>
  pivot_longer(
    cols = starts_with("rice_"),
    names_to = "region",
    values_to = "rice_price"
  ) |>
  mutate(
    region = recode(
      region,
      rice_pyongyang = "Pyongyang",
      rice_sinuiju   = "Sinuiju",
      rice_hyesan    = "Hyesan"
    )
  ) |>
  group_by(region) |>
  arrange(month, .by_group = TRUE) |>
  mutate(
    rice_price_lag = lag(rice_price),
    price_change = rice_price - rice_price_lag,
    price_change_percent =
      (rice_price / rice_price_lag - 1) * 100
  ) |>
  ungroup()


# 2-8. Check monthly rice price increases of 20% or more ------------------

rice_price_increase_20percent <- rice_price_change |>
  filter(price_change_percent >= 20) |>
  mutate(
    region = factor(
      region,
      levels = c("Pyongyang", "Sinuiju", "Hyesan")
    )
  ) |>
  arrange(
    month,
    region
  ) |>
  select(
    month,
    region,
    rice_price_lag,
    rice_price,
    price_change,
    price_change_percent
  )

print(rice_price_increase_20percent, n = Inf)


# 2-9. Create wide table of monthly rice price increases ------------------

rice_price_increase_table <- rice_price_increase_20percent |>
  select(
    month,
    region,
    price_change_percent
  ) |>
  pivot_wider(
    names_from = region,
    values_from = price_change_percent
  ) |>
  select(
    month,
    Pyongyang,
    Sinuiju,
    Hyesan
  ) |>
  mutate(
    Pyongyang = ifelse(
      is.na(Pyongyang),
      "–",
      paste0(round(Pyongyang, 1), "%")
    ),
    Sinuiju = ifelse(
      is.na(Sinuiju),
      "–",
      paste0(round(Sinuiju, 1), "%")
    ),
    Hyesan = ifelse(
      is.na(Hyesan),
      "–",
      paste0(round(Hyesan, 1), "%")
    )
  ) |>
  arrange(month)

knitr::kable(
  rice_price_increase_table,
  col.names = c("Month", "Pyongyang", "Sinuiju", "Hyesan"),
  align = c("l", "c", "c", "c"),
  caption = "Monthly rice price increases of 20% or more"
)

# 3. Save data ------------------------------------------------------------

# 3-1. Create data folders ------------------------------------------------

dir.create(
  "data/raw/weather",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "data/raw/rice_price",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "data/processed/weather",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "data/processed/rice_price",
  recursive = TRUE,
  showWarnings = FALSE
)


# 3-2. Save weather data --------------------------------------------------

# Save daily weather data in R format
# Original Stata file (.dta) is preserved in data/raw/weather/

saveRDS(
  weather_daily_raw,
  "data/raw/weather/nk_weather_daily_197301_202607.rds"
)


# 3-3. Save rice price data -----------------------------------------------

# Save monthly rice price data in R format

saveRDS(
  rice_price_monthly,
  "data/processed/rice_price/nk_rice_price_monthly_201301_202607.rds"
)


# Save monthly rice price data in Stata format

write_dta(
  rice_price_monthly,
  "data/processed/rice_price/nk_rice_price_monthly_201301_202607.dta"
)

list.files("data/raw/weather")
list.files("data/processed/rice_price")