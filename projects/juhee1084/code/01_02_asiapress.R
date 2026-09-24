# ========================================================================
# 01_02_asiapress.R (Web Crawling)
# Import and prepare AsiaPress market price data
# Project: Drought Variability and Regional Rice Prices
# Author: Ju Hee Jeung
#
# Note
# - Variables: gasoline, diesel, rice, corn, CNY exchange rate,
#   and USD exchange rate.
# - Survey locations vary over time; therefore, the data do not represent
#   a national average or a specific region.
# - The data are used only as supplementary market information.
#
# Workflow
# 1. Import AsiaPress data
#         ↓
# 2. Clean AsiaPress data
#         ↓
# 3. Check Missing Values
#         ↓
# 4. Monthly Data Aggregation
#         ↓
# 5. Plot Monthly Trends
#         ↓
# 6. Save Data
# ========================================================================



# 0. Initial settings -----------------------------------------------------

source("code/00_initial.R")


# =========================================================================
# 0-1. Research Data Dates
# =========================================================================

# AsiaPress 자료를 실제로 수집한 날짜
# 새로 크롤링할 때 이 날짜만 변경
collection_date <- as.Date("2026-09-24")


# 논문에서 사용할 월별 분석 종료일
# 완전한 월까지만 지정
analysis_end_date <- as.Date("2026-08-31")


# 분석 종료일이 수집일보다 늦으면 오류 발생
stopifnot(
  analysis_end_date <= collection_date
)


# 분석 종료일이 해당 월의 마지막 날인지 확인
stopifnot(
  analysis_end_date ==
    ceiling_date(analysis_end_date, "month") - days(1)
)


# 파일명 및 분석에 사용할 날짜 정보
collection_tag <- format(
  collection_date,
  "%Y%m%d"
)

analysis_end_month <- format(
  analysis_end_date,
  "%Y-%m"
)

analysis_end_tag <- format(
  analysis_end_date,
  "%Y%m"
)


# 최근 가격동향 그래프의 시작연도
recent_start_year <- 2023

# 최근 가격동향 그래프의 마지막 연도는 분석 종료일에서 자동 생성
recent_end_year <- year(analysis_end_date)

recent_period_label <- paste0(
  recent_start_year,
  "–",
  recent_end_year
)



# 1. Import AsiaPress market price data ----------------------------------

asiapress_url <- "https://www.asiapress.org/korean/nk-korea-prices/"


# 웹페이지 읽기
asiapress_data <- read_html(
  asiapress_url
)


# 웹페이지의 표 추출
asiapress_tables <- asiapress_data |>
  html_elements("table") |>
  html_table(fill = TRUE)


# 표 개수 확인
length(asiapress_tables)


# 첫 번째 표를 원자료로 저장
asiapress_raw <- asiapress_tables[[1]]


# 원자료 확인
glimpse(asiapress_raw)
names(asiapress_raw)
head(asiapress_raw)



# 2. Clean AsiaPress data -------------------------------------------------
# 중국 1元 환율
# 물가는 1kg당 북한 원 기준
# 괄호 안의 한국원 환산값은 사용하지 않음


# 2-1. Clean Survey Dates and Variables ----------------------------------

asiapress_data_clean <- asiapress_raw |>

  transmute(

    # 조사일에서 날짜 부분만 추출하여 Date 형식으로 변환
    date = ymd(
      str_remove_all(
        str_extract(
          조사일,
          "\\d{4}\\s*/\\s*\\d{1,2}\\s*/\\s*\\d{1,2}"
        ),
        "\\s"
      )
    ),

    # 휘발유 가격
    gasoline_asiapress = parse_number(
      휘발유,
      na = c("", "NA", "／", "/")
    ),

    # 디젤 가격
    diesel_asiapress = parse_number(
      디젤유,
      na = c("", "NA", "／", "/")
    ),

    # 쌀 가격
    rice_asiapress = parse_number(
      백미,
      na = c("", "NA", "／", "/")
    ),

    # 옥수수 가격
    corn_asiapress = parse_number(
      옥수수,
      na = c("", "NA", "／", "/")
    ),

    # 중국 위안 환율
    cny_asiapress = parse_number(
      `중국元 환율`,
      na = c("", "NA", "／", "/")
    ),

    # 미국 달러 환율
    usd_asiapress = parse_number(
      `1USD 환율`,
      na = c("", "NA", "／", "/")
    )
  ) |>

  # 날짜순 정렬
  arrange(date) |>

  # 지정한 자료 수집일까지의 관측치만 사용
  filter(
    date <= collection_date
  )


# 정제자료 확인
glimpse(asiapress_data_clean)
head(asiapress_data_clean)
tail(asiapress_data_clean)


# 관측치 수 확인
nrow(asiapress_raw)
nrow(asiapress_data_clean)


# 조사기간 확인
min(
  asiapress_data_clean$date,
  na.rm = TRUE
)

max(
  asiapress_data_clean$date,
  na.rm = TRUE
)


# 날짜변환 실패 확인
sum(
  is.na(asiapress_data_clean$date)
)



# 2-2. Check Duplicate Survey Dates --------------------------------------

asiapress_data_clean |>
  count(date) |>
  filter(
    n > 1
  )



# 2-3. Check Rice Price Observation Period -------------------------------

min(
  asiapress_data_clean$date[
    !is.na(asiapress_data_clean$rice_asiapress)
  ]
)

max(
  asiapress_data_clean$date[
    !is.na(asiapress_data_clean$rice_asiapress)
  ]
)



# 2-4. Check Variable Ranges ---------------------------------------------

asiapress_range_table <- asiapress_data_clean |>

  summarise(
    across(
      where(is.numeric),
      list(
        min = ~ min(., na.rm = TRUE),
        max = ~ max(., na.rm = TRUE)
      )
    )
  ) |>

  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(min|max)"
  ) |>

  mutate(
    across(
      c(min, max),
      ~ format(
        .,
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      )
    )
  )


knitr::kable(
  asiapress_range_table,
  col.names = c(
    "Variable",
    "Minimum",
    "Maximum"
  ),
  align = c("l", "c", "c"),
  caption = "Range of AsiaPress market variables"
)



# 3. Check Missing Values -------------------------------------------------


# 3-1. Remove Observations with All Market Variables Missing -------------

# 날짜는 있지만 모든 시장변수가 결측인 조사일 확인
asiapress_all_missing <- asiapress_data_clean |>

  filter(
    if_all(
      -date,
      is.na
    )
  )


asiapress_all_missing


# 모든 시장변수가 결측인 조사일 제거
asiapress_data_clean <- asiapress_data_clean |>

  filter(
    !if_all(
      -date,
      is.na
    )
  )


# 제거 후 관측치 수 확인
nrow(asiapress_data_clean)



# 3-2. Observation-Level Missing Values ----------------------------------

# 최종 정제자료를 기준으로 변수별 결측치 및 결측률 계산
asiapress_missing_observation <- asiapress_data_clean |>

  summarise(
    across(
      everything(),
      list(
        n_missing = ~ sum(is.na(.)),
        rate_missing = ~ round(
          mean(is.na(.)) * 100,
          2
        )
      )
    )
  ) |>

  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(n_missing|rate_missing)"
  ) |>

  mutate(
    rate_missing_percent = paste0(
      rate_missing,
      "%"
    )
  ) |>

  select(
    variable,
    n_missing,
    rate_missing_percent
  )


asiapress_missing_observation


knitr::kable(
  asiapress_missing_observation,
  col.names = c(
    "Variable",
    "Missing observations",
    "Missing rate"
  ),
  align = c("l", "c", "c"),
  caption = "Missing values in AsiaPress market data"
)



# 3-3. Dates with Missing Values -----------------------------------------

# 하나 이상의 시장변수가 결측인 조사일 확인
asiapress_missing_dates <- asiapress_data_clean |>

  filter(
    if_any(
      -date,
      is.na
    )
  )


asiapress_missing_dates


# 어느 조사일의 어느 변수가 결측인지 표시
asiapress_missing_dates_table <- asiapress_missing_dates |>

  mutate(
    across(
      -date,
      ~ ifelse(
        is.na(.),
        "Missing",
        ""
      )
    )
  )


knitr::kable(
  asiapress_missing_dates_table,
  col.names = c(
    "Date",
    "Gasoline",
    "Diesel",
    "Rice",
    "Corn",
    "CNY",
    "USD"
  ),
  align = c(
    "l",
    "c",
    "c",
    "c",
    "c",
    "c",
    "c"
  ),
  caption = "Dates and variables with missing values in AsiaPress data"
)



# 3-4. Initial Availability of USD Exchange Rate Data --------------------

# 전체 정제자료의 시작일
first_data_date <- min(
  asiapress_data_clean$date,
  na.rm = TRUE
)


# USD 환율이 처음 관측된 날짜
first_usd_date <- min(
  asiapress_data_clean$date[
    !is.na(asiapress_data_clean$usd_asiapress)
  ],
  na.rm = TRUE
)


# USD 최초 관측 이전 기간의 자료 상태 확인
usd_initial_missing_period <- asiapress_data_clean |>

  filter(
    date < first_usd_date
  ) |>

  summarise(
    start_date = min(date),
    end_date = max(date),
    n_observations = n(),
    n_usd_observed = sum(
      !is.na(usd_asiapress)
    ),
    n_usd_missing = sum(
      is.na(usd_asiapress)
    )
  )


usd_initial_missing_period


# USD 초기 자료 가용성 표
usd_availability_table <- tibble(

  period = c(
    "Initial period",
    "First USD observation"
  ),

  date_or_period = c(

    paste0(
      format(
        usd_initial_missing_period$start_date,
        "%Y-%m-%d"
      ),
      " – ",
      format(
        usd_initial_missing_period$end_date,
        "%Y-%m-%d"
      )
    ),

    format(
      first_usd_date,
      "%Y-%m-%d"
    )
  ),

  usd_data_status = c(

    paste0(
      "No USD values reported across ",
      usd_initial_missing_period$n_observations,
      " observations"
    ),

    "USD value first reported"
  )
)


knitr::kable(
  usd_availability_table,
  col.names = c(
    "Period",
    "Date / Period",
    "USD Data Status"
  ),
  align = c("l", "c", "c"),
  caption = "Initial availability of USD exchange rate data in AsiaPress"
)


# 4. Monthly Data Aggregation --------------------------------------------


# =========================================================================
# 4-1. Create Monthly Data
# =========================================================================

# 관측값이 있으면 평균을 계산하고,
# 해당 월의 값이 모두 결측이면 NA로 유지
mean_or_na <- function(x) {

  if (all(is.na(x))) {

    NA_real_

  } else {

    mean(
      x,
      na.rm = TRUE
    )

  }
}


# 관측자료를 월평균 자료로 변환
# 관측자료 자체는 collection_date까지 보존
# 월별 분석자료는 analysis_end_date까지만 사용
# 조사 자체가 없었던 월도 행을 생성하여 NA로 유지

asiapress_monthly <- asiapress_data_clean |>

  # 논문의 분석 종료일까지 사용
  filter(
    date <= analysis_end_date
  ) |>

  # 각 조사일을 해당 월의 첫째 날로 변환
  mutate(
    month_date = floor_date(
      date,
      "month"
    )
  ) |>

  # 같은 월끼리 그룹화
  group_by(month_date) |>

  # 월평균 계산
  summarise(
    gasoline_asiapress = mean_or_na(gasoline_asiapress),
    diesel_asiapress   = mean_or_na(diesel_asiapress),
    rice_asiapress     = mean_or_na(rice_asiapress),
    corn_asiapress     = mean_or_na(corn_asiapress),
    cny_asiapress      = mean_or_na(cny_asiapress),
    usd_asiapress      = mean_or_na(usd_asiapress),

    # 해당 월의 실제 조사 횟수
    n_obs = n(),

    .groups = "drop"
  ) |>

  # 분석기간의 모든 월을 생성
  # 조사 자체가 없었던 월도 행으로 추가
  complete(
    month_date = seq(
      min(month_date),
      floor_date(
        analysis_end_date,
        "month"
      ),
      by = "month"
    )
  ) |>

  # 조사 자체가 없었던 월은 n_obs = 0
  mutate(
    n_obs = replace_na(
      n_obs,
      0L
    ),

    # 기존 코드와 동일하게 YYYY-MM 변수 생성
    month = format(
      month_date,
      "%Y-%m"
    )
  ) |>

  # 기존 데이터 구조 유지
  select(
    month,
    gasoline_asiapress,
    diesel_asiapress,
    rice_asiapress,
    corn_asiapress,
    cny_asiapress,
    usd_asiapress,
    n_obs
  )


# 월별자료 확인
glimpse(asiapress_monthly)
asiapress_monthly


# 월별 분석기간 확인
min(
  asiapress_monthly$month
)

max(
  asiapress_monthly$month
)


# 월별 관측치 수 확인
nrow(
  asiapress_monthly
)



# =========================================================================
# 4-2. Create Monthly Data Table
# =========================================================================

asiapress_monthly_table <- asiapress_monthly |>

  mutate(
    across(
      c(
        gasoline_asiapress,
        diesel_asiapress,
        rice_asiapress,
        corn_asiapress,
        cny_asiapress,
        usd_asiapress
      ),

      ~ ifelse(
        is.na(.),

        "–",                                   # 결측값은 – 로 표시

        format(
          round(., 1),                         # 소수점 첫째 자리까지 표시
          big.mark = ",",                      # 천 단위 쉼표
          trim = TRUE
        )
      )
    )
  )


knitr::kable(
  asiapress_monthly_table,

  col.names = c(
    "Month",
    "Gasoline",
    "Diesel",
    "Rice",
    "Corn",
    "CNY",
    "USD",
    "N"
  ),

  align = c(
    "l",
    "c",
    "c",
    "c",
    "c",
    "c",
    "c",
    "c"
  ),

  caption = "Monthly averages of AsiaPress market data"
)



# =========================================================================
# 4-3. Check Missing Monthly Data
# =========================================================================

# 월별자료에서 변수별 결측 월 수 확인
colSums(
  is.na(asiapress_monthly)
)


asiapress_monthly_missing <- asiapress_monthly |>

  summarise(
    across(
      -c(month, n_obs),
      ~ sum(is.na(.))
    )
  ) |>

  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "n_missing"
  )


asiapress_monthly_missing


knitr::kable(
  asiapress_monthly_missing,

  col.names = c(
    "Variable",
    "Missing months"
  ),

  align = c(
    "l",
    "c"
  ),

  caption = "Missing values in monthly AsiaPress data"
)



# =========================================================================
# 4-4. Check Months with No Survey Observations
# =========================================================================

# 조사 자체가 없었던 월 확인
asiapress_no_observation_months <- asiapress_monthly |>

  filter(
    n_obs == 0
  )


asiapress_no_observation_months


knitr::kable(
  asiapress_no_observation_months,

  col.names = c(
    "Month",
    "Gasoline",
    "Diesel",
    "Rice",
    "Corn",
    "CNY",
    "USD",
    "N"
  ),

  align = c(
    "l",
    "c",
    "c",
    "c",
    "c",
    "c",
    "c",
    "c"
  ),

  caption = "Months with no AsiaPress survey observations"
)



# =========================================================================
# 4-5. Check Final Monthly Period
# =========================================================================

# 관측자료의 실제 마지막 관측일
max(
  asiapress_data_clean$date,
  na.rm = TRUE
)


# 설정한 자료 수집일
collection_date


# 월별 분석자료의 실제 마지막 월
max(
  asiapress_monthly$month
)


# 설정한 분석 종료월
analysis_end_month


# 분석 종료월 이후 자료가 존재하는지 확인
# 정상이라면 0행
asiapress_monthly |>

  filter(
    month > analysis_end_month
  )


# 분석기간의 전체 월 수 확인
expected_months <- seq(
  floor_date(
    min(
      asiapress_data_clean$date,
      na.rm = TRUE
    ),
    "month"
  ),

  floor_date(
    analysis_end_date,
    "month"
  ),

  by = "month"
)


# 실제 월별자료 행 수와 전체 달력월 수 비교
length(expected_months)
nrow(asiapress_monthly)


# 두 값이 다르면 오류 발생
stopifnot(
  nrow(asiapress_monthly) == length(expected_months)
)


# 5. Plot Monthly Trends --------------------------------------------------

# 5. Plot Monthly Trends --------------------------------------------------


# =========================================================================
# 5-0. Prepare Common Data for Plots
# =========================================================================

# 모든 그래프에서 공통으로 사용할 월별 기본자료
asiapress_monthly_base <- asiapress_monthly |>

  mutate(
    month_date = as.Date(
      paste0(
        month,
        "-01"
      )
    )
  )


# 전체 변수 그래프용 long format
asiapress_monthly_long <- asiapress_monthly_base |>

  select(
    month_date,
    gasoline_asiapress,
    diesel_asiapress,
    rice_asiapress,
    corn_asiapress,
    cny_asiapress,
    usd_asiapress
  ) |>

  pivot_longer(
    cols = -month_date,
    names_to = "variable",
    values_to = "value"
  ) |>

  mutate(
    variable = recode(
      variable,
      gasoline_asiapress = "Gasoline",
      diesel_asiapress   = "Diesel",
      rice_asiapress     = "Rice",
      corn_asiapress     = "Corn",
      cny_asiapress      = "CNY Exchange Rate",
      usd_asiapress      = "USD Exchange Rate"
    )
  )


# 쌀·옥수수의 전년동월 대비 변화율(YoY) 계산
# 4번에서 이미 모든 달을 생성했으므로 complete()는 필요 없음

asiapress_yoy <- asiapress_monthly_base |>

  select(
    month_date,
    rice_asiapress,
    corn_asiapress
  ) |>

  arrange(
    month_date
  ) |>

  mutate(

    # 정확히 12개월 전 가격
    rice_lag12 = lag(
      rice_asiapress,
      12
    ),

    corn_lag12 = lag(
      corn_asiapress,
      12
    ),

    # 전년동월 대비 변화율(%)
    rice_yoy = (
      rice_asiapress / rice_lag12 - 1
    ) * 100,

    corn_yoy = (
      corn_asiapress / corn_lag12 - 1
    ) * 100,

    # 연도 및 월
    year = year(
      month_date
    ),

    month_num = month(
      month_date
    )
  )


# 최근 가격변동 분석자료
asiapress_recent <- asiapress_yoy |>

  filter(
    year >= recent_start_year,
    year <= recent_end_year
  ) |>

  mutate(
    year = factor(
      year,
      levels = recent_start_year:recent_end_year
    )
  )

# =========================================================================
# 5-1. Overview of All Market Variables
# =========================================================================

asiapress_monthly_plot <- ggplot(
  asiapress_monthly_long,
  aes(
    x = month_date,
    y = value
  )
) +

  geom_line(
    color = "blue",
    na.rm = TRUE
  ) +

  geom_point(
    color = "blue",
    na.rm = TRUE
  ) +

  facet_wrap(
    ~ variable,
    scales = "free_y",
    ncol = 2
  ) +

  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +

  scale_y_continuous(
    labels = scales::label_comma()
  ) +

  labs(
    title = "Monthly Trends in AsiaPress Market Variables",
    subtitle = "Monthly averages of market prices and exchange rates",
    x = "Year",
    y = "Value"
  ) +

  theme_minimal() +

  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


asiapress_monthly_plot



# =========================================================================
# 5-2. Rice and Corn Price Trends
# =========================================================================

asiapress_rice_corn_plot <- asiapress_monthly_long |>

  filter(
    variable %in% c(
      "Rice",
      "Corn"
    )
  ) |>

  ggplot(
    aes(
      x = month_date,
      y = value,
      color = variable
    )
  ) +

  geom_line(
    linewidth = 1,
    na.rm = TRUE
  ) +

  geom_point(
    size = 1.5,
    na.rm = TRUE
  ) +

  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +

  scale_y_continuous(
    labels = scales::label_comma()
  ) +

  labs(
    title = "Monthly Rice and Corn Prices in AsiaPress",
    subtitle = "Monthly average prices",
    x = "Year",
    y = "Price (KPW/kg)",
    color = "Commodity"
  ) +

  theme_minimal() +

  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "top"
  )


asiapress_rice_corn_plot



# =========================================================================
# 5-3. Recent Rice Price Dynamics
# =========================================================================


# 5-3-1. Rice Price Levels -----------------------------------------------

rice_price_recent_plot <- ggplot(
  asiapress_recent,
  aes(
    x = month_num,
    y = rice_asiapress,
    color = year,
    group = year
  )
) +

  geom_line(
    linewidth = 1,
    na.rm = TRUE
  ) +

  geom_point(
    size = 2,
    na.rm = TRUE
  ) +

  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +

  scale_y_continuous(
    labels = scales::label_comma()
  ) +

  labs(
    title = paste0(
      "Monthly Rice Prices: ",
      recent_period_label
    ),
    subtitle = "AsiaPress monthly market price data",
    x = "Month",
    y = "Rice Price (KPW/kg)",
    color = "Year"
  ) +

  theme_minimal() +

  theme(
    legend.position = "top"
  )


rice_price_recent_plot



# 5-3-2. Rice Year-on-Year Changes ---------------------------------------

rice_yoy_recent_plot <- ggplot(
  asiapress_recent,
  aes(
    x = month_num,
    y = rice_yoy,
    color = year,
    group = year
  )
) +

  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +

  geom_line(
    linewidth = 1,
    na.rm = TRUE
  ) +

  geom_point(
    size = 2,
    na.rm = TRUE
  ) +

  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +

  scale_y_continuous(
    labels = scales::label_number(
      suffix = "%"
    )
  ) +

  labs(
    title = paste0(
      "Year-on-Year Changes in Rice Prices: ",
      recent_period_label
    ),
    subtitle = "AsiaPress monthly market price data",
    x = "Month",
    y = "Year-on-Year Change (%)",
    color = "Year"
  ) +

  theme_minimal() +

  theme(
    legend.position = "top"
  )


rice_yoy_recent_plot



# =========================================================================
# 5-4. Recent Corn Price Dynamics
# =========================================================================


# 5-4-1. Corn Price Levels -----------------------------------------------

corn_price_recent_plot <- ggplot(
  asiapress_recent,
  aes(
    x = month_num,
    y = corn_asiapress,
    color = year,
    group = year
  )
) +

  geom_line(
    linewidth = 1,
    na.rm = TRUE
  ) +

  geom_point(
    size = 2,
    na.rm = TRUE
  ) +

  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +

  scale_y_continuous(
    labels = scales::label_comma()
  ) +

  labs(
    title = paste0(
      "Monthly Corn Prices: ",
      recent_period_label
    ),
    subtitle = "AsiaPress monthly market price data",
    x = "Month",
    y = "Corn Price (KPW/kg)",
    color = "Year"
  ) +

  theme_minimal() +

  theme(
    legend.position = "top"
  )


corn_price_recent_plot



# 5-4-2. Corn Year-on-Year Changes ---------------------------------------

corn_yoy_recent_plot <- ggplot(
  asiapress_recent,
  aes(
    x = month_num,
    y = corn_yoy,
    color = year,
    group = year
  )
) +

  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +

  geom_line(
    linewidth = 1,
    na.rm = TRUE
  ) +

  geom_point(
    size = 2,
    na.rm = TRUE
  ) +

  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +

  scale_y_continuous(
    labels = scales::label_number(
      suffix = "%"
    )
  ) +

  labs(
    title = paste0(
      "Year-on-Year Changes in Corn Prices: ",
      recent_period_label
    ),
    subtitle = "AsiaPress monthly market price data",
    x = "Month",
    y = "Year-on-Year Change (%)",
    color = "Year"
  ) +

  theme_minimal() +

  theme(
    legend.position = "top"
  )


corn_yoy_recent_plot



# =========================================================================
# 5-5. Check Recent Price Surges
# =========================================================================


# 최근 분석기간 중 쌀 YoY 상승률 상위 5개 월
rice_surge_check <- asiapress_recent |>

  filter(
    !is.na(rice_yoy)
  ) |>

  slice_max(
    order_by = rice_yoy,
    n = 5,
    with_ties = FALSE
  ) |>

  transmute(
    commodity = "Rice",
    month = format(
      month_date,
      "%Y-%m"
    ),
    current_price = round(
      rice_asiapress,
      1
    ),
    price_12m_earlier = round(
      rice_lag12,
      1
    ),
    yoy_change = round(
      rice_yoy,
      1
    )
  )


# 최근 분석기간 중 옥수수 YoY 상승률 상위 5개 월
corn_surge_check <- asiapress_recent |>

  filter(
    !is.na(corn_yoy)
  ) |>

  slice_max(
    order_by = corn_yoy,
    n = 5,
    with_ties = FALSE
  ) |>

  transmute(
    commodity = "Corn",
    month = format(
      month_date,
      "%Y-%m"
    ),
    current_price = round(
      corn_asiapress,
      1
    ),
    price_12m_earlier = round(
      corn_lag12,
      1
    ),
    yoy_change = round(
      corn_yoy,
      1
    )
  )


# 쌀과 옥수수 급등월 결합
recent_price_surge_table <- bind_rows(
  rice_surge_check,
  corn_surge_check
)


# 표 출력용: 가격에 천 단위 쉼표 추가
recent_price_surge_table_display <- recent_price_surge_table |>

  mutate(

    current_price = format(
      current_price,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    ),

    price_12m_earlier = format(
      price_12m_earlier,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  )


knitr::kable(
  recent_price_surge_table_display,
  col.names = c(
    "Commodity",
    "Month",
    "Current Price",
    "Price 12 Months Earlier",
    "YoY Change (%)"
  ),
  align = c(
    "l",
    "c",
    "c",
    "c",
    "c"
  ),
  caption = paste0(
    "Largest Year-on-Year Increases in Rice and Corn Prices, ",
    recent_period_label
  )
)


# Interpretation
# - Rice prices remained relatively stable during 2023–2024 and entered
#   a marked upward phase beginning in 2025.
# - Corn prices remained relatively stable through 2025 and rose sharply
#   in 2026.
# - The timing suggests a lagged pattern in which the increase in rice
#   prices appeared earlier than the increase in corn prices.
# - This temporal ordering alone does not establish a causal relationship
#   between rice and corn prices.



# 6. Save Data ------------------------------------------------------------


# =========================================================================
# 6-1. Check Analysis Period
# =========================================================================

# 관측자료의 실제 시작일과 마지막 관측일
min(
  asiapress_data_clean$date,
  na.rm = TRUE
)

max(
  asiapress_data_clean$date,
  na.rm = TRUE
)


# 월별 분석자료의 실제 시작월과 마지막 월
min(
  asiapress_monthly$month
)

max(
  asiapress_monthly$month
)



# =========================================================================
# 6-2. Create File Name Tags
# =========================================================================

# 관측자료 실제 시작일
observation_start_tag <- format(
  min(
    asiapress_data_clean$date,
    na.rm = TRUE
  ),
  "%Y%m%d"
)


# 관측자료 실제 마지막 관측일
observation_end_tag <- format(
  max(
    asiapress_data_clean$date,
    na.rm = TRUE
  ),
  "%Y%m%d"
)


# 월별 분석자료 실제 시작월
monthly_start_tag <- gsub(
  "-",
  "",
  min(
    asiapress_monthly$month
  )
)


# 월별 분석자료 실제 마지막 월
monthly_end_tag <- gsub(
  "-",
  "",
  max(
    asiapress_monthly$month
  )
)



# =========================================================================
# 6-3. Create Folders
# =========================================================================

dir.create(
  "data/raw/asiapress",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "data/processed/asiapress",
  recursive = TRUE,
  showWarnings = FALSE
)



# =========================================================================
# 6-4. Create File Names
# =========================================================================

# Raw 자료
raw_file <- paste0(
  "data/raw/asiapress/",
  "asiapress_raw_",
  collection_tag,
  ".rds"
)


# 정제 관측자료: R
observation_rds_file <- paste0(
  "data/processed/asiapress/",
  "asiapress_market_observation_",
  observation_start_tag,
  "_",
  observation_end_tag,
  ".rds"
)


# 정제 관측자료: Stata
observation_dta_file <- paste0(
  "data/processed/asiapress/",
  "asiapress_market_observation_",
  observation_start_tag,
  "_",
  observation_end_tag,
  ".dta"
)


# 월별 분석자료: R
monthly_rds_file <- paste0(
  "data/processed/asiapress/",
  "asiapress_market_monthly_",
  monthly_start_tag,
  "_",
  monthly_end_tag,
  ".rds"
)


# 월별 분석자료: Stata
monthly_dta_file <- paste0(
  "data/processed/asiapress/",
  "asiapress_market_monthly_",
  monthly_start_tag,
  "_",
  monthly_end_tag,
  ".dta"
)



# =========================================================================
# 6-5. Save Raw Data
# =========================================================================

saveRDS(
  asiapress_raw,
  raw_file
)



# =========================================================================
# 6-6. Save Cleaned Observation-Level Data
# =========================================================================

# R용
saveRDS(
  asiapress_data_clean,
  observation_rds_file
)


# Stata용
write_dta(
  asiapress_data_clean,
  observation_dta_file
)



# =========================================================================
# 6-7. Save Monthly Analysis Data
# =========================================================================

# R용
saveRDS(
  asiapress_monthly,
  monthly_rds_file
)


# Stata용
write_dta(
  asiapress_monthly,
  monthly_dta_file
)



# =========================================================================
# 6-8. Check Saved Files
# =========================================================================

# Raw 파일 확인
list.files(
  "data/raw/asiapress"
)


# Processed 파일 확인
list.files(
  "data/processed/asiapress"
)


# =========================================================================
# 6-9. Final Check
# =========================================================================

# 설정한 수집일
collection_date

# 설정한 분석 종료일
analysis_end_date

# 관측자료 실제 시작일
min(
  asiapress_data_clean$date,
  na.rm = TRUE
)

# 관측자료 실제 마지막 관측일
max(
  asiapress_data_clean$date,
  na.rm = TRUE
)

# 월별 분석자료 실제 시작월
min(
  asiapress_monthly$month
)

# 월별 분석자료 실제 마지막 월
max(
  asiapress_monthly$month
)

# 분석 종료월 이후 자료가 없는지 확인
# 정상이라면 0행
asiapress_monthly |>

  filter(
    month > analysis_end_month
  )

# 실제 저장되는 파일명 확인
raw_file
observation_rds_file
observation_dta_file
monthly_rds_file
monthly_dta_file