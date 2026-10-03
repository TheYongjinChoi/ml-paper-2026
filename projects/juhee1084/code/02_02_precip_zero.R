# ============================================================
# 02_02 Precipitation Data _ Zero
# 27 stations, daily weather data
# Rule-based zero imputation:
# Note:
# If precipitation is missing but mean temperature is observed,
# missing precipitation is replaced with 0 mm.
# => 그 달의 precip_zero가 모두 존재 → 합계 계산
# => 하나라도 precip_zero == NA → precip_month_zero = NA로 처리
# Otherwise, precipitation remains missing.
# ============================================================


# 0. Initial settings -----------------------------------------------------

source("code/00_initial.R")


# 1. Import raw precipitation data ----------------------------------------

nk_weather_daily_raw <- readRDS(
  "data/processed/precipitation/nk_weather_daily_precip_raw_197301_202607.rds"
)

# 2. Create zero-imputed precipitation ------------------------------------

nk_weather_daily_zero <- nk_weather_daily_raw |>          # raw 강수량 자료를 불러와 새 객체 생성
  dplyr::mutate(                                          # 새로운 변수 생성
    precip_zero = dplyr::case_when(                       # 조건별로 precip_zero 값 지정

      !is.na(precip_raw) ~ precip_raw,                    # 원래 강수량 값이 있으면 그대로 유지

      is.na(precip_raw) &                                 # 원래 강수량이 결측(NA)이면서
        !is.na(temperature_mean_day) ~ 0,                 # 평균기온 값이 있으면 강수량을 0 mm로 대체

      TRUE ~ NA_real_                                     # 위 조건에 해당하지 않으면 NA 유지
    )
  )


sum(
  is.na(nk_weather_daily_raw$precip_raw) &
    !is.na(nk_weather_daily_raw$temperature_mean_day))     # 304,427


# 원래 강수 결측 개수
sum(is.na(nk_weather_daily_raw$precip_raw))                # 315,671
# zero 보정 후 남은 결측 개수
sum(is.na(nk_weather_daily_zero$precip_zero))              #  11,244


# 결측치 변화표 (1)
precip_zero_summary |>
  dplyr::summarise(
    total_missing_raw = sum(n_precip_missing_raw),
    total_zero_imputed = sum(n_precip_zero_imputed),
    total_missing_after = sum(n_precip_missing_after)
  )

# 결측치 변화표 (2)
precip_zero_total_summary <- precip_zero_summary |>
  dplyr::summarise(
    missing_raw   = sum(n_precip_missing_raw),
    zero_imputed  = sum(n_precip_zero_imputed),
    missing_after = sum(n_precip_missing_after)
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "category",
    values_to = "n"
  ) |>
  dplyr::mutate(
    category = dplyr::recode(
      category,
      missing_raw   = "Original precipitation missing",
      zero_imputed  = "Replaced with 0 mm",
      missing_after = "Missing after imputation"
    ),
    percent = round(
      n / n[category == "Original precipitation missing"] * 100,
      1
    ),
    percent = paste0(percent, "%")
  )

precip_zero_total_summary


#3. Summary : Pyongyang, Sinuiju, Hyesan -------------------------------------

precip_zero_summary_3cities <- nk_weather_daily_zero |>

  # 평양, 신의주, 혜산만 선택
  dplyr::filter(
    station_name_en %in% c("Pyongyang", "Sinuiju", "Hyesan")
  ) |>

  # 관측소별 그룹화
  dplyr::group_by(
    station,
    station_name_en,
    station_name_kr
  ) |>

  # 관측소별 결측치 및 보정 건수 계산
  dplyr::summarise(

    n_total = dplyr::n(),                         # 전체 일별 관측치 수

    n_precip_missing_raw =
      sum(is.na(precip_raw)),                     # 원래 강수 결측치 수

    n_precip_zero_imputed =
      sum(
        is.na(precip_raw) &
          precip_zero == 0,
        na.rm = TRUE
      ),                                          # 0 mm로 대체된 결측치 수

    n_precip_missing_after =
      sum(is.na(precip_zero)),                    # 보정 후 남은 결측치 수

    .groups = "drop"
  ) |>

  # 비율 계산
  dplyr::mutate(

    missing_rate_raw =
      round(n_precip_missing_raw / n_total * 100, 1),
                                                    # 전체 관측치 중 원래 결측률(%)

    zero_imputed_rate =
      round(n_precip_zero_imputed /
              n_precip_missing_raw * 100, 1),
                                                    # 원래 결측치 중 0으로 대체된 비율(%)

    missing_rate_after =
      round(n_precip_missing_after / n_total * 100, 1)
                                                    # 전체 관측치 중 보정 후 결측률(%)
  ) |>

  dplyr::arrange(station)


# Console에서 표 출력
print(
  precip_zero_summary_3cities,
  n = Inf,
  width = Inf
)

# Positron Data Viewer에서 표로 보기
View(precip_zero_summary_3cities)


#4. Save Daily Data _zero

dir.create(
  "data/processed/precipitation",
  recursive = TRUE,
  showWarnings = FALSE
)

# R file
saveRDS(
  nk_weather_daily_zero,
  "data/processed/precipitation/nk_weather_daily_precip_zero_197301_202607.rds"
)

# Stata file
haven::write_dta(
  nk_weather_daily_zero,
  "data/processed/precipitation/nk_weather_daily_precip_zero_197301_202607.dta"
)

# 결측의 96% 이상을 단순히 0으로 바꾸게 되므로, precip_zero를 주 분석값으로 쓰기보다는 강한 가정의 benchmark / sensitivity specification으로 두는 것이 좋음.
# 추후, MI, RF, XGBoost 결과와 비교했을 때 SPI-6이나 쌀가격 회귀 결과가 얼마나 달라지는지를 보는 데 유용.


# 표저장

saveRDS(
  precip_zero_summary_3cities,
  "precip_zero_summary_3cities.rds"
)

#5. Create monthly zero precipitation data

nk_weather_monthly_zero <- nk_weather_daily_zero |>

  # Create monthly identifier
  # Example:
  # 1973-07-03, 1973-07-15, 1973-07-31
  # -> 1973-07-01
  #
  # 1973-07-01 represents "July 1973".
  dplyr::mutate(
    monthdate = lubridate::floor_date(
      date,
      unit = "month"
    )
  ) |>

  # Aggregate by station and month
  dplyr::group_by(
    station,
    station_name_en,
    station_name_kr,
    monthdate
  ) |>

  dplyr::summarise(

    # Number of daily records in each month
    n_days = dplyr::n(),

    # Number of observed precipitation values
    # after zero imputation
    n_precip_observed_zero =
      sum(!is.na(precip_zero)),

    # Number of precipitation values still missing
    # after zero imputation
    n_precip_missing_zero =
      sum(is.na(precip_zero)),

    # Number of originally missing precipitation values
    # replaced with 0 mm
    n_precip_zero_imputed =
      sum(
        is.na(precip_raw) &
          precip_zero == 0,
        na.rm = TRUE
      ),

    # Monthly total precipitation
    #
    # If all daily precip_zero values are available:
    #   sum daily precipitation
    #
    # If at least one precip_zero value remains missing:
    #   monthly precipitation remains NA
    precip_month_zero =
      dplyr::if_else(
        n_precip_missing_zero == 0,
        sum(precip_zero),
        NA_real_
      ),

    .groups = "drop"
  ) |>

  # Optional display variable
  dplyr::mutate(
    year_month = format(
      monthdate,
      "%Y-%m"
    )
  )


# 6. Check monthly zero precipitation

dplyr::glimpse(
  nk_weather_monthly_zero
)

summary(
  nk_weather_monthly_zero$precip_month_zero
)


# Number of complete monthly precipitation values
sum(
  !is.na(
    nk_weather_monthly_zero$precip_month_zero
  )
)


# Number of monthly precipitation values still missing
sum(
  is.na(
    nk_weather_monthly_zero$precip_month_zero
  )
)


# Check months that still contain missing daily precipitation
nk_weather_monthly_zero |>
  dplyr::filter(
    n_precip_missing_zero > 0
  ) |>
  dplyr::arrange(
    station,
    monthdate
  ) |>
  print(
    n = 50,
    width = Inf
  )


# 7. Summary: Pyongyang, Sinuiju, Hyesan

precip_month_zero_summary_3cities <-
  nk_weather_monthly_zero |>

  dplyr::filter(
    station_name_en %in%
      c(
        "Pyongyang",
        "Sinuiju",
        "Hyesan"
      )
  ) |>

  dplyr::select(
    station,
    station_name_en,
    station_name_kr,
    monthdate,
    year_month,
    n_days,
    n_precip_observed_zero,
    n_precip_missing_zero,
    n_precip_zero_imputed,
    precip_month_zero
  ) |>

  dplyr::arrange(
    station,
    monthdate
  )


View(
  precip_month_zero_summary_3cities
)



# 8. Save monthly zero precipitation data

saveRDS(
  nk_weather_monthly_zero,
  "data/processed/precipitation/nk_weather_monthly_precip_zero_197301_202607.rds"
)

haven::write_dta(
  nk_weather_monthly_zero,
  "data/processed/precipitation/nk_weather_monthly_precip_zero_197301_202607.dta"
)
