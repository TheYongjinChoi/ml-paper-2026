# ============================================================
# 02_01 Precipitation Data _ Raw
# 27 stations, daily weather data
# No missing-value imputation
# ============================================================


# 0. Initial settings -----------------------------------------------------

source("code/00_initial.R")


# 1. Import ---------------------------------------------------------------

nk_weather_daily <- readRDS(
  "data/raw/weather/nk_weather_daily_197301_202607.rds"
)

dplyr::glimpse(nk_weather_daily)


# 2. Create raw precipitation variable -----------------------------------

nk_weather_daily_raw <- nk_weather_daily |>
  dplyr::mutate(
    precip_raw = precipitation_total_day
  )


# 3. Check raw precipitation ---------------------------------------------

summary(nk_weather_daily_raw$precip_raw)

sum(is.na(nk_weather_daily_raw$precip_raw))

sum(!is.na(nk_weather_daily_raw$precip_raw))

sum(nk_weather_daily_raw$precip_raw == 0, na.rm = TRUE)


# Confirm that precip_raw is identical to original precipitation
identical(
  nk_weather_daily_raw$precip_raw,
  nk_weather_daily_raw$precipitation_total_day
)


# 4. Save -----------------------------------------------------------------

saveRDS(
  nk_weather_daily_raw,
  "data/processed/precipitation/nk_weather_daily_precip_raw_197301_202607.rds"
)

View(nk_weather_daily_raw)



# 5. Create monthly raw precipitation data -------------------------------
# 31일중 하루라도 없으면 Na달 (https://journals.ametsoc.org/view/journals/clim/36/22/JCLI-D-23-0193.1.xml?utm_source=chatgpt.com)
# WMO는 일자료를 합산하여 만드는 월 강수량 같은 변수는 모든 일별 관측값이 존재하거나, 누락기간의 강수량이 이후 누적관측값에 포함된 경우에만 월값을 계산하도록 권고 
# WMO가이드: Guidelines on the Calculation of Climate Normals (WMO-No. 1203)

nk_weather_monthly_raw <- nk_weather_daily_raw |>
  dplyr::mutate(
    monthdate = lubridate::floor_date(date, "month")
  ) |>
  dplyr::group_by(
    station,
    station_name_en,
    station_name_kr,
    monthdate
  ) |>
  dplyr::summarise(

    # Number of daily records
    n_days = dplyr::n(),

    # Number of observed precipitation days
    n_precip_observed = sum(
      !is.na(precip_raw)
    ),

    # Number of missing precipitation days
    n_precip_missing = sum(
      is.na(precip_raw)
    ),

    # Monthly total precipitation
    # Raw rule:
    # if at least one daily precipitation value is missing,
    # monthly precipitation remains missing
    precip_month_raw = dplyr::if_else(
      n_precip_missing == 0,
      sum(precip_raw),
      NA_real_
    ),

    .groups = "drop"
  )


# 6. Check monthly raw precipitation -------------------------------------

dplyr::glimpse(nk_weather_monthly_raw)

summary(
  nk_weather_monthly_raw$precip_month_raw
)

sum(
  is.na(nk_weather_monthly_raw$precip_month_raw)
)

sum(
  !is.na(nk_weather_monthly_raw$precip_month_raw)
)


# Check months with missing daily precipitation
nk_weather_monthly_raw |>
  dplyr::filter(
    n_precip_missing > 0
  ) |>
  dplyr::arrange(
    station,
    monthdate
  ) |>
  print(n = 50)


# 7. Save monthly raw precipitation data ---------------------------------

saveRDS(
  nk_weather_monthly_raw,
  "data/processed/precipitation/nk_weather_monthly_precip_raw_197301_202607.rds"
)

haven::write_dta(
 nk_weather_monthly_raw,
  "data/processed/precipitation/nk_weather_monthly_precip_raw_197301_202607.dta"
)


# 8. View -----------------------------------------------------------------

View(nk_weather_daily_raw)

View(nk_weather_monthly_raw)