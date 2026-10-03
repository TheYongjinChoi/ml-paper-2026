# =============================================================================
# 02_03_02 Precipitation Data _ MICE Predictor Diagnostics
# 27 stations, daily precipitation
# Diagnose candidate predictors before finalizing the MICE-PMM predictor matrix
#
# Purpose:
# - Check deterministic / structural redundancy among candidate predictors
# - Examine the relationship between each predictor and observed precipitation
# - Check co-missingness among predictors
#
# Workflow (Total steps are in 02_03_01_mi_prepare.R)
#   Step 6-2. Check deterministic / structural redundancy
#   Step 6-3. Check relationship with observed precipitation
#   Step 6-4. Check co-missingness among predictors
#
# Input:
#   data/processed/precipitation/
#   nk_weather_daily_02_03_01_mi_candidates.rds
#
# Output:
#   Diagnostic tables and figures for selecting the final MICE predictors
# =============================================================================


### 0. Initial settings -----------------------------------------------------

source("code/00_initial.R")    # library(mice) 기포함


### 1. Load MICE candidate data --------------------------------------------

nk_weather_daily_mi_candidates <- readRDS(
  "data/processed/precipitation/nk_weather_daily_02_03_01_mi_candidates.rds"
)

dplyr::glimpse(nk_weather_daily_mi_candidates)


### 2. Step 6-2. Check deterministic / structural redundancy --------------
# 목적: 후보 predictor 중 다른 변수들로부터 정확히 계산되는 변수가 있는지 확인
#       dew_dep는 temperature_mean_day - dewpoint_mean_day로 직접 생성한
#       파생변수이므로 구조적 중복 여부를 확인


# 2-1. Check deterministic relationship of dew_dep -----------------------
# dew_dep가 실제로
# temperature_mean_day - dewpoint_mean_day
# 와 정확히 일치하는지 확인

# 분석에 필요한 변수가 모두 존재하는지 확인
required_variables_step_6_2 <- c(
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

stopifnot(
  all(
    required_variables_step_6_2 %in%
      names(nk_weather_daily_mi_candidates)
  )
)


# temperature_mean_day, dewpoint_mean_day, dew_dep가 모두 존재하는 관측치만 이용하여 관계 확인
dew_dep_check <- nk_weather_daily_mi_candidates |>

  dplyr::filter(
    !is.na(temperature_mean_day),
    !is.na(dewpoint_mean_day),
    !is.na(dew_dep)
  ) |>

  dplyr::mutate(

    # 원래 정의에 따라 dew-point depression을 다시 계산
    dew_dep_calculated =
      temperature_mean_day - dewpoint_mean_day,

    # 기존 dew_dep와 재계산값의 차이
    difference =
      dew_dep - dew_dep_calculated
  )


# 차이의 기초 통계 확인
summary(
  dew_dep_check$difference
)


# 가장 큰 절대 차이 확인
max_dew_dep_difference <- max(
  abs(dew_dep_check$difference),
  na.rm = TRUE
)

max_dew_dep_difference


# 컴퓨터의 미세한 부동소수점 오차를 고려하여
# 1e-10보다 큰 차이가 있는 관측치 수 확인
n_dew_dep_difference <- sum(
  abs(dew_dep_check$difference) > 1e-10,
  na.rm = TRUE
)

n_dew_dep_difference


# 결과를 하나의 표로 정리
dew_dep_deterministic_check <- tibble::tibble(

  relationship =
    "dew_dep = temperature_mean_day - dewpoint_mean_day",

  n_complete_observations =
    nrow(dew_dep_check),

  maximum_absolute_difference =
    max_dew_dep_difference,

  n_difference_over_tolerance =
    n_dew_dep_difference
)


print(
  dew_dep_deterministic_check
)


# 2-2. Check exact duplicate predictors ----------------------------------

# 2-3. Step 6-2 conclusion ------------------------------------------------


### 3. Step 6-3. Check relationship with observed precipitation -----------

# 3-1. Check precipitation distribution ----------------------------------

# 3-2. Relationship with rain occurrence ---------------------------------

# 3-3. Relationship with positive precipitation amount -------------------

# 3-4. Step 6-3 conclusion ------------------------------------------------


### 4. Step 6-4. Check co-missingness among predictors --------------------

# 4-1. Check pairwise co-missingness -------------------------------------

# 4-2. Check major missingness patterns ----------------------------------

# 4-3. Step 6-4 conclusion ------------------------------------------------