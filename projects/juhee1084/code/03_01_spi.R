# ============================================================
# 03_spi.R
# Standardized Precipitation Index (SPI) calculation
# Project: Drought Variability and Regional Rice Prices
# Author: Ju Hee Jeung
# ============================================================


# 1. Load packages --------------------------------------------------------

library(tidyverse)   # 데이터 정리 및 변환
library(SPEI)        # SPI 및 SPEI 산정


# 2. Prepare monthly precipitation data ----------------------------------

# 02_missing_precip.R에서 생성된 월별 강수량 자료 사용
#
# 분석 단위:
# station-month
#
# 강수량 specification:
# precip_raw   : 결측치 미보정
# precip_zero  : 기온 관측 시 강수량 결측치를 0 mm로 보정
# precip_mi    : Multiple Imputation
# precip_rf    : Random Forest imputation
# precip_xgb   : XGBoost imputation
#
# SPI 계산 전에 확인할 사항:
# - 관측소별 월 순서 정렬
# - 중복 station-month 여부 확인
# - 누락된 월 존재 여부 확인
# - 강수량 단위(mm) 확인


# 3. Check monthly time series --------------------------------------------

# 관측소별 관측기간 확인

# 예:
# precip_data %>%
#   group_by(station) %>%
#   summarise(
#     start_month = min(month_date, na.rm = TRUE),
#     end_month   = max(month_date, na.rm = TRUE),
#     n_months    = n()
#   )


# 4. Calculate SPI-6: Spec A (Raw) ----------------------------------------

# 결측치를 보정하지 않은 원 강수량으로 SPI-6 산정

# 변수명:
# spi6_raw


# 5. Calculate SPI-6: Spec B (Zero replacement) ---------------------------

# 기온자료가 존재하는 경우
# 강수량 결측치를 0 mm로 처리한 자료로 SPI-6 산정

# 변수명:
# spi6_zero


# 6. Calculate SPI-6: Spec C (Multiple Imputation) ------------------------

# Multiple Imputation으로 보정한 강수량을 이용하여 SPI-6 산정

# 변수명:
# spi6_mi

# 주의:
# Multiple Imputation의 경우 여러 imputed dataset을 생성하므로
# 최종 분석에서는 각 imputation별 SPI 및 회귀결과를
# 적절하게 결합(pooling)하는 방법 검토


# 7. Calculate SPI-6: Spec D (Random Forest) ------------------------------

# Random Forest로 보정한 강수량을 이용하여 SPI-6 산정

# 변수명:
# spi6_rf


# 8. Calculate SPI-6: Spec E (XGBoost) ------------------------------------

# XGBoost로 보정한 강수량을 이용하여 SPI-6 산정

# 변수명:
# spi6_xgb


# 9. Compare SPI-6 specifications -----------------------------------------

# 비교 대상:
# spi6_raw
# spi6_zero
# spi6_mi
# spi6_rf
# spi6_xgb
#
# 비교 항목:
# - 평균 및 표준편차
# - 결측치 수
# - 상관계수
# - 가뭄 발생시점
# - 극심한 가뭄 발생 빈도
# - 관측소별 차이
# - 시계열 패턴


# 10. Classify drought severity -------------------------------------------

# SPI 기준에 따라 가뭄 정도 분류
#
# 예:
# SPI >=  2.0        : Extremely wet
# 1.5 ~ 1.99         : Severely wet
# 1.0 ~ 1.49         : Moderately wet
# -0.99 ~ 0.99       : Near normal
# -1.0 ~ -1.49       : Moderately dry
# -1.5 ~ -1.99       : Severely dry
# SPI <= -2.0        : Extremely dry


# 11. Save SPI dataset ----------------------------------------------------

# 이후 06_descriptive.R 및 07_estimation.R에서 사용할
# station-month SPI 데이터셋 저장
#
# 최종 주요 변수 예:
#
# station
# month_date
# spi6_raw
# spi6_zero
# spi6_mi
# spi6_rf
# spi6_xgb