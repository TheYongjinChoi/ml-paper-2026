# ============================================================
# 04_spei.R
# Standardized Precipitation Evapotranspiration Index (SPEI)
# Project: Drought Variability and Regional Rice Prices
# Author: Ju Hee Jeung
# ============================================================

# hornthwaite와 Hargreaves는 둘 다 잠재증발산량(PET, Potential Evapotranspiration)을 추정하는 방법

# 1. Load packages --------------------------------------------------------

library(tidyverse)   # 데이터 정리 및 변환
library(SPEI)        # SPEI 산정


# 2. Prepare monthly climate data -----------------------------------------

# 02_missing_precip.R에서 생성한 강수량 자료와
# 기온자료를 이용하여 SPEI 계산용 월별 자료 준비
#
# 분석 단위:
# station-month
#
# 주요 변수:
# precipitation
# temperature_mean
# temperature_max
# temperature_min
#
# 강수량 specification:
# precip_raw
# precip_zero
# precip_mi
# precip_rf
# precip_xgb
#
# 확인사항:
# - 관측소별 월 순서 정렬
# - 중복 station-month 확인
# - 누락 월 확인
# - 강수량 단위(mm) 확인
# - 기온 단위(°C) 확인


# 3. Estimate potential evapotranspiration (PET) --------------------------

# SPEI 계산에는 잠재증발산량(PET)이 필요
#
# PET 산정방법 검토:
# - Thornthwaite method
# - Hargreaves method
#
# 사용 가능한 기온자료 및 관측소 위치정보에 따라
# 최종 PET 산정방법 결정
#
# 생성 변수 예:
# pet


# 4. Calculate climatic water balance ------------------------------------

# Climatic water balance:
#
# D = P - PET
#
# 각 강수량 specification별 생성:
#
# balance_raw
# balance_zero
# balance_mi
# balance_rf
# balance_xgb


# 5. Calculate SPEI-6: Spec A (Raw) ---------------------------------------

# Raw precipitation을 이용한 SPEI-6
#
# 변수명:
# spei6_raw


# 6. Calculate SPEI-6: Spec B (Zero replacement) --------------------------

# Temperature-based zero replacement 자료를 이용한 SPEI-6
#
# 변수명:
# spei6_zero


# 7. Calculate SPEI-6: Spec C (Multiple Imputation) -----------------------

# Multiple Imputation 자료를 이용한 SPEI-6
#
# 변수명:
# spei6_mi
#
# 주의:
# MI는 복수의 imputed dataset을 생성하므로
# 각 데이터셋별 SPEI 산정 및 후속 분석의 pooling 방법 검토


# 8. Calculate SPEI-6: Spec D (Random Forest) -----------------------------

# Random Forest 보정 강수량을 이용한 SPEI-6
#
# 변수명:
# spei6_rf


# 9. Calculate SPEI-6: Spec E (XGBoost) -----------------------------------

# XGBoost 보정 강수량을 이용한 SPEI-6
#
# 변수명:
# spei6_xgb


# 10. Compare SPEI-6 specifications ---------------------------------------

# 비교 대상:
# spei6_raw
# spei6_zero
# spei6_mi
# spei6_rf
# spei6_xgb
#
# 비교 항목:
# - 평균 및 표준편차
# - 결측치 수
# - 상관계수
# - 가뭄 발생시점
# - 가뭄 강도
# - 관측소별 차이
# - 시계열 패턴


# 11. Compare SPI-6 and SPEI-6 --------------------------------------------

# SPI-6와 SPEI-6 비교
#
# - 상관관계
# - 가뭄 발생시점 일치 여부
# - 고온기에 SPEI가 더 강한 가뭄을 나타내는지 확인
# - 지역별 차이 비교


# 12. Save SPEI dataset ---------------------------------------------------

# 이후 06_descriptive.R 및 07_estimation.R에서 사용할
# station-month SPEI 자료 저장
#
# 최종 주요 변수 예:
#
# station
# month_date
# pet
# spei6_raw
# spei6_zero
# spei6_mi
# spei6_rf
# spei6_xgb