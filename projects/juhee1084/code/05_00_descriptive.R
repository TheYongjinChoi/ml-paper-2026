# ============================================================
# 06_descriptive.R 내 데이터가 어떻게 생겼는가
# Descriptive statistics and exploratory data analysis
# Project: Drought Variability and Regional Rice Prices
# Author: Ju Hee Jeung
# ============================================================


# 1. Load packages --------------------------------------------------------

library(tidyverse)   # 데이터 정리, 요약 및 기초 시각화


# 2. Check analysis sample ------------------------------------------------

# 최종 분석자료의 구조 확인
#
# 확인사항:
# - 분석기간
# - 지역: Pyongyang / Sinuiju / Hyesan
# - region-month 관측치 수
# - 지역별 관측기간
# - 중복 관측치 여부
# - 결측치 현황


# 3. Descriptive statistics: Rice prices ---------------------------------

# 지역별 쌀가격 기술통계
#
# 주요 변수:
# rice_price
# log_rice_price
#
# 산출 통계:
# - N
# - Mean
# - SD
# - Min
# - Median
# - Max
#
# 지역별 비교:
# - Pyongyang
# - Sinuiju
# - Hyesan
# - Pooled sample


# 4. Descriptive statistics: Climate variables ---------------------------

# 주요 기상변수 기술통계
#
# 예:
# precipitation
# temperature_mean
# temperature_max
# temperature_min
# humidity
# dewpoint
# cloud
# wind
# pressure


# 5. Compare precipitation treatments ------------------------------------

# 강수량 결측치 처리방법별 비교
#
# precip_raw
# precip_zero
# precip_mi
# precip_rf
# precip_xgb
#
# 비교 항목:
# - N
# - Missing observations
# - Mean
# - SD
# - Min
# - Median
# - Max
# - Zero-rain frequency


# 6. Descriptive statistics: Drought indices -----------------------------

# 가뭄지수별 기술통계
#
# SPI:
# spi6_raw
# spi6_zero
# spi6_mi
# spi6_rf
# spi6_xgb
#
# SPEI:
# spei6_raw
# spei6_zero
# spei6_mi
# spei6_rf
# spei6_xgb
#
# SMI:
# smi
#
# 산출 통계:
# - N
# - Mean
# - SD
# - Min
# - Median
# - Max


# 7. Regional comparison --------------------------------------------------

# 평양 / 신의주 / 혜산별 비교
#
# 주요 변수:
# - rice price
# - precipitation
# - SPI-6
# - SPEI-6
# - SMI
#
# 지역별 평균 및 변동성 비교


# 8. Correlation analysis -------------------------------------------------

# 주요 변수 간 상관관계 확인
#
# 예:
# rice_price
# SPI-6
# SPEI-6
# SMI
# precipitation
# temperature
#
# 특히 확인:
# - SPI vs SPEI
# - SPI vs SMI
# - SPEI vs SMI
# - drought indices vs rice price


# 9. Drought-event summary ------------------------------------------------

# 가뭄지수별 가뭄 발생 빈도 확인
#
# 예:
# SPI <= -1
# SPEI <= -1
# SMI 기준값
#
# 지역별:
# - 가뭄 발생 개월 수
# - 가뭄 비율
# - 최소 지수값
# - 연속 가뭄기간


# 10. Time-series diagnostics ---------------------------------------------

# 회귀분석 전 주요 시계열 특성 확인
#
# - 쌀가격 추세
# - 가뭄지수 추세
# - 지역별 변동
# - 극단값
# - 구조적 변화 가능성
#
# 본격적인 논문용 그래프는 08_plot.R에서 작성


# 11. Save descriptive tables ---------------------------------------------

# 논문 및 _master.qmd에서 사용할 기술통계 결과 저장
#
# 예:
# table_descriptive_rice
# table_descriptive_climate
# table_descriptive_drought
# table_correlation
# table_drought_events