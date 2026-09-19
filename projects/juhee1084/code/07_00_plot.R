# ============================================================
# 08_plot.R
# Figures and visualization
# Project: Drought Variability and Regional Rice Prices
# Author: Ju Hee Jeung
# ============================================================


# 1. Load packages --------------------------------------------------------

library(tidyverse)
library(lubridate)


# 2. Set common plot theme -----------------------------------------------

# 논문 전체 그래프 형식 통일


# 3. Create pooled three-city series -------------------------------------

# 평양 / 신의주 / 혜산의 월별 평균값 생성
#
# 주의:
# 이것은 북한 전체 평균이 아니라
# "three-city average" 또는 "pooled three-city sample"로 표현
#
# 예:
# rice_price_3city_mean
# spi6_3city_mean
# spei6_3city_mean
# smi_3city_mean


# 4. Plot rice-price trends ----------------------------------------------

# 4.1 Pyongyang
# 4.2 Sinuiju
# 4.3 Hyesan
# 4.4 Three-city average
#
# 개별 지역 + 3개 지역 평균 비교


# 5. Plot precipitation trends -------------------------------------------

# 평양 / 신의주 / 혜산
# + three-city average


# 6. Plot missing precipitation patterns ---------------------------------

# 관측소별 / 연도별 결측 패턴
# 결측치 처리방법별 비교


# 7. Plot imputation-method comparison -----------------------------------

# Raw
# Zero replacement
# Multiple Imputation
# Random Forest
# XGBoost


# 8. Plot SPI-6 -----------------------------------------------------------

# 8.1 Pyongyang SPI-6
# 8.2 Sinuiju SPI-6
# 8.3 Hyesan SPI-6
# 8.4 Three-city average SPI-6


# 9. Plot SPEI-6 ----------------------------------------------------------

# 9.1 Pyongyang SPEI-6
# 9.2 Sinuiju SPEI-6
# 9.3 Hyesan SPEI-6
# 9.4 Three-city average SPEI-6


# 10. Plot SMI ------------------------------------------------------------

# 10.1 Pyongyang SMI
# 10.2 Sinuiju SMI
# 10.3 Hyesan SMI
# 10.4 Three-city average SMI


# 11. Compare drought indices --------------------------------------------

# SPI-6 / SPEI-6 / SMI 비교
#
# 개별 지역:
# - Pyongyang
# - Sinuiju
# - Hyesan
#
# 통합:
# - Three-city average


# 12. Plot drought and rice prices ---------------------------------------

# 12.1 Three-city pooled / average trend
#      Rice price vs SPI-6
#      Rice price vs SPEI-6
#      Rice price vs SMI
#
# 12.2 Pyongyang
# 12.3 Sinuiju
# 12.4 Hyesan


# 13. Plot regional heterogeneity ----------------------------------------

# 회귀계수 비교:
#
# Pooled three-city model
# Pyongyang
# Sinuiju
# Hyesan
#
# coefficient plot


# 14. Plot lagged effects -------------------------------------------------

# Pooled three-city model:
# lag 0 / 1 / 3 / 6
#
# 이후 지역별 결과와 비교


# 15. Plot robustness results --------------------------------------------

# Pooled model을 중심으로
# Raw / Zero / MI / RF / XGBoost 결과 비교
#
# 필요 시 지역별 robustness 추가


# 16. Save figures --------------------------------------------------------

# output/figures/
#
# fig_rice_price_3city.png
# fig_spi6_3city.png
# fig_spei6_3city.png
# fig_smi_3city.png
# fig_drought_price_pooled.png
# fig_regional_effects.png
# fig_lagged_effects.png
# fig_robustness.png