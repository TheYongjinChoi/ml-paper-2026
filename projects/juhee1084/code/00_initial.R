# ============================================================
# initial.R
# Initial settings and required packages
# Project: Drought Variability and Regional Rice Prices
# Author: Ju Hee Jeung
# ============================================================

library(tidyverse)    # 데이터 전처리, 집계, 시각화
library(haven)        # Stata .dta 파일 읽기·쓰기
library(lubridate)    # 날짜 및 월별 시계열 처리
library(SPEI)         # SPI 및 SPEI 산정