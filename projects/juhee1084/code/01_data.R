# ============================================================
# 01_data.R
# Data import and preprocessing
# Project: Drought Variability and Regional Rice Prices
# Author: Ju Hee Jeung
# ============================================================

# Stata : 기존 데이터 정리 기존 .do코드 활용, .dta파일 저장,
# R / Positron → haven::read_dta()로 .dta 불러오기 → 결측치 보정 → Random Forest / XGBoost → SPI / SPEI / SMI → 회귀분석 → Quarto 문서 작성 → Git/GitHub
# R = 분석·머신러닝, Positron = R 코드 작성, 연구 전체관리와 작업 공간, Git = 변경 이력 관리, GitHub = 연구모임과 코드·진행상황 공유

# 1. Load packages --------------------------------------------------------

library(tidyverse)   # 데이터 전처리, 집계, 그래프
library(haven)       # Stata/SPSS/SAS 파일 읽기·쓰기
library(lubridate)   # 일·월 날짜 처리 및 시계열 변수 생성

# 2. Import data ----------------------------------------------------------

# Data paths will be added here.

# 3. Data cleaning --------------------------------------------------------

# Cleaning and preprocessing code will be added here.