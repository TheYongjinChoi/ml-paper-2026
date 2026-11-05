
# ==========================================
# 1. 분석 환경 설정
# ==========================================

# 작업 디렉터리
setwd("C:/Users/jw_5202/Desktop/ml-paper-2026/projects/zxcv1977/data")

# install.packages(c("tidyverse", "lubridate", "skimr", "janitor"))

# 데이터 분석 패키지
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)

# 결제 데이터 변수별 결측치
colSums(is.na(payment))

# 구매 데이터 변수별 결측치
colSums(is.na(purchase))

# ==========================================
# 2. 결제 및 구매 데이터의 변수 구조 파악
# ==========================================

# 데이터 행과 열 개수
dim(payment)
dim(purchase)

# 각각의 관측치 수
nrow(payment)
nrow(purchase)

# 각각의 변수 수
ncol(payment)
ncol(purchase)

# 결제 데이터 변수명
names(payment)

# 구매 데이터 변수명
names(purchase)

# 결제 데이터
str(payment)

# 구매 데이터
str(purchase)

# 결제 데이터 변수별 결측치
colSums(is.na(payment))

# 구매 데이터 변수별 결측치
colSums(is.na(purchase))

# 결제 데이터 결측치 비율(%)
colMeans(is.na(payment)) * 100

# 구매 데이터 결측치 비율(%)
colMeans(is.na(purchase)) * 100

# ==========================================
# 3. 결제 및 구매 데이터 기초통계량
# ==========================================

# 결제 데이터
summary(payment)

# 구매 데이터
summary(purchase)

skim(payment)
skim(purchase)

