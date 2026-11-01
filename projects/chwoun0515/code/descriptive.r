# 데이터 전처리·시각화
library(tidyverse)
library(janitor)

# 기술통계
library(skimr)
library(psych)
library(naniar)

# 패널회귀
library(fixest)
library(plm)
library(lmtest)
library(sandwich)
library(modelsummary)

# 머신러닝·랜덤포레스트
library(tidymodels)
library(ranger)
library(vip)

# 시각화
library(patchwork)
library(scales)
library(plotly)

# 난수 재현성
set.seed(2026)

df <- read.csv(
  "C:/Users/chwou/Desktop/graduate/04_analysis/workspace/ml-paper-2026/projects/chwoun0515/code/data.csv",
  fileEncoding = "CP949",
  check.names = FALSE
)

# 정상적으로 읽혔는지 확인
head(df)
names(df)
dim(df)
