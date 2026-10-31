# ==========================================
# 1. 프로젝트 초기 설정
# ==========================================

# 작업 디렉터리
setwd("C:/Users/jw_5202/Desktop/ml-paper-2026/projects/zxcv1977/data")

# ==========================================
# 2. 데이터 불러오기
# ==========================================

# readr 패키지 불러오기
library(readr)

# 결제 데이터 인코딩 확인
guess_encoding(
  "2025_결제_통합.csv"
)

# 결제 데이터
payment <- read.csv(
  "2025_결제_통합.csv",
  fileEncoding = "UTF-8",
  check.names = FALSE
)

# 구매 데이터
purchase <- read.csv(
  "2025_구매_통합.csv",
  fileEncoding = "UTF-8",
  check.names = FALSE
)

# ==========================================
# 3. 데이터 확인
# ==========================================

# 데이터 크기
dim(payment)
dim(purchase)

# 변수명 확인
names(payment)
names(purchase)

# 데이터 구조 확인
str(payment)
str(purchase)