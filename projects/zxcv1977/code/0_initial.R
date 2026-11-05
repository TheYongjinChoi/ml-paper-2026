# ==========================================
# 1. 프로젝트 초기 설정
# ==========================================

# 작업 디렉터리
setwd("C:/Users/jw_5202/Desktop/ml-paper-2026/projects/zxcv1977/data")

# ==========================================
# 2. 데이터 불러오기
# ==========================================

# 패키지 불러오기
library(readr)
library(data.table)

# 1. 시스템의 모든 코어/스레드를 활용하도록 설정
setDTthreads(0)

# 2. UTF-8 인코딩을 지정하여 대용량 파일 빠르게 불러오기
payment <- fread("2025_결제_통합.csv", encoding = "UTF-8", nThread = getDTthreads())
purchase <- fread("2025_구매_통합.csv", encoding = "UTF-8", nThread = getDTthreads())

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
