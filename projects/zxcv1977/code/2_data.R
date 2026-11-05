
# ==========================================
# 목적:
# 1. 가맹점주소에서 서울시 자치구 추출
# 2. 주요 문자형 변수의 factor 변환
# 3. 결제일시 및 구매일시에서 날짜 변수 생성
# 4. 전처리 결과 및 품질 점검 자료 저장
#
# 원칙:
# - payment와 purchase는 서로 결합하지 않음
# - 원본 payment, purchase 객체는 수정하지 않음
# - 별도의 분석용 객체 생성
# ==========================================


# ==========================================
# 0. 기본 설정
# ==========================================

# 작업 디렉터리
setwd("C:/Users/jw_5202/Desktop/ml-paper-2026/projects/zxcv1977")

# install.packages(c("다운받을패키지(1)", "다운받을패키지(2)"...))

# 패키지 로드
library(data.table)
library(stringi)

# ==========================================
# 1. 데이터 불러오기
# ==========================================

# payment, purchase 객체가 없는 경우에만 initial.R 실행
# if (!exists("payment") || !exists("purchase")) {source("code/initial.R")}

# 데이터 존재 여부 확인
stopifnot(exists("payment"))
stopifnot(exists("purchase"))

cat("Payment 행 수:", nrow(payment), "\n")
cat("Purchase 행 수:", nrow(purchase), "\n")

# ==========================================
# 2. 서울시 자치구 추출 함수
# ==========================================

# 서울시 25개 자치구
seoul_gu <- c(
  "종로구", "중구", "용산구", "성동구", "광진구",
  "동대문구", "중랑구", "성북구", "강북구", "도봉구",
  "노원구", "은평구", "서대문구", "마포구", "양천구",
  "강서구", "구로구", "금천구", "영등포구", "동작구",
  "관악구", "서초구", "강남구", "송파구", "강동구"
)

# 정규표현식 생성
district_pattern <- paste0(
  "(",
  paste(seoul_gu, collapse = "|"),
  ")"
)

# 주소에서 자치구 추출
extract_district <- function(address) {
  
  address <- as.character(address)
  
  district <- stringi::stri_extract_first_regex(
    address,
    district_pattern
  )
  
  # 추출된 자치구가 25개 목록에 있는지 확인
  district[!district %in% seoul_gu] <- NA_character_
  
  return(district)
}

# ==========================================
# 3. 날짜 변수 생성 함수
# ==========================================

# YYYYMMDD 또는 YYYYMMDDHHMMSS 형식 처리
make_date_variables <- function(DT, datetime_col, prefix) {
  
  # 날짜 및 시간 원본은 문자형으로 변환하여 처리
  datetime_chr <- as.character(DT[[datetime_col]])
  
  # 8자리 또는 14자리 날짜 형식 확인
  valid_format <- !is.na(datetime_chr) &
    grepl("^\\d{8}(\\d{6})?$", datetime_chr)
  
  # 날짜 부분 추출
  date_chr <- rep(NA_character_, length(datetime_chr))
  
  date_chr[valid_format] <- substr(
    datetime_chr[valid_format],
    1,
    8
  )
  
  # Date 형식 생성
  date_value <- as.IDate(
    date_chr,
    format = "%Y%m%d"
  )
  
  # 실제 날짜로 변환되지 않은 경우도 제외
  valid_date <- !is.na(date_value)
  
  # 연도
  year_value <- rep(NA_integer_, length(datetime_chr))
  
  year_value[valid_date] <- as.integer(
    substr(date_chr[valid_date], 1, 4)
  )
  
  # 월
  month_value <- rep(NA_integer_, length(datetime_chr))
  
  month_value[valid_date] <- as.integer(
    substr(date_chr[valid_date], 5, 6)
  )
  
  # 연월
  year_month <- rep(NA_character_, length(datetime_chr))
  
  year_month[valid_date] <- paste0(
    year_value[valid_date],
    "-",
    sprintf("%02d", month_value[valid_date])
  )
  
  # 날짜 변수 추가
  DT[, (paste0(prefix, "일자")) := date_value]
  
  DT[, (paste0(prefix, "연도")) := year_value]
  
  DT[, (paste0(prefix, "월")) := factor(
    month_value,
    levels = 1:12,
    labels = paste0(sprintf("%02d", 1:12), "월"),
    ordered = TRUE
  )]
  
  DT[, (paste0(prefix, "연월")) := year_month]
  
  # 날짜 형식 오류 건수
  invalid_n <- sum(!valid_date)
  
  return(invalid_n)
}

# ==========================================
# 4. 문자형 변수 Factor 변환 함수
# ==========================================

convert_to_factor <- function(DT, target_vars) {
  
  # 실제 데이터에 존재하는 변수만 선택
  target_vars <- intersect(
    target_vars,
    names(DT)
  )
  
  # 문자형 변수만 factor 변환
  for (v in target_vars) {
    
    if (is.character(DT[[v]])) {
      
      set(
        DT,
        j = v,
        value = as.factor(DT[[v]])
      )
      
    }
  }
  
  return(target_vars)
}

# ==========================================
# 5. 결제 데이터 전처리
# ==========================================

# 원본 보존 및 분석용 데이터 생성
payment_clean <- copy(payment)

# data.table 형식 확인
setDT(payment_clean)


# 5-1. 자치구 추출

payment_clean[
  ,
  구 := extract_district(가맹점주소)
]


# 5-2. 주요 문자형 변수 Factor 변환

payment_factor_vars <- c(
  "결제자성별",
  "상품권명",
  "표준산업명",
  "서울페이코드명",
  "구"
)

payment_factor_converted <- convert_to_factor(
  payment_clean,
  payment_factor_vars
)

# 5-3. 결제일시에서 날짜 변수 생성

payment_invalid_date_n <- make_date_variables(
  DT = payment_clean,
  datetime_col = "결제일시",
  prefix = "결제"
)

# 5-4. 결제자 나이 및 연령대 생성

# 생년월일을 날짜형으로 변환하는 함수
birth_to_date <- function(birth) {
  
  birth_chr <- as.character(birth)
  
  # YYYYMMDD 형식 확인
  valid_birth <- !is.na(birth_chr) &
    grepl("^\\d{8}$", birth_chr)
  
  birth_date_chr <- rep(NA_character_, length(birth_chr))
  
  birth_date_chr[valid_birth] <- birth_chr[valid_birth]
  
  birth_date <- as.IDate(
    birth_date_chr,
    format = "%Y%m%d"
  )
  
  return(birth_date)
}


# 생년월일 날짜형 변환
payment_clean[
  ,
  결제자생년월일_date := birth_to_date(결제자생년월일)
]


# 결제 당시 만 나이 계산
payment_clean[
  ,
  결제자나이 := {
    
    age <- as.integer(
      format(결제일자, "%Y")
    ) - as.integer(
      format(결제자생년월일_date, "%Y")
    )
    
    # 생일이 지나지 않은 경우 1세 차감
    birthday_not_passed <-
      format(결제일자, "%m%d") <
      format(결제자생년월일_date, "%m%d")
    
    age <- age - as.integer(birthday_not_passed)
    
    # 유효하지 않은 날짜 및 비정상 연령 제외
    age[
      is.na(결제일자) |
        is.na(결제자생년월일_date) |
        age < 0 |
        age > 120
    ] <- NA_integer_
    
    age
  }
]


# 연령대 생성
payment_clean[
  ,
  결제자연령대 := cut(
    결제자나이,
    breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
    labels = c(
      "10대 이하",
      "20대",
      "30대",
      "40대",
      "50대",
      "60대",
      "70대 이상"
    ),
    right = TRUE,
    ordered_result = TRUE
  )
]

# 나이 기초통계
summary(payment_clean$결제자나이)

# 연령대별 결제건수
payment_clean[
  ,
  .(결제건수 = .N),
  by = 결제자연령대
][order(결제자연령대)]

# 연령대별 결제금액 및 평균 결제금액
payment_clean[
  ,
  .(
    결제건수 = .N,
    총결제금액 = sum(결제금액, na.rm = TRUE),
    평균결제금액 = mean(결제금액, na.rm = TRUE)
  ),
  by = 결제자연령대
][order(결제자연령대)]

# ==========================================
# 6. 구매 데이터 전처리
# ==========================================

# 원본 보존 및 분석용 데이터 생성
purchase_clean <- copy(purchase)

# data.table 형식 확인
setDT(purchase_clean)


# 6-1. 주요 문자형 변수 Factor 변환

purchase_factor_vars <- c(
  "구매자성별",
  "상품권명"
)

purchase_factor_converted <- convert_to_factor(
  purchase_clean,
  purchase_factor_vars
)

# 6-2. 구매일시에서 날짜 변수 생성

purchase_invalid_date_n <- make_date_variables(
  DT = purchase_clean,
  datetime_col = "구매일시",
  prefix = "구매"
)

# ==========================================
# 7. 전처리 결과 점검 함수
# ==========================================

make_quality_summary <- function(DT, dataset_name) {
  
  # 변수별 결측치 수
  missing_n <- sapply(
    DT,
    function(x) sum(is.na(x))
  )
  
  # 변수별 결측치 비율
  missing_pct <- round(
    missing_n / nrow(DT) * 100,
    2
  )
  
  result <- data.table(
    데이터셋 = dataset_name,
    변수명 = names(DT),
    자료형 = sapply(
      DT,
      function(x) paste(class(x), collapse = "/")
    ),
    결측치수 = as.numeric(missing_n),
    결측치비율 = as.numeric(missing_pct)
  )
  
  return(result)
}

# ==========================================
# 8. 전처리 결과 요약
# ==========================================

# 데이터 크기
preprocessing_size <- data.table(
  데이터셋 = c("payment", "purchase"),
  행수 = c(
    nrow(payment_clean),
    nrow(purchase_clean)
  ),
  변수수 = c(
    ncol(payment_clean),
    ncol(purchase_clean)
  )
)

# 결제 데이터 품질 요약
payment_quality <- make_quality_summary(
  payment_clean,
  "payment"
)

# 구매 데이터 품질 요약
purchase_quality <- make_quality_summary(
  purchase_clean,
  "purchase"
)

# 자치구별 결제 데이터 건수
district_summary <- payment_clean[
  ,
  .(결제건수 = .N),
  by = 구
][order(-결제건수)]

# 자치구 미추출 건수
district_missing_n <- sum(
  is.na(payment_clean$구)
)

# 날짜 변수 점검
date_summary <- data.table(
  데이터셋 = c("payment", "purchase"),
  날짜오류건수 = c(
    payment_invalid_date_n,
    purchase_invalid_date_n
  )
)

# ==========================================
# 9. 결과 저장
# ==========================================

# 결과 폴더 생성
# dir.create("output", showWarnings = FALSE)
# dir.create("output/payment", recursive = TRUE, showWarnings = FALSE)
# dir.create("output/purchase", recursive = TRUE, showWarnings = FALSE)

# 데이터 크기
fwrite(
  preprocessing_size,
  "output/preprocessing_size.csv",
  bom = TRUE
)

# 결제 데이터 품질
fwrite(
  payment_quality,
  "output/payment/preprocessing_quality.csv",
  bom = TRUE
)

# 구매 데이터 품질
fwrite(
  purchase_quality,
  "output/purchase/preprocessing_quality.csv",
  bom = TRUE
)

# 자치구별 추출 결과
fwrite(
  district_summary,
  "output/payment/district_extraction_summary.csv",
  bom = TRUE
)

# 날짜 오류 현황
fwrite(
  date_summary,
  "output/date_error_summary.csv",
  bom = TRUE
)

# ==========================================
# 10. 전처리 데이터 저장
# ==========================================

# RDS 파일 저장 여부
SAVE_RDS <- TRUE

if (SAVE_RDS) {
  
  saveRDS(
    payment_clean,
    "output/payment/payment_clean.rds",
    compress = FALSE
  )
  
  saveRDS(
    purchase_clean,
    "output/purchase/purchase_clean.rds",
    compress = FALSE
  )
}

# ==========================================
# 11. 최종 결과 확인
# ==========================================

cat("\n[결제 데이터]\n")
print(dim(payment_clean))

cat("\n[구매 데이터]\n")
print(dim(purchase_clean))

cat("\n[자치구 추출 결과]\n")
cat("자치구 추출 성공:",
    sum(!is.na(payment_clean$구)), "\n")

cat("자치구 미추출:",
    district_missing_n, "\n")

cat("\n[날짜 변수 오류]\n")
print(date_summary)

cat("\n[전처리된 주요 변수]\n")

print(
  names(payment_clean)
)

print(
  names(purchase_clean)
)

