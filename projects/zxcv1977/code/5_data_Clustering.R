
setwd("C:/Users/jw_5202/Desktop/ml-paper-2026/projects/zxcv1977")

# ==================================================
# 1. 변수 확인
# ==================================================

library(data.table)

# 데이터 불러오기
payment_seoul <- readRDS(
  "2025_결제_서울25개자치구_분석용.rds"
)

# 변수명과 자료형 확인
data.table(
  변수명 = names(payment_seoul),
  자료형 = sapply(payment_seoul, class) |>
    sapply(function(x) paste(x, collapse = ", "))
)

# 연령·성별·상품권 관련 변수 확인
grep(
  "연령|나이|성별|상품권|금액|일시|날짜",
  names(payment_seoul),
  value = TRUE
)


# ==================================================
# 2. 변수 변환
# ==================================================

# 원본 보존
payment_cluster <- copy(payment_seoul)

# 1. 자치구를 범주형으로 변환
payment_cluster[
  , 구 := factor(구)
]

# 2. 주요 범주형 변수 확인
categorical_vars <- c(
  "구",
  "상품권명",
  "표준산업명",
  "서울페이코드명",
  "결제자성별",
  "결제자연령대"
)

# 범주형 변환
payment_cluster[
  ,
  (categorical_vars) := lapply(.SD, as.factor),
  .SDcols = categorical_vars
]

# 3. 자료형 확인
sapply(
  payment_cluster[, ..categorical_vars],
  class
)

# 자치구별 기본 소비지표 생성
cluster_district <- payment_cluster[
  ,
  .(
    결제건수 = .N,
    총결제금액 = sum(결제금액, na.rm = TRUE),
    평균결제금액 = mean(결제금액, na.rm = TRUE)
  ),
  by = 구
]

print(cluster_district)


# ==================================================
# 3. 데이터 분포 확인
# ==================================================

# 연령대 분포
payment_cluster[, .N, by = 결제자연령대][
  order(결제자연령대)
]

# 성별 분포
payment_cluster[, .N, by = 결제자성별]

# 상품권 종류
payment_cluster[, .N, by = 상품권명][order(-N)]

# 업종 분류 개수
uniqueN(payment_cluster$표준산업명)
uniqueN(payment_cluster$서울페이코드명)

# 결제일시 값 확인
head(payment_cluster$결제일시)


# ==================================================
# 4. 업종 확인
# ==================================================

# 서울페이 업종별 결제 현황
industry_summary <- payment_cluster[
  ,
  .(
    결제건수 = .N,
    총결제금액 = sum(결제금액, na.rm = TRUE)
  ),
  by = 서울페이코드명
][order(-총결제금액)]

# 결제금액 비중 계산
industry_summary[
  ,
  결제금액비중 := round(
    총결제금액 / sum(총결제금액) * 100,
    2
  )
]

print(industry_summary, nrows = Inf)


# ==================================================
# 5. 자치구별 20개 업종 집계
# ==================================================

# 자치구 × 업종별 결제금액 집계
gu_industry <- payment_cluster[
  ,
  .(
    결제건수 = .N,
    총결제금액 = sum(결제금액, na.rm = TRUE)
  ),
  by = .(구, 서울페이코드명)
]

# 자치구 내 업종별 결제금액 비중
gu_industry[
  ,
  결제금액비중 :=
    총결제금액 / sum(총결제금액) * 100,
  by = 구
]

# 결과 확인
print(gu_industry)

# 저장
fwrite(
  gu_industry,
  "자치구별_업종별_결제현황.csv",
  bom = TRUE
)


# ==================================================
# 6. 자치구별 연령대 소비구조
# ==================================================

# 소비규모 로그 변환 변수 추가
cluster_district[
  ,
  로그총결제금액 := log1p(총결제금액)
]

cluster_district[
  ,
  로그결제건수 := log1p(결제건수)
]

# 연령대별 결제금액 집계
gu_age <- payment_cluster[
  !is.na(결제자연령대),
  .(
    결제건수 = .N,
    결제금액 = sum(결제금액, na.rm = TRUE)
  ),
  by = .(구, 결제자연령대)
]

# 연령대별 결제금액 비중
gu_age[
  ,
  금액비중 := 100 * 결제금액 / sum(결제금액),
  by = 구
]

# 가로 형태 변환
gu_age_wide <- dcast(
  gu_age,
  구 ~ 결제자연령대,
  value.var = "금액비중",
  fill = 0
)

age_cols <- setdiff(names(gu_age_wide), "구")

setnames(
  gu_age_wide,
  age_cols,
  paste0("연령_", age_cols, "_비중")
)

# 날짜 자료형 확인
payment_cluster[
  ,
  분석일자 := as.IDate(결제일자)
]

# 주말 여부 생성
# POSIXlt의 wday: 일요일=0, 토요일=6
payment_cluster[
  ,
  주말여부 := as.integer(
    as.POSIXlt(분석일자)$wday %in% c(0, 6)
  )
]


# ==================================================
# 7. 자치구별 성별 소비구조
# ==================================================

# 1. 성별 결제현황 집계
gu_gender <- payment_cluster[
  !is.na(결제자성별) & !is.na(결제금액),
  .(
    결제건수 = .N,
    결제금액 = sum(결제금액)
  ),
  by = .(구, 결제자성별)
]

# 2. 자치구별 성별 결제금액 비중
gu_gender[
  ,
  금액비중 := 100 * 결제금액 / sum(결제금액),
  by = 구
]

# 3. 가로 형태 변환
gu_gender_wide <- dcast(
  gu_gender,
  구 ~ 결제자성별,
  value.var = "금액비중",
  fill = 0,
  drop = FALSE
)

# 4. 변수명 변경
gender_cols <- setdiff(
  names(gu_gender_wide),
  "구"
)

setnames(
  gu_gender_wide,
  gender_cols,
  paste0("성별_", gender_cols, "_비중")
)

# 5. 결과 확인
print(gu_gender_wide)


# ==================================================
# 8. 자치구별 시간적 소비구조
# ==================================================

# 1. 결제일자 확인
payment_cluster[
  ,
  분석일자 := as.IDate(결제일자)
]

# 2. 주말 여부 생성
# POSIXlt 기준: 일요일=0, 토요일=6
payment_cluster[
  ,
  주말여부 := as.integer(
    as.POSIXlt(분석일자)$wday %in% c(0, 6)
  )
]

# 날짜 결측은 주말 여부도 결측 처리
payment_cluster[
  is.na(분석일자),
  주말여부 := NA_integer_
]

# 3. 자치구별 주말 결제금액 비중
gu_weekend <- payment_cluster[
  !is.na(주말여부) & !is.na(결제금액),
  .(
    주말결제금액비중 =
      100 * sum(결제금액[주말여부 == 1L]) /
      sum(결제금액)
  ),
  by = 구
]

# 4. 자치구 × 월별 결제금액
gu_month <- payment_cluster[
  !is.na(분석일자) & !is.na(결제금액),
  .(
    월결제금액 = sum(결제금액)
  ),
  by = .(
    구,
    결제연월 = format(분석일자, "%Y-%m")
  )
]

# 5. 자치구별 월별 변동계수
gu_month_cv <- gu_month[
  ,
  .(
    월별결제금액CV =
      if (.N >= 2L && mean(월결제금액) > 0) {
        sd(월결제금액) / mean(월결제금액)
      } else {
        NA_real_
      },
    관측월수 = .N
  ),
  by = 구
]

# 6. 결과 확인
print(gu_weekend)
print(gu_month_cv)


# ==================================================
# 9. 자치구별 업종 소비구조 변환
# ==================================================

# 1. 업종별 결제금액 비중을 가로 형태로 변환
gu_industry_wide <- dcast(
  gu_industry,
  구 ~ 서울페이코드명,
  value.var = "결제금액비중",
  fill = 0
)

# 2. 변수명 변경
industry_cols <- setdiff(
  names(gu_industry_wide),
  "구"
)

setnames(
  gu_industry_wide,
  industry_cols,
  paste0("업종_", industry_cols, "_비중")
)

# 3. 결과 확인
print(gu_industry_wide)

cat(
  "업종 관련 변수 수:",
  length(industry_cols),
  "\n"
)


# ==================================================
# 10. 자치구별 군집분석 데이터 통합
# ==================================================

# 1. 결합 대상 데이터 목록
data_list <- list(
  cluster_district,
  gu_age_wide,
  gu_gender_wide,
  gu_industry_wide,
  gu_weekend,
  gu_month_cv
)

# 2. 결합 전 중복키 확인
for (i in seq_along(data_list)) {
  
  stopifnot(
    uniqueN(data_list[[i]]$구) ==
      nrow(data_list[[i]])
  )
  
}

# 3. 자치구 기준 결합
cluster_district_full <- Reduce(
  function(x, y) {
    merge(
      x,
      y,
      by = "구",
      all = TRUE,
      sort = FALSE
    )
  },
  data_list
)

# 4. 정렬
setorder(cluster_district_full, 구)

# 5. 결과 확인
cat(
  "최종 자치구 수:",
  nrow(cluster_district_full),
  "\n"
)

cat(
  "최종 변수 수:",
  ncol(cluster_district_full),
  "\n"
)

# 6. 25개 자치구 확인
stopifnot(
  nrow(cluster_district_full) == 25L,
  uniqueN(cluster_district_full$구) == 25L
)

# 7. 변수명 확인
print(names(cluster_district_full))

# 8. 데이터 확인
print(cluster_district_full)


# ==================================================
# 11. 데이터 검증 및 저장
# ==================================================

# ----------------------------------------------
# 11-1. 결측값 확인
# ----------------------------------------------

missing_summary <- data.table(
  변수명 = names(cluster_district_full),
  결측건수 = vapply(
    cluster_district_full,
    function(x) sum(is.na(x)),
    integer(1)
  )
)

print(
  missing_summary[결측건수 > 0]
)


# ----------------------------------------------
# 11-2. 연령·성별·업종 비중 합계 검증
# ----------------------------------------------

check_share <- function(prefix) {
  
  cols <- grep(
    paste0("^", prefix, "_.*_비중$"),
    names(cluster_district_full),
    value = TRUE
  )
  
  if (length(cols) == 0L) {
    stop("해당 비중 변수가 없습니다: ", prefix)
  }
  
  result <- cluster_district_full[
    ,
    .(
      구,
      비중합계 = rowSums(.SD)
    ),
    .SDcols = cols
  ]
  
  return(result)
}

age_check <- check_share("연령")
gender_check <- check_share("성별")
industry_check <- check_share("업종")

# 비중 합계가 100%에서 벗어나는 자치구 확인
print(
  age_check[abs(비중합계 - 100) > 0.01]
)

print(
  gender_check[abs(비중합계 - 100) > 0.01]
)

print(
  industry_check[abs(비중합계 - 100) > 0.01]
)


# ----------------------------------------------
# 11-3. 월별 관측기간 확인
# ----------------------------------------------

print(
  cluster_district_full[
    ,
    .(구, 관측월수)
  ]
)


# ----------------------------------------------
# 11-4. 변수 간 상관관계 분석
# ----------------------------------------------

# 수치형 변수 추출
numeric_vars <- names(cluster_district_full)[
  vapply(
    cluster_district_full,
    is.numeric,
    logical(1)
  )
]

# 상관계수 행렬
cor_matrix <- cor(
  cluster_district_full[, ..numeric_vars],
  use = "pairwise.complete.obs"
)

# 상관계수 표 변환
cor_dt <- as.data.table(
  as.table(cor_matrix)
)

setnames(
  cor_dt,
  c("변수1", "변수2", "상관계수")
)

# 절댓값 0.8 이상인 변수쌍
high_cor <- cor_dt[
  as.character(변수1) < as.character(변수2) &
    !is.na(상관계수) &
    abs(상관계수) >= 0.8
][order(-abs(상관계수))]

print(high_cor, nrows = 100)


# ----------------------------------------------
# 11-5. 분석용 데이터 저장
# ----------------------------------------------

fwrite(
  cluster_district_full,
  "서울25개자치구_군집분석_전체후보변수.csv",
  bom = TRUE
)

saveRDS(
  cluster_district_full,
  "서울25개자치구_군집분석_전체후보변수.rds"
)

fwrite(
  high_cor,
  "서울25개자치구_군집변수_높은상관관계.csv",
  bom = TRUE
)

cat("군집분석 기초 데이터 구축 완료\n")
