
# ==========================================
# 목적:
# 1. 결제 데이터 교차분석
# 2. 구매 데이터 교차분석
# 3. 교차분석 결과 CSV 저장
#
# 주의:
# - payment와 purchase는 결합하지 않음
# - 전처리된 RDS 파일 활용
# ==========================================


# ==========================================
# 0. 기본 설정
# ==========================================

setwd("C:/Users/jw_5202/Desktop/ml-paper-2026/projects/zxcv1977")

library(data.table)

# 결과 폴더 생성
cross_dir <- file.path("output", "cross_analysis")

payment_cross_dir <- file.path(
  cross_dir, "payment"
)

purchase_cross_dir <- file.path(
  cross_dir, "purchase"
)

dir.create(
  payment_cross_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  purchase_cross_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ==========================================
# 1. 전처리 데이터 불러오기
# ==========================================

# payment_clean <- readRDS("output/payment/payment_clean.rds")

# purchase_clean <- readRDS("output/purchase/purchase_clean.rds")

# ==========================================
# 2. 교차분석 함수
# ==========================================

# 두 범주형 변수에 따른 집계표 생성
cross_summary <- function(
    DT,
    var1,
    var2,
    amount_var) {
  
  result <- DT[
    ,
    .(
      건수 = .N,
      총금액 = as.numeric(
        sum(get(amount_var), na.rm = TRUE)
      ),
      평균금액 = as.numeric(
        mean(get(amount_var), na.rm = TRUE)
      )
    ),
    by = c(var1, var2)
  ]
  
  # 그룹별 건수 비율
  result[
    ,
    건수비율 := 건수 / sum(건수) * 100,
    by = var1
  ]
  
  # 그룹별 금액 비율
  result[
    ,
    금액비율 := 총금액 / sum(총금액) * 100,
    by = var1
  ]
  
  setorderv(result, c(var1, var2))
  
  return(result)
}

# ==========================================
# 3. 상품권 결제 데이터 교차분석
# ==========================================

# 3-1. 성별 × 연령대

payment_gender_age <- cross_summary(
  DT = payment_clean,
  var1 = "결제자성별",
  var2 = "결제자연령대",
  amount_var = "결제금액"
)

fwrite(
  payment_gender_age,
  file.path(
    payment_cross_dir,
    "01_gender_age.csv"
  ),
  bom = TRUE
)


# 3-2. 연령대 × 상품권 종류

payment_age_product <- cross_summary(
  DT = payment_clean,
  var1 = "결제자연령대",
  var2 = "상품권명",
  amount_var = "결제금액"
)

fwrite(
  payment_age_product,
  file.path(
    payment_cross_dir,
    "02_age_product.csv"
  ),
  bom = TRUE
)


# 3-3. 자치구 × 상품권 종류

payment_region_product <- cross_summary(
  DT = payment_clean,
  var1 = "구",
  var2 = "상품권명",
  amount_var = "결제금액"
)

fwrite(
  payment_region_product,
  file.path(
    payment_cross_dir,
    "03_region_product.csv"
  ),
  bom = TRUE
)


# 3-4. 자치구 × 성별

payment_region_gender <- cross_summary(
  DT = payment_clean,
  var1 = "구",
  var2 = "결제자성별",
  amount_var = "결제금액"
)

fwrite(
  payment_region_gender,
  file.path(
    payment_cross_dir,
    "04_region_gender.csv"
  ),
  bom = TRUE
)

# ==========================================
# 4. 상품권 구매 데이터 교차분석
# ==========================================

# 구매금액 변수명
purchase_amount_var <- "구매금액(액면가)"


# 4-1. 성별 × 상품권 종류

purchase_gender_product <- cross_summary(
  DT = purchase_clean,
  var1 = "구매자성별",
  var2 = "상품권명",
  amount_var = purchase_amount_var
)

fwrite(
  purchase_gender_product,
  file.path(
    purchase_cross_dir,
    "01_gender_product.csv"
  ),
  bom = TRUE
)


# 4-2. 월별 × 할인율

purchase_month_discount <- purchase_clean[
  ,
  .(
    구매건수 = .N,
    
    총구매금액 = sum(
      as.numeric(get(purchase_amount_var)),
      na.rm = TRUE
    ),
    
    평균구매금액 = mean(
      as.numeric(get(purchase_amount_var)),
      na.rm = TRUE
    )
  ),
  by = .(구매연월, 할인율)
]

# 월별·할인율별 정렬
setorder(
  purchase_month_discount,
  구매연월,
  할인율
)

# 결과 저장
fwrite(
  purchase_month_discount,
  file.path(
    purchase_cross_dir,
    "02_month_discount.csv"
  ),
  bom = TRUE
)


# 4-3. 성별 × 할인율

purchase_gender_discount <- purchase_clean[
  ,
  .(
    구매건수 = .N,
    
    총구매금액 = sum(
      as.numeric(get(purchase_amount_var)),
      na.rm = TRUE
    ),
    
    평균구매금액 = mean(
      as.numeric(get(purchase_amount_var)),
      na.rm = TRUE
    )
  ),
  by = .(구매자성별, 할인율)
]

# 정렬
setorder(
  purchase_gender_discount,
  구매자성별,
  할인율
)

# 결과 저장
fwrite(
  purchase_gender_discount,
  file.path(
    purchase_cross_dir,
    "03_gender_discount.csv"
  ),
  bom = TRUE
)


# 4-4. 상품권 종류 × 할인율

purchase_product_discount <- purchase_clean[
  ,
  .(
    구매건수 = .N,
    
    총구매금액 = sum(
      as.numeric(get(purchase_amount_var)),
      na.rm = TRUE
    ),
    
    평균구매금액 = mean(
      as.numeric(get(purchase_amount_var)),
      na.rm = TRUE
    )
  ),
  by = .(상품권명, 할인율)
]

# 상품권명 및 할인율 기준 정렬
setorder(
  purchase_product_discount,
  상품권명,
  할인율
)

# 결과 저장
fwrite(
  purchase_product_discount,
  file.path(
    purchase_cross_dir,
    "04_product_discount.csv"
  ),
  bom = TRUE
)

# ==========================================
# 5. 분석 완료
# ==========================================

cat("결제 교차분석 결과:", payment_cross_dir, "\n")
cat("구매 교차분석 결과:", purchase_cross_dir, "\n")
