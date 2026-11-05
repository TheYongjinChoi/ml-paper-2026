setwd("C:/Users/jw_5202/Desktop/ml-paper-2026/projects/zxcv1977")

#---- 1. 결측값 424개만 추출 ---- 

library(data.table)

setDT(payment_clean)

ls(payment_clean)
class(payment_clean)

# 원본 행 번호 생성
payment_clean[, row_id := .I]

# 구 결측값 추출
gu_missing <- payment_clean[
  is.na(구) | trimws(구) == ""
]

# 결측값 개수 확인
nrow(gu_missing)

#---- 2. 필요한 변수만 선택해서 저장 ---- 

gu_edit <- gu_missing[
  ,
  .(row_id, 가맹점명, 가맹점주소, 가맹점상세주소, 구)
]

fwrite(
  gu_edit,
  "output/구_결측값_424개.csv",
  bom = TRUE
)

#---- 3. 수정한 CSV를 원본에 반영 ---- 

# 수정한 파일 불러오기
gu_fixed <- fread(
  "구_결측값_424개.csv",
  encoding = "UTF-8"
)

# 원본의 결측값만 수정
data[
  gu_fixed,
  on = "row_id",
  구 := i.구
]

# 남은 결측값 확인
data[
  is.na(구) | trimws(구) == "",
  .N
]