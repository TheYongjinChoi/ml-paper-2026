# 최초 한 번만 실행: 미설치 패키지만 설치
packages <- c(
  # 데이터 불러오기·전처리
  "tidyverse", "readxl", "writexl", "janitor",
  
  # 기술통계·결측치 점검
  "skimr", "psych", "naniar",
  
  # 패널회귀·모형 결과표
  "fixest", "plm", "lmtest", "sandwich", "modelsummary",
  
  # 머신러닝·랜덤포레스트
  "tidymodels", "ranger",
  
  # 시각화·그래프 배치·인터랙티브 시각화
  "patchwork", "scales", "plotly"
)

new_packages <- setdiff(
  packages,
  rownames(installed.packages())
)

if (length(new_packages) > 0) {
  install.packages(
    new_packages,
    repos = "https://cloud.r-project.org"
  )
}

# 설치: 최초 한 번
install.packages(
  "vip",
  repos = c(
    "https://bgreenwell.r-universe.dev",
    "https://cloud.r-project.org"
  )
)

# 설치가 끝난 다음 불러오기
library(vip)

# 설치 버전 확인
packageVersion("vip")
