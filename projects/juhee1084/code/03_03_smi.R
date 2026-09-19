# ============================================================
# 05_smi.R
# Soil Moisture Index (SMI) calculation
# Project: Drought Variability and Regional Rice Prices
# Author: Ju Hee Jeung
# ============================================================


# 1. Load packages --------------------------------------------------------

library(tidyverse)   # 데이터 정리 및 변환
library(lubridate)   # 월별 날짜 처리


# 2. Import soil moisture data --------------------------------------------

# 토양수분(soil moisture) 자료 불러오기
#
# 후보 자료:
# - ERA5-Land soil moisture
# - 기타 재분석 또는 위성 기반 토양수분 자료
#
# 확인사항:
# - 자료의 공간해상도
# - 시간해상도
# - 토양층(depth)
# - 단위
# - 북한 지역 커버리지
# - 분석기간


# 3. Prepare monthly soil moisture data -----------------------------------

# 분석 단위:
# region-month 또는 station-month
#
# 처리사항:
# - 일별 또는 sub-daily 자료를 월별 자료로 집계
# - 평양 / 신의주 / 혜산 지역과 공간적으로 매칭
# - 날짜 변수 통일
# - 결측치 확인
# - 이상치 확인


# 4. Select soil moisture layer -------------------------------------------

# 사용할 토양층 결정
#
# 예:
# - surface soil moisture
# - root-zone soil moisture
#
# 농업 및 쌀 생산과의 관련성을 고려하여
# 최종 토양층 선택


# 5. Calculate SMI ---------------------------------------------------------

# Soil Moisture Index 산정
#
# 기본 개념:
# 각 지역 또는 격자의 토양수분을
# 장기 분포 또는 기준기간과 비교하여 표준화
#
# 예시 변수명:
# smi
#
# 실제 계산식과 표준화 방법은
# 사용할 자료 및 선행연구 검토 후 확정


# 6. Check SMI distribution -----------------------------------------------

# 확인항목:
# - 평균
# - 표준편차
# - 최소 / 최대값
# - 결측치
# - 지역별 분포
# - 계절별 패턴


# 7. Identify soil-moisture drought ---------------------------------------

# SMI 값에 따른 토양수분 부족 상태 확인
#
# 가뭄 threshold는
# 사용한 SMI 정의 및 선행연구에 따라 설정


# 8. Compare SMI across regions -------------------------------------------

# 평양 / 신의주 / 혜산 비교
#
# - 평균 SMI
# - 가뭄 발생시점
# - 가뭄 지속기간
# - 가뭄 강도
# - 지역별 차이


# 9. Compare SMI with SPI and SPEI ----------------------------------------

# SMI / SPI-6 / SPEI-6 비교
#
# - 상관관계
# - 가뭄 발생시점 일치 여부
# - 기상학적 가뭄과 토양수분 가뭄의 시차
# - 지역별 차이


# 10. Save SMI dataset ----------------------------------------------------

# 이후 06_descriptive.R 및 07_estimation.R에서 사용할
# region-month 또는 station-month SMI 자료 저장
#
# 최종 주요 변수 예:
#
# region
# month_date
# soil_moisture
# smi