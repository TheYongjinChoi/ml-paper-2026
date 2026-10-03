# =============================================================================
# 00_01_make_project_folders.R
# R script 이름을 기준으로 프로젝트 폴더를 자동 생성
# Project: Drought Variability and Regional Rice Prices in North Korea
# Author: Ju Hee Jeung
#
# 역할
# - code/에 있는 R script 이름을 읽음
# - 각 script별 output/figures, output/tables 폴더 생성
# - 데이터 생성 script별 data/processed 폴더 생성
# - data/original 및 data/final 폴더 확인·생성
#
# 중요
# - 기존 폴더나 파일은 삭제하지 않음
# - data/original/의 원자료는 수정하지 않음
# - raw는 폴더명이 아니라 분석 사양명으로 사용
#   예: precip_raw, spi6_raw
# =============================================================================


# =============================================================================
# Project Folder Structure
# =============================================================================
#
# nk-drought-rice/
# │
# ├─ code/                                  # R 분석 script
# │  ├─ 00_00_initial.R                     # 공통 package와 저장 함수
# │  ├─ 00_01_make_project_folders.R        # 프로젝트 폴더 자동 생성
# │  ├─ 00_02_project_rules.R               # 프로젝트 관리 규칙
# │  │
# │  ├─ 01_*                                # 원자료 정리 및 데이터 구축
# │  ├─ 02_*                                # 강수량 결측 처리
# │  │  ├─ 02_03_01_main_mi_prepar_.R
# │  │  ├─ 02_03_02_mi_predictor_diagnostics.R
# │  │  ├─ 02_03_03_mi_spatial_predictors.R
# │  │  ├─ 02_03_04_mi_predictor_matrix.R
# │  │  ├─ 02_03_05_mi_pilot.R
# │  │  ├─ 02_03_06_mi_validation.R
# │  │  └─ 02_03_07_mi_final.R
# │  │
# │  ├─ 03_*                                # SPI·SPEI·SMI 등 가뭄지수
# │  ├─ 04_*                                # 최종 분석자료 구축
# │  ├─ 05_*                                # 기술통계 및 시각화
# │  └─ 06_*                                # 회귀 및 계량분석
# │
# ├─ data/
# │  │
# │  ├─ original/                           # 외부에서 확보한 원자료 보관
# │  │  ├─ weather/                         # WMO 기상 원자료
# │  │  └─ rice_price/                      # 북한 쌀가격 원 관측자료
# │  │
# │  ├─ processed/                          # 각 R script가 만든 중간자료
# │  │  ├─ 01_*/
# │  │  ├─ 02_*/
# │  │  ├─ 03_*/
# │  │  └─ ...
# │  │
# │  └─ final/                              # 최종 계량분석용 데이터
# │
# ├─ output/                                # 표·그림·해석 등 분석 결과
# │  └─ <R script 이름>/
# │     ├─ figures/                         # PNG·PDF 그림
# │     └─ tables/                          # CSV·TEX 표
# │
# ├─ test/                                  # 본 분석과 분리된 TEST 작업
# │
# ├─ _master.qmd                            # 연구 전체 분석·결과 문서
# └─ README.md                              # 연구 프로젝트 설명
#
# 관리 원칙
# -----------------------------------------------------------------------------
# 1. code/는 R script를 번호순으로 한 곳에서 관리
# 2. data/original/은 외부에서 확보한 원자료를 수정하지 않고 보존
# 3. data/processed/는 R script 이름별 폴더에 중간자료 저장
# 4. data/final/에는 최종 분석에 실제 사용하는 데이터만 저장
# 5. output/은 R script 이름별로 figures/와 tables/를 분리
# 6. test/는 본 분석과 완전히 분리하여 관리
# 7. R script와 processed/output 폴더 이름을 동일하게 유지
# 8. original은 원자료를 의미하고, raw는 분석 사양을 의미
# =============================================================================


# =============================================================================
# 1. R script별 output 폴더 만들기
# =============================================================================


# code/ 폴더에 있는 모든 .R 파일 이름 가져오기
r_scripts <- list.files(
  path = "code",                    # 살펴볼 폴더
  pattern = "\\.R$",                # .R로 끝나는 파일만 선택
  full.names = FALSE                # 전체 경로가 아니라 파일명만 가져옴
)


# 분석결과 폴더가 필요하지 않은 관리용 script 제외
r_scripts <- r_scripts[
  !r_scripts %in% c(
    "00_00_initial.R",              # 공통 package와 함수
    "00_01_make_project_folders.R", # 프로젝트 폴더 생성
    "00_02_project_rules.R"         # 프로젝트 규칙 기록
  )
]


# TEST 관련 script는 본 분석 output에서 제외
r_scripts <- r_scripts[
  !grepl("Test|test", r_scripts)    # 파일명에 Test 또는 test가 있으면 제외
]


# 파일명에서 .R 확장자를 제거하여 script 이름만 남김
script_names <- tools::file_path_sans_ext(r_scripts)


# 각 분석 script별 figures/와 tables/ 폴더 생성
for (script_name in script_names) {

  figure_dir <- file.path("output", script_name, "figures")  # 그림 저장 폴더
  table_dir  <- file.path("output", script_name, "tables")   # 표 저장 폴더

  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)  # 없으면 생성
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)   # 없으면 생성
}


# output 폴더 생성 결과를 Console에서 확인
cat(
  "\nOutput folders created.\n",
  "Number of scripts:", length(script_names), "\n\n"
)

print(file.path("output", script_names))


# =============================================================================
# 2. 연구 데이터 폴더 구조화
# =============================================================================
#
# 목적
# - 외부 원자료는 data/original/에 보존
# - 각 데이터 생성 script의 중간자료는 data/processed/<script_name>/에 저장
# - 최종 분석자료는 data/final/에 저장
#
# 원칙
# - data/original/의 원자료는 수정하지 않음
# - 01_*, 02_*, 03_* script만 processed 데이터 폴더 생성
# - 04_*에서 만드는 최종 분석자료는 data/final/에 저장
# - 05_* 기술통계와 06_* 회귀분석은 주로 output/을 사용
# - TEST는 Step 1에서 이미 제외
# - 기존 폴더나 파일은 삭제하지 않음
# =============================================================================


# -----------------------------------------------------------------------------
# 2-0. data/original 폴더 확인
# -----------------------------------------------------------------------------


# 외부에서 확보한 원자료를 보관할 최상위 폴더
original_data_dir <- file.path("data", "original")

dir.create(
  original_data_dir,
  recursive = TRUE,
  showWarnings = FALSE             # 이미 존재해도 경고 없이 그대로 유지
)


# 현재 연구의 원자료 유형별 하위 폴더
dir.create(
  file.path(original_data_dir, "weather"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  file.path(original_data_dir, "rice_price"),
  recursive = TRUE,
  showWarnings = FALSE
)


# original 폴더 준비 결과 확인
cat(
  "\nOriginal-data folder ready:\n",
  original_data_dir,
  "\n"
)


# -----------------------------------------------------------------------------
# 2-1. data/processed/<script_name>/ 폴더 생성
# -----------------------------------------------------------------------------


# 중간 데이터를 생성하는 01_*, 02_*, 03_* script만 선택
processed_script_names <- script_names[
  grepl("^(01_|02_|03_)", script_names)
]


# 각 script 이름과 동일한 processed 데이터 폴더 생성
for (script_name in processed_script_names) {

  processed_data_dir <- file.path(
    "data",
    "processed",
    script_name
  )

  dir.create(
    processed_data_dir,
    recursive = TRUE,
    showWarnings = FALSE            # 이미 존재하는 폴더는 그대로 유지
  )
}


# processed 폴더 생성 결과 확인
cat(
  "\nProcessed-data folders created.\n",
  "Number of scripts:", length(processed_script_names), "\n\n"
)

print(
  file.path(
    "data",
    "processed",
    processed_script_names
  )
)


# -----------------------------------------------------------------------------
# 2-2. data/final 폴더 생성
# -----------------------------------------------------------------------------


# 최종 회귀·계량분석에 사용할 데이터만 저장하는 폴더
final_data_dir <- file.path("data", "final")

dir.create(
  final_data_dir,
  recursive = TRUE,
  showWarnings = FALSE              # 이미 있으면 그대로 유지
)


# final 폴더 준비 결과 확인
cat(
  "\nFinal-data folder ready:\n",
  final_data_dir,
  "\n"
)


# =============================================================================
# 3. R script ↔ data ↔ output 위치 확인
# =============================================================================
#
# script 이름만 알면 그 script와 연결된
# processed/final 데이터 및 output 위치를 확인할 수 있도록 표 생성
# =============================================================================


project_folder_map <- data.frame(

  script = script_names,             # R script 이름

  data = ifelse(
    grepl("^(01_|02_|03_)", script_names),

    file.path(
      "data",
      "processed",
      script_names
    ),                               # 01~03은 script별 processed 폴더

    ifelse(
      grepl("^04_", script_names),
      "data/final",                  # 04는 최종 데이터 폴더
      NA_character_                  # 05~06 등은 별도 데이터 폴더 없음
    )
  ),

  output = file.path(
    "output",
    script_names
  ),                                 # 모든 분석 script의 결과 폴더

  stringsAsFactors = FALSE
)


# Console에서 script ↔ data ↔ output 대응관계 확인
print(project_folder_map, row.names = FALSE)

# 직접 R을 실행할 때만 View 창으로 확인
if (interactive()) View(project_folder_map)


# =============================================================================
# 4. 전체 폴더 생성 완료
# =============================================================================


cat(
  "\n============================================================\n",
  "Project folder setup completed.\n",
  "\n",
  "R scripts       : code/\n",
  "Original data   : data/original/\n",
  "Processed data  : data/processed/<script name>/\n",
  "Final data      : data/final/\n",
  "Figures         : output/<script name>/figures/\n",
  "Tables          : output/<script name>/tables/\n",
  "TEST            : test/  (managed separately)\n",
  "============================================================\n"
)