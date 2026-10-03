# =============================================================================
# 00_00_initial.R
# Common packages and project functions
# Project: Drought Variability and Regional Rice Prices in North Korea
# Author: Ju Hee Jeung
#
# Output rules
# - Generated data : .rds 기본 / 필요한 핵심자료만 .dta 추가
# - Tables         : .csv 기본
# - Paper table    : .csv + .tex
# - Figures        : .png + .pdf
# - Text           : .txt
#
# Naming rules
# - Original data  : 외부에서 확보한 원자료, 파일명 끝에 _original 사용
# - Processed data : original은 사용하지 않음 / raw는 분석 사양명으로 사용 가능
# - raw            : 결측보정·가공 전 기준 사양을 의미 (예: precip_raw, spi6_raw)
# - Date range     : YYYYMM_YYYYMM 형식 사용
# - Generated file : <script_prefix>_<content_name>.<extension>
#
# Folder rules
# - Original data  : data/original/
# - Processed data : data/processed/<script_name>/
# - Final data     : data/final/
# - Figures        : output/<script_name>/figures/
# - Tables         : output/<script_name>/tables/
# - Text           : output/<script_name>/
#
# - script_name과 script_prefix는 각 분석 script에서 정의
# - data_name은 자료내용 + 빈도/처리단계 + 기간이 드러나도록 작성
# - 기본 폴더는 00_01_make_project_folders.R에서 생성
# - 아래 dir.create()는 저장 실패 방지를 위한 안전장치
# - View()는 if (interactive()) View(...) 형식 사용
# =============================================================================


# =============================================================================
# 1. Required packages
# =============================================================================

library(tidyverse)   # 데이터 전처리·집계·시각화
library(haven)       # Stata .dta 파일 읽기·쓰기
library(lubridate)   # 날짜·연도·월 등 시계열 처리
library(SPEI)        # SPI·SPEI 가뭄지수 계산
library(rvest)       # 웹페이지 데이터 수집
library(mice)        # Multiple Imputation 다중대치


# =============================================================================
# 2. Internal helper functions
# =============================================================================

# 지정한 폴더가 없으면 자동으로 생성
ensure_project_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)   # 화면에는 출력하지 않고 폴더 경로를 반환
}


# 프로젝트 규칙에 맞는 전체 파일 경로 생성
project_file <- function(dir, script_prefix, output_name, extension) {
  file.path(                                  # 폴더 경로와 파일명을 안전하게 연결
    dir,                                      # 파일을 저장할 폴더
    paste0(                                   # 여러 문자열을 하나의 파일명으로 연결
      script_prefix, "_",                     # 파일명 앞에 R script 이름 추가
      output_name, ".",                       # 산출물 이름 뒤에 점(.) 추가
      extension                               # rds, csv, png, pdf 등의 확장자
    )
  )
}


# =============================================================================
# 3. Save data
# =============================================================================
# 기본은 RDS로 저장하고, 필요하면 save_dta = TRUE로 DTA도 함께 저장
# location = "processed" → 중간자료
# location = "final"     → 최종 분석자료

save_project_data <- function(
  data,                                       # 저장할 데이터 객체
  data_name,                                  # 파일에 사용할 데이터 이름
  script_name,                                # 실제 R script 이름(.R 제외)
  script_prefix,                              # 저장파일 앞에 붙일 script 이름
  location = c("processed", "final"),         # 저장 위치 선택
  save_dta = FALSE                            # TRUE이면 Stata .dta도 추가 저장
) {

  location <- match.arg(location)             # processed 또는 final 중 하나만 허용

  data_dir <- if (location == "processed") {
    file.path("data", "processed", script_name)   # script별 중간자료 폴더
  } else {
    file.path("data", "final")                    # 최종 분석자료 공통 폴더
  }

  ensure_project_dir(data_dir)                # 폴더가 없으면 안전장치로 생성

  rds_file <- project_file(                   # RDS 전체 저장경로 생성
    data_dir,
    script_prefix,
    data_name,
    "rds"
  )

  saveRDS(data, rds_file)                     # R 데이터 객체를 .rds로 저장

  dta_file <- NULL                            # 기본값: DTA는 저장하지 않음

  if (save_dta) {                             # save_dta = TRUE인 경우만 실행
    dta_file <- project_file(
      data_dir,
      script_prefix,
      data_name,
      "dta"
    )

    haven::write_dta(data, dta_file)          # Stata .dta 파일로 추가 저장
  }

  cat("\nData saved:\nRDS:", rds_file, "\n")  # 실제 저장 위치를 Console에 표시
  if (save_dta) cat("DTA:", dta_file, "\n")   # DTA를 저장한 경우 위치도 표시

  invisible(                                  # 저장경로를 객체로 반환하지만 화면에는 출력하지 않음
    list(
      rds = rds_file,
      dta = dta_file
    )
  )
}


# =============================================================================
# 4. Save table
# =============================================================================
# 기본은 CSV로 저장
# 논문에 사용할 핵심표는 save_tex = TRUE → CSV + TEX 동시 저장

save_project_table <- function(
  table,                                      # 저장할 표 객체
  table_name,                                 # 파일명에 사용할 표 이름
  script_name,                                # 실제 R script 이름
  script_prefix,                              # 파일명 앞에 붙일 script 이름
  save_tex = FALSE,                           # TRUE이면 Overleaf용 .tex 추가 저장
  caption = NULL,                             # LaTeX 표 제목
  digits = NULL                               # 표시할 소수점 자릿수
) {

  table_dir <- file.path(                     # 해당 script의 tables 폴더 지정
    "output",
    script_name,
    "tables"
  )

  ensure_project_dir(table_dir)               # 폴더가 없으면 자동 생성

  csv_file <- project_file(                   # CSV 전체 저장경로 생성
    table_dir,
    script_prefix,
    table_name,
    "csv"
  )

  readr::write_csv(table, csv_file)           # 표를 CSV 파일로 저장

  tex_file <- NULL                            # 기본값: TEX는 저장하지 않음

  if (save_tex) {                             # 논문 핵심표인 경우만 실행

    if (!requireNamespace("knitr", quietly = TRUE)) {
      stop("TEX 저장을 위해 knitr package가 필요합니다.")   # knitr가 없으면 중단
    }

    tex_file <- project_file(                 # TEX 전체 저장경로 생성
      table_dir,
      script_prefix,
      table_name,
      "tex"
    )

  # 소수점 자릿수를 지정하지 않았으면 원래 숫자 그대로 사용
if (is.null(digits)) {

  latex_table <- knitr::kable(
    table,
    format = "latex",
    booktabs = TRUE,
    caption = caption
  )

} else {

  latex_table <- knitr::kable(
    table,
    format = "latex",
    booktabs = TRUE,
    caption = caption,
    digits = digits
  )
}

    writeLines(as.character(latex_table), tex_file)   # LaTeX 코드를 .tex 파일로 저장
  }

  cat("\nTable saved:\nCSV:", csv_file, "\n")         # CSV 저장 위치 출력
  if (save_tex) cat("TEX:", tex_file, "\n")           # TEX 저장 위치 출력

  invisible(
    list(
      csv = csv_file,
      tex = tex_file
    )
  )
}


# =============================================================================
# 5. Save plot
# =============================================================================
# 하나의 ggplot 그림을 PNG와 PDF로 동시에 저장
# PNG → Quarto·PPT·확인용
# PDF → 논문·Overleaf용 벡터 그림

save_project_plot <- function(
  plot,                                       # 저장할 ggplot 객체
  plot_name,                                  # 파일명에 사용할 그림 이름
  script_name,                                # 실제 R script 이름
  script_prefix,                              # 파일명 앞에 붙일 script 이름
  width = 9,                                  # 그림 너비(inch)
  height = 6,                                 # 그림 높이(inch)
  dpi = 300                                   # PNG 해상도
) {

  figure_dir <- file.path(                    # 해당 script의 figures 폴더 지정
    "output",
    script_name,
    "figures"
  )

  ensure_project_dir(figure_dir)              # 폴더가 없으면 자동 생성

  png_file <- project_file(                   # PNG 전체 저장경로 생성
    figure_dir,
    script_prefix,
    plot_name,
    "png"
  )

  pdf_file <- project_file(                   # PDF 전체 저장경로 생성
    figure_dir,
    script_prefix,
    plot_name,
    "pdf"
  )

  ggplot2::ggsave(                            # PNG 실제 저장
    filename = png_file,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )

  ggplot2::ggsave(                            # PDF 실제 저장
    filename = pdf_file,
    plot = plot,
    width = width,
    height = height
  )

  cat(                                        # 저장된 두 파일 위치 출력
    "\nPlot saved:\n",
    "PNG:", png_file, "\n",
    "PDF:", pdf_file, "\n"
  )

  invisible(
    list(
      png = png_file,
      pdf = pdf_file
    )
  )
}


# =============================================================================
# 6. Save interpretation / conclusion
# =============================================================================
# 분석 과정의 해석·판단·결론을 TXT 파일로 저장

save_project_text <- function(
  text,                                       # 저장할 문장 또는 문자열 벡터
  text_name,                                  # interpretation, conclusion 등의 이름
  script_name,                                # 실제 R script 이름
  script_prefix                               # 파일명 앞에 붙일 script 이름
) {

  output_dir <- file.path(                    # 해당 script의 output 최상위 폴더
    "output",
    script_name
  )

  ensure_project_dir(output_dir)              # 폴더가 없으면 자동 생성

  txt_file <- project_file(                   # TXT 전체 저장경로 생성
    output_dir,
    script_prefix,
    text_name,
    "txt"
  )

  readr::write_lines(text, txt_file)          # 문장을 .txt 파일로 저장

  cat("\nText saved:\nTXT:", txt_file, "\n")  # 저장 위치 출력

  invisible(txt_file)                         # 경로를 반환하지만 화면 출력은 생략
}