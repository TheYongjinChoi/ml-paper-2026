install.packages(c("readr", "dplyr", "purrr", "stringr"))

library(readr)
library(dplyr)
library(purrr)
library(stringr)

root <- "C:/Users/chwou/Desktop/graduate/03_data/raw/seoul_commercial/districts"

out_dir <- "C:/Users/chwou/Desktop/graduate/03_data/processed"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# 영문 컬럼 → 한글 컬럼
name_map <- c(
  stdr_yyqu_cd = "기준_년분기_코드",
  trdar_cd = "상권_코드",
  svc_induty_cd = "서비스_업종_코드",
  stor_co = "점포_수",
  opbiz_stor_co = "개업_점포_수",
  clsbiz_stor_co = "폐업_점포_수",
  frc_stor_co = "프랜차이즈_점포_수"
)

keys <- c(
  "상권_코드",
  "기준_년분기_코드",
  "서비스_업종_코드"
)

read_one <- function(path, value_cols) {
  
  # 확인한 원자료의 한글 인코딩에 맞춤
  x <- read_csv(
    path,
    locale = locale(encoding = "CP949"),
    col_types = cols(.default = col_character()),
    na = c("", "NA", "NULL"),
    name_repair = "check_unique",
    show_col_types = FALSE
  )
  
  if (nrow(problems(x)) > 0) {
    stop("CSV 읽기 오류: ", basename(path))
  }
  
  # 영문 컬럼이 존재하는 경우에만 이름 변경
  idx <- match(tolower(names(x)), names(name_map))
  matched <- !is.na(idx)
  names(x)[matched] <- unname(name_map[idx[matched]])
  
  required <- c(keys, value_cols)
  missing_cols <- setdiff(required, names(x))
  
  if (length(missing_cols) > 0) {
    stop(
      basename(path), "에서 컬럼을 찾지 못했습니다: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  x <- x |>
    select(all_of(required)) |>
    mutate(across(everything(), str_trim)) |>
    mutate(across(everything(), ~ na_if(.x, "")))
  
  # 코드 또는 분기가 없으면 임의로 삭제하지 않고 중단
  if (any(!complete.cases(x[keys]))) {
    stop("병합 기준값에 결측이 있습니다: ", basename(path))
  }
  
  if (any(!str_detect(x$기준_년분기_코드, "^[0-9]{4}[1-4]$"))) {
    stop("분기 형식을 확인하세요: ", basename(path))
  }
  
  # 숫자 변환 실패를 놓치지 않도록 검사
  for (col in value_cols) {
    value <- parse_double(str_remove_all(x[[col]], ","))
    
    if (any(!is.na(x[[col]]) & is.na(value))) {
      stop("숫자 변환 실패: ", basename(path), " / ", col)
    }
    
    x[[col]] <- value
  }
  
  x |>
    filter(
      기준_년분기_코드 >= "20211",
      기준_년분기_코드 <= "20254"
    ) |>
    mutate(원본파일 = basename(path))
}

read_group <- function(folder, pattern, value_cols) {
  files <- list.files(
    file.path(root, folder),
    pattern = pattern,
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("대상 파일이 없습니다: ", folder)
  }
  
  map_dfr(files, ~ read_one(.x, value_cols))
}



#연도별 자료 통합 및 중복 검사

# 자료 읽기
stores <- read_group(
  folder = "stores",
  pattern = "^2[1-5]_store\\.csv$",
  value_cols = c(
    "점포_수",
    "유사_업종_점포_수",
    "개업_율",
    "개업_점포_수",
    "폐업_률",
    "폐업_점포_수",
    "프랜차이즈_점포_수"
  )
)

sales <- read_group(
  folder = "est_earn",
  pattern = "^2[1-5]_est\\.csv$",
  value_cols = c("당월_매출_금액",	"당월_매출_건수",
                 "주중_매출_금액",	"주말_매출_금액",	"주중_매출_건수",	"주말_매출_건수",
                 "남성_매출_금액",	"여성_매출_금액",	"남성_매출_건수",	"여성_매출_건수",
                 "연령대_10_매출_금액",	"연령대_20_매출_금액",	"연령대_30_매출_금액",	"연령대_40_매출_금액",	"연령대_50_매출_금액",	"연령대_60_이상_매출_금액",
                 "연령대_10_매출_건수",	"연령대_20_매출_건수",	"연령대_30_매출_건수",	"연령대_40_매출_건수",	"연령대_50_매출_건수",	"연령대_60_이상_매출_건수"
)
)

check_unique <- function(x, label) {
  duplicates <- x |>
    group_by(across(all_of(keys))) |>
    summarise(행수 = n(), .groups = "drop") |>
    filter(행수 > 1)
  
  if (nrow(duplicates) > 0) {
    print(duplicates)
    stop(label, ": 중복 키를 확인한 후 진행하세요.")
  }
}

check_unique(stores, "stores")
check_unique(sales, "est_earn")

#그 외 데이터

# CSV 읽기 함수
read_data <- function(file, encoding = "CP949") {
  read.csv(
    file.path(root, file),
    fileEncoding = encoding,
    colClasses = "character",
    check.names = FALSE
  )
}

# 데이터 불러오기
apart    <- read_data("apart/apart.csv")
cmsp     <- read_data("cmsp/cmsp.csv")
d_change <- read_data("d_change/d_change.csv")
infra    <- read_data("infra/infra.csv")
living   <- read_data("living/living.csv")
street   <- read_data("street/street.csv")
working  <- read_data("working/working.csv")

# 점포·매출은 연도별 리스트로 불러오기
stores <- setNames(
  lapply(21:25, function(y) {
    read_data(paste0("stores/", y, "_store.csv"))
  }),
  2021:2025
)

sales <- setNames(
  lapply(21:25, function(y) {
    read_data(paste0("est_earn/", y, "_est.csv"))
  }),
  2021:2025
)

# 2026년 행 제거 함수
remove_2026 <- function(df) {
  col <- intersect(
    c("기준_년분기_코드", "stdr_yyqu_cd"),
    names(df)
  )
  
  if (length(col) == 0) return(df)
  
  year <- substr(trimws(as.character(df[[col[1]]])), 1, 4)
  df[is.na(year) | year != "2026", , drop = FALSE]
}

# 개별 데이터에 적용
apart    <- remove_2026(apart)
cmsp     <- remove_2026(cmsp)
d_change <- remove_2026(d_change)
infra    <- remove_2026(infra)
living   <- remove_2026(living)
street   <- remove_2026(street)
working  <- remove_2026(working)

# 연도별 리스트에 적용
stores <- lapply(stores, remove_2026)
sales  <- lapply(sales, remove_2026)


# 연도별 리스트를 하나의 데이터프레임으로 결합
sales_all  <- bind_rows(sales)
stores_all <- bind_rows(stores)


keys <- c("상권_코드", "기준_년분기_코드")
info <- c("상권_코드_명", "상권_구분_코드", "상권_구분_코드_명")

tables <- list(
  d_change = d_change,
  apart = apart,
  cmsp = cmsp,
  infra = infra,
  living = living,
  street = street,
  working = working
)

tables <- imap(tables, function(df, name) {
  # 병합 기준을 문자열로 통일
  df <- df |>
    mutate(across(all_of(keys), ~ trimws(as.character(.x))))
  
  # 중복으로 인한 행 증가 방지
  if (anyDuplicated(df[keys]) > 0) {
    stop(name, ": 상권·분기 조합이 중복됩니다.")
  }
  
  if (name != "d_change") {
    df <- select(df, -any_of(info))
  }
  
  df
})

combined <- reduce(
  tables,
  ~ full_join(
    .x, .y,
    by = keys,
    relationship = "one-to-one",
    na_matches = "never"
  )
) |>
  arrange(상권_코드, 기준_년분기_코드)

# 결과 확인
glimpse(combined)


dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_excel_csv(combined,  file.path(out_dir, "combined.csv"))
readr::write_excel_csv(sales_all, file.path(out_dir, "sales_all.csv"))
readr::write_excel_csv(stores_all, file.path(out_dir, "store_all.csv"))
