library(readr)
library(dplyr)

root <- "C:/Users/chwou/Desktop/graduate/03_data/processed"
stores <- read_csv(file.path(root, "store_all.csv"),
                   col_types = cols(.default = col_character()))

# 주분석: 골목상권·발달상권·전통시장, 2021~2025년
quarterly <- stores |>
  filter(상권_구분_코드 %in% c("A", "D", "R")) |>
  mutate(연도 = as.integer(substr(기준_년분기_코드, 1, 4)),
         점포_수 = parse_double(점포_수)) |>
  filter(between(연도, 2021, 2025)) |>
  group_by(상권_코드, 연도, 기준_년분기_코드) |>
  summarise(분기_점포수 = sum(점포_수), .groups = "drop")

store_annual <- quarterly |>
  group_by(상권_코드, 연도) |>
  summarise(관측분기수 = n(), 연평균_점포수 = mean(분기_점포수),
            .groups = "drop")

# 누락 분기나 결측을 평균에서 임의로 제외하지 않음
stopifnot(all(store_annual$관측분기수 == 4), !anyNA(store_annual$연평균_점포수))

stats_store <- store_annual |>
  group_by(연도) |>
  summarise(상권수 = n(), 평균 = mean(연평균_점포수),
            표준편차 = sd(연평균_점포수),
            최솟값 = min(연평균_점포수),
            중앙값 = median(연평균_점포수),
            최댓값 = max(연평균_점포수), .groups = "drop")

print(stats_store)

sales <- read_csv(file.path(root, "sales_all.csv"),
                  col_types = cols(.default = col_character()))

quarterly <- sales |>
  filter(상권_구분_코드 %in% c("A", "D", "R")) |>
  mutate(연도 = as.integer(substr(기준_년분기_코드, 1, 4)),
         매출 = parse_double(gsub(",", "", 당월_매출_금액))) |>
  filter(between(연도, 2021, 2025)) |>
  group_by(상권_코드, 연도, 기준_년분기_코드) |>
  summarise(분기매출 = sum(매출), .groups = "drop")

# 한 분기라도 누락/결측이면 연간 매출을 확정하지 않음.
# 관측 업종의 합계이며 업종별 포괄 범위의 동일성을 보장하지 않음.
sales_annual <- quarterly |>
  group_by(상권_코드, 연도) |>
  summarise(관측분기수 = n(), 연간매출액_원 = sum(분기매출),
            .groups = "drop") |>
  mutate(완전관측 = 관측분기수 == 4 & !is.na(연간매출액_원),
         연간매출액_원 = if_else(완전관측, 연간매출액_원, NA_real_),
         연간매출액_억원 = 연간매출액_원 / 1e8)

sales_stats <- sales_annual |>
  filter(완전관측) |>
  group_by(연도) |>
  summarise(상권수 = n(), 평균 = mean(연간매출액_억원),
            표준편차 = sd(연간매출액_억원),
            최솟값 = min(연간매출액_억원),
            중앙값 = median(연간매출액_억원),
            최댓값 = max(연간매출액_억원), .groups = "drop")

print(sales_stats)

library(purrr)
df <- read_csv(file.path(root, "combined.csv"),
               locale = locale(encoding = "CP949"),
               col_types = cols(.default = col_character()))
exclude <- c("기준_년분기_코드", "상권_구분_코드", "상권_구분_코드_명",
             "상권_코드", "상권_코드_명", "상권_변화_지표", "상권_변화_지표_명")
vars <- setdiff(names(df), exclude)

annual_df <- df |>
  filter(상권_구분_코드 %in% c("A", "D", "R")) |>
  mutate(연도 = as.integer(substr(기준_년분기_코드, 1, 4)),
         across(all_of(vars), ~ parse_double(gsub(",", "", .x)))) |>
  filter(between(연도, 2021, 2025)) |>
  group_by(상권_코드, 연도) |>
  summarise(across(all_of(vars),
                   ~ if (sum(!is.na(.x)) == 4) mean(.x) else NA_real_),
            .groups = "drop")

stats_df <- map_dfr(vars, function(v) {
  annual_df |>
    group_by(연도) |>
    summarise(변수 = v, 대상상권수 = n(),
              분석상권수 = sum(!is.na(.data[[v]])),
              제외상권수 = sum(is.na(.data[[v]])),
              평균 = if (all(is.na(.data[[v]]))) NA_real_ else mean(.data[[v]], na.rm = TRUE),
              표준편차 = sd(.data[[v]], na.rm = TRUE),
              최솟값 = if (all(is.na(.data[[v]]))) NA_real_ else min(.data[[v]], na.rm = TRUE),
              중앙값 = median(.data[[v]], na.rm = TRUE),
              최댓값 = if (all(is.na(.data[[v]]))) NA_real_ else max(.data[[v]], na.rm = TRUE),
              .groups = "drop")
}) |> arrange(연도, 변수)

print(stats_df)
