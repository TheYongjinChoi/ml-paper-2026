# 06_robustness.R ---------------------------------------------------------
# 목적: 결과 산식과 표본 선택에 따라 주요 관계가 달라지는지 확인합니다.
source("C:/Users/chwou/Desktop/graduate/04_analysis/workspace/ml-paper-2026/projects/chwoun0515/code/01_package.R")

run_fe <- function(outcome, predictors, label, data = analysis_data) {
  d <- data |>
    dplyr::select(code, period, dplyr::all_of(c(outcome, predictors))) |>
    tidyr::drop_na()
  form <- stats::as.formula(paste(outcome, "~", paste(predictors, collapse = " + "),
                                  "| code + period"))
  fit <- fixest::feols(form, data = d, vcov = ~ code)
  tab <- as.data.frame(fixest::coeftable(fit))
  tab$variable <- rownames(tab)
  tab$model <- label
  tab$n <- stats::nobs(fit)
  tab
}

results <- list()
# 1. 매출 변화의 대체 정의: 로그 차이와 일반 백분율 변화율.
results[[1]] <- run_fe("sales_yoy_log", sales_x, "sales_log")
results[[2]] <- run_fe("sales_yoy_pct", sales_x, "sales_percent")

# 2. 순진입률을 개업률과 폐업률로 분해합니다.
results[[3]] <- run_fe("net_entry_rate", entry_x, "net_entry")
results[[4]] <- run_fe("opening_rate", entry_x, "opening")
results[[5]] <- run_fe("closure_rate", entry_x, "closure")

# 3. 상인조직 유무가 확인된 시장만으로 한정한 결과입니다.
known_organization <- analysis_data |>
  dplyr::filter(!is.na(organization_presence_2020_0_1))
results[[6]] <- run_fe("sales_yoy_log", sales_x, "sales_known_org", known_organization)
results[[7]] <- run_fe("net_entry_rate", entry_x, "entry_known_org", known_organization)

robust_table <- dplyr::bind_rows(results)
readr::write_csv(robust_table, file.path(result_dir, "06_robustness_coefficients.csv"))
print(robust_table |>
  dplyr::filter(grepl("support|entropy", variable)) |>
  dplyr::select(model, variable, Estimate, `Std. Error`, n))
cat("서로 다른 결과지표의 계수 크기를 직접 비교하지 마세요. 방향과 불확실성을 살펴보세요.\n")
