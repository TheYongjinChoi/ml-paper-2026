# 07_adjusted_model.R -----------------------------------------------------
# 목적: 이전 지원 이력까지 고려해 지원중 변수의 관련성이 달라지는지 확인합니다.
# 모형을 복잡하게 했다는 이유로 인과효과가 되는 것은 아닙니다.
source("C:/Users/chwou/Desktop/graduate/04_analysis/workspace/ml-paper-2026/projects/chwoun0515/code/01_package.R")

run_adjusted <- function(outcome, predictors, label) {
  needed <- c("code", "period", outcome, predictors, "x_support_history")
  d <- analysis_data |>
    dplyr::select(dplyr::all_of(needed)) |>
    tidyr::drop_na()
  cat(label, ":", nrow(d), "행 /", dplyr::n_distinct(d$code), "개 시장\n")

  base_formula <- stats::as.formula(paste(outcome, "~",
    paste(predictors, collapse = " + "), "| code + period"))
  adjusted_formula <- stats::as.formula(paste(outcome, "~",
    paste(c(predictors, "x_support_history"), collapse = " + "),
    "| code + period"))
  # 같은 표본에서 기본모형과 조정모형을 비교합니다.
  base_fit <- fixest::feols(base_formula, data = d, vcov = ~ code)
  adjusted_fit <- fixest::feols(adjusted_formula, data = d, vcov = ~ code)
  print(summary(adjusted_fit))
  capture.output(base_fit, adjusted_fit,
    file = file.path(result_dir, paste0("07_adjusted_", label, ".txt")))
  invisible(list(base = base_fit, adjusted = adjusted_fit))
}

sales_adjusted <- run_adjusted("sales_yoy_log", sales_x, "sales")
entry_adjusted <- run_adjusted("net_entry_rate", entry_x, "entry")

# 조직이 지원사업의 신청 요건인 경우, 조직 없음 × 지원중 조합의 부재는
# 자료 결함이 아니라 자격요건에서 비롯될 수 있습니다. 이 표는 그 구조를 확인합니다.
overlap <- analysis_data |>
  dplyr::filter(year >= 2022, !is.na(organization_presence_2020_0_1)) |>
  dplyr::count(organization_presence_2020_0_1, x_support_sales,
               name = "market_quarters")
print(overlap)
readr::write_csv(overlap, file.path(result_dir, "07_organization_support_overlap.csv"))
if (!all(c(0, 1) %in% overlap$x_support_sales[
    overlap$organization_presence_2020_0_1 == 0])) {
  cat("조직 없음 시장의 전년도 지원중 사례가 없어 조직 유무 × 지원중 상호작용을 추정하지 않습니다. 사업 자격요건과 조직 형성 시점을 함께 해석하세요.\n")
}
