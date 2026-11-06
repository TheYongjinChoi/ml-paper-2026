# 03_CRE.R ----------------------------------------------------------------
# 목적: 같은 시장 내부의 변화(FE)와 시장 간 차이(CRE)를 구분합니다.
# 이 계수들은 조건부 관련성입니다. 지원사업의 인과효과가 아닙니다.
source("C:/Users/chwou/Desktop/graduate/04_analysis/workspace/ml-paper-2026/projects/chwoun0515/code/01_package.R")

run_models <- function(outcome, predictors, label) {
  needed <- c("code", "period", outcome, predictors, "organization_presence_2020_0_1")
  d_fe <- analysis_data |>
    dplyr::select(dplyr::all_of(needed)) |>
    tidyr::drop_na(dplyr::all_of(c(outcome, predictors)))
  cat("\n", label, "FE 표본:", nrow(d_fe), "행 /",
      dplyr::n_distinct(d_fe$code), "개 시장\n")
  if (nrow(d_fe) < 100) stop("분석 표본이 너무 적습니다.")

  # FE: 시장별 고정 특성과 모든 시장에 공통인 분기 충격을 고려합니다.
  fe_formula <- stats::as.formula(paste(outcome, "~", paste(predictors, collapse = " + "),
                                 "| code + period"))
  fe_fit <- fixest::feols(fe_formula, data = d_fe, vcov = ~ code)
  print(summary(fe_fit))
  capture.output(summary(fe_fit), file = file.path(result_dir, paste0("03_FE_", label, ".txt")))

  # CRE(Mundlak): 시장별 평균을 추가해 시장 내부와 시장 간 관계를 나눕니다.
  # 평균은 실제 추정에 사용한 행에서만 계산합니다.
  d <- d_fe |>
    tidyr::drop_na(organization_presence_2020_0_1) |>
    dplyr::group_by(code) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(predictors),
                                ~ mean(.x), .names = "mean_{.col}")) |>
    dplyr::ungroup()
  cat(label, "CRE 표본:", nrow(d), "행 /",
      dplyr::n_distinct(d$code), "개 시장\n")
  mean_names <- paste0("mean_", predictors)
  cre_formula <- stats::as.formula(paste(
    outcome, "~", paste(c(predictors, mean_names,
                            "organization_presence_2020_0_1", "factor(period)"), collapse = " + "),
    "+ (1 | code)"))
  # lmer의 기본 표준오차는 모형 가정에 의존합니다. FE의 시장 군집 표준오차와
  # 같은 추론으로 취급하지 않고 CRE의 시장 간 관련성은 보완 결과로 읽습니다.
  cre_fit <- lme4::lmer(cre_formula, data = d, REML = FALSE)
  print(summary(cre_fit))
  capture.output(summary(cre_fit), file = file.path(result_dir, paste0("03_CRE_", label, ".txt")))
  fe_table <- as.data.frame(fixest::coeftable(fe_fit))
  fe_table$variable <- rownames(fe_table)
  fe_table$model <- "FE"
  fe_table$n <- nrow(d_fe)
  cre_table <- as.data.frame(summary(cre_fit)$coefficients)
  cre_table$variable <- rownames(cre_table)
  cre_table$model <- "CRE"
  cre_table$n <- nrow(d)
  invisible(list(fe = fe_fit, cre = cre_fit,
                 fe_table = fe_table, cre_table = cre_table,
                 n_fe = nrow(d_fe), n_cre = nrow(d)))
}

sales_models <- run_models("sales_yoy_log", sales_x, "sales")
entry_models <- run_models("net_entry_rate", entry_x, "entry")
coefficient_table <- dplyr::bind_rows(
  dplyr::bind_rows(sales_models$fe_table, sales_models$cre_table) |>
    dplyr::mutate(outcome = "sales_yoy_log"),
  dplyr::bind_rows(entry_models$fe_table, entry_models$cre_table) |>
    dplyr::mutate(outcome = "net_entry_rate"))
readr::write_csv(coefficient_table, file.path(result_dir, "03_model_coefficients.csv"))
