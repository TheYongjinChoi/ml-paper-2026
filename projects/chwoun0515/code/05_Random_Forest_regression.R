# 05_Random_Forest_regression.R -------------------------------------------
# 목적: 연속형 매출 변화와 순진입률을 연도별로 따로 예측합니다.
source("C:/Users/chwou/Desktop/graduate/04_analysis/workspace/ml-paper-2026/projects/chwoun0515/code/01_package.R")

metric_rows <- list()
importance_rows <- list()
prediction_rows <- list()

for (outcome in c("sales_yoy_log", "net_entry_rate")) {
  chosen_x <- if (outcome == "sales_yoy_log") sales_x else entry_x
  features <- c(chosen_x, "organization_2020")
  rf_data <- analysis_data |>
    dplyr::mutate(organization_2020 = dplyr::case_when(
      organization_presence_2020_0_1 == 1 ~ "yes",
      organization_presence_2020_0_1 == 0 ~ "no",
      TRUE ~ "unknown"),
      organization_2020 = factor(organization_2020,
        levels = c("no", "yes", "unknown"))) |>
    dplyr::filter(year >= 2022, !is.na(.data[[outcome]]))

  for (this_year in 2022:2025) {
    year_data <- rf_data |> dplyr::filter(year == this_year)
    train <- year_data |> dplyr::filter(quarter <= 2)
    valid <- year_data |> dplyr::filter(quarter == 3)
    test  <- year_data |> dplyr::filter(quarter == 4)
    if (nrow(train) < 100 || nrow(test) < 30) stop(this_year, "년의 표본 부족")

    numeric_features <- setdiff(features, "organization_2020")
    for (v in numeric_features) {
      fill <- median(train[[v]], na.rm = TRUE)
      if (!is.finite(fill)) stop(v, "의 학습자료가 모두 결측입니다.")
      train[[v]][is.na(train[[v]])] <- fill
      valid[[v]][is.na(valid[[v]])] <- fill
      test[[v]][is.na(test[[v]])] <- fill
    }
    form <- stats::reformulate(features, response = outcome)
    fit <- ranger::ranger(form, data = train,
                          num.trees = 300,
                          mtry = max(1, floor(sqrt(length(features)))),
                          min.node.size = 5, seed = 2026 + this_year)
    predict_value <- function(new_data) {
      as.numeric(predict(fit, data = new_data)$predictions)
    }
    train_mean <- mean(train[[outcome]])
    for (part in c("validation", "test")) {
      current <- if (part == "validation") valid else test
      if (nrow(current) == 0) next
      predicted <- predict_value(current)
      truth <- current[[outcome]]
      mae <- mean(abs(truth - predicted))
      metric_rows[[length(metric_rows) + 1]] <- data.frame(
        outcome = outcome, year = this_year, split = part, n = nrow(current),
        mae = mae, rmse = sqrt(mean((truth - predicted)^2)),
        mean_baseline_mae = mean(abs(truth - train_mean)))
      if (part == "test") {
        prediction_rows[[length(prediction_rows) + 1]] <- data.frame(
          outcome = outcome, year = this_year, code = current$code,
          period = current$period, truth = truth, prediction = predicted)
        # 시험자료의 변수를 하나씩 섞어 MAE 증가량을 측정합니다.
        for (v in features) {
          increase <- numeric(5)
          for (rep in 1:5) {
            changed <- current
            set.seed(2000 + this_year + rep)
            changed[[v]] <- sample(changed[[v]])
            increase[rep] <- mean(abs(truth - predict_value(changed))) - mae
          }
          importance_rows[[length(importance_rows) + 1]] <- data.frame(
            outcome = outcome, year = this_year,
            variable = v, mae_increase = mean(increase))
        }
      }
    }
  }
}

metrics <- dplyr::bind_rows(metric_rows)
importance <- dplyr::bind_rows(importance_rows)
predictions <- dplyr::bind_rows(prediction_rows)
print(metrics)
readr::write_csv(metrics, file.path(result_dir, "05_regression_metrics.csv"))
readr::write_csv(importance, file.path(result_dir, "05_regression_importance.csv"))
readr::write_csv(predictions, file.path(result_dir, "05_regression_predictions.csv"))
