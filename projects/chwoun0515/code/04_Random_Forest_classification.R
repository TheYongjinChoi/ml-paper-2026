# 04_Random_Forest_classification.R ---------------------------------------
# 목적: 매출 변화(0 이상/미만) × 순진입률(0 이상/미만)의 네 조합을 예측합니다.
# 이 네 유형은 경제지표의 방향을 기술할 뿐, 리질리언스의 확정적 등급이 아닙니다.
source("C:/Users/chwou/Desktop/graduate/04_analysis/workspace/ml-paper-2026/projects/chwoun0515/code/01_package.R")

features <- c(sales_x, "organization_2020")
rf_data <- analysis_data |>
  dplyr::mutate(
    organization_2020 = dplyr::case_when(
      organization_presence_2020_0_1 == 1 ~ "yes",
      organization_presence_2020_0_1 == 0 ~ "no",
      TRUE ~ "unknown"),
    organization_2020 = factor(organization_2020,
      levels = c("no", "yes", "unknown")),
    economic_pattern = factor(economic_pattern,
      levels = c("both_nonnegative", "sales_only", "entry_only", "both_negative"))
  ) |>
  dplyr::filter(year >= 2022, !is.na(economic_pattern))

metric_rows <- list()
importance_rows <- list()
confusion_rows <- list()

for (this_year in 2022:2025) {
  # 같은 연도의 앞선 분기로 학습하고, 4분기만 최종 평가합니다.
  year_data <- rf_data |> dplyr::filter(year == this_year)
  train <- year_data |> dplyr::filter(quarter <= 2)
  valid <- year_data |> dplyr::filter(quarter == 3)
  test  <- year_data |> dplyr::filter(quarter == 4)
  if (nrow(train) < 100 || nrow(test) < 30) stop(this_year, "년의 학습/평가 표본 부족")

  # 수치형 결측은 학습자료 중앙값으로만 채웁니다. 평가자료로 중앙값을 정하지 않습니다.
  numeric_features <- setdiff(features, "organization_2020")
  for (v in numeric_features) {
    fill <- median(train[[v]], na.rm = TRUE)
    if (!is.finite(fill)) stop(v, "의 학습자료가 모두 결측입니다.")
    train[[v]][is.na(train[[v]])] <- fill
    valid[[v]][is.na(valid[[v]])] <- fill
    test[[v]][is.na(test[[v]])] <- fill
  }
  form <- stats::reformulate(features, response = "economic_pattern")
  fit <- ranger::ranger(form, data = train,
                        num.trees = 300, mtry = max(1, floor(sqrt(length(features)))),
                        min.node.size = 5, seed = 2026 + this_year)
  predict_class <- function(new_data) {
    as.character(predict(fit, data = new_data)$predictions)
  }
  train_majority <- names(sort(table(train$economic_pattern), decreasing = TRUE))[1]
  for (part in c("validation", "test")) {
    current <- if (part == "validation") valid else test
    if (nrow(current) == 0) next
    predicted <- predict_class(current)
    truth <- as.character(current$economic_pattern)
    accuracy <- mean(predicted == truth)
    baseline <- mean(train_majority == truth)
    metric_rows[[length(metric_rows) + 1]] <- data.frame(
      year = this_year, split = part, n = nrow(current),
      accuracy = accuracy, majority_baseline = baseline)
    if (part == "test") {
      confusion_rows[[length(confusion_rows) + 1]] <- data.frame(
        year = this_year, truth = truth, predicted = predicted)
      # 평가자료에서 각 변수를 섞어 정확도가 얼마나 줄어드는지 확인합니다.
      for (v in features) {
        score_drop <- numeric(5)
        for (rep in 1:5) {
          changed <- current
          set.seed(1000 + this_year + rep)
          changed[[v]] <- sample(changed[[v]])
          score_drop[rep] <- accuracy - mean(predict_class(changed) == truth)
        }
        importance_rows[[length(importance_rows) + 1]] <- data.frame(
          year = this_year, variable = v, accuracy_drop = mean(score_drop))
      }
    }
  }
}

metrics <- dplyr::bind_rows(metric_rows)
importance <- dplyr::bind_rows(importance_rows)
confusion <- dplyr::bind_rows(confusion_rows)
print(metrics)
readr::write_csv(metrics, file.path(result_dir, "04_classification_metrics.csv"))
readr::write_csv(importance, file.path(result_dir, "04_classification_importance.csv"))
readr::write_csv(confusion, file.path(result_dir, "04_classification_predictions.csv"))

# 유형별 정밀도와 재현율을 별도로 보고해 다수 유형에만 잘 맞는지 확인합니다.
class_metrics <- confusion |>
  tidyr::expand_grid(class = levels(rf_data$economic_pattern)) |>
  dplyr::group_by(year, class) |>
  dplyr::summarise(
    precision = sum(truth == class & predicted == class) /
      max(sum(predicted == class), 1),
    recall = sum(truth == class & predicted == class) /
      max(sum(truth == class), 1),
    .groups = "drop")
readr::write_csv(class_metrics, file.path(result_dir, "04_classification_by_class.csv"))
print(class_metrics)
