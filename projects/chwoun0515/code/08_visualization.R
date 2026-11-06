# 08_visualization.R ------------------------------------------------------
# 목적: 분기별 경제 변화와 연도별 지원 상태를 그림으로 확인합니다.
source("C:/Users/chwou/Desktop/graduate/04_analysis/workspace/ml-paper-2026/projects/chwoun0515/code/01_package.R")

plot_theme <- ggplot2::theme_minimal(base_size = 12, base_family = "Malgun Gothic")

trend <- analysis_data |>
  dplyr::group_by(year, quarter) |>
  dplyr::summarise(
    sales_median = median(sales_yoy_log, na.rm = TRUE),
    entry_median = median(net_entry_rate, na.rm = TRUE),
    .groups = "drop") |>
  dplyr::mutate(time = factor(paste0(year, " Q", quarter),
                              levels = paste0(rep(2021:2025, each = 4),
                                              " Q", rep(1:4, times = 5))))

p_sales <- ggplot2::ggplot(dplyr::filter(trend, !is.na(sales_median)),
                           ggplot2::aes(x = time, y = sales_median, group = 1)) +
  ggplot2::geom_hline(yintercept = 0, color = "grey70") +
  ggplot2::geom_line(color = "#2A6F97") + ggplot2::geom_point(color = "#2A6F97") +
  ggplot2::labs(title = "Median year-on-year sales change",
                x = NULL, y = "Log difference x 100") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)) + plot_theme
ggplot2::ggsave(file.path(result_dir, "08_sales_trend.png"), p_sales,
                width = 10, height = 5, dpi = 300, device = ragg::agg_png)

p_entry <- ggplot2::ggplot(dplyr::filter(trend, !is.na(entry_median)),
                           ggplot2::aes(x = time, y = entry_median, group = 1)) +
  ggplot2::geom_hline(yintercept = 0, color = "grey70") +
  ggplot2::geom_line(color = "#BC6C25") + ggplot2::geom_point(color = "#BC6C25") +
  ggplot2::labs(title = "Median net store entry rate", x = NULL, y = "Percent") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)) + plot_theme
ggplot2::ggsave(file.path(result_dir, "08_entry_trend.png"), p_entry,
                width = 10, height = 5, dpi = 300, device = ragg::agg_png)

support_by_year <- analysis_data |>
  dplyr::filter(quarter == 1) |>
  dplyr::count(year, support_stage_observed, name = "markets")
p_support <- ggplot2::ggplot(support_by_year,
    ggplot2::aes(x = factor(year), y = markets, fill = support_stage_observed)) +
  ggplot2::geom_col() +
  ggplot2::labs(title = "Recorded support status by year", x = "Year", y = "Markets",
                fill = "Status") + plot_theme
ggplot2::ggsave(file.path(result_dir, "08_support_by_year.png"), p_support,
                width = 8, height = 5, dpi = 300, device = ragg::agg_png)

# RF 분석을 실행한 뒤에는 변수 중요도도 그립니다.
importance_file <- file.path(result_dir, "05_regression_importance.csv")
if (file.exists(importance_file)) {
  importance <- readr::read_csv(importance_file, show_col_types = FALSE)
  p_importance <- ggplot2::ggplot(importance,
      ggplot2::aes(x = variable, y = mae_increase)) +
    ggplot2::geom_col(fill = "#52796F") +
    ggplot2::facet_grid(outcome ~ year, scales = "free_y") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Permutation importance on held-out quarters", x = NULL,
                  y = "Increase in MAE") + plot_theme
  ggplot2::ggsave(file.path(result_dir, "08_rf_importance.png"), p_importance,
                  width = 12, height = 7, dpi = 300, device = ragg::agg_png)
}

cat("그림이 저장된 위치:", result_dir, "\n")
