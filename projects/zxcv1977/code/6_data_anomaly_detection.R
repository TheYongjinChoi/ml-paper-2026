# ================================================================
# 2단계: 서울사랑상품권 소비구조 이상탐지 및 상대적 저이용 탐색
# 1단계 결과 CSV를 재사용하므로 1,188만 건 원자료 재로딩 불필요
# 관측단위: 서울 25개 자치구 / 업종: 표준산업명 상위 10개 + 기타
# ================================================================

# 0. 작업 경로: 본인 PC 환경에 맞게 수정하세요.
base_dir <- "C:/Users/jw_5202/Desktop/ml-paper-2026/projects/zxcv1977"
input_dir <- file.path(base_dir, "output", "2. clustering (2)")
out_dir <- file.path(base_dir, "output", "3. anomaly_detection")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

packages <- c("data.table", "ggplot2", "isotree", "dbscan")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) install.packages(missing_packages)
library(data.table)
library(ggplot2)

# 1. 1단계 산출물 읽기
required_files <- c("02_industry_shares_wide.csv", "02_industry_shares_long.csv",
                    "01_district_scale.csv", "04_clusters_B_structure_scale.csv")
paths <- file.path(input_dir, required_files)
if (any(!file.exists(paths))) {
  stop("다음 입력 파일이 없습니다: ", paste(required_files[!file.exists(paths)], collapse = ", "),
       "\ninput_dir 경로를 확인하세요: ", input_dir)
}
shares <- fread(paths[1], encoding = "UTF-8")
shares_long <- fread(paths[2], encoding = "UTF-8")
scale_dt <- fread(paths[3], encoding = "UTF-8")
clusters <- fread(paths[4], encoding = "UTF-8")
stopifnot(nrow(shares) == 25L, uniqueN(shares$district) == 25L,
          uniqueN(scale_dt$district) == 25L,
          uniqueN(clusters$district) == 25L)
share_cols <- setdiff(names(shares), "district")
stopifnot(all(vapply(shares[, ..share_cols], is.numeric, logical(1))))
stopifnot(all(abs(rowSums(as.matrix(shares[, ..share_cols])) - 1) < 1e-6))

# 2. 두 분석행렬: A 소비구조만, B 소비구조+소비규모
merged <- merge(shares, scale_dt, by = "district", sort = TRUE)
A <- as.matrix(merged[, ..share_cols])
rownames(A) <- merged$district
A <- A[, apply(A, 2, sd) > 0, drop = FALSE]
A_z <- scale(A)
S <- as.matrix(merged[, .(total_amount = log1p(total_amount),
                           transaction_count = log1p(transaction_count),
                           mean_amount = log1p(mean_amount))])
S <- S[, apply(S, 2, sd) > 0, drop = FALSE]
S_z <- scale(S)
B <- cbind(A_z / sqrt(ncol(A_z)), S_z / sqrt(ncol(S_z)))
rownames(B) <- merged$district

# 3. Isolation Forest와 LOF (25개 구의 탐색적 이상치 점수)
# IF 점수: 높을수록 이상, LOF: 1보다 클수록 주변에 비해 이례적
# 점수의 크기를 두 알고리즘 사이에서 직접 비교하지 않습니다.
score_anomaly <- function(mat, model_name) {
  set.seed(20261121)
  iforest <- isotree::isolation.forest(
    as.data.frame(mat), ntrees = 500L, sample_size = nrow(mat),
    ndim = 1L, nthreads = 1L
  )
  if_score <- as.numeric(predict(iforest, as.data.frame(mat), type = "score"))
  lof_score <- as.numeric(dbscan::lof(mat, minPts = 5L))
  data.table(district = rownames(mat), model = model_name,
             iforest_score = if_score, lof_score = lof_score,
             iforest_rank = frank(-if_score, ties.method = "min"),
             lof_rank = frank(-lof_score, ties.method = "min"))
}
score_dt <- rbindlist(list(score_anomaly(A_z, "A_structure"),
                           score_anomaly(B, "B_structure_scale")))
score_dt <- merge(score_dt,
                  clusters[, .(district, kmeans_cluster)],
                  by = "district", all.x = TRUE)
setorder(score_dt, model, iforest_rank)
fwrite(score_dt, file.path(out_dir, "01_anomaly_scores.csv"), bom = TRUE)

# 4. 점수 상위 20%는 '추가 검토 대상' 표시 (정책 사각지대 아님)
# 작은 n=25에서 상위 5개 표시는 순위상 편의를 위한 기준입니다.
score_dt[, iforest_review := iforest_rank <= 5L]
score_dt[, lof_review := lof_rank <= 5L]
score_dt[, both_review := iforest_review & lof_review]
fwrite(score_dt, file.path(out_dir, "02_anomaly_review.csv"), bom = TRUE)

# 5. 자치구 × 업종 상대적 저이용: 각 업종의 자치구별 구성비 비교
# 서울시 전체 업종의 결제금액 순위에 근거한 '기타'는 개별 업종이 아니므로 제외
industry_cols <- setdiff(share_cols, "기타")
low_long <- melt(shares, id.vars = "district", measure.vars = industry_cols,
                 variable.name = "industry_group", value.name = "share")
low_long[, `:=`(
  median_share = median(share),
  q25_share = as.numeric(quantile(share, probs = .25, type = 7)),
  mean_share = mean(share)
), by = industry_group]
low_long[, `:=`(
  gap_pp = 100 * (share - median_share),
  share_pct = 100 * share,
  median_pct = 100 * median_share,
  low_quartile = share < q25_share,
  relative_ratio = fifelse(median_share > 0, share / median_share, NA_real_)
)]
# 소액/희소 업종에 대한 과도한 해석 방지: 업종의 자치구 중앙값이 2% 이상일 때만 검토
low_long[, low_review := low_quartile & median_share >= 0.02]
setorder(low_long, industry_group, share)
fwrite(low_long, file.path(out_dir, "03_district_industry_low_share.csv"), bom = TRUE)

# 6. 자치구별 검토 업종 수와 1단계 군집 정보
low_summary <- low_long[, .(
  low_review_n = sum(low_review),
  low_review_industries = paste(industry_group[low_review], collapse = "; "),
  max_negative_gap_pp = min(gap_pp)
), by = district]
low_summary <- merge(low_summary, clusters[, .(district, kmeans_cluster)], by = "district")
setorder(low_summary, -low_review_n, district)
fwrite(low_summary, file.path(out_dir, "04_district_low_share_summary.csv"), bom = TRUE)

# 7. 시각화: 이상점수(모형 A/B), 업종별 상대적 저이용
p1 <- ggplot(score_dt, aes(x = reorder(district, iforest_score), y = iforest_score,
                           fill = factor(kmeans_cluster))) +
  geom_col() + coord_flip() + facet_wrap(~ model, scales = "free_y") +
  labs(title = "자치구별 Isolation Forest 이상점수", x = "자치구",
       y = "이상점수 (높을수록 이례적)", fill = "1단계 군집") +
  theme_minimal(base_size = 11)
ggsave(file.path(out_dir, "05_iforest_scores.png"), p1,
       width = 12, height = 9, dpi = 160)

p2 <- ggplot(score_dt, aes(x = reorder(district, lof_score), y = lof_score,
                           fill = factor(kmeans_cluster))) +
  geom_col() + coord_flip() + facet_wrap(~ model, scales = "free_y") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  labs(title = "자치구별 LOF 점수 (minPts=5)", x = "자치구",
       y = "LOF 점수", fill = "1단계 군집") + theme_minimal(base_size = 11)
ggsave(file.path(out_dir, "06_lof_scores.png"), p2,
       width = 12, height = 9, dpi = 160)

p3 <- ggplot(low_long, aes(x = industry_group, y = district, fill = gap_pp)) +
  geom_tile(color = "white", linewidth = .15) +
  scale_fill_gradient2(low = "#3266B0", mid = "white", high = "#CB574D",
                       midpoint = 0, name = "중앙값 대비\n차이(%p)") +
  labs(title = "자치구별 업종 결제금액 비중의 상대적 차이",
       subtitle = "파란색: 해당 업종의 자치구 중앙값보다 낮음 (정책 사각지대 판정 아님)",
       x = "업종", y = "자치구") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
ggsave(file.path(out_dir, "07_industry_low_share_heatmap.png"), p3,
       width = 14, height = 10, dpi = 160)

# 8. 검증: 이 단계에서는 인구/가맹점 수 보정 자료가 없으므로
# 실제 정책적 저이용, 이용 불평등, 사각지대라고 확정하지 않습니다.
cat("\n[2단계 완료] 저장 경로: ", normalizePath(out_dir), "\n", sep = "")
cat("분석 자치구: ", nrow(shares), "개 / 업종 구성비: ", length(share_cols), "개\n", sep = "")
cat("\n[모형 B 이상점수 상위 5개]\n")
print(score_dt[model == "B_structure_scale"][order(iforest_rank)][1:5,
  .(district, kmeans_cluster, iforest_score, lof_score, iforest_rank, lof_rank)])
cat("\n[상대적 저이용 검토 건수]\n")
print(low_long[, .(검토대상_자치구업종쌍 = sum(low_review))])
cat("주의: 이상탐지와 업종 구성비 저하만으로 정책 사각지대를 판정할 수 없습니다.\n")

