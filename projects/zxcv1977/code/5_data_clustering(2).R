# 서울사랑상품권 자치구별 소비패턴 군집분석
# 입력: 서울25개자치구_분석용.rds (지역 정제 완료, 음수 거래 포함 가능)
# 출력: 지정한 outdir 아래 표본선정표, 군집 CSV 및 PNG
# 구매 자료는 사용하지 않습니다.

packages <- c('data.table','ggplot2','cluster','mclust')
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly=TRUE)]
if(length(missing_packages)) install.packages(missing_packages)
library(data.table)
library(ggplot2)

setwd("C:/Users/jw_5202/Desktop/ml-paper-2026/projects/zxcv1977/data")

# 확정된 서울 25개 자치구 분석용 RDS를 명시적으로 로드합니다.
# 작업 폴더에 파일이 없다면 아래 경로를 절대경로로 수정하세요.
clean_file <- '2025_결제_서울25개자치구_분석용.rds'
if (!file.exists(clean_file)) stop('분석용 RDS 파일을 찾지 못했습니다: ', normalizePath(clean_file, mustWork=FALSE))
dt <- as.data.table(readRDS(clean_file))
cat('\n[분석 파일] ', normalizePath(clean_file), '\n', sep='')
cat('[변수명]\n'); print(names(dt))
cat('[자료 크기]\n'); print(dim(dt))

choose_col <- function(candidates, label) {
  found <- candidates[candidates %in% names(dt)]
  if (!length(found)) stop(label, ' 변수가 없습니다. 후보: ', paste(candidates, collapse=', '))
  found[1]
}
col_district <- choose_col(c('구','자치구','가맹점자치구','결제자치구'), '자치구')
col_industry <- choose_col(c('표준산업명','서울페이코드명'), '업종')
col_amount <- choose_col(c('결제금액'), '결제금액')

seoul25 <- c('종로구','중구','용산구','성동구','광진구','동대문구','중랑구','성북구','강북구','도봉구',
             '노원구','은평구','서대문구','마포구','양천구','강서구','구로구','금천구','영등포구',
             '동작구','관악구','서초구','강남구','송파구','강동구')

outdir <- 'C:/Users/jw_5202/Desktop/ml-paper-2026/projects/zxcv1977/output/2. clustering (2)'
dir.create(outdir, showWarnings=FALSE, recursive=TRUE)

# 원본 RDS 객체를 변경하지 않고 필요한 열만 복사합니다.
x <- dt[, .(district=trimws(as.character(get(col_district))),
            industry=trimws(as.character(get(col_industry))),
            amount_raw=get(col_amount))]
x[, amount := suppressWarnings(as.numeric(gsub(',', '', trimws(as.character(amount_raw)), fixed=TRUE)))]
quality <- data.table(
  metric=c('분석용 파일 행수','자치구 결측','서울25구 외 행수','업종 결측','금액 결측/변환 실패','음수 금액','0원 결제'),
  value=c(nrow(x), sum(is.na(x$district)|x$district==''),
          sum(!is.na(x$district) & !(x$district %in% seoul25)),
          sum(is.na(x$industry)|x$industry==''), sum(is.na(x$amount)),
          sum(x$amount<0, na.rm=TRUE), sum(x$amount==0, na.rm=TRUE))
)
cat('\n[클린 파일 검증]\n'); print(quality)
fwrite(quality, file.path(outdir,'00_quality_check.csv'), bom=TRUE)

# 서울 25개 구 자료의 음수 거래는 예상된 상태입니다.
# 원본을 보존하고 양수 결제만 군집분석에 사용합니다.
# 취소·환불 여부는 거래 코드북에서 별도 확인해야 합니다.
if (quality[metric == '자치구 결측', value] > 0 ||
    quality[metric == '서울25구 외 행수', value] > 0) {
  stop('서울 25개 구 분석용 자료에 지역 결측/비서울 거래가 있습니다. 입력 파일을 확인하세요.')
}
if (quality[metric == '업종 결측', value] > 0 ||
    quality[metric == '금액 결측/변환 실패', value] > 0) {
  stop('업종 또는 금액 결측이 있습니다. 처리 원칙을 먼저 확인하세요.')
}

n_before <- nrow(x)
x <- x[!is.na(amount) & amount > 0, .(district, industry, amount)]
if (uniqueN(x$district) != 25L) {
  stop('최종 분석에서 서울 25개 구가 모두 포함되지 않았습니다: ',
       paste(setdiff(seoul25, unique(x$district)), collapse = ', '))
}

sample_flow <- data.table(
  항목 = c('서울25구 분석용 거래', '음수 거래 제외', '0원 거래 제외', '최종 양수 결제 거래'),
  건수 = c(n_before, quality[metric == '음수 금액', value],
         quality[metric == '0원 결제', value], nrow(x))
)
cat('\n[최종 분석 표본]\n')
print(sample_flow)
fwrite(sample_flow, file.path(outdir, '00_sample_flow.csv'), bom = TRUE)
cat('분석 자치구 수:', uniqueN(x$district), '\n')

# 원자료 27개 변수를 보존한 양수 거래 파일을 별도로 저장하고 싶으면 TRUE로 변경.
# 대용량 파일이므로 저장 공간과 메모리를 고려해 기본값은 FALSE입니다.
save_final_rds <- FALSE
if (save_final_rds) {
  amount_all <- suppressWarnings(as.numeric(gsub(',', '',
                                                 trimws(as.character(dt[[col_amount]])), fixed = TRUE)))
  dt_final <- dt[!is.na(amount_all) & amount_all > 0]
  final_path <- file.path(dirname(normalizePath(clean_file)),
                          '2025_결제_서울25개자치구_최종클린.rds')
  saveRDS(dt_final, final_path, compress = FALSE)
  cat('최종 클린 RDS 저장:', final_path, '\n')
  rm(dt_final, amount_all)
  gc()
}

# 1. 자치구별 소비 규모
scale_dt <- x[, .(total_amount=sum(amount), transaction_count=.N,
                  mean_amount=mean(amount)),by=district]
setorder(scale_dt,district)
fwrite(scale_dt,file.path(outdir,'01_district_scale.csv'),bom=TRUE)

# 2. 업종별 구성비. 업종이 지나치게 세분화되면 희소성이 커지므로 상위 10개 + 기타
industry_totals <- x[,.(total=sum(amount)),by=industry][order(-total)]
top_n <- min(10L,nrow(industry_totals))
top_industries <- industry_totals$industry[seq_len(top_n)]
x[, industry_group := fifelse(industry %in% top_industries,industry,'기타')]
structure_long <- x[,.(amount=sum(amount)),by=.(district,industry_group)]
structure_long[, share := amount/sum(amount),by=district]
structure_wide <- dcast(structure_long,district~industry_group,value.var='share',fill=0)
fwrite(structure_long,file.path(outdir,'02_industry_shares_long.csv'),bom=TRUE)
fwrite(structure_wide,file.path(outdir,'02_industry_shares_wide.csv'),bom=TRUE)

# 3. 두 모형의 변수행렬
merged <- merge(structure_wide,scale_dt,by='district',sort=TRUE)
districts <- merged$district
share_cols <- setdiff(names(structure_wide),'district')
A <- as.matrix(merged[,..share_cols]); storage.mode(A) <- 'double'
rownames(A) <- districts
# 0 분산 업종은 군집에 기여하지 않음
A <- A[,apply(A,2,sd)>0,drop=FALSE]
if(ncol(A)<1) stop('군집분석 가능한 업종 구성비 변수가 없습니다.')
A_z <- scale(A)

scale_vars <- as.matrix(merged[,.(total_amount=log1p(total_amount),
                                  transaction_count=log1p(transaction_count),
                                  mean_amount=log1p(mean_amount))])
rownames(scale_vars) <- districts
scale_vars <- scale_vars[,apply(scale_vars,2,sd)>0,drop=FALSE]
if(!ncol(scale_vars)) stop('소비규모 변수에 변동이 없습니다.')
scale_z <- scale(scale_vars)
# 모형 B: 소비구조 블록과 소비규모 블록의 총 제곱가중치가 같도록 조정
B <- cbind(A_z/sqrt(ncol(A_z)),scale_z/sqrt(ncol(scale_z)))
rownames(B) <- districts

# 4. 군집 수 선정: 평균 실루엣, k=2~6 (표본 크기 고려)
# 실루엣 최고값을 기본 선택하되, 극소 군집(1개 자치구)이면 해석 시 유의
k_candidates <- 2:min(6L,nrow(B)-1L)
set.seed(20261111)
fit_kmeans <- function(mat,k) kmeans(mat,centers=k,nstart=100,iter.max=100)
choose_k <- function(mat,label) {
  d <- dist(mat)
  scores <- rbindlist(lapply(k_candidates,function(k){
    fit <- fit_kmeans(mat,k)
    sil <- cluster::silhouette(fit$cluster,d)
    data.table(model=label,k=k,mean_silhouette=mean(sil[,3]),
               min_cluster_size=min(tabulate(fit$cluster,nbins=k)),
               total_withinss=fit$tot.withinss)
  }))
  fwrite(scores,file.path(outdir,paste0('03_k_selection_',label,'.csv')),bom=TRUE)
  # 일단 평균 실루엣 최대를 선택; 군집 크기는 반드시 함께 검토
  best_k <- scores[which.max(mean_silhouette),k]
  list(scores=scores,k=best_k)
}
ka <- choose_k(A_z,'A_structure')
kb <- choose_k(B,'B_structure_scale')
cat('\n[모형 A 군집 수 평가]\n');print(ka$scores)
cat('\n[모형 B 군집 수 평가]\n');print(kb$scores)

# 5. K-means 및 계층적 군집(Ward.D2) 비교
run_models <- function(mat,k,label){
  set.seed(20261111)
  km <- fit_kmeans(mat,k)
  hc <- hclust(dist(mat),method='ward.D2')
  hc_labels <- cutree(hc,k=k)
  assignments <- data.table(district=rownames(mat),
                            kmeans_cluster=as.integer(km$cluster),
                            hierarchical_cluster=as.integer(hc_labels))
  fwrite(assignments,file.path(outdir,paste0('04_clusters_',label,'.csv')),bom=TRUE)
  ari <- mclust::adjustedRandIndex(km$cluster,hc_labels)
  cat(sprintf('%s: 선택 k=%d, K-means vs 계층적 군집 ARI=%.3f\n',label,k,ari))
  list(km=km,hc=hc,assignments=assignments,ari=ari)
}
ra <- run_models(A_z,ka$k,'A_structure')
rb <- run_models(B,kb$k,'B_structure_scale')

# 6. 안정성 검증: 업종/규모 특성변수 80% 반복 추출 (자치구 25개는 고정)
# 재표본별 군집 라벨은 ARI로 비교하므로 라벨 번호가 달라도 무관
# 단, 25개 자치구라는 작은 표본과 변수 상관구조를 감안해 보조 진단으로만 해석
feature_stability <- function(mat,k,reference,iterations=200L,seed=1234L){
  set.seed(seed)
  if(ncol(mat)<3) return(data.table(mean_ari=NA_real_,median_ari=NA_real_,q05_ari=NA_real_))
  aris <- replicate(iterations,{
    selected <- sample(seq_len(ncol(mat)),size=max(2L,ceiling(ncol(mat)*0.8)),replace=FALSE)
    fit <- fit_kmeans(mat[,selected,drop=FALSE],k)
    mclust::adjustedRandIndex(reference,fit$cluster)
  })
  data.table(mean_ari=mean(aris),median_ari=median(aris),q05_ari=unname(quantile(aris,.05)))
}
stab_a <- feature_stability(A_z,ka$k,ra$km$cluster)
stab_b <- feature_stability(B,kb$k,rb$km$cluster)
stability <- rbindlist(list(cbind(model='A_structure',stab_a),cbind(model='B_structure_scale',stab_b)))
print(stability)
fwrite(stability,file.path(outdir,'05_feature_stability.csv'),bom=TRUE)

# 7. 군집별 기술통계와 자치구 배정표
summarize_model <- function(result,label){
  d <- merge(merged,result$assignments[,.(district,cluster=kmeans_cluster)],by='district')
  profile <- d[,.(district_n=.N,
                  total_amount_mean=mean(total_amount),
                  transaction_count_mean=mean(transaction_count),
                  mean_amount_mean=mean(mean_amount)),by=cluster][order(cluster)]
  shares <- d[,lapply(.SD,mean),by=cluster,.SDcols=share_cols]
  profile <- merge(profile,shares,by='cluster')
  fwrite(profile,file.path(outdir,paste0('06_cluster_profile_',label,'.csv')),bom=TRUE)
  fwrite(d,file.path(outdir,paste0('06_district_profile_',label,'.csv')),bom=TRUE)
  print(profile[,.(cluster,district_n,total_amount_mean,transaction_count_mean,mean_amount_mean)])
  invisible(d)
}
da <- summarize_model(ra,'A_structure')
db <- summarize_model(rb,'B_structure_scale')

# 8. 그래프: 실루엣, PCA 2차원, 규모 비교, 업종 구성비
scores_all <- rbind(ka$scores,kb$scores)
p1 <- ggplot(scores_all,aes(x=k,y=mean_silhouette,color=model,group=model))+
  geom_line()+geom_point(size=2)+scale_x_continuous(breaks=k_candidates)+
  labs(title='군집 수에 따른 평균 실루엣 계수',x='군집 수(k)',y='평균 실루엣')+theme_minimal(base_size=12)
ggsave(file.path(outdir,'07_silhouette_k.png'),p1,width=8,height=5,dpi=160)

plot_pca <- function(mat,assignments,label){
  pca <- prcomp(mat,center=TRUE,scale.=FALSE)
  df <- data.table(district=rownames(mat),PC1=pca$x[,1],PC2=pca$x[,2])
  df <- merge(df,assignments[,.(district,cluster=factor(kmeans_cluster))],by='district')
  variance <- 100*pca$sdev^2/sum(pca$sdev^2)
  p <- ggplot(df,aes(PC1,PC2,color=cluster,label=district))+
    geom_point(size=3)+geom_text(vjust=-0.7,size=3,check_overlap=TRUE)+
    labs(title=paste('자치구 군집 PCA:',label),
         x=sprintf('PC1 (%.1f%%)',variance[1]),y=sprintf('PC2 (%.1f%%)',variance[2]),color='군집')+
    theme_minimal(base_size=12)
  ggsave(file.path(outdir,paste0('08_pca_',label,'.png')),p,width=10,height=7,dpi=160)
}
plot_pca(A_z,ra$assignments,'A_structure')
plot_pca(B,rb$assignments,'B_structure_scale')

p3 <- ggplot(db,aes(x=reorder(district,total_amount),y=total_amount/1e8,fill=factor(cluster)))+
  geom_col()+coord_flip()+labs(title='자치구별 총결제금액 및 모형 B 군집',
                               x='자치구',y='총결제금액 (억원)',fill='군집')+theme_minimal(base_size=11)
ggsave(file.path(outdir,'09_amount_by_district.png'),p3,width=9,height=9,dpi=160)

plot_share <- merge(structure_long,rb$assignments[,.(district,cluster=factor(kmeans_cluster))],by='district')
p4 <- ggplot(plot_share,aes(x=district,y=share,fill=industry_group))+
  geom_col()+facet_wrap(~cluster,scales='free_x')+coord_flip()+
  scale_y_continuous(labels=function(z) paste0(round(z*100),'%'))+
  labs(title='자치구별 업종 결제액 구성비 (모형 B)',x='자치구',y='구성비',fill='업종')+
  theme_minimal(base_size=10)
ggsave(file.path(outdir,'10_industry_share_by_cluster.png'),p4,width=13,height=10,dpi=160)

cat('\n분석 완료. 결과 폴더: ',normalizePath(outdir),'\n',sep='')
cat('해석 주의: 소비규모는 인구/가맹점 수로 표준화되지 않았으며, 군집은 사각지대 판정이 아닙니다.\n')

