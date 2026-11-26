# ================================================================
# 3단계: 서울사랑상품권 개별 결제금액 예측
# Random Forest (ranger), LightGBM, SHAP
# 분석단위: 양수 결제 1건 / 구매자료와 결제자료 결합하지 않음
# ================================================================

# 최초 1회 설치 (LightGBM은 설치 환경에 따라 별도 설치가 필요할 수 있음)
# install.packages(c('data.table','ranger','ggplot2','fastshap'))
# LightGBM 설치: https://lightgbm.readthedocs.io/en/latest/R/index.html

required <- c('data.table', 'ranger', 'ggplot2')
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)) stop('다음 패키지를 먼저 설치하세요: ', paste(missing, collapse=', '))
library(data.table)
library(ggplot2)


setwd("C:/Users/jw_5202/Desktop/ml-paper-2026/projects/zxcv1977/data")

set.seed(20261122)
INPUT <- '2025_결제_서울25개자치구_분석용.rds'
OUT <- file.path('output', '3_prediction')
dir.create(OUT, recursive=TRUE, showWarnings=FALSE)

# 16GB RAM 기준 보수적 학습 표본: 월별 균형을 유지하는 표본
TRAIN_MAX <- 150000L
TEST_MAX <- 40000L
SHAP_N <- 200L

if (!file.exists(INPUT)) stop('입력 파일이 없습니다: ', INPUT)
dt <- readRDS(INPUT)
setDT(dt)

# 실제 변수명과 후보 변수명 자동 대조
pick <- function(candidates, required=TRUE) {
  hit <- candidates[candidates %in% names(dt)]
  if (!length(hit)) {
    if (required) stop('필수 변수를 찾을 수 없습니다. 후보: ', paste(candidates, collapse=', '),
                       '\n현재 변수명: ', paste(names(dt), collapse=', '))
    return(NA_character_)
  }
  hit[1L]
}
amount_col <- pick(c('결제금액'))
date_col <- "결제일자"
district_col <- "구"
industry_col <- pick(c('표준산업명','서울페이코드명'))
sex_col <- pick(c('결제자성별','성별'), FALSE)
age_col <- pick(c('결제자연령대','연령대','결제자나이'), FALSE)

message('선택된 변수: ', paste(c(amount_col,date_col,district_col,industry_col,sex_col,age_col), collapse=' / '))

# 필요한 열만 복사하고 큰 원본 메모리 해제
keep <- unique(na.omit(c(amount_col,date_col,district_col,industry_col,sex_col,age_col)))
x <- copy(dt[, ..keep])
rm(dt); invisible(gc())
setnames(x, c(amount_col,date_col,district_col,industry_col),
         c('amount','date_raw','district','industry'))
if (!is.na(sex_col)) setnames(x, sex_col, 'sex') else x[, sex := '정보없음']
if (!is.na(age_col)) setnames(x, age_col, 'age') else x[, age := '정보없음']

x[, amount := suppressWarnings(as.numeric(as.character(amount)))]
x <- x[is.finite(amount) & amount > 0]

# POSIXct / Date / YYYYMMDD / YYYY-MM-DD HH:MM:SS 형태 지원
parse_date <- function(v) {
  if (inherits(v, 'POSIXt') || inherits(v, 'Date')) return(as.Date(v))
  z <- as.character(v)
  z <- trimws(z)
  # YYYYMMDDHHMMSS / YYYYMMDD / YYYY-MM-DD ...
  numeric_date <- grepl('^[0-9]{8}', z)
  z[numeric_date] <- paste0(substr(z[numeric_date],1,4), '-',
                             substr(z[numeric_date],5,6), '-',
                             substr(z[numeric_date],7,8))
  as.Date(substr(z,1,10), format='%Y-%m-%d')
}

# 결제일시 자료형 확인
class(x$date_raw)

# 실제 데이터 10개 확인
head(x$date_raw, 10)

# 문자 형태로 확인
head(as.character(x$date_raw), 10)

# 결측값 개수 확인
sum(is.na(x$date_raw))

x[, date := parse_date(date_raw)]
x <- x[!is.na(date)]
x[, month := as.integer(format(date, '%m'))]
x[, weekday := as.integer(format(date, '%u'))] # 월=1, 일=7
x[, log_amount := log1p(amount)]
x[, c('date_raw') := NULL]

for (v in c('district','industry','sex','age')) {
  set(x, j=v, value=as.character(x[[v]]))
  x[is.na(get(v)) | trimws(get(v))=='', (v) := '정보없음']
}

# 시간순 홀드아웃: 1~9월 학습, 10~12월 테스트
train_pool <- x[month <= 9L]
test_pool <- x[month >= 10L]
if (!nrow(train_pool) || !nrow(test_pool)) stop('학습/검증 기간을 나눌 수 없습니다. 날짜를 확인하세요.')

# 월별 비례 층화 무작위 표본 추출(메모리 및 학습시간 절감)
sample_by_month <- function(d, max_n) {
  if (nrow(d) <= max_n) return(copy(d))
  d[, .SD[sample.int(.N, size=max(1L, floor(.N/nrow(d)*max_n)))], by=month]
}
train <- sample_by_month(train_pool, TRAIN_MAX)
test <- sample_by_month(test_pool, TEST_MAX)
rm(x,train_pool,test_pool); invisible(gc())

# 학습자료에서만 업종 상위 30개 결정 (검증 데이터 누출 방지)
top_industry <- train[, .(n=.N), by=industry][order(-n)]$industry[1:min(30L,uniqueN(train$industry))]
train[!industry %in% top_industry, industry := '기타업종']
test[!industry %in% top_industry, industry := '기타업종']

features <- c('district','industry','sex','age','month','weekday')
for (v in c('district','industry','sex','age')) {
  lv <- unique(c(train[[v]], '정보없음'))
  train[, (v) := factor(get(v), levels=lv)]
  test[, (v) := factor(ifelse(get(v) %in% lv, get(v), '정보없음'), levels=lv)]
}

fwrite(data.table(구분=c('학습(1~9월)','검증(10~12월)'),
                  표본수=c(nrow(train),nrow(test))),
       file.path(OUT,'01_sample_sizes.csv'), bom=TRUE)

# ---------------- Random Forest ----------------
rf_formula <- as.formula(paste('log_amount ~',paste(features,collapse=' + ')))
rf <- ranger::ranger(rf_formula, data=as.data.frame(train[,c('log_amount',features),with=FALSE]),
                     num.trees=300, mtry=3, min.node.size=20,
                     importance='permutation', num.threads=2, seed=20261122)
rf_pred <- predict(rf, data=as.data.frame(test[,..features]))$predictions

# ---------------- LightGBM ----------------
lgb_pred <- NULL
lgb <- NULL

if (requireNamespace("lightgbm", quietly = TRUE)) {
  
  # 1. 원-핫 인코딩 함수
  make_matrix <- function(d, template = NULL) {
    
    z <- as.data.frame(d[, ..features])
    
    m <- model.matrix(~ . - 1, data = z)
    
    if (!is.null(template)) {
      
      out <- matrix(
        0,
        nrow = nrow(m),
        ncol = length(template),
        dimnames = list(NULL, template)
      )
      
      common <- intersect(colnames(m), template)
      
      out[, common] <- m[, common, drop = FALSE]
      
      return(out)
    }
    
    m
  }
  
  # 2. 학습·테스트 행렬 생성
  X_train <- make_matrix(train)
  
  X_test <- make_matrix(
    test,
    template = colnames(X_train)
  )
  
  # 3. 기존 변수명 보관
  original_names <- colnames(X_train)
  
  # 4. LightGBM에서 사용 가능한 변수명 생성
  safe_names <- paste0(
    "feature_",
    seq_along(original_names)
  )
  
  # 5. 학습·테스트 변수명 동일하게 변경
  colnames(X_train) <- safe_names
  colnames(X_test) <- safe_names
  
  # 6. 변수명 대응표 저장
  feature_mapping <- data.table(
    LightGBM변수명 = safe_names,
    원래변수명 = original_names
  )
  
  fwrite(
    feature_mapping,
    file.path(OUT, "04_lightgbm_feature_mapping.csv"),
    bom = TRUE
  )
  
  # 7. LightGBM 데이터 생성
  dtrain <- lightgbm::lgb.Dataset(
    data = X_train,
    label = train$log_amount
  )
  
  # 8. LightGBM 학습
  lgb <- lightgbm::lgb.train(
    params = list(
      objective = "regression",
      metric = "rmse",
      learning_rate = 0.05,
      num_leaves = 15L,
      min_data_in_leaf = 100L,
      feature_fraction = 0.9,
      bagging_fraction = 0.8,
      bagging_freq = 1L,
      num_threads = 2L,
      verbosity = -1L
    ),
    data = dtrain,
    nrounds = 200L,
    verbose = -1L
  )
  
  # 9. 테스트자료 예측
  lgb_pred <- predict(lgb, X_test)
  
  # 10. 변수 중요도 추출
  imp <- as.data.table(
    lightgbm::lgb.importance(lgb)
  )
  
  # 11. 원래 변수명 복원
  imp <- merge(
    imp,
    feature_mapping,
    by.x = "Feature",
    by.y = "LightGBM변수명",
    all.x = TRUE,
    sort = FALSE
  )
  
  # 12. 중요도 높은 순으로 정렬
  setorder(imp, -Gain)
  
  # 13. 변수 중요도 저장
  fwrite(
    imp,
    file.path(OUT, "04_lightgbm_importance.csv"),
    bom = TRUE
  )
  
  # 14. 결과 확인
  print(head(imp, 20))
  
  # 15. 메모리 정리
  rm(X_train, X_test, dtrain)
  invisible(gc())
  
} else {
  
  message(
    "lightgbm 패키지가 설치되어 있지 않습니다."
  )
  
}

# ---------------- 평가 ----------------
# 원 단위 성능 평가. expm1 예측치가 음수이면 0으로 절단
actual <- test$amount
metrics <- function(name,pred_log) {
  pred <- pmax(0,expm1(pred_log))
  data.table(모형=name, MAE=mean(abs(actual-pred)),
             RMSE=sqrt(mean((actual-pred)^2)),
             R2=1-sum((actual-pred)^2)/sum((actual-mean(actual))^2))
}
res <- metrics('Random Forest',rf_pred)
if (!is.null(lgb_pred)) res <- rbind(res,metrics('LightGBM',lgb_pred))
print(res)
fwrite(res,file.path(OUT,'02_model_performance.csv'),bom=TRUE)

rf_imp <- data.table(변수=names(rf$variable.importance), 중요도=as.numeric(rf$variable.importance))
setorder(rf_imp,-중요도)
fwrite(rf_imp,file.path(OUT,'03_random_forest_importance.csv'),bom=TRUE)

# ---------------- SHAP (RF, 소규모 표본) ----------------
# 패키지 설치 시에만 실행. 학습자료를 배경자료로 사용하고 SHAP 표본을 제한합니다.
if (requireNamespace('fastshap',quietly=TRUE)) {
  n_shap <- min(SHAP_N,nrow(test))
  shap_x <- as.data.frame(test[sample.int(nrow(test),n_shap), ..features])
  background <- as.data.frame(train[sample.int(nrow(train),min(1000L,nrow(train))), ..features])
  pred_fun <- function(object,newdata) {
    as.numeric(predict(object,data=newdata)$predictions)
  }
  shap <- fastshap::explain(rf,X=background,pred_wrapper=pred_fun,
                            newdata=shap_x,nsim=30,adjust=TRUE)
  shap_imp <- data.table(변수=colnames(shap),평균절대SHAP=colMeans(abs(shap)))
  setorder(shap_imp,-평균절대SHAP)
  fwrite(shap_imp,file.path(OUT,'05_shap_importance_RF.csv'),bom=TRUE)
  p <- ggplot(shap_imp,aes(x=reorder(변수,평균절대SHAP),y=평균절대SHAP))+
    geom_col()+coord_flip()+labs(x=NULL,y='평균 |SHAP| (log1p 결제금액)',
                                title='Random Forest SHAP 변수 중요도')+theme_minimal()
  ggsave(file.path(OUT,'06_shap_importance_RF.png'),p,width=8,height=5,dpi=150)
} else {
  message('fastshap 패키지가 없어 SHAP 계산을 건너뜁니다. install.packages("fastshap")')
}

saveRDS(list(model=rf,features=features,top_industry=top_industry),
        file.path(OUT,'07_rf_model.rds'))
if (!is.null(lgb)) lightgbm::lgb.save(lgb,file.path(OUT,'08_lightgbm_model.txt'))
cat('\n분석 완료. 결과 폴더: ',normalizePath(OUT),'\n',sep='')
