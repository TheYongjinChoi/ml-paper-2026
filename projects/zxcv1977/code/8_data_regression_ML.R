rm(list = ls())

# 한용희·문경준(2022) 방법론 응용: 결제금액 관련 요인 및 예측
# UTF-8 / 결제자료만 사용 / 1행=양수 결제 1건
# 최초 설치: install.packages(c('data.table','Matrix','sandwich','ranger','xgboost','lightgbm','ggplot2'))
# CatBoost는 공식 R 설치 안내에 따라 별도 설치 (선택)
options(repos=c(CRAN='https://cloud.r-project.org'))
BASE <- 'C:/Users/jw_5202/Desktop/ml-paper-2026/projects/zxcv1977'
INPUT <- file.path(BASE,'data','2025_결제_서울25개자치구_분석용.rds')
OUT <- file.path(BASE,'output','regression_ml')
if (!file.exists(INPUT)) stop('입력 경로 확인: ', INPUT)
dir.create(OUT,recursive=TRUE,showWarnings=FALSE)
N <- 200000L; REPEATS <- 5L; THREADS <- 2L; TOP <- 30L; SEED <- 20261124L
pkgs <- c('data.table','Matrix','sandwich','ranger','xgboost','lightgbm','ggplot2')
missing <- pkgs[!vapply(pkgs,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) stop('먼저 설치: ',paste(missing,collapse=', '))
library(data.table); library(ggplot2)
setDTthreads(THREADS)
write <- function(d,f) fwrite(d,file.path(OUT,f),bom=TRUE)
# 1. 전처리: 기존 스크립트의 변수명과 날짜 처리 유지
raw <- readRDS(INPUT); setDT(raw)
pick <- function(v) { h <- intersect(v,names(raw)); if(!length(h)) stop('변수 없음: ',paste(v,collapse='/')); h[1] }
cols <- c(pick('결제금액'),pick('결제일자'),pick('구'),pick(c('표준산업명','서울페이코드명')),pick(c('결제자성별','성별')),pick(c('결제자연령대','연령대','결제자나이')))
write(data.table(original=cols,model=c('amount','date','district','industry','sex','age')),'00_variables.csv')
n0 <- nrow(raw); d <- raw[, ..cols]; rm(raw); gc()
setnames(d,c('amount','date','district','industry','sex','age'))
if(!inherits(d$date,'Date')) { z <- as.character(d$date); i <- grepl('^[0-9]{8}',z); z[i] <- paste(substr(z[i],1,4),substr(z[i],5,6),substr(z[i],7,8),sep='-'); d[,date:=as.Date(substr(z,1,10),format='%Y-%m-%d')]; rm(z) }
d[,amount:=suppressWarnings(as.numeric(as.character(amount)))]
d <- d[is.finite(amount)&amount>0 & !is.na(date) & date>=as.Date('2025-01-01') & date<as.Date('2026-01-01')]
features <- c('district','industry','sex','age','month','weekday')
d[,month:=format(date,'%m')]; d[,weekday:=format(date,'%u')]; d[,date:=NULL]
for(v in features) { d[,(v):=trimws(as.character(get(v)))]; d[is.na(get(v))|get(v)=='',(v):='정보없음'] }
write(data.table(raw=n0,eligible=nrow(d),excluded=n0-nrow(d)),'01_sample_flow.csv')
write(d[,.(n=.N,mean=mean(amount),median=median(amount),sd=sd(amount),p95=quantile(amount,.95)),by=month],'01_monthly_summary.csv')
if(nrow(d)<100) stop('유효 표본 부족')
# 매 반복 월별 비례 층화 표본 및 75:25 분할. 시간 예측이 아닌 동년도 거래 일반화.
sample_split <- function(d,seed) {
 set.seed(seed)
 s <- d[, .SD[sample.int(.N,min(.N,max(1L,as.integer(round(.N/nrow(d)*N)))))],by=month]
 s[,test:=FALSE]; s[,test:=seq_len(.N) %in% sample.int(.N,floor(.N*.25)),by=month]
 list(train=s[test==FALSE][,test:=NULL],test=s[test==TRUE][,test:=NULL])
}
prep <- function(a,b) {
 top <- a[, .N,by=industry][order(-N,industry)][seq_len(min(TOP,.N)),industry]
 for(v in features) {
  if(v=='industry') { a[!industry %in% top,industry:='기타업종']; b[!industry %in% top,industry:='기타업종'] }
  lev <- a[, .N,by=v][order(-N,get(v))][[v]]
  # 학습 미관측 범주는 최빈 범주로 대체하고 건수 기록
  bad <- !b[[v]] %in% lev
  if(any(bad)) message(v,': 미관측 테스트 범주 ',sum(bad),'건 최빈값 대체')
  b[bad,(v):=lev[1]]
  a[,(v):=factor(get(v),levels=lev)]; b[,(v):=factor(get(v),levels=lev)]
 }
 active <- features[vapply(a[,..features],nlevels,integer(1))>1L]
 if(!length(active)) stop('변동 있는 설명변수 없음')
 list(a=a,b=b,active=active)
}
metric <- function(y,p) { if(any(!is.finite(p))) stop('비유한 예측값'); data.table(MAE=mean(abs(y-p)),RMSE=sqrt(mean((y-p)^2)),MAPE=100*mean(abs(y-p)/y),R2=1-sum((y-p)^2)/sum((y-mean(y))^2)) }
perf <- list(); imp <- list(); errors <- list(); sizes <- list()
# 2~4. 로그 선형회귀, 개별 ML, 성능 검증 및 공통 permutation importance
for(r in seq_len(REPEATS)) {
 message('반복 ',r,' / ',REPEATS)
 sp <- sample_split(d,SEED+r); q <- prep(sp$train,sp$test); rm(sp)
 a <- q$a; b <- q$b; active <- q$active; rm(q)
 sizes[[r]] <- data.table(repeat_id=r,train=nrow(a),test=nrow(b))
 a[,log_amount:=log1p(amount)]; b[,log_amount:=log1p(amount)]
 f <- reformulate(active,response='log_amount'); lmfit <- lm(f,data=a)
 # 추론은 원래 표본의 첫 반복만 사용 (반복 p값 평균 금지)
 if(r==1L) {
  V <- sandwich::vcovHC(lmfit,type='HC3'); se <- sqrt(diag(V)); cf <- coef(lmfit); t <- cf/se
  write(data.table(term=names(cf),beta=cf,robust_se=se,p=2*pt(abs(t),df.residual(lmfit),lower.tail=FALSE),pct_log1p=100*expm1(cf),CI_low=cf-qt(.975,df.residual(lmfit))*se,CI_high=cf+qt(.975,df.residual(lmfit))*se),'02_OLS_coefficients.csv')
  write(data.table(R2=summary(lmfit)$r.squared,adjusted_R2=summary(lmfit)$adj.r.squared,n=nobs(lmfit)),'02_OLS_fit.csv')
  write(rbindlist(lapply(active,function(v) data.table(variable=v,reference=levels(a[[v]])[1]))),'02_reference_levels.csv')
  if(requireNamespace('car',quietly=TRUE)) tryCatch({ vv <- car::vif(lmfit); write(as.data.table(as.matrix(vv),keep.rownames='variable'),'02_VIF.csv') },error=function(e) message('VIF: ',conditionMessage(e)))
 }
 # OLS는 로그 척도 추론. 모든 예측모형은 원 단위 목표로 공정 비교.
 # OLS 원 단위 예측용 별도 선형회귀: 로그 역변환 편향 방지
 ff <- reformulate(active,response='amount'); linear <- lm(ff,data=a)
 makeX <- function(z) { m <- Matrix::sparse.model.matrix(reformulate(active),data=z); m <- m[,colnames(m)!='(Intercept)',drop=FALSE]; colnames(m) <- paste0('f',seq_len(ncol(m))); m }
 X <- makeX(a); Z <- makeX(b); stopifnot(identical(colnames(X),colnames(Z)))
 ya <- a$amount; yb <- b$amount
 # 동일 테스트 자료에서 원래 변수 전체를 섞어 RMSE 증가량 측정
 evaluate <- function(name,fun,importance=TRUE) {
  p <- pmax(0,as.numeric(fun(b,Z))); stopifnot(length(p)==nrow(b))
  m <- metric(yb,p); m[,`:=`(model=name,repeat_id=r)]; perf[[length(perf)+1L]] <<- m
  if(importance) {
   set.seed(SEED+1000L+r); ids <- sample.int(nrow(b),min(10000L,nrow(b))); sub <- copy(b[ids]); base <- sqrt(mean((sub$amount-p[ids])^2))
   for(v in active) {
    z <- copy(sub); z[,(v):=sample(get(v))]; pp <- pmax(0,as.numeric(fun(z,makeX(z))))
    imp[[length(imp)+1L]] <<- data.table(model=name,repeat_id=r,variable=v,delta_RMSE=sqrt(mean((sub$amount-pp)^2))-base)
   }
  }
 }
 run <- function(name,expr) tryCatch(force(expr),error=function(e) { message(name,': ',conditionMessage(e)); errors[[length(errors)+1L]] <<- data.table(repeat_id=r,model=name,error=conditionMessage(e)) })
 evaluate('Mean baseline',function(z,m) rep(mean(ya),nrow(z)),FALSE)
 evaluate('Linear Regression',function(z,m) predict(linear,newdata=z))
 for(method in c('Random Forest','Extra Trees')) run(method,{
  set.seed(SEED+r); fit <- ranger::ranger(ff,data=as.data.frame(a[,c('amount',active),with=FALSE]),num.trees=300,mtry=min(3,length(active)),min.node.size=20,splitrule=if(method=='Extra Trees') 'extratrees' else 'variance',num.random.splits=1,replace=method!='Extra Trees',sample.fraction=if(method=='Extra Trees') 1 else 1,num.threads=THREADS,respect.unordered.factors='order',seed=SEED+r)
  evaluate(method,function(z,m) predict(fit,data=as.data.frame(z),num.threads=THREADS)$predictions); rm(fit)
 })
 run('XGBoost',{
  fit <- xgboost::xgb.train(params=list(objective='reg:squarederror',eta=.05,max_depth=6,subsample=.8,colsample_bytree=.9,nthread=THREADS,seed=SEED+r),data=xgboost::xgb.DMatrix(X,label=ya),nrounds=200,verbose=0)
  evaluate('XGBoost',function(z,m) predict(fit,xgboost::xgb.DMatrix(m))); rm(fit)
 })
 run('LightGBM',{
  fit <- lightgbm::lgb.train(params=list(objective='regression',metric='rmse',learning_rate=.05,num_leaves=31,min_data_in_leaf=100,num_threads=THREADS,verbosity=-1,seed=SEED+r),data=lightgbm::lgb.Dataset(X,label=ya),nrounds=200)
  evaluate('LightGBM',function(z,m) predict(fit,m)); rm(fit)
 })
 run('CatBoost',{
  if(!requireNamespace('catboost',quietly=TRUE)) stop('catboost 미설치: 공식 R 설치 안내 참고')
  # 공통 원핫 행렬 사용 (CatBoost 고유 범주형 인코딩은 사용하지 않음)
  fit <- catboost::catboost.train(catboost::catboost.load_pool(as.matrix(X),label=ya),params=list(loss_function='RMSE',iterations=200,depth=6,learning_rate=.05,thread_count=THREADS,random_seed=SEED+r,logging_level='Silent'))
  evaluate('CatBoost',function(z,m) catboost::catboost.predict(fit,catboost::catboost.load_pool(as.matrix(m)))); rm(fit)
 })
 write(rbindlist(perf),'03_performance_each.csv'); write(rbindlist(sizes),'03_sample_sizes.csv')
 if(length(imp)) write(rbindlist(imp),'04_importance_each.csv')
 if(length(errors)) write(rbindlist(errors),'00_model_errors.csv')
 rm(a,b,X,Z,linear,lmfit); gc()
}
# 5. 반복 평균/표준편차 및 비교 그래프
p <- rbindlist(perf); summary_p <- p[,lapply(.SD,mean),by=model,.SDcols=c('MAE','RMSE','MAPE','R2')]
sd_p <- p[,.(RMSE_SD=sd(RMSE),MAE_SD=sd(MAE),MAPE_SD=sd(MAPE),R2_SD=sd(R2),repeats=.N),by=model]
summary_p <- merge(summary_p,sd_p,by='model'); setorder(summary_p,RMSE); write(summary_p,'05_performance_summary.csv'); print(summary_p)
g <- ggplot(summary_p,aes(reorder(model,RMSE),RMSE))+geom_col()+coord_flip()+labs(x=NULL,y='RMSE (KRW)',title='Test RMSE: repeated split mean')+theme_minimal(); ggsave(file.path(OUT,'05_RMSE.png'),g,width=8,height=5,dpi=160)
# 6. 변수 중요도: 원 변수 단위 공통 평가척도. 음수는 그대로 보존.
if(length(imp)) {
 ii <- rbindlist(imp); agg <- ii[,.(mean_delta_RMSE=mean(delta_RMSE),sd_delta_RMSE=sd(delta_RMSE),repeats=.N),by=.(model,variable)]; write(agg,'06_importance_by_model.csv')
 # 세 부스팅 모델이 모두 성공한 반복만 종합. 양의 증가량을 모델별 100%로 정규화.
 boost <- ii[model %in% c('XGBoost','LightGBM','CatBoost')]; valid <- boost[,.(models=uniqueN(model)),by=repeat_id][models==3,repeat_id]; boost <- boost[repeat_id %in% valid]
 if(nrow(boost)) {
  boost[,den:=sum(pmax(delta_RMSE,0)),by=.(repeat_id,model)]; boost <- boost[den>0]; good <- boost[,.(models=uniqueN(model)),by=repeat_id][models==3,repeat_id]; boost <- boost[repeat_id %in% good]; boost[,share:=100*pmax(delta_RMSE,0)/den]
  if(nrow(boost)) { comb <- boost[,.(importance_pct=mean(share),repeats=uniqueN(repeat_id)),by=variable][order(-importance_pct)]; write(comb,'06_boosting_combined.csv'); g <- ggplot(comb,aes(reorder(variable,importance_pct),importance_pct))+geom_col()+coord_flip()+labs(x=NULL,y='Importance (%)')+theme_minimal(); ggsave(file.path(OUT,'06_importance.png'),g,width=7,height=5,dpi=160) }
 } else message('세 부스팅 모델 모두 성공한 반복 없음: 종합 중요도 미산출')
}
writeLines(capture.output(sessionInfo()),file.path(OUT,'99_sessionInfo.txt'))
