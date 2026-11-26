# 서울사랑상품권 결제수요의 미래 예측: 서울 전체 → 25개 자치구 (일별 결제금액)
# UTF-8 / 결제자료만 사용 / 8_data_regression_ML.R 과 같은 입력 파일·전처리 기준
# 검증: 시간 기반 rolling-origin (1~9월 학습 → 10~12월을 7일 단위로 반복 예측)
# 최초 설치: install.packages(c('data.table','forecast','lightgbm','ggplot2'))
options(repos=c(CRAN='https://cloud.r-project.org'))
BASE <- 'C:/Users/UOS/Desktop/머신러닝 스터디용'
INPUT <- file.path(BASE,'2025_결제_서울25개자치구_분석용.rds')
OUT <- file.path(BASE,'output','forecast_district')
if (!file.exists(INPUT)) stop('입력 경로 확인: ', INPUT)
dir.create(OUT,recursive=TRUE,showWarnings=FALSE)

START <- as.Date('2025-01-01'); END <- as.Date('2025-12-31')
TEST_START <- as.Date('2025-10-01')   # 검증 시작일 (이전 자료만 학습에 사용)
H <- 7L                               # 예측 시계: 기준일부터 7일 앞까지
USE_ARIMA <- TRUE                     # 오래 걸리면 FALSE
THREADS <- 2L; SEED <- 20261126L
pkgs <- c('data.table','forecast','lightgbm','ggplot2')
missing <- pkgs[!vapply(pkgs,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) stop('먼저 설치: ',paste(missing,collapse=', '))
library(data.table); library(ggplot2)
setDTthreads(THREADS)
write <- function(d,f) fwrite(d,file.path(OUT,f),bom=TRUE)

# 2025년 공휴일·대체공휴일·임시공휴일 (발표 전 확인 필요)
HOL <- as.Date(c('2025-01-01','2025-01-27','2025-01-28','2025-01-29','2025-01-30',
                 '2025-03-01','2025-03-03','2025-05-05','2025-05-06','2025-06-03','2025-06-06',
                 '2025-08-15','2025-10-03','2025-10-05','2025-10-06','2025-10-07','2025-10-08',
                 '2025-10-09','2025-12-25'))

# 1. 일별 자치구 결제금액 집계 -------------------------------------------------
raw <- readRDS(INPUT); setDT(raw)
pick <- function(v) { h <- intersect(v,names(raw)); if(!length(h)) stop('변수 없음: ',paste(v,collapse='/')); h[1] }
cols <- c(pick('결제금액'),pick('결제일자'),pick('구'))
d <- raw[, ..cols]; rm(raw); gc()
setnames(d,c('amount','date','district'))
if(!inherits(d$date,'Date')) { z <- as.character(d$date); i <- grepl('^[0-9]{8}',z); z[i] <- paste(substr(z[i],1,4),substr(z[i],5,6),substr(z[i],7,8),sep='-'); d[,date:=as.Date(substr(z,1,10),format='%Y-%m-%d')]; rm(z) }
d[,amount:=suppressWarnings(as.numeric(as.character(amount)))]
d[,district:=trimws(as.character(district))]
d <- d[is.finite(amount)&amount>0 & !is.na(date) & date>=START & date<=END & !is.na(district) & district!='']
daily <- d[,.(y=sum(amount)/1e6,n=.N),by=.(district,date)]; rm(d); gc()   # y 단위: 백만 원
grid <- CJ(district=sort(unique(daily$district)),date=seq(START,END,by='day'))
daily <- daily[grid,on=.(district,date)]; daily[is.na(y),`:=`(y=0,n=0L)]
DIST <- sort(unique(daily$district))
message('자치구 수: ',length(DIST),' / 일수: ',uniqueN(daily$date))
if(length(DIST)!=25L) warning('자치구 수가 25개가 아님: 00_district_list.csv 확인')
write(data.table(district=DIST),'00_district_list.csv')
write(daily,'10_daily_district.csv')
total <- daily[,.(y=sum(y),n=sum(n)),by=date][order(date)]
write(total,'10_daily_seoul.csv')
# 자료 점검: 월별 건수가 거의 같으면 월 단위 추출 표본일 가능성 -> 수준(총량) 해석 주의
mchk <- total[,.(n=sum(n),amount_million=sum(y),days=.N),by=.(month=format(date,'%Y-%m'))]
write(mchk,'10_monthly_check.csv'); print(mchk)

# 2. 예측용 변수: 시차 7일 이상만 사용 -> 7일 앞 예측에서 미래 정보 누출 없음 ----
feat_build <- function(x) {
 x <- copy(x); setorder(x,district,date)
 x[,`:=`(lag7=shift(y,7),lag14=shift(y,14),lag21=shift(y,21),lag28=shift(y,28)),by=district]
 x[,`:=`(roll7=frollmean(shift(y,7),7),roll28=frollmean(shift(y,7),28)),by=district]
 x[,`:=`(wday=as.integer(format(date,'%u')),mday=as.integer(format(date,'%d')),
         hol=as.integer(date %in% HOL),hol_lag7=as.integer((date-7) %in% HOL),
         pre_hol=as.integer((date+1) %in% HOL),post_hol=as.integer((date-1) %in% HOL))]
 x[,dcode:=match(district,DIST)-1L]
 x
}
F <- feat_build(daily)
FEAT <- c('dcode','wday','mday','hol','hol_lag7','pre_hol','post_hol','lag7','lag14','lag21','lag28','roll7','roll28')

# 통계모형: 계절 naive(지난주 같은 요일), ETS, ARIMA (주기 7일)
fc_stat <- function(ytr,h) {
 out <- list(SNaive=as.numeric(tail(ytr,7))[seq_len(h)])
 ts_y <- ts(ytr,frequency=7)
 out$ETS <- tryCatch(as.numeric(forecast::forecast(forecast::ets(ts_y),h=h)$mean),error=function(e) { message('ETS 실패: ',conditionMessage(e)); out$SNaive })
 if(USE_ARIMA) out$ARIMA <- tryCatch(as.numeric(forecast::forecast(forecast::auto.arima(ts_y),h=h)$mean),error=function(e) { message('ARIMA 실패: ',conditionMessage(e)); out$SNaive })
 lapply(out,function(v) pmax(0,v))
}

# 3. rolling-origin 예측 --------------------------------------------------------
origins <- seq(TEST_START,END,by=H)
res <- list(); k <- 0L
add <- function(dt) { k <<- k+1L; res[[k]] <<- dt }
for(i in seq_along(origins)) {
 o <- origins[i]; hh <- min(H,as.integer(END-o)+1L); dts <- o+0:(hh-1L)
 message('기준일 ',format(o),' (',i,'/',length(origins),')')
 # (1) 자치구별 통계모형
 for(g in DIST) {
  s <- daily[district==g][order(date)]
  fc <- fc_stat(s[date<o,y],hh); act <- s[date %in% dts,y]
  for(m in names(fc)) add(data.table(origin=o,date=dts,h=seq_len(hh),level='district',series=g,model=m,actual=act,forecast=fc[[m]]))
 }
 # (2) 서울 전체 직접 예측(통계모형)
 fc <- fc_stat(total[date<o,y],hh); act <- total[date %in% dts,y]
 for(m in names(fc)) add(data.table(origin=o,date=dts,h=seq_len(hh),level='seoul_direct',series='서울 전체',model=m,actual=act,forecast=fc[[m]]))
 # (3) LightGBM 전역모형: 25개 자치구를 한 모형으로 학습 (log1p 척도)
 tr <- F[date<o & !is.na(roll28)]; te <- F[date %in% dts][order(district,date)]
 set.seed(SEED+i)
 fit <- lightgbm::lgb.train(params=list(objective='regression',metric='rmse',learning_rate=.05,num_leaves=15,min_data_in_leaf=20,
          feature_fraction=.9,bagging_fraction=.8,bagging_freq=1,num_threads=THREADS,verbosity=-1,seed=SEED+i),
          data=lightgbm::lgb.Dataset(as.matrix(tr[,..FEAT]),label=log1p(tr$y),categorical_feature='dcode'),nrounds=300)
 p <- pmax(0,expm1(predict(fit,as.matrix(te[,..FEAT]))))
 add(data.table(origin=o,date=te$date,h=as.integer(te$date-o)+1L,level='district',series=te$district,model='LightGBM',actual=te$y,forecast=p))
 if(i==length(origins)) { imp <- as.data.table(lightgbm::lgb.importance(fit)); write(imp,'14_lightgbm_importance_last.csv') }
 rm(fit,tr,te)
}
fcst <- rbindlist(res)
# (4) 서울 전체 상향식(bottom-up): 자치구 예측치의 합 -> 자치구와 서울 예측이 정합
bu <- fcst[level=='district',.(actual=sum(actual),forecast=sum(forecast)),by=.(origin,date,h,model)]
bu[,`:=`(level='seoul_bottomup',series='서울 전체')]
fcst <- rbind(fcst,bu,use.names=TRUE)
write(fcst,'11_forecasts.csv')

# 4. 평가 -----------------------------------------------------------------------
# MASE 척도: 학습기간(1~9월)의 계절 naive(7일) 평균절대오차
sc <- rbind(daily[date<TEST_START,.(scale=mean(abs(diff(y,lag=7)))),by=.(series=district)],
            data.table(series='서울 전체',scale=total[date<TEST_START,mean(abs(diff(y,lag=7)))]))
fcst <- merge(fcst,sc,by='series',all.x=TRUE)
ev <- function(a,f,s) list(MAE=mean(abs(a-f)),RMSE=sqrt(mean((a-f)^2)),WAPE=100*sum(abs(a-f))/sum(a),
                          MAPE=100*mean(abs(a-f)[a>0]/a[a>0]),MASE=mean(abs(a-f))/s[1])
by_series <- fcst[,ev(actual,forecast,scale),by=.(level,series,model)]
write(by_series,'12_metrics_by_series.csv')
summ <- rbind(
 fcst[level!='district',ev(actual,forecast,scale),by=.(level,model)],
 by_series[level=='district',.(MAE=mean(MAE),RMSE=mean(RMSE),WAPE=NA_real_,MAPE=mean(MAPE),MASE=mean(MASE)),by=.(level,model)],
 fill=TRUE)
# 자치구 수준 WAPE는 25개 자치구 오차를 합산한 값(pooled)
pool <- fcst[level=='district',.(WAPE=100*sum(abs(actual-forecast))/sum(actual)),by=.(level,model)]
summ[pool,on=.(level,model),WAPE:=i.WAPE]
setorder(summ,level,MASE)
write(summ,'12_metrics_summary.csv'); print(summ)
write(fcst[,.(WAPE=100*sum(abs(actual-forecast))/sum(actual)),by=.(level,model,h)][order(level,model,h)],'12_metrics_by_horizon.csv')
# 기준모형(계절 naive) 대비 MASE 개선 여부: 자치구별 승패
win <- dcast(by_series[level=='district'],series~model,value.var='MASE')
write(win,'12_district_MASE_wide.csv')

# 5. 그래프 ---------------------------------------------------------------------
best_d <- summ[level=='district'][which.min(MASE),model]
pl <- rbind(fcst[level=='seoul_bottomup' & model %in% c(best_d,'SNaive'),.(date,model=paste0(model,' (상향식)'),value=forecast)],
            fcst[level=='seoul_direct' & model=='ETS',.(date,model='ETS (직접)',value=forecast)],
            total[date>=TEST_START,.(date,model='실제값',value=y)])
g <- ggplot(pl,aes(date,value,colour=model))+geom_line(linewidth=.6)+
 labs(x=NULL,y='일별 결제금액(백만 원)',colour=NULL,title='서울 전체 일별 결제금액: 실제값과 7일 앞 예측(2025.10~12)')+theme_minimal()+theme(legend.position='top')
ggsave(file.path(OUT,'13_seoul_forecast.png'),g,width=10,height=5,dpi=160)
wd <- by_series[level=='district' & model==best_d][order(-WAPE)]
g <- ggplot(wd,aes(reorder(series,WAPE),WAPE))+geom_col()+coord_flip()+
 labs(x=NULL,y='WAPE(%)',title=paste0('자치구별 예측오차: ',best_d))+theme_minimal()
ggsave(file.path(OUT,'13_district_wape.png'),g,width=7,height=7,dpi=160)
writeLines(capture.output(sessionInfo()),file.path(OUT,'99_sessionInfo.txt'))
message('완료: ',OUT)

