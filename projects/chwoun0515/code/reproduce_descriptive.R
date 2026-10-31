# Reproduce 14 descriptive tables and the summary figure using base R only.
if (.Platform$OS.type == 'windows')
  invisible(Sys.setlocale('LC_CTYPE', 'English_United States.utf8'))

root <- 'C:/Users/chwou/Desktop/graduate/03_data/processed'
out <- file.path(root, 'master_descriptive_R')
dir.create(out, recursive = TRUE, showWarnings = FALSE)
d <- read.csv(file.path(root, 'regression_ml_master.csv'),
              fileEncoding = 'UTF-8-BOM', check.names = FALSE,
              stringsAsFactors = FALSE, na.strings = 'NA',
              colClasses = c(district_id='character', period='character',
                            feature_period_l1='character', feature_period_l2='character'))
stopifnot(!anyDuplicated(d[c('district_id','period')]),
          all(table(d$district_id) == 20))

# UTF-8 BOM output for Korean text in Excel. No packages are required.
save_csv <- function(x, name) {
  names(x)[names(x) == 'type_name'] <- '\uc720\ud615\uba85'
  con <- file(file.path(out, name), open='wt', encoding='UTF-8')
  on.exit(close(con))
  cat('\ufeff', file=con)
  write.table(x, con, sep=',', row.names=FALSE, col.names=TRUE,
              quote=TRUE, na='NA', qmethod='double')
}
type_names <- c(A='\uace8\ubaa9\uc0c1\uad8c', D='\ubc1c\ub2ec\uc0c1\uad8c', R='\uc804\ud1b5\uc2dc\uc7a5')
vars <- c('stores','similar_stores','openings','closures','franchises',
          'closure_rate','opening_rate','sales_won','transactions',
          'sales_yoy_log_pct','sales_yoy_common_industries','sales_yoy_same_industries',
          'entropy','hhi','food_share','service_share','retail_share',
          'residents','workers','footfall','store_industries','sales_industries')
labels <- setNames(c('\uc810\ud3ec \uc218','\uc720\uc0ac\uc5c5\uc885\uc810\ud3ec \uc218','\uac1c\uc5c5\uc810\ud3ec \uc218','\ud3d0\uc5c5\uc810\ud3ec \uc218',
  '\ud504\ub79c\ucc28\uc774\uc988\uc810\ud3ec \uc218','\ud3d0\uc5c5\ub960','\uac1c\uc5c5\ub960','\ubd84\uae30 \uad00\uce21\uc5c5\uc885 \ub9e4\ucd9c','\ubd84\uae30 \ub9e4\ucd9c \uac74\uc218',
  '\ub9e4\ucd9c \uc804\ub144\ub3d9\uae30 \ub85c\uadf8 \ubcc0\ud654\uc728','\uacf5\ud1b5\uc5c5\uc885 \ub9e4\ucd9c \ub85c\uadf8 \ubcc0\ud654\uc728','\ub3d9\uc77c\uc5c5\uc885\uc9d1\ud569 \ub9e4\ucd9c \ub85c\uadf8 \ubcc0\ud654\uc728',
  '\uc5c5\uc885 \ub2e4\uc591\uc131(\uc5d4\ud2b8\ub85c\ud53c)','\uc5c5\uc885 \uc9d1\uc911\ub3c4(HHI)','\uc678\uc2dd\uc5c5 \uc810\ud3ec \ube44\uc911','\uc11c\ube44\uc2a4\uc5c5 \uc810\ud3ec \ube44\uc911',
  '\uc18c\ub9e4\uc5c5 \uc810\ud3ec \ube44\uc911','\uc0c1\uc8fc\uc778\uad6c','\uc9c1\uc7a5\uc778\uad6c','\uc720\ub3d9\uc778\uad6c(\uc6d0\uc790\ub8cc \uc9d1\uacc4\uac12)',
  '\uc810\ud3ec\uc790\ub8cc \uad00\uce21 \uc5c5\uc885 \uc218','\ub9e4\ucd9c\uc790\ub8cc \uad00\uce21 \uc5c5\uc885 \uc218'), vars)
units <- setNames(c(rep('\uac1c',5),'%','%','\uc5b5\uc6d0','\uac74',rep('\ub85c\uadf8%',3),
                    '\uc9c0\uc218','0~1',rep('%',3),'\uba85','\uba85','\uc6d0\uc790\ub8cc \ub2e8\uc704','\uac1c','\uac1c'), vars)
avg <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm=TRUE)
med <- function(x) if (all(is.na(x))) NA_real_ else median(x, na.rm=TRUE)
# Adjusted Fisher-Pearson skewness, matching pandas.Series.skew().
skew <- function(x) {
  n <- length(x)
  if (n < 3) return(NA_real_)
  s <- sd(x)
  if (s == 0) return(0)
  n / ((n-1)*(n-2)) * sum(((x-mean(x))/s)^3)
}
describe <- function(g, columns=vars, display=TRUE) {
  do.call(rbind, lapply(columns, function(v) {
    x <- g[[v]]
    if (display && v == 'sales_won') x <- x/1e8
    if (display && grepl('_share$',v)) x <- x*100
    z <- x[!is.na(x)]
    q <- if (length(z)) quantile(z,c(0,.01,.25,.5,.75,.99,1),type=7,names=FALSE) else rep(NA_real_,7)
    data.frame(variable=v, label=if(v %in% vars) labels[[v]] else v,
      unit=if(display && v %in% vars) units[[v]] else 'original',
      rows=length(x),n=length(z),missing=sum(is.na(x)),missing_pct=100*mean(is.na(x)),
      zero_n=sum(z==0),mean=avg(z),sd=sd(z),min=q[1],p01=q[2],q1=q[3],
      median=q[4],q3=q[5],p99=q[6],max=q[7],skew=skew(z))
  }))
}
profile <- function(g) data.frame(
  rows=nrow(g),districts=length(unique(g$district_id)),
  sales_observed=sum(!is.na(g$sales_won)),sales_missing=sum(is.na(g$sales_won)),
  sales_missing_pct=100*mean(is.na(g$sales_won)),
  stores_mean=avg(g$stores),stores_median=med(g$stores),
  closure_mean=avg(g$closure_rate),closure_pooled=100*sum(g$closures)/sum(g$similar_stores),
  sales_mean_100m=avg(g$sales_won)/1e8,sales_median_100m=med(g$sales_won)/1e8,
  sales_yoy_n=sum(!is.na(g$sales_yoy_log_pct)),sales_yoy_mean=avg(g$sales_yoy_log_pct),
  sales_yoy_median=med(g$sales_yoy_log_pct),residents_missing=sum(is.na(g$residents)),
  workers_missing=sum(is.na(g$workers)),footfall_missing=sum(is.na(g$footfall)),
  reg_closure=sum(g$reg_closure_ok),reg_sales=sum(g$reg_sales_ok),
  ml_closure=sum(g$ml_closure_ok),ml_sales=sum(g$ml_sales_ok))
group_profile <- function(column) do.call(rbind,lapply(sort(unique(d[[column]])),function(k) {
  cbind(setNames(data.frame(k),column),profile(d[d[[column]]==k,,drop=FALSE]))
}))

# 01-03: overall distribution and schema; keep raw and imputed fields separate.
key <- describe(d)
save_csv(key,'01_\uc8fc\uc694\ubcc0\uc218_\uae30\uc220\ud1b5\uacc4.csv')
numeric_vars <- names(d)[vapply(d,is.numeric,logical(1)) |
                         vapply(d,function(x) all(is.na(x)),logical(1))]
numeric_vars <- setdiff(numeric_vars,c('year','quarter','time_index'))
save_csv(describe(d,numeric_vars,FALSE),'02_\uc804\uccb4\uc218\uce58\ubcc0\uc218_\uae30\uc220\ud1b5\uacc4_\uc6d0\ub2e8\uc704.csv')
schema <- do.call(rbind,lapply(names(d),function(v) data.frame(variable=v,
  dtype=class(d[[v]])[1],unique=length(unique(na.omit(d[[v]]))),
  missing=sum(is.na(d[[v]])),missing_pct=100*mean(is.na(d[[v]])))))
save_csv(schema,'03_\ubcc0\uc218\uad6c\uc870_\uacb0\uce21\ud604\ud669.csv')

# 04-09: year, district type, quarter and model split summaries.
annual <- group_profile('year'); types <- group_profile('district_type')
types <- cbind(types[1],type_name=unname(type_names[types$district_type]),types[-1])
periods <- group_profile('period'); splits <- group_profile('split')
save_csv(annual,'04_\uc5f0\ub3c4\ubcc4_\uc694\uc57d.csv'); save_csv(types,'05_\uc0c1\uad8c\uc720\ud615\ubcc4_\uc694\uc57d.csv')
save_csv(periods,'06_\ubd84\uae30\ubcc4_\uc694\uc57d.csv'); save_csv(splits,'07_\ubd84\uc11d\uad6c\uac04\ubcc4_\ud45c\ubcf8.csv')
save_csv(do.call(rbind,lapply(sort(unique(d$year)),function(y)
  cbind(describe(d[d$year==y,]),year=y))),'08_\uc5f0\ub3c4\ubcc4_\uc8fc\uc694\ubcc0\uc218_\uc0c1\uc138\ud1b5\uacc4.csv')
save_csv(do.call(rbind,lapply(sort(unique(d$district_type)),function(t)
  cbind(describe(d[d$district_type==t,]),district_type=t,type_name=type_names[[t]]))),
  '09_\uc720\ud615\ubcc4_\uc8fc\uc694\ubcc0\uc218_\uc0c1\uc138\ud1b5\uacc4.csv')

# 10: report stored imputation results, without re-imputing the data.
features <- paste0(c('log1p_stores','entropy','food_share','retail_share',
  'log1p_residents','log1p_workers','log1p_footfall','closure_rate','opening_rate',
  'log1p_sales_won','log1p_transactions','sales_industries'),'_l2')
imputation <- do.call(rbind,lapply(sort(unique(d$split)),function(k) {
  g <- d[d$split==k,]
  do.call(rbind,lapply(features,function(v) {
    x <- g[[v]]; w <- g[[paste0(v,'_imp')]]
    data.frame(split=k,variable=v,rows=nrow(g),before_na=sum(is.na(x)),
      before_na_pct=100*mean(is.na(x)),after_na=sum(is.na(w)),
      imputed_n=sum(is.na(x)&!is.na(w)),before_mean=avg(x),after_mean=avg(w),
      before_sd=sd(x,na.rm=TRUE),after_sd=sd(w,na.rm=TRUE))
  }))
}))
save_csv(imputation,'10_ML\ub300\uce58\uc804\ud6c4_\ube44\uad50.csv')

# 11-14: coverage, missingness reasons, sample selection and extremes.
coverage <- do.call(rbind,lapply(split(d,d$district_id),function(g) {
  stopifnot(length(unique(g$district_type))==1)
  data.frame(district_id=g$district_id[1],district_type=g$district_type[1],
    district_name=tail(na.omit(g$district_name),1),periods=length(unique(g$period)),
    sales_periods=sum(!is.na(g$sales_won)),sales_yoy_periods=sum(!is.na(g$sales_yoy_log_pct)),
    regression_closure_periods=sum(g$reg_closure_ok),regression_sales_periods=sum(g$reg_sales_ok))
}))
save_csv(coverage,'11_\uc0c1\uad8c\ubcc4_\uad00\uce21\uae30\uac04.csv')
status <- do.call(rbind,lapply(c('sales_status','sales_yoy_status'),function(v) {
  z <- aggregate(rep(1L,nrow(d)),list(year=d$year,status=d[[v]]),sum)
  names(z)[3] <- 'rows'
  z <- z[order(z$year,z$status),]; z$variable <- v; z
}))
save_csv(status,'12_\ub9e4\ucd9c\uacb0\uce21_\uc0ac\uc720.csv')
selection <- do.call(rbind,lapply(c('closure_rate','sales_yoy_log_pct'),function(target) {
  flag <- if(target=='closure_rate') 'reg_closure_ok' else 'reg_sales_ok'
  z <- d[!is.na(d[[target]])&!is.na(d$feature_period_l1),]
  do.call(rbind,lapply(sort(unique(z[[flag]])),function(k)
    cbind(target=target,group=if(k==1) 'included' else 'excluded',profile(z[z[[flag]]==k,]))))
}))
save_csv(selection,'13_\ud68c\uadc0\ud45c\ubcf8_\ud3ec\ud568\uc81c\uc678_\ube44\uad50.csv')
extremes <- do.call(rbind,lapply(c('stores','sales_won','closure_rate',
  'sales_yoy_log_pct','residents','workers','footfall'),function(v) {
  do.call(rbind,lapply(c('max','min'),function(side) {
    z <- d[head(order(d[[v]],decreasing=side=='max',na.last=NA,method='radix'),5),]
    data.frame(variable=v,side=side,z[c('district_id','district_name','period')],
      value=z[[v]],z[c('stores','similar_stores','closures','sales_won','sales_won_l4')],
      row.names=NULL,check.names=FALSE)
  }))
}))
save_csv(extremes,'14_\uadf9\ub2e8\uac12_\ud655\uc778\uc6a9.csv')

# Plot all quarters; observed values only. No truncation of extreme values.
draw_summary <- function() {
  png(file.path(out,'\uc790\ub8cc\ud2b9\uc131_\uc694\uc57d\uadf8\ub9bc.png'),width=2000,height=1500,res=170)
  on.exit(dev.off())
  if (.Platform$OS.type=='windows') {
    windowsFonts(Korean=windowsFont('Malgun Gothic')); par(family='Korean')
  }
  par(mfrow=c(2,2),mar=c(4.5,4.7,3,1),oma=c(1.5,0,3,0),las=1)
  p <- periods; x <- seq_len(nrow(p)); ticks <- seq(1,nrow(p),4)
  blue <- '#2463A0'; orange <- '#D27D28'
  axis_quarters <- function() axis(1,at=ticks,labels=paste0(substr(p$period[ticks],1,4),'Q1'))
  plot(x,p$stores_mean,type='l',lwd=2,col=blue,ylim=c(0,max(p$stores_mean)*1.15),
       xaxt='n',xlab='',ylab='\uc810\ud3ec \uc218 (\uac1c)',main='\uc0c1\uad8c\u00b7\ubd84\uae30 \uc810\ud3ec \uc218: \ud3c9\uade0\uacfc \uc911\uc559\uac12')
  lines(x,p$stores_median,col=orange,lwd=2); axis_quarters()
  legend('bottomleft',c('\ud3c9\uade0','\uc911\uc559\uac12'),col=c(blue,orange),lty=1,lwd=2,bty='n')
  plot(x,p$closure_mean,type='l',lwd=2,col=blue,ylim=c(0,max(p$closure_mean,p$closure_pooled)*1.2),
       xaxt='n',xlab='',ylab='\ud3d0\uc5c5\ub960 (%)',main='\ubd84\uae30\ubcc4 \ud3d0\uc5c5\ub960')
  lines(x,p$closure_pooled,col=orange,lwd=2); axis_quarters()
  legend('bottomleft',c('\uc0c1\uad8c\ubcc4 \ud3d0\uc5c5\ub960 \ub2e8\uc21c\ud3c9\uade0','\ud1b5\ud569 \ud3d0\uc5c5\ub960(\uc810\ud3ec \uaddc\ubaa8 \ubc18\uc601)'),
         col=c(blue,orange),lty=1,lwd=2,bty='n',cex=.85)
  hist(log10(na.omit(d$sales_won)/1e8),breaks=45,col=blue,border='white',xaxt='n',
       main='\uc0c1\uad8c\u00b7\ubd84\uae30 \uad00\uce21 \ub9e4\ucd9c\uc758 \ubd84\ud3ec',xlab='\ub9e4\ucd9c (\uc5b5\uc6d0, \ub85c\uadf8 \ub208\uae08)',ylab='\uad00\uce21\ud589 \uc218')
  axis(1,at=-3:4,labels=c('0.001','0.01','0.1','1','10','100','1,000','10,000'),cex.axis=.85)
  b <- barplot(types$sales_missing_pct,names.arg=unname(type_names[types$district_type]),
       col=blue,border=NA,ylim=c(0,max(types$sales_missing_pct)*1.25),ylab='\uacb0\uce21\ub960 (%)',main='\uc720\ud615\ubcc4 \ub9e4\ucd9c \uacb0\uce21\ub960')
  text(b,types$sales_missing_pct+.5,sprintf('%.2f%%',types$sales_missing_pct))
  mtext(sprintf('%s\uac1c \uc0c1\uad8c \u00d7 %s\ubd84\uae30: \uaddc\ubaa8 \ubd84\ud3ec\u00b7\ucd94\uc774\u00b7\ub9e4\ucd9c \uacb0\uce21',
        format(length(unique(d$district_id)),big.mark=','),length(unique(d$period))),outer=TRUE,side=3,line=1,cex=1.25)
  mtext('\uc790\ub8cc: regression_ml_master.csv | \ub300\uce58 \uc804 \uad00\uce21\uac12 \uae30\uc900 | \ub9e4\ucd9c\uc740 \uad00\uce21 \uc5c5\uc885\uc758 \uba85\ubaa9 \ucd94\uc815\ub9e4\ucd9c \ud569',
        outer=TRUE,side=1,line=0,cex=.8)
}
draw_summary()

# Compact, calculated readout. The original analysis files are not overwritten.
report <- c('\ud68c\uadc0\u00b7\uba38\uc2e0\ub7ec\ub2dd \uacf5\uc6a9 \ub370\uc774\ud130: R \uc7ac\ud604 \uacb0\uacfc',
  sprintf('\uc790\ub8cc: %s\ud589, %s\uc5f4, %s\uac1c \uc0c1\uad8c, %s\ubd84\uae30',nrow(d),ncol(d),nrow(coverage),length(unique(d$period))),
  sprintf('\ub9e4\ucd9c 20\ubd84\uae30 \uad00\uce21: %s\uac1c; \uc77c\ubd80 \uad00\uce21: %s\uac1c; \uc804 \uae30\uac04 \ubbf8\uad00\uce21: %s\uac1c',
    sum(coverage$sales_periods==20),sum(coverage$sales_periods>0&coverage$sales_periods<20),sum(coverage$sales_periods==0)),
  '\uc8fc\uc694 \ubcc0\uc218: \uad00\uce21\uac12\ub9cc \uc0ac\uc6a9; \ub9e4\ucd9c \uc5b5\uc6d0, \uc5c5\uc885 \ube44\uc911 %; \ud45c\ubcf8 \ud45c\uc900\ud3b8\ucc28, type=7 \ubd84\uc704\uc218.',
  '\ub9e4\ucd9c \ubcc0\ud654\uc728\uc740 100*ln(\ub2f9\uae30/\uc804\ub144\ub3d9\uae30)\uc774\uba70 \uc77c\ubc18\uc801\uc778 \ubc31\ubd84\uc728 \uc99d\uac00\uc728\uacfc \ub2e4\ub985\ub2c8\ub2e4.',
  '\uc5f0\ub3c4\ubcc4 \uc810\ud3ec \ubc0f \ub9e4\ucd9c \ud3c9\uade0\uc740 \uc0c1\uad8c\u00b7\ubd84\uae30 \uac12\uc758 \ud3c9\uade0\uc774\uba70 \uc5f0\uac04 \ud569\uacc4\uac00 \uc544\ub2d9\ub2c8\ub2e4.',
  '\uc804\ubd80 \uacb0\uce21\uc778 \ubcc0\uc218:',schema$variable[schema$missing==nrow(d)],
  capture.output(print(key,row.names=FALSE,digits=5)),
  '\uc5f0\ub3c4\ubcc4 \uc694\uc57d',capture.output(print(annual,row.names=FALSE,digits=5)),
  '\uc0c1\uad8c \uc720\ud615\ubcc4 \uc694\uc57d',capture.output(print(types,row.names=FALSE,digits=5)),
  '\ubd84\uc11d\uad6c\uac04\ubcc4 \ud45c\ubcf8',capture.output(print(splits,row.names=FALSE,digits=5)))
con <- file(file.path(out,'\uae30\uc220\ud1b5\uacc4_R_\uacb0\uacfc.txt'),'wt',encoding='UTF-8')
writeLines(report,con); close(con)
stopifnot(all(key$n+key$missing==nrow(d)),sum(annual$rows)==nrow(d),
          sum(coverage$sales_periods)==sum(!is.na(d$sales_won)))
print(key[c('variable','n','missing','mean','sd','median')],row.names=FALSE)
cat('\nSaved to:',out,'\n')
