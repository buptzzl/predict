#######################################################
##  浏览器按天数据预测
#######################################################
require(xts);
require(forecast);
require(MASS);

## 数据加载
args=c('D:/project/github/predict/channel-KPI/',20140604,20140605,20140704,
       'data/baidubrowser_detail.txt','data/baidubox_f1.txt',
       'baidubrowser_predict.log', 'R/utils.R')
workpath <- args[1]
basetime <- args[2]
predict_start_date <- args[3]
predict_end_date <- args[4]
datfile <- args[5]
feafile <- args[6]
outpath <- args[7]
Rutils <- args[8]

setwd(workpath);

t.df <- read.csv(datfile,header=TRUE);  # CSV已, 为分隔符
t.t = as.POSIXlt(strptime(t.df$date_id, '%Y%m%d')); 
t.tsa = xts(t.df$activate_count, t.t);
t.tse = xts(t.df$active_count, t.t);

# BoxCox test.
BoxCox.lambda(t.tsa); BoxCox.lambda(t.tse);

#### LM
## @discard 1. 特征构造：由于预测一个月，构造前1月的特征
#t.a.mx_mn = period.apply(t.tsa, INDEX=endpoints(t.t, on='months'), FUN=max, TRUE) - period.apply(t.tsa, INDEX=endpoints(t.t, on='months'), FUN=sum, TRUE);
idx_f = function(x, ...) {  
  # 基于指定的函数抽取对应的值，再返回最后一个最值的索引
  if (is.na(x) || length(x) == 0)
    return (0);
  FUN = match.fun(...);
  idx = last(which(x == FUN(x))); # 保证返回最后1个目标值的下标 
  return (idx);
}
t.a.i_mx = period.apply(t.tsa, INDEX=endpoints(t.t, on='months'), FUN=idx_f, max); 
t.a.i_mn = period.apply(t.tsa, INDEX=endpoints(t.t, on='months'), FUN=idx_f, min); 
par_lm = function(x) {
  if (is.na(x) || length(x) == 0) 
    return (0,0,0); 
  xi = c(1:length(x));
  lm_ = lm(x~xi);
  return (c(lm_$coefficients, MMAE=sum(abs(lm_$residuals))/length(x)));
}
t.a.lm = period.apply(t.tsa, INDEX=endpoints(t.t, on='months'), FUN=par_lm); # 3维特征
t.a.ave = period.apply(t.tsa, INDEX=endpoints(t.t, on='months'), FUN=mean);
t.a.var = period.apply(t.tsa, INDEX=endpoints(t.t, on='months'), FUN=var);
# 前1月的特征合并（暂无归一化）
t.a.f_m. = cbind(t.a.i_mx, t.a.i_mn, t.a.lm, t.a.ave, t.a.var);
t.an = c('m.idx_max', 'm.idx_min', 'm.lm_intercept', 'm.lm_x', 'm.lm_MMAE', 'm.ave','m.var');
names(t.a.f_m.) = t.an; 
t. = list(rep(NA, length(t.t)));
t.a.f_m = c(t., t., t., t., t., t., t.);
t.a.f_m = as.data.frame(t.a.f_m); names(t.a.f_m) = names(t.a.f_m.);
t.a.f_m = xts(t.a.f_m, t.t); 
for(y in 1:length(t.a.ave)) {  
  # 将当前月份的特征填充到下一个月的所有数据中
  ti = strftime(as.Date(time(t.a.f_m.[y])) + 1, '%Y-%m');
  t.a.f_m[ti, t.an] = t.a.f_m.[y, t.an];
};

source(Rutils);
t.f_time = build_xts_feature(t.tsa);
t.1=as.data.frame(t.a.f_m); t.2=as.data.frame(t.f_time);
t.a.f = cbind(t.1, t.2);

t.a.obs = coredata(t.tsa['2013/2014']); # 有效数据区间[2013, 当前预测前]  
t.len = length(t.a.obs);
t.len.s = length(t.a.f[,1]);
if ( t.len > 800) 
  t.a.obs = t.a.obs[(t.len-800+1):t.len];
t.a.exp = t.a.f[(t.len.s-t.len+1):t.len.s,];  
t.a.t = t.t[(length(t.t)-t.len+1):t.len.s];

## 特征分解
t.a.exp_wday = build_matrix(t.a.exp$wday, idx=c(1:length(levels(as.factor(t.a.exp$wday)))), col_name='wday', f_intercept=F);
t.a.exp_mday = build_matrix(t.a.exp$mday, idx=c(1:10, 20:29), col_name='mday', f_intercept=F);
t.a.exp_mon = build_matrix(t.a.exp$mon, idx=c(1:length(levels(as.factor(t.a.exp$mon)))), col_name='mon', f_intercept=F);
t.a.exp_qtr = build_matrix(t.a.exp$qtr, idx=c(1:length(levels(as.factor(t.a.exp$qtr)))), col_name='qtr', f_intercept=F);
t.a.exp_yea = build_matrix(t.a.exp$yea, idx=c(1:length(levels(as.factor(t.a.exp$yea)))), col_name='yea', f_intercept=F);
t.a.exp$wday = NULL; t.a.exp = cbind(t.a.exp, as.data.frame(t.a.exp_wday)); 
t.a.exp$mday = NULL; t.a.exp = cbind(t.a.exp, as.data.frame(t.a.exp_mday));
t.a.exp$mon = NULL;  t.a.exp = cbind(t.a.exp, as.data.frame(t.a.exp_mon));
t.a.exp$qtr = NULL;  t.a.exp = cbind(t.a.exp, as.data.frame(t.a.exp_qtr));
t.a.exp$yea = NULL;  t.a.exp = cbind(t.a.exp, as.data.frame(t.a.exp_yea));
# 20140423进入的新渠道异常值插入
#t.idx = which(coredata(t.tsa['2014-04-23'])[,1] ==t.a.obs[,1], arr.ind=T);
#t.abn.a = list(abn.add = c(rep(0, t.idx-1), rep(350061, length(t.a.obs)-t.idx+1)));  # 基于加入后数据做平均得到
#t.a.exp = cbind(t.a.exp, t.abn.a);

######################### TOP10 子渠道处理 ####################### 
## 预测时，如果当前渠道已经停止则不对其建模。

t.begin = '2013-01-01'; t.end = '2014-06-04';  # 训练的起止时间点
t.top10 = read.csv('./data//baidubrowser_det.top10.txt', header=T, sep='\t');
t.xts10 = list();
t.lm10 = list();
for (fi in levels(t.top10$attribute)) {
  t.fi = subset(t.top10, t.top10$attribute == fi);
  t.fi$attribute = NULL;
  t. = xts(t.fi$activate_count, as.POSIXlt(strptime(t.fi$date_id, '%Y%m%d')));
  t.t = index(t.);
  #前后空白区间用0填充，中间的NA用样条插值填充    
  t. = fill_xts(t., FUN_f=na.spline, range_t=paste(t.begin,t.end,sep='/'), fillEmpty=0);  
  t. = t.[paste(t.begin,t.end,sep='/')];
  # 对拓展与插值后的TOP渠道数据, 如果取值一直到t.end 则做最优lm拟合
  if (as.character(tail(t.t, n=1)) >= t.end) {  
    t.obs = coredata(t.)[,1]; 
    if(length(t.obs) > 800) {
      t.obs = t.obs[(length(t.obs)-800+1):length(t.obs)];  # 约定最大长度为800
    }
    t.lm = lm(t.obs~ .-1, data=t.a.exp); # 使用共同的时间特征
    t.lm = stepAIC(t.lm, direction='both'); 
    t.lm = lm(formula(t.lm), data=t.a.exp, weights=c(1:length(t.obs))/length(t.obs));
    t.lm = list(t.lm); names(t.lm) = fi; 
  } else {
    t.lm = list(NA); names(t.lm) = fi;
  }
  t.lm10 = c(t.lm10, t.lm);
  
  t. = list(t.); names(t.) = fi; 
  t.xts10 = c(t.xts10, t.);
}

t.a.topK = as.data.frame(t.xts10);
t.a.exp = cbind(t.a.exp, t.a.topK);

#################### 总量数据拟合  ###############################
## 1 线性回归
# paste(names(t.a.exp), collapse='+');  # 返回全部因子构成的回归式
# 基于未组合的字符串集合  
t.a.lm = lm(t.a.obs ~ .-1, data=t.a.exp);  # -m.idx_max-m.lm_x-m.lm_MMAE
t.a.rsd = residuals(t.a.lm);
t.a.step = stepAIC(t.a.lm, direction='both'); 
# t.a.step$anova;
t.a.lm = lm(formula(t.a.step), data=t.a.exp, weigths=c(1:length(t.a.obs))/length(t.a.obs));

## 特征实验
# [31200,0.9915,960.3] 全部特征—1
# [30370,0.9919,2128] BEST-AIC:10763.32; N.args=56; 

# [31300,0.9914,1002] 全部特征 -1-m.idx_max-m.lm_x-m.lm_MMAE-yday
# [30500,0.9918,2345] BEST-AIC:10764.78; 

# [31010,0.9916,1250] .-1-m.idx_max-m.lm_x-m.lm_MMAE-yday-mday11-mday12-mday13-mday14-mday15-mday16-mday17-mday18-mday19-mday29-mday30-mday31
#### [30640,0.9918,2023] BETS-ACI:10773.43; N.args=31; # 构造每月第几天的特征只考虑1-7, 22-29; 

# [30080,0.9921,525.8] m.idx_min + m.lm_intercept + m.ave + m.var + idx + dQM + d51 + dDM + dMid + d10 + dSum + dCJ + (wday1 + wday2 + wday3 + wday4 + wday5 + wday6 + wday7 + mday1 + mday2 + mday3 + mon1 + mon2 + mon3 + mon4 + mon5 + mon6 + mon8 + mon9 + mon10 + mon11 )^2 + qtr1 - 1
# [28610,0.9928,1092] BEST-AIC: 10733.36, N.args=66; 
# BEST-AIC 去除wday*组合 21个: [30310,0.99,1425]; 去除mday* 16个: [30220,0.99,1265];

# [30300,0.99,1284] m.idx_min + m.lm_intercept + m.ave + m.var + idx + dQM + d51 + dDM + dMid + d10 + dSum + dCJ + (wday1 + wday2 + wday3 + wday4 + wday5 + wday6 + wday7 + mday1 + mday2 + mday3)^2 + mon1 + mon2 + mon3 + mon4 + mon5 + mon6 + mon8 + mon9 + mon10 + mon11 + qtr1 - 1
# [30100,0.99,1711] BETS-ACI: 10761.36; N.args=38;
