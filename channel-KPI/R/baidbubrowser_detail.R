#######################################################
##  浏览器数据分析  
#######################################################

## 数据加载
args=c('D:/project/github/predict/channel-KPI/',20140604,20140605,20140704,
       'data/baidubrowser_detail.txt','data/baidubox_f1.txt','baidubrowser_predict.log')
workpath <- args[1]
basetime <- args[2]
predict_start_date <- args[3]
predict_end_date <- args[4]
datfile <- args[5]
feafile <- args[6]
outpath <- args[7]

t.df <- read.csv(datfile,header=TRUE);  # CSV已, 为分隔符
t.t = as.POSIXlt(strptime(t.df$date_id, '%Y%m%d')); 
t.tsa = xts(t.df$activate_count, t.t);
t.tse = xts(t.df$active_count, t.t);

# BoxCox test.
BoxCox.lambda(t.tsa); BoxCox.lambda(t.tse);

#### LM
## 1. 特征构造：由于预测一个月，构造前1月的特征
#t.a.mx_mn = period.apply(t.tsa, INDEX=endpoints(t.t, on='months'), FUN=max, TRUE) - period.apply(t.tsa, INDEX=endpoints(t.t, on='months'), FUN=sum, TRUE);
idx_f = function(x, ...) {  
  # 基于指定的函数抽取对应的值，再返回最后一个最值的索引
  if (is.na(x) || length(x) == 0)
    return 0;
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

t.tsa = t.tsa['2013/2014-05']; t.tse = t.tse['2013/2014-05']; # 有效数据区间[2013, 当前预测前]  


