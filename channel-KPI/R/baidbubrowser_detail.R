#######################################################
##  浏览器按天数据预测
#######################################################
require(xts);
require(forecast);
require(MASS);

#### 数据加载 #### 
print('Usage: work_root_path N:max_train_instance start_time end_time total_data topK_data output_file R.utils.file N:predict_distance');
args = commandArgs(TRUE);
## 测试
args=c('D:/project/github/predict/channel-KPI/',800,20140605,20140704,
       'data/baidubrowser_detail.txt','./data//baidubrowser_det.top10.txt',
       'baidubrowser_predict.log', 'R/utils.R', 30);
workpath <- args[1]
N.max <- args[2]
t.start <- args[3]
t.end <- args[4]
f.sum <- args[5]
f.topK <- args[6]
f.out <- args[7]
Rutils <- args[8]
N.predict <- args[9]

setwd(workpath);
source(Rutils);

t.df <- read.csv(datfile,header=TRUE);  # CSV已, 为分隔符
t.t = as.POSIXlt(strptime(t.df$date_id, '%Y%m%d')); 
t.tsa = xts(t.df$activate_count, t.t);
t.tse = xts(t.df$active_count, t.t);
t.top10 = read.csv(f.topK, header=T, sep='\t');

## 特征构造  
t.f_time = build_xts_feature(t.tsa);
t.f=as.data.frame(t.f_time);

t.range = paste(t.start, t.end, sep='/');
t.a.obs = coredata(t.tsa[t.range]); t.e.obs = coredata(t.tse[t.range]);
t.len = length(t.a.obs);
t.len.s = length(t.f[,1]);
if ( t.len > N.max) {
  t.a.obs = t.a.obs[(t.len-N.max+1):t.len];
  t.e.obs = t.e.obs[(t.len-N.max+1):t.len];
}
t.exp = t.f[(t.len.s-t.len+1):t.len.s,];  
t.f.t = t.t[(length(t.t)-t.len+1):t.len.s];
## 特征分解
t.exp_wday = build_matrix(t.exp$wday, idx=c(1:length(levels(as.factor(t.exp$wday)))), col_name='wday', f_intercept=F);
t.exp_mday = build_matrix(t.exp$mday, idx=c(1:10, 20:29), col_name='mday', f_intercept=F);
t.exp_mon = build_matrix(t.exp$mon, idx=c(1:length(levels(as.factor(t.exp$mon)))), col_name='mon', f_intercept=F);
t.exp_qtr = build_matrix(t.exp$qtr, idx=c(1:length(levels(as.factor(t.exp$qtr)))), col_name='qtr', f_intercept=F);
t.exp_yea = build_matrix(t.exp$yea, idx=c(1:length(levels(as.factor(t.exp$yea)))), col_name='yea', f_intercept=F);
t.exp$wday = NULL; t.exp = cbind(t.exp, as.data.frame(t.exp_wday)); 
t.exp$mday = NULL; t.exp = cbind(t.exp, as.data.frame(t.exp_mday));
t.exp$mon = NULL;  t.exp = cbind(t.exp, as.data.frame(t.exp_mon));
t.exp$qtr = NULL;  t.exp = cbind(t.exp, as.data.frame(t.exp_qtr));
t.exp$yea = NULL;  t.exp = cbind(t.exp, as.data.frame(t.exp_yea));
t.exp$yday = NULL; # 删除，overfit. 

######################### TOP10 子渠道处理 ####################### 
t.xts10 = list();
t.lm10 = list();
for (fi in levels(t.top10$attribute)) {
  t.fi = subset(t.top10, t.top10$attribute == fi);
  t.fi$attribute = NULL;
  t. = xts(t.fi$activate_count, as.POSIXlt(strptime(t.fi$date_id, '%Y%m%d')));
  t.t = index(t.);
  #前后空白区间用0填充，中间的NA用样条插值填充    
  t. = fill_xts(t., FUN_f=na.spline, range_t=paste(t.start,t.end,sep='/'), fillEmpty=0);  
  t. = t.[paste(t.start,t.end,sep='/')];
  # 对拓展与插值后的TOP渠道数据, 如果取值一直到t.end 则做最优lm拟合
  if (as.character(tail(t.t, n=1)) >= t.end) {  
    t.obs = coredata(t.)[,1]; 
    if(length(t.obs) > N.max) {
      t.obs = t.obs[(length(t.obs)-N.max+1):length(t.obs)];  # 约定最大长度为N.max
    }
    t.lm = lm(t.obs~ .-1, data=t.exp); # 使用共同的时间特征
    t.lm = stepAIC(t.lm, direction='both'); 
    t.lm = lm(formula(t.lm), data=t.exp, weights=c(1:length(t.obs))/length(t.obs));
    t.lm = list(t.lm); names(t.lm) = fi; 
  } else {
    t.lm = list(NA); names(t.lm) = fi;
  }
  t.lm10 = c(t.lm10, t.lm);
  
  t. = list(t.); names(t.) = fi; 
  t.xts10 = c(t.xts10, t.);
}

t.f.topK = as.data.frame(t.xts10);
t.exp = cbind(t.exp, t.f.topK);

#################### 总量数据拟合  ###############################
# TODO: 预测输入数据Ready
t.a.lm = lm(t.a.obs ~ .-1, data=t.exp);  # -m.idx_max-m.lm_x-m.lm_MMAE
t.a.rsd = residuals(t.a.lm);
t.a.step = stepAIC(t.a.lm, direction='both'); 
t.a.lm = lm(formula(t.a.step), data=t.exp, weigths=c(1:length(t.a.obs))/length(t.a.obs));

