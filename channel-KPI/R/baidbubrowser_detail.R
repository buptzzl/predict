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
#args=c('D:/project/github/predict/channel-KPI/',800,'2013-01-01','2014-06-04',
#       'data/baidubrowser_detail.txt','./data/baidubrowser_det.top10.txt','./data/baidubrowser_active.top10.txt',
#       'R/utils.R', 30,'baidubrowser_predict.log');
workpath <- args[1]
N.max <- as.numeric(args[2])
t.start <- args[3]
t.end <- args[4]
f.sum <- args[5]
f.topK <- args[6]
#f.out <- args[7]
f.activeK <- args[7]
Rutils <- args[8]
N.predict <- as.numeric(args[9])
f.out  <- args[10]

setwd(workpath);
source(Rutils);

t.df <- read.csv(datfile,header=TRUE);  # CSV已, 为分隔符
t.t = as.POSIXlt(strptime(t.df$date_id, '%Y%m%d')); 
t.tsa = xts(t.df$activate_count, t.t);
t.tse = xts(t.df$active_count, t.t);
t.top10 = read.csv(f.topK, header=T, sep='\t');
t.active10 = read.csv(f.activeK, header=T, sep='\t');

t.range = paste(t.start, t.end, sep='/');
t.a.obs = coredata(t.tsa[t.range]); t.e.obs = coredata(t.tse[t.range]);
t.len = length(t.a.obs);
if (t.len > N.max) {
  t.a.obs = t.a.obs[(t.len-N.max+1):t.len];
  t.e.obs = t.e.obs[(t.len-N.max+1):t.len];
}

#### 特征构造 #### 
## Part1: 构造训练数据
t.f_time = build_xts_feature(t.tsa[t.range]);
t.f=as.data.frame(t.f_time);
t.len.s = length(t.f[,1]);
t.exp = t.f;
if (t.len.s > N.max) {
  t.exp = t.f[(t.len.s-N.max+1):t.len.s,];  
}
t.len.e = dim(t.exp)[1];
## Part2: 预测特征数据
p.t = seq(as.POSIXlt(t.end),len=(N.predict+1), by='day');
p.t = p.t[2:length(p.t)];
p.xts = xts(rep(NA, length(p.t)), p.t);
p.exp = build_xts_feature(p.xts);
p.exp = as.data.frame(p.exp);
p.exp$idx = p.exp$idx+t.exp$idx[length(t.exp$idx)];
## Part3: 统一进行时间特征分解
f.exp = rbind(t.exp, p.exp); 
feature_decompose = function (t.exp) {
  t.exp_wday = build_matrix(t.exp$wday, idx=levels(as.factor(t.exp$wday)), col_name='wday', f_intercept=F);
  t.exp_mday = build_matrix(t.exp$mday, idx=levels(as.factor(t.exp$mday))[c(1:10, 20:29)], col_name='mday', f_intercept=F);
  t.exp_mon = build_matrix(t.exp$mon, idx=levels(as.factor(t.exp$mon)), col_name='mon', f_intercept=F);
  t.exp_qtr = build_matrix(t.exp$qtr, idx=levels(as.factor(t.exp$qtr)), col_name='qtr', f_intercept=F);
  t.exp_yea = build_matrix(t.exp$yea, idx=levels(as.factor(t.exp$yea)), col_name='yea', f_intercept=F); #c(1:length(levels(as.factor(t.exp$yea))))
  t.exp$wday = NULL; t.exp = cbind(t.exp, as.data.frame(t.exp_wday)); 
  t.exp$mday = NULL; t.exp = cbind(t.exp, as.data.frame(t.exp_mday));
  t.exp$mon = NULL;  t.exp = cbind(t.exp, as.data.frame(t.exp_mon));
  t.exp$qtr = NULL;  t.exp = cbind(t.exp, as.data.frame(t.exp_qtr));
  t.exp$yea = NULL;  t.exp = cbind(t.exp, as.data.frame(t.exp_yea));
  t.exp$yday = NULL; # 删除，overfit. 
  return (t.exp);
}
f.exp = feature_decompose(f.exp);
t.exp = f.exp[1:t.len.e,]; p.exp = f.exp[(t.len.e+1):dim(f.exp)[1],]; 

#####################
#### TOP10 子渠道处理  
detail_model = function(t.exp, p.exp, t.range, t.a.obs, t.top10) {
  t.xts10 = list();
  t.lm10 = list();
  p.lm10 = list();
  for (fi in levels(t.top10$attribute)) {
    t.fi = subset(t.top10, t.top10$attribute == fi);
    t.fi$attribute = NULL;
    t. = xts(t.fi$activate_count, as.POSIXlt(strptime(t.fi$date_id, '%Y%m%d')));
    #stop("for test");
    t.t = index(t.);
    # 两个边界间取更小者
    tmp.start = as.character(head(index(t.), n=1));  tmp.end = as.character(tail(index(t.), n=1));
    if (tmp.start < t.start) {
      tmp.start = t.start;
    }
    if (t.end < tmp.end) {
      tmp.end = t.end;
    }
    tmp.range = paste(tmp.start, tmp.end, sep='/');
    # 训练特征： 前后空白区间用0填充，中间的NA用样条插值填充    
    t.K = fill_xts(t., FUN_f=na.spline, range_t=t.range, fillEmpty=0);  
    t. = list(t.K[t.range]); names(t.) = fi; 
    t.xts10 = c(t.xts10, t.);
    
    # 仅使用非0的（后半段）数据拟合lm用于预测N.predict个特征值
    t.K = t.K[tmp.range];
    # 对拓展与插值后的TOP渠道数据, 如果取值一直到t.end 则做最优lm拟合
    if (as.character(tail(t.t, n=1)) >= t.end) {  
      t.obs = coredata(t.K)[,1]; 
      if(length(t.obs) > N.max) {
        t.obs = t.obs[(length(t.obs)-N.max+1):length(t.obs)];  # 约定最大长度为N.max
      } 
      if(length(t.obs) < t.len.e) {
        k.exp = t.exp[(t.len.e+1-length(t.obs)):t.len.e,];
      } else {
        k.exp = t.exp; 
      }
      t.lm = lm(t.obs~ .-1, data=k.exp); # 使用共同的时间特征
      t.lm = stepAIC(t.lm, direction='both'); 
      t.lm = lm(formula(t.lm), data=k.exp, weights=c(1:length(t.obs))/length(t.obs));
      t.rsd = residuals(t.lm); 
      p.lm = predict(t.lm, p.exp);
      p.lm = p.lm + t.rsd[(length(t.rsd)-N.predict+1):length(t.rsd)];
      # TEST 
      plot(t.obs, type='l'); lines(fitted(t.lm), col='red');  legend(x=0,y=max(t.obs), fi);
      
      p.lm = list(p.lm); names(p.lm) = fi; 
      t.lm = list(t.lm); names(t.lm) = fi; 
    } else {
      t.lm = list(NA); names(t.lm) = fi;
      p.lm = list(rep(0, N.predict));  names(p.lm) = fi;
    }
    t.lm10 = c(t.lm10, t.lm);
    p.lm10 = c(p.lm10, p.lm); 
  }
  ## 总量的训练特征
  t.f10 = as.data.frame(t.xts10);
  tot.exp = cbind(t.exp, t.f10);
  ## 总量的预测特征
  p.f10 = as.data.frame(p.lm10);
  p.tot.exp = cbind(p.exp, p.f10);
  
  #### 总量数据拟合  ########
  t.a.lm = lm(t.a.obs ~ .-1, data=tot.exp);  # -m.idx_max-m.lm_x-m.lm_MMAE
  t.a.rsd = residuals(t.a.lm);
  t.a.step = stepAIC(t.a.lm, direction='both'); 
  t.a.lm = lm(formula(t.a.step), data=tot.exp, weigths=c(1:length(t.a.obs))/length(t.a.obs));
  p.res = predict(t.a.lm, p.tot.exp);
  
  plot(t.a.obs, type='l'); lines(fitted(t.a.lm), col='red'); 
  lines(x=(length(t.a.obs)+1):(length(t.a.obs)+N.predict),y=p.res, col='green');
  legend(x=0, y=max(t.a.obs), 'black: raw\n red: fiteed.\n green: predict.');

  return(p.res);
}

activate.res = detail_model(t.exp, p.exp, t.range, t.a.obs, t.top10);
active.res =detail_model(t.exp, p.exp, t.range, t.e.obs, t.active10);
res = data.frame(date_id=strftime(p.t, '%Y%m%d'), activate_count=activate.res, active_count=active.res);
write.table(x=res, file=f.out, quote=F, row.names=F, col.names=F, sep='\t', fileEncoding='utf-8');

