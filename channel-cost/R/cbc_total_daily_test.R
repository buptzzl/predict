require(forecast);
require(TSA);
require(xts);

rm(list=ls(all=TRUE));

#t = build_matrix(tsx.lm_in$qtr,idx=c(1,3),col_name='qtr',f_intercept=F)
# 将向量的因子做哑变量转换; @desc 异常时返回负数 
build_matrix = function(x, idx, col_name='V', f_intercept=FALSE, f_reverser=FALSE, f_fill=TRUE) {
  if (f_intercept==TRUE) {
    t. = model.matrix(~as.factor(x));
  } else {
    t. = model.matrix(~as.factor(x)-1);
  }
  if (length(t.) == 0) {
    return (-1);
  }
  t.idx = sub('as.factor\\(x\\)', col_name, names(t.[1,]));
  colnames(t.) = t.idx;
  idx = as.character(sort(unique(idx))); 
  idx_name = paste(col_name,idx,sep='');
  
  if (f_reverser==TRUE) {
    idx_name = t.idx[!t.idx %in% idx_name];
  } else {
    idx_name = t.idx[t.idx %in% idx_name];
  }
  # 对缺失部分 填充0补全
  if (f_fill==TRUE && f_reverser==FALSE && length(idx_name) != length(idx)) {
    idx_fill = paste(col_name,idx,sep='');
    d_empty = rep(0, length(t.[,1]));
    for(it in idx_fill) {
      if(!it %in% t.idx) {
        #t. = cbind(t., assign(it,d_empty));
        t. = cbind(t., d_empty); colnames(t.) = c(colnames(t.)[1:dim(t.)[2]-1],it);
        idx_name = c(idx_name, it);
      }
    }
  }
  res = t.[,idx_name];
  if (length(idx_name)==1) {
    res = as.matrix(res);
  }
  return (res);
}

# Load Arguments
args = commandArgs(TRUE);
#### TEST only.  ####
args = c('D:/project/github/predict/channel-cost/','2014-01-01','2014-05-04','2014-12-31','data/total.txt', 'data/modeloutput_lm_0506.txt');
workpath = args[1]
basetime = args[2] 
predict_start_date = args[3] # @FMT: 2014-01-01
predict_end_date = args[4]
datfile = args[5]
outfile = args[6]

setwd(workpath);
## 数据准备
ts = read.csv(file=datfile, header=T, sep='\t');
if (!'date' %in% names(ts) || !'s_active_num2' %in% names(ts)) {
  print("[ERROR] data header miss.");
  quit(save=F,status=1);
}
tsx = xts(ts$s_active_num2, as.POSIXlt(ts$date));
tsx.t = as.POSIXlt(ts$date); # tsx.d = ts$s_active_num2;

# 假日： 元旦、清明、劳动、端午、中秋、国庆; 后2天。
tsx.dH = c('2013-01-01/2013-01-03', '2013-04-04/2013-04-06', '2013-04-29/2013-05-05','2013-06-10/2013-06-12','2013-09-19/2013-09-21','2013-10-01/2013-10-07',
           '2014-01-01/2014-01-03', '2014-04-05/2014-04-07', '2014-05-01/2014-05-03','2014-06-02/2014-06-04','2014-09-08/2014-09-10','2014-10-01/2014-10-07'
);
# 春节 @Fix 2014.
tsx.dCJ = c('2013-02-09/2013-02-15','2014-02-02/2014-02-05'); 

tsx.lm_in = list(qtr=format(as.yearqtr(tsx.t), '%q'), mon=tsx.t$mon, yea = tsx.t$year,
                 mday=tsx.t$mday, wday=tsx.t$wday, yday=tsx.t$yday, 
                 idx=c(1:length(tsx.t)), 
                 dCJ=as.numeric(tsx.t %in% index(tsx[tsx.dCJ])),
                 dFST=as.numeric(tsx.t %in% index(tsx[tsx.dH]))
);

tsx.reg = build_matrix(x=tsx.lm_in$mday,idx=c(1:3,26,30),col_name='mday',f_intercept=F); 
tsx.reg = cbind(tsx.reg, FST=tsx.lm_in$dFST, CJ=tsx.lm_in$dCJ);
#t = build_matrix(tsx.lm_in$qtr,idx=c(1,3),col_name='qtr',f_intercept=F); tsx.reg = cbind(tsx.reg, t); 
t = build_matrix(tsx.lm_in$mon,idx=c(1,3),col_name='mon',f_intercept=F); tsx.reg = cbind(tsx.reg, t); 
## outliers
t = rep(0, length(tsx.t));
#t = data.frame(outlier9=t, outlier10=t, outlier29=t, outlier91=t, outlier243=t, outlier_52=t, outlier_53=t);
#t$outlier9[9]=1; t$outlier10[10]=1; t$outlier29[29]=1; t$outlier91[91]=1; t$outlier243[243]=1; t$outlier_52[455]=1; t$outlier_53[456]=1; 
t = data.frame(outlier1=t,outlier90=t,outlier454=t,outlier243=t);
t$outlier1[1]=1; t$outlier90[90]=1;t$outlier454[454]=1;t$outlier243[243]=1;
tsx.reg = cbind(tsx.reg, t); 

######################### 预测数据准备 ###################################
t. = as.POSIXlt(seq(as.POSIXct(predict_start_date), len=(as.POSIXct(predict_end_date)-as.POSIXct(predict_start_date)), by='day'));
tsx.p_in = list(qtr=format(as.yearqtr(t.), '%q'), mon=t.$mon, yea = t.$year,
                mday=t.$mday, wday=t.$wday, yday=t.$yday, 
                idx=c(1:length(t.)),
                dCJ=as.numeric(t. %in% index(t.[tsx.dCJ])),
                dFST=as.numeric(t. %in% index(t.[tsx.dH]))
);
tsx.p_reg = build_matrix(x=tsx.p_in$mday,idx=c(1:3,26,30),col_name='mday',f_intercept=F); 
tsx.p_reg = cbind(tsx.p_reg, FST=tsx.p_in$dFST,CJ=tsx.p_in$dCJ);
#t = build_matrix(tsx.p_in$qtr,idx=c(1,3),col_name='qtr',f_intercept=F); tsx.p_reg = cbind(tsx.p_reg, t);
t = build_matrix(tsx.p_in$mon,idx=c(1,3),col_name='mon',f_intercept=F); tsx.p_reg = cbind(tsx.p_reg, t);

##outliers 
t = rep(0, length(t.));
#t = data.frame(outlier9=t, outlier10=t, outlier29=t, outlier91=t, outlier243=t, outlier_52=t, outlier_53=t);
#t$outlier243[which(t. == as.POSIXlt('2014-10-02'), arr.ind=T)] = 1; #十一特殊时间点
t = data.frame(outlier1=t,outlier90=t,outlier454=t,outlier243=t);
t$outlier243[which(t. == as.POSIXlt('2014-10-02'), arr.ind=T)]=1; #十一特殊时间点
tsx.p_reg = cbind(tsx.p_reg, t); # 按原来的顺序构造预测参数

#### lm & ARIMA 
tsx.lm = lm(ts$s_active_num2 ~ yea+qtr+mday+wday+dFST+yea:mon+qtr:mday+mon:yday+wday:dFST+mday:yday+ yea:mon:mday+yea:mday:yday+qtr:mon:yday+mon:mday:yday-1, data=tsx.lm_in);
tsx.rd_lm = xts(residuals(tsx.lm), tsx.t);
# plot(tsx.lm); plot(tsx.rd_lm); qqnorm(tsx.rd_lm); qqline(tsx.rd_lm);
# outlier: tsx.t[c(1,90,454, 243)]; 

t = rep(0, length(tsx.t));  #c(433,440,447,454,455)
t = data.frame(outlier1=t,outlier90=t,outlier454=t,outlier243=t);
t$outlier1[1]=1; t$outlier90[90]=1;t$outlier454[454]=1;t$outlier243[243]=1;
tsx.aim_reg = t;
## 预测部分
t = rep(0, length(t.));  #c(433,440,447,454,455)
t = data.frame(outlier1=t,outlier90=t,outlier454=t,outlier243=t);
t$outlier243[which(t. == as.POSIXlt('2014-10-02'), arr.ind=T)]=1; #十一特殊时间点
tsx.aim_p_reg = t;

tsx.aim_rd = Arima(x=tsx.rd_lm, order=c(1,0,2), seasonal=list(order=c(0,1,1),period=7), 
                   fixed=c(NA,0,NA,NA,rep(NA,4)), xreg=tsx.aim_reg, lambda=BoxCox.lambda(tsx.rd_lm));
## 判断对历史数据的拟合程度
# t$outlier1[1]=0; t$outlier90[90]=0;t$outlier454[454]=0;t$outlier243[243]=0;
# plot(tsx); lines(x=tsx.t, y=fitted(tsx.aim_rd, xreg=t)+fitted(tsx.lm), col='red');
test = data.frame(date_data=tsx, 
                  fitted_history=fitted(tsx.aim_rd)+fitted(tsx.lm), 
                  predict_history=forecast(tsx.aim_rd, xreg=tsx.aim_reg)$mean+predict(tsx.lm,newdata=tsx.lm_in));
write.table(x=test, file=outfile, append=F, fileEncoding='utf-8');
test = data.frame(date=t., predict_future=forecast(tsx.aim_rd, xreg=tsx.aim_p_reg)$mean+predict(tsx.lm,newdata=tsx.p_in));
write.table(x=test, file=outfile, append=T, fileEncoding='utf-8');


## 修改拟合时间窗，判断对已有数据的预测效果
N = 458;
tsx.lm_in_ = data.frame(qtr=head(tsx.lm_in$qtr,N),mon=head(tsx.lm_in$mon,N),mday=head(tsx.lm_in$mday,N),wday=head(tsx.lm_in$wday,N),yday=head(tsx.lm_in$yday,N),idx=head(tsx.lm_in$idx,N),dCJ=head(tsx.lm_in$dCJ,N),dFST=head(tsx.lm_in$dFST,N),yea=head(tsx.lm_in$yea,N));
t$outlier1[1]=1; t$outlier90[90]=1;t$outlier454[454]=1;t$outlier243[243]=1;
t_ = data.frame(outlier1=head(t$outlier1, N),outlier90=head(t$outlier90, N),outlier243=head(t$outlier243, N)); #outlier454=head(t$outlier454, N));
tsx.lm_ = lm(head(ts$s_active_num2,N) ~ yea+qtr+mday+wday+dFST+yea:mon+qtr:mday+mon:yday+wday:dFST+mday:yday+ yea:mon:mday+yea:mday:yday+qtr:mon:yday+mon:mday:yday-1, data=tsx.lm_in_);
tsx.rd_lm_ = residuals(tsx.lm_);
tsx.aim_rd_ = Arima(x=tsx.rd_lm_, order=c(1,0,2), seasonal=list(order=c(0,1,1),period=7), fixed=c(NA,0,NA,NA,rep(NA,3)), lambda=BoxCox.lambda(tsx.rd_lm_), xreg=t_);
t_ = data.frame(outlier1=t$outlier1,outlier90=t$outlier90,outlier243=t$outlier243);
t_$outlier1=0; t_$outlier90=0; t_$outlier243=0; 
plot(tsx); lines(x=tsx.t, y=forecast(tsx.aim_rd_, xreg=t_, level=95)$mean+predict(tsx.lm_,newdata=tsx.lm_in,level=0.95), col='red');


## 预测
#tsx.p_dat = forecast(tsx.AIM, xreg=tsx.p_reg);
#plot(tsx.p_dat);