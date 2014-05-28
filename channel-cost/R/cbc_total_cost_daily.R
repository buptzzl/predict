require(forecast);
#require(TSA);
require(xts);

rm(list=ls(all=TRUE));
# 将向量的因子做哑变量转换; @desc 异常时返回负数。
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
args = c('D:/project/channel-cost/','2014-01-01','2014-05-04','2014-12-31','doc/total.txt', 'doc/modeloutput_lm_0506.txt', '1.0', 'total');
workpath = args[1]
basetime = args[2] 
predict_start_date = args[3] # @FMT: 2014-01-01
predict_end_date = args[4]
datfile = args[5]
outfile = args[6]
partner_id = args[7]
partner_name = args[8]

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

######################### 预测数据准备 ###################################
t. = as.POSIXlt(seq(as.POSIXct(predict_start_date), len=(as.POSIXct(predict_end_date)-as.POSIXct(predict_start_date)), by='day'));
tsx.p_in = list(qtr=format(as.yearqtr(t.), '%q'), mon=t.$mon, yea = t.$year,
                  mday=t.$mday, wday=t.$wday, yday=t.$yday, 
                  idx=c(1:length(t.)),
                  dCJ=as.numeric(t. %in% index(t.[tsx.dCJ])),
                  dFST=as.numeric(t. %in% index(t.[tsx.dH]))
);

#### lm & ARIMA 
tsx.lm = lm(ts$s_active_num2 ~ yea+qtr+mday+wday+dFST+yea:mon+qtr:mday+mon:yday+wday:dFST+mday:yday+ yea:mon:mday+yea:mday:yday+qtr:mon:yday+mon:mday:yday-1, data=tsx.lm_in);
tsx.rd_lm = xts(residuals(tsx.lm), tsx.t);

# plot(tsx.lm); plot(tsx.rd_lm); qqnorm(tsx.rd_lm); qqline(tsx.rd_lm);
# outlier: tsx.t[c(1,90,454, 243)]; 
t = rep(0, length(tsx.t)); 
t = data.frame(outlier1=t,outlier90=t,outlier454=t,outlier243=t);
t$outlier1[1]=1; t$outlier90[90]=1;t$outlier454[454]=1;t$outlier243[243]=1;
tsx.aim_reg = t;
## 预测部分
t = rep(0, length(t.));  #c(433,440,447,454,455)
t = data.frame(outlier1=t,outlier90=t,outlier454=t,outlier243=t);
t$outlier243[which(t. == as.POSIXlt('2014-10-02'), arr.ind=T)]=1; #十一特殊时间点
tsx.aim_p_reg = t;

tsx.aim_rd = Arima(x=tsx.rd_lm, order=c(1,0,2), seasonal=list(order=c(0,1,1),period=7), fixed=c(NA,0,NA,NA,rep(NA,4)), lambda=BoxCox.lambda(tsx.rd_lm), xreg=tsx.aim_reg);


## 将预测结果写文件保存
test = data.frame(date=tsx.t, partner_id=rep(partner_id, length(tsx)),partner_name=rep(partner_name,length(tsx)),
                  predict=fitted(tsx.aim_rd)+fitted(tsx.lm)
                  #predict=coredata(forecast(tsx.aim_rd, xreg=tsx.aim_reg)$mean)+predict(tsx.lm,newdata=tsx.lm_in)
                  );
write.table(x=test, file=outfile, append=F, quote=F, row.names=F, col.names=T, fileEncoding='utf-8');
test = data.frame(date=t., partner_id=rep(partner_id, length(t.)),partner_name=rep(partner_name,length(t.)),
                  predict=coredata(forecast(tsx.aim_rd, xreg=tsx.aim_p_reg)$mean)+predict(tsx.lm,newdata=tsx.p_in));
write.table(x=test, file=outfile, append=T, quote=F, row.names=F, col.names=F, fileEncoding='utf-8');

## 预测
#tsx.p_dat = forecast(tsx.AIM, xreg=tsx.p_reg);
#plot(tsx.p_dat);