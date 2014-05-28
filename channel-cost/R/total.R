require(forecast);
require(TSA);
require(xts);

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
        t. = cbind(t., assign(it,d_empty));#t. = cbind(t., print(it)=d_empty);
        #t. = cbind(t., d_empty); colnames(t.) = c(colnames(t.)[1:length(t.)-1],it);
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

# 加载源数据
ts = read.csv(file='./doc/total.txt', header=T, sep='\t');
# 构造ts 对象
tsx = xts(ts$s_active_num2, as.POSIXlt(ts$date));
tsx.t = as.POSIXlt(ts$date); tsx.d = ts$s_active_num2;
# 假日信息都往后滞后若干天;
# @Discard-TODO: 将假日时间段中在相邻日期拟合直线修正后的值，进行分布拟合，将对应日期的值赋为对应分布的值
tsx.h = c('2013-01-01', '2013-02-09/2013-02-15', '2013-04-04', '2013-04-29/2013-05-01','2013-06-10', '2013-09-19','2013-10-01/2013-10-07',
          '2014-01-01', '2014-01-31/2014-02-06','2014-04-05/2014-04-07', '2014-05-01/2014-05-03','2014-06-02','2014-09-08','2014-10-01/2014-10-07')
# 作图{查看数据分布}
plot(tsx); plot(log(tsx)); plot(diff(tsx));
points(tsx[tsx.h])

  # 滑动平均，判断趋势项
  lines(tsx.t, rollapply(tsx.d, 2, mean, partial=T), col='red')
  lines(tsx.t, rollapply(tsx.d, 7, mean, partial=T), col='yellow')
  lines(tsx.t, rollapply(tsx.d, 30, mean, partial=T), col='green')
##################################################################
# 构造特征
tsx.lm_in = list(y=tsx.d, qtr=format(as.yearqtr(tsx.t), '%q'), mon=tsx.t$mon, yea=tsx.t$year, 
                 mday=tsx.t$mday, wday=tsx.t$wday, yday=tsx.t$yday, 
                 idx=c(1:length(tsx.d)), 
                 d51=as.numeric(tsx.t %in% index(tsx[c('2013-04-29/2013-05-03','2014-04-29/2014-05-03')])),
                 d10=as.numeric(tsx.t %in% index(tsx[c('2013-10-01/2013-10-07','2014-10-01/2014-10-07')])),
                 dCJ=as.numeric(tsx.t %in% index(tsx[c('2013-02-09/2013-02-15', '2014-01-31/2014-02-06')])),
                 # 假期前后修正3天
                 dFST=as.numeric(tsx.t %in% index(tsx[c('2013-04-04/2013-04-07','2013-06-10/2013-06-13','2013-09-19/2013-09-21',
                                                        '2014-01-01/2014-01-03','2014-04-05/2014-04-08','2014-06-02/2014-06-05','2014-09-08/2014-09-11')]))
)
# 基于线性回归捕捉趋势项 (no arima)
tsx.lm1 = lm(y~yea+qtr+mon, data=tsx.lm_in)
  summary(tsx.lm1);  plot(rstudent(tsx.lm1)); 
  # 趋势项模型修正：
#lm1: y~yea+qtr+mon-1; 
#lm2:[47260,.992] y~yea+qtr+mon+qtr:mon-1; 
#lm3:[41580,.9939] y ~ yea + qtr + mon + qtr:mon + idx + I(idx^2) - 1; 
#lm4:[42950,.9935] y ~ yea + qtr + mon + wday + mday + yday + qtr:mon + yday:yea + qtr:yea - 1
#lm5:[40010,.877]: y ~ yea + qtr + mon + qtr:yea + qtr:mon + wday + mday + yday + yea:yday +yea:mday+ yea:mon:mday
#lm6:[37360,.8901]: y ~ yea + qtr + mon + qtr:yea + qtr:mon + wday + mday + yday + yea:yday +yea:mday+ yea:mon:mday + d51+d10+d2013+d2014
#[34310,.9073]  yea+qtr+mday+yea:qtr+yea:mon+yea:mday+yea:yday+qtr:mday+mon:yday+ wday:d10+mday:yday + yea:mon:mday+yea:mday:yday+qtr:mon:yday+qtr:mday:yday+mon:mday:yday+wday:mday:d10
#[28840,0.9365] yea+qtr+mday+dFST+yea:qtr+yea:mon+qtr:mday+mon:yday+wday:dFST+mday:yday+ yea:mon:mday+yea:mday:yday+qtr:mon:yday+qtr:mday:yday+mon:mday:yday
#[30230,0.9968] yea+qtr+mday+wday+dFST+yea:mon+qtr:mday+mon:yday+wday:dFST+mday:yday+ yea:mon:mday+yea:mday:yday+qtr:mon:yday+mon:mday:yday-1
#### 对比拟合的残差曲线
  plot(rstudent(tsx.lm),type='l');
  lines(rstudent(tsx.lm1),col='red');

  # 预测结果，含：预测区间 & 置信区间
  #tsx.lm_res = data.frame(prediction=predict(tsx.lm1, interval='prediction', newdata=c()),
  #                        confidence=predict(tsx.lm1, interval='confidence')); 
  #plot(tsx); 
  #lines(x=tsx.t, y=tsx.lm_res$fit, col='red'); 
  #lines(x=tsx.t, y=tsx.lm_res$lwr, col='green'); lines(x=tsx.t, y=tsx.lm_res$upr, col='green');
  # TODO： 部署到项目中
  
# 基于ARIMA
plot(tsx, main='row data. begine ARIMA test.')  # 观测时序：是否平稳 or 差分；平稳且正态.
tsx.lm = lm(y~index(y), data=tsx.lm_in)  # 获取均值 or 截距
abline(h=tsx.lm$coefficients[[1]], col='blue'); 
abline(h=mean(tsx.lm_in$y), col='red'); 

#ts. = as.zoo(tsx); ts. = ts(coredata(ts.), frequency=7, start=c(2013, 1, 1));
# 数据变换
BoxCox.ar(tsx, method='burg');# 其他方法： ols, yw, yule-walker, mle:报错
t = BoxCox.ar(tsx, method='burg', lambda=seq(0,1,0.01)); 
lambda = t$lambda[which.max(t$loglike)]
plot((tsx^lambda - 1)/lambda); plot(tsx); # 比较Box-Cox 变换前后的曲线差异大小

# 平稳性
acf(tsx); pacf(tsx); eacf(tsx);
tsx.diff = diff(tsx); # tsx.diff = as.ts(aggregate(tsx.diff, round(time(tsx.diff), 1), tail, 1))
plot(tsx.diff);  # tsx.diff['2013-02-02'] = tsx['2013-02-02']; # 用真实值填充NA 
# 正态性检验
shapiro.test(coredata(tsx.diff)); shapiro.test(coredata(diff(tsx.diff,lag=7)));

# ARIMA {aicc, bic, loglike}
#1-S0 pacf:9;  acf:1,2,3,4,5,7; 
#1-S7 pacf: 1,7; acf: 1, 6,7,8; # diff(as.zoo(tsx.diff), lag=7)
#[10131.68,10160.09] auto.arima(tsx, d=1, D=0, max.order=15, start.p=1, start.q=1, ic='aicc')
#[10104.19,10145.15,-5043.09] order=c(5,1,4); 
#[9937.78,9986,-4957.52] order=c(5,1,4), seasonal=list(order=c(0,1,1), period=7) [9940.82,9989.04,-4962.04] @ADD fixed=c(NA,0,NA,0,NA,NA,NA,NA,NA,NA)
#[9937.43,9965.73,-4961.59] order=c(3,1,2), seasonal=list(order=c(0,1,1), period=7),method='ML'
#[?] order=c(3,1,2), seasonal=list(order=c(1,1,1), period=7),method='ML'
#[10279.9,10312.43,-5132.75] (3,0,2)(1,1,1)[7] &sar1=0
#[9909.03,10016.32,-4925.72] (3,0,2)(1,1,1)[7] & xreg
#[9892.57,10003.7,-4920.35] (4,0,2)(1,1,1)[7] & ma1=0 & reg &[qtr1=qtr3=mon10=0]

# AIM检验
tsx.AIM = Arima(tsx,order=c(5,1,4), seasonal=list(order=c(0,1,1), period=7),fixed=c(NA,0,NA,0,NA,NA,NA,NA,NA,NA));
print('[INFO] test diff ARIMA orders.'); print(tsx.AIM);
plot(x=tsx.t, y=rstandard(tsx.AIM), type='l', main='test AIM'); abline(h=c(3,-3));
#points(x=tsx.t[c(9,29)],y=tsx.d[c(9,29)]); # 基于detectIO 的异常点

# 增加回归项
# [9686.82,9735.04,-4831.04]

##tsx.reg = model.matrix(~as.factor(tsx.lm_in$mday)-1)[,c(1:3,26,30)]; colnames(tsx.reg) = c(paste("mday",c(1:3,26,30),sep=''));
tsx.reg = build_matrix(x=tsx.lm_in$mday,idx=c(1:3,26,30),col_name='mday',f_intercept=F); 
tsx.reg = cbind(tsx.reg, FST=tsx.lm_in$dFST + tsx.lm_in$d51 + tsx.lm_in$d10, CJ=tsx.lm_in$dCJ);
tsx.reg[,'CJ'][c(364,365,370)] = 0; # 修正春节数据的范围:头2天+尾1天
# 迭代实验
  #增加异常值: 假日前后 c(9,29,90,243,335) 243,9,335,29, (361,265), 388,395
  t = rep(0, length(tsx.t)); #t[c(9,10, 29,91,243)]=c(-40,-1, 0,-40, 80);
  t = data.frame(outlier9=t, outlier10=t, outlier29=t, outlier91=t, outlier243=t, outlier_52=t, outlier_53=t);
  t$outlier9[9]=1; t$outlier10[10]=1; t$outlier29[29]=1; t$outlier91[91]=1; t$outlier_52[455]=1; t$outlier_53[456]=1; 
  t$outlier243[243]=1;
  tsx.reg = cbind(tsx.reg, t); 
  #plot(x=tsx.t,y=tsx,type='l',main='');  # AIM2为无outer的拟合，AIM1实验outer值
  #lines(x=tsx.t, y=fitted(tsx.AIM2), col='blue');points(x=tsx.t, y=fitted(tsx.AIM), col='green');
  #tsx.lm_in$y[c(9,10, 29,91,243,355,365)] + 0.1; # 抽取异常点的值 与 修正后的值
  #fitted(tsx.AIM2)[c(9,10, 29,91,243,355,365)]; fitted(tsx.AIM)[c(9,10, 29,91,243,355,365)];

  # 增加维度
  N = length(tsx.reg);
##model.matrix(~as.factor(tsx.lm_in$qtr)-1)[,c(1,3)],model.matrix(~as.factor(tsx.lm_in$mon)-1)[,c(1,3,10)] 
##tsx.reg = cbind(tsx.reg, model.matrix(~as.factor(tsx.lm_in$qtr)-1)[,c(1,3)],model.matrix(~as.factor(tsx.lm_in$mon)-1)[,c(1,3,10)]); 
##colnames(tsx.reg) = names(tsx.reg[1,])[1:N] + c(paste('qtr'),c(1,3),sep='') + c(paste('mon'), c(1,3,10),sep='');
  t = build_matrix(tsx.lm_in$qtr,idx=c(1,3),col_name='qtr',f_intercept=F); tsx.reg = cbind(tsx.reg, t); 
  t = build_matrix(tsx.lm_in$mon,idx=c(1,3,10),col_name='mon',f_intercept=F); tsx.reg = cbind(tsx.reg, t); 
  print(names(tsx.reg[1,])); # 打印names 

#[9609.18,9711.83,-4776.88]
tsx.AIM = Arima(tsx,order=c(3,1,2), seasonal=list(order=c(1,1,1), period=7), xreg=tsx.reg, method='ML');
print('[INFO] xreg by multi factors and outliers.'); print(tsx.AIM);

# fixed=c(rep(NA,6),NA,NA,NA,0,NA,0,NA,rep(NA,2),rep(NA,4),0o,0,NA,0,NA)
  # 模型的拟合效果分析
  plot(x=tsx.t, y=rstandard(tsx.AIM), type='l'); abline(h=c(3,-3));
  t. = residuals(tsx.AIM); plot(x=tsx.t, y=t., type='l');  # points(x=tsx.t[9:11], y=t.[9:11], col='blue')
  qqnorm(t.); qqline(t.); acf(t.); pacf(t.); 
  detectAO(tsx.AIM); detectIO(tsx.AIM); 

################################### 实验： 独立整合ARIMA 与 其残差的LM 模型到一个ARIMA ###############
#@ADD RD-lm: Intercept, mday1, mday3,mday14, mon0, mon2,mon8,mon9,V32,V33,mday1:V32,mday2:V32,mday3:V32
t. = model.matrix(~as.factor(tsx.lm_in$mday)-1)[,c(1:3,26,30)];  # names(t.[1,]);
t. = cbind(t., model.matrix(~as.factor(tsx.lm_in$mon)-1));
t. = cbind(t., tsx.lm_in$dFST + tsx.lm_in$d51 + tsx.lm_in$d10, tsx.lm_in$dCJ);
tsx.reg[,12[c(8,9,14, 364,365,370)]] = 0; 
tsx.df = as.data.frame(t.);
t.m = model.matrix(~tsx.df$"as.factor(tsx.lm_in$mday)1":tsx.df$V44-1); # 最终无截距 否则opitm failed.
t.m = cbind(t.m, model.matrix(~tsx.df$"as.factor(tsx.lm_in$mday)2":tsx.df$V44 - 1), model.matrix(~tsx.df$"as.factor(tsx.lm_in$mday)3":tsx.df$V44 - 1));
t.m = cbind(t.m_, t.[,c(1,3, 14, 32, 34, 40,41, 44,45)]); 
# 单独对残差 拟合LM 参数，结果与xreg方法得到的参数不同
tsx.AIM = Arima(tsx,order=c(3,1,2), seasonal=list(order=c(0,1,1), period=7),method='ML');
t.coef_aim = as.data.frame(print(tsx.AIM2)$coef)[,1]; # 抽取参数权重
t.coef_lm = summary(lm(residuals(tsx.AIM2)~t.m))$coefficients;  t.coef_lm=t.coef_lm[c(2:dim(t.coef_lm)[1])];
# 组合出最终的模型 [9874.04,9949.74,-4935.11] 
tsx.AIM = Arima(tsx,order=c(3,1,2), seasonal=list(order=c(0,1,1), period=7),method='ML',xreg=t.m, fixed=c(t.coef_aim,t.coef_lm))
print('[INFO] test merge ARIMA and LM model together.'); tsx.AIM;

#################################################### 预测  ######################################################
# 数据准备
t. = as.POSIXlt(seq(as.POSIXct('2014-05-04'), len=(as.POSIXct('2014-12-31')-as.POSIXct('2014-05-04')), by='day'));
tsx.pre_in = list(time=t., qtr=format(as.yearqtr(t.), '%q'), mon=t.$mon, yea=t.$year, 
                  mday=t.$mday, wday=t.$wday, yday=t.$yday, 
                  d51=as.numeric(t. %in% index(t.[c('2013-04-29/2013-05-03','2014-04-29/2014-05-03')])),
                  d10=as.numeric(t. %in% index(t.[c('2013-10-01/2013-10-07','2014-10-01/2014-10-07')])),
                  dCJ=as.numeric(t. %in% index(t.[c('2013-02-09/2013-02-15', '2014-01-31/2014-02-06')])),
                  # 假期前后修正3天
                  dFST=as.numeric(t. %in% index(t.[c('2013-04-04/2013-04-07','2013-06-10/2013-06-13','2013-09-19/2013-09-21',
                                                     '2014-01-01/2014-01-03','2014-04-05/2014-04-08','2014-06-02/2014-06-05','2014-09-08/2014-09-11')]))
);
tsx.p_reg = build_matrix(x=tsx.pre_in$mday,idx=c(1:3,26,30),col_name='mday',f_intercept=F); 
tsx.p_reg = cbind(tsx.p_reg, FST=tsx.pre_in$dFST+tsx.pre_in$d51+tsx.pre_in$d10,CJ=tsx.pre_in$dCJ);
t = rep(0, length(tsx.pre_in$time));
t = data.frame(outlier9=t, outlier10=t, outlier29=t, outlier91=t, outlier243=t, outlier_52=t, outlier_53=t);
t$outlier243[which(t. == as.POSIXlt('2014-10-02'), arr.ind=T)] = 1;#十一特殊时间点
tsx.p_reg = cbind(tsx.p_reg, t); # 按原来的顺序构造预测参数

t = build_matrix(tsx.pre_in$qtr,idx=c(1,3),col_name='qtr',f_intercept=F); tsx.p_reg = cbind(tsx.p_reg, t);
t = build_matrix(tsx.pre_in$mon,idx=c(1,3,10),col_name='mon',f_intercept=F); tsx.p_reg = cbind(tsx.p_reg, t);
