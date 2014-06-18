########################################
##  基本特征维度
########################################


## 1. 时间特征 
# 假日： 元旦、清明、劳动、端午、中秋、国庆; 后2天。
t.fx.dNew = c('2013-01-01/2013-01-03','2014-01-01/2014-01-03'); 
t.fx.dQM = c('2013-04-04/2013-04-06','2014-04-05/2014-04-07');
t.fx.d51 = c('2013-04-29/2013-05-05','2014-05-01/2014-05-03');
t.fx.dDW = c('2013-06-10/2013-06-12','2014-06-02/2014-06-04');
t.fx.dMid = c('2013-09-19/2013-09-21','2014-09-08/2014-09-10');
t.fx.d10 = c('2013-10-01/2013-10-07','2014-10-01/2014-10-07');
t.fx.dH = c(t.fx.dNew,t.fx.dQM,t.fx.d51,t.fx.dDW,t.fx.dMid,t.fx.d10);
# 暑假
t.fx.dSum = c('2013-07/2013-08', '2014-07/2014-08');  #, '2015-07/2015-08');
# 春节 
t.fx.dCJ = c('2013-02-09/2013-02-15','2014-02-02/2014-02-05'); 

###############################
## @from cbc_total_daily_test.R
## @exp: 
## 基于时间、节假日信息 构造时间特征集合 
###############################
build_xts_feature = function(x) {
  # 基于xts对象生成时间维度相关的哑变量特征 注：不可直接输入时间
  t.fx = try.xts(x);
  t.fx.t = as.POSIXlt(index(t.fx));  #time(t.fx) = t.fx.t;  # 不能修改
  res = list(qtr=as.numeric(format(as.yearqtr(t.fx.t), '%q')), mon=t.fx.t$mon, yea = t.fx.t$year,
             mday=t.fx.t$mday, wday=t.fx.t$wday, yday=t.fx.t$yday, 
             idx=c(1:length(t.fx.t))
  );
  idx.bad = rep(0, length(t.fx.t));
  fea.idx = function(fi) {
    idx = index(t.fx[fi]);
    if (length(idx) != 0) {
      resi= as.numeric(t.fx.t %in% idx);
    } else {
      resi = idx.bad;
    }
    return(resi);
  }
  res$dNew = fea.idx(t.fx.dNew);
  res$dQM = fea.idx(t.fx.dQM);
  res$d51 = fea.idx(t.fx.d51);
  res$dDM = fea.idx(t.fx.dDW);
  res$dMid = fea.idx(t.fx.dMid);
  res$d10 = fea.idx(t.fx.d10);
  res$dSum = fea.idx(t.fx.dSum);
  res$dCJ = fea.idx(t.fx.dCJ);
  
  return (res);
}


################################
## @from cbc_total_daily_test.R
## @exp: build_matrix(tsx.lm_in$qtr,idx=levels(as.factor(tsx.lm_in$qtr))[c(1,3)],col_name='qtr',f_intercept=F)
## 将枚举|数值型的特征拓展为哑变量特征; @desc 异常时返回负数 
################################
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

################################
## @from baidubrowser_detail_test.R
## @use fill_xts(t., FUN_f=na.spline, range_t='2013-01-01/2014-06-04', fillEmpty=0);  #前后空白区间用0填充，中间的NA用样条插值填充
## 对给定的xts 数据的指定区间range_t，用给定的插值函数进行插值;区间内的空白间隔基于步长step[2]插值. fillEmpty指对两侧空白填充值
################################
fill_xts = function(x, FUN_f, range_t, fillEmpty=NULL, step=c('day', 1)) {
  # 对给定的xts 数据的指定区间range_t，用给定的插值函数进行插值;区间内的空白间隔基于步长step[2]插值. fillEmpty指对两侧空白填充值
  t.x = try.xts(x);
  
  #print('time range effective.');
  t.r_t = strsplit(range_t, '/')[[1]];
  
  t.t = index(t.x);
  # 时间范围之间的缺失用NA 填充
  for (i in 1:(length(t.t)-1)) { 
    if (as.numeric(as.character(t.t[i+1] - t.t[i])) > step[2]) {
      t.ntime = seq(t.t[i], to=t.t[i+1], by=step[1]);
      t.ntime = t.ntime[2:(length(t.ntime)-1)]; # 不含首尾元素
      t.x = c(t.x[1:i], xts(rep(NA, length(t.ntime)), t.ntime), t.x[(i+1):length(t.x)]);
    }
  }
  t.t = index(t.x);
  
  # 可选： 将首尾的空白用fillEmpty 值填充
  if (!is.null(fillEmpty)) {  
    t.h = as.character(head(t.t, n=1));
    if (!is.na(t.r_t[1]) && t.h > t.r_t[1]) {
      t.ntime = seq(as.POSIXlt(t.r_t[1]), to = as.POSIXlt(t.h), by=step[1]);
      t.ntime = t.ntime[1:(length(t.ntime)-1)]; 
      t.x = c(xts(rep(fillEmpty, length(t.ntime)), t.ntime), t.x); 
      #t.r_t[1] = t.h; # 插值的新的起点      
    }
    t.l = as.character(tail(t.t, n=1));
    if (!is.na(t.r_t[2]) && t.l < t.r_t[2]) {
      t.ntime = seq(as.POSIXlt(t.l), to = as.POSIXlt(t.r_t[2]), by=step[1]);
      t.ntime = t.ntime[2:length(t.ntime)];
      t.x = c(t.x, xts(rep(fillEmpty, length(t.ntime)), t.ntime)); 
      #t.r_t[2] = t.l; # 插值的新的终点
    }
    #t.dat. = t.x[paste(t.r_t, sep='/')];
  }
  t.dat. = t.x[range_t];
  FUN = match.fun(FUN_f);  # 执行插值
  t.dat.ins = FUN(t.dat.);
  indexTZ(t.dat.ins) = Sys.getenv('TZ');
  t.dat.h = t.x[paste('', t.r_t[1], sep='/')]; 
  if (length(t.dat.h) <= 1) {
    t.dat.h = NULL;
  } else {
    t.dat.h = t.dat.h[1:(length(t.dat.h)-1)];
  }
  t.dat.t = t.x[paste(t.r_t[2], '', sep='/')]; 
  if (length(t.dat.t) <= 1){
    t.dat.t = NULL;
  } else {
    t.dat.t = t.dat.t[2:length(t.dat.t)];
  }
  t.res = rbind(t.dat.h, t.dat.ins, t.dat.t);
  #print(paste("[TEST] input: x", str(x), "as-xts: ", str(t.x), "index: ", str(t.t), "insert data: ", str(t.dat.), 
  #            "result:", str(t.res), sep='\n')); 
  return(t.res);
}


