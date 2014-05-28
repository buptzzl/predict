#@REF total.R


## 数据加载
tsx.brow = read.table('doc/browser_in_total_140508.txt', sep='\t', header=T);
xbrow.t = as.POSIXlt(tsx.brow$date);
xbrow = xts(tsx.brow$s_active_num2, xbrow.t);
# 数据规划化： 含去除前10个特殊值
xbrow.log = log(xbrow);
xbrow.log = xbrow.log[11:length(xbrow.t)]; 
xbrow.t = xbrow.t[11:length(xbrow)];

xbrow.lm_in = list(qtr=format(as.yearqtr(xbrow.t), '%q'), mon=xbrow.t$mon, yea = xbrow.t$year,
                   mday=xbrow.t$mday, wday=xbrow.t$wday, yday=xbrow.t$yday, 
                   idx=c(1:length(xbrow.t)), 
                   dCJ=as.numeric(xbrow.t %in% index(xbrow.log[tsx.dCJ])),
                   dFST=as.numeric(xbrow.t %in% index(xbrow.log[tsx.dH]))
                   );


