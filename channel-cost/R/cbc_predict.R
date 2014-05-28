
library(zoo)
library(forecast)

cbc_predict <- function(fp,fp2,num) {
  mydataframe<-read.table(fp,header=TRUE,sep=",")
  dt0<-mydataframe$date
  nw0<-mydataframe$new
  od0<-mydataframe$old
  et0<-mydataframe$est
  dt<-dt0[nw0>0]
  nw1<-nw0[nw0>0]
  od1<-od0[nw0>0]
  et1<-et0[nw0>0]
  nw<-vector(length=2*length(dt))
  od<-vector(length=2*length(dt))
  et<-vector(length=2*length(dt))  
  for (n in 1:length(dt)) {
    x<-length(dt)-floor(1/runif(1,0,1))+1
    if (x>=1 & x<=length(dt)) {
      nw[n]<-nw1[x]*(1+runif(1,0,0.0001))
      od[n]<-od1[x]*(1+runif(1,0,0.0001))
      et[n]<-et1[x]*(1+runif(1,0,0.0001))  
    } else {
      nw[n]<-nw1[length(dt)]*(1+runif(1,0,0.0001))
      od[n]<-od1[length(dt)]*(1+runif(1,0,0.0001))
      et[n]<-et1[length(dt)]*(1+runif(1,0,0.0001))  
    }
    
  } 
  for (n in 1:length(dt)) {
    i<-floor(rexp(1,rate=2))
    j<-length(dt)-i
    if(j<=0) { j<-1 }
    nw[length(dt)+n]<-nw1[j]
    od[length(dt)+n]<-od1[j]
    et[length(dt)+n]<-et1[j]
  }
  sj<-data.frame(nw,od,et)
  res<-c()
  if (length(sj)>=3) {
    myfit<-lm(nw~0+od+et,sj)
    ce<-coefficients(myfit)
    print(ce)
    # ADD
#t_summ = summary(myfit)
#print(paste(t_summ$r.squared,t_summ$adj.r.squared,t_summ$sigma, num))

    c1<-ce[[1]]
    c2<-ce[[2]]
    if(is.na(c1)) { c1<-0 }
    if(is.na(c2)) { c2<-0 }
    for (n in 1:length(dt0)) {
      if (is.na(od0[n]) | is.na(et0[n])) {
        res<-c(res,NA)
      } else if (c1*od0[n]+c2*et0[n]>=0) {
        res<-c(res,c1*od0[n]+c2*et0[n]+runif(1,0,10))
      } else {
        res<-c(res,0)
      }
    }
  } else {
    res<-et0
  }
  res2<-c()
  for (n in 1:length(dt0)) {
    if (nw0[n]>0) {
      res2<-c(res2,res[n]/nw0[n]-1)
    } else {
      res2<-c(res2,1)
    }
  }
  inum<-rep(num,length(dt0))
  itime<-rep(as.character(Sys.Date()),length(dt0))
  y<-data.frame(dt0,nw0,od0,et0,res,res2,inum,itime)
  print(y)
  write.table(y,fp2,sep=",",row.names=FALSE,col.names=FALSE)
  return(y)
}

ratio_predict <- function(fp,fp2) {
  mydataframe<-read.table(fp,header=TRUE,sep=",",as.is = c(TRUE,TRUE))
  dt0<-mydataframe$date
  rt0<-mydataframe$ratio
  dt<-dt0[rt0>0]
  rt<-rt0[rt0>0]
  rt2<-c()
  for (i in 1:length(rt)) {
    rt2<-c(rt2,as.numeric(rt[i]))
  }
  ts<-zoo(rt2,dt)
  print("data to zoo:"); str(zoo(rt2, as.Date(dt)));
  idx<-1:length(ts)
  # @ÐÞ¸Ä
  #m<-lm(coredata(ts)~poly(idx,2))
  #detr<-zoo(resid(m),index(ts))
  #plot(coredata(ts), main='ratio of total Num.', type='l')
  print("data is fin"); str(ts)
  
  m<-arima(coredata(ts),order=c(5,1,4))
  # @ADD
  print('summary of arima c(5,1,4)')
  print(m)#summary(m)
  m_ <- auto.arima(coredata(ts))
  print('summary of auto.arima')
  summary(m_)
  
  p<-predict(m,n.ahead=366)
  for (i in 1:length(dt0)) {
    if (rt0[length(dt0)-i+1]>0) {
      nn<-length(dt0)-i+1
      break
    }
  }
  q<-p$pred
  # @ADD 
  #t_ = data.frame(p$pred, rt0)
  print (length(p$pred)); print (length(rt0))
  
  for (i in nn:length(dt0)) {
    rt0[i]<-q[i-nn+1]
  }
  ts0<-zoo(rt0[1:(nn+365)],dt0[1:(nn+365)])
  itime<-rep(as.character(Sys.Date()),(nn+365))
  y<-data.frame(dt0[1:(nn+365)],rt0[1:(nn+365)],itime)
  #print(y)
  write.table(y,fp2,sep=",",row.names=FALSE,col.names=FALSE)
  return(y)
} 

#@modify for windows 
P_ROOT = "D:/project/channel-cost/"
#P_ROOT= "/home/qerd/channel_predict/CBC/"
fp<-paste(P_ROOT, "exc/all_top_cops.txt",sep="")
fpdf<-read.table(fp,header=TRUE,sep=",")
fpdf2<-fpdf$X1
for(line in fpdf2) {
  fpp1<-paste(paste(P_ROOT, "exc/cid/",sep=""),line,".txt",sep="")
  fpp2<-paste(paste(P_ROOT, "exc/cid2/",sep=""),line,"_predict.txt",sep="")
  #x=cbc_predict(fpp1,fpp2,as.integer(line))
}
fp<-paste(P_ROOT, "exc/rat/ratio_ts.txt",sep="")
fp2<-paste(P_ROOT, "exc/rat/ratio_predict.txt",sep="")
x=ratio_predict(fp,fp2)
print("finished!")
