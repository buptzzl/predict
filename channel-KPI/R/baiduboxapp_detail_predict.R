#百度框KPI时间序列数据预测
rm(list=ls(all=TRUE))
ls()

# 时间序列数据预处理，返回dataframe类型结果
ts_prepare <- function(factFrame, featFrame, dataRange, type="activate",fact="activate_count") {
  dat <- subset(factFrame, factFrame$date_id>=dataRange[1]&factFrame$date_id<=dataRange[2])
  fea <- subset(featFrame, featFrame$date_id>=dataRange[1]&featFrame$date_id<=dataRange[2])
  if (type=="activate") {
    date <- fea$date_id
    xm <- fea$date_month/10
    xf <- fea$date_fest
    xw <- cbind(xw1=fea$week_1,xw2=fea$week_2,xw3=fea$week_3,xw4=fea$week_4,xw5=fea$week_5,xw6=fea$week_6,xw7=fea$week_7)
    xe <- fea$extrawork
    y <- c(dat[,fact],rep(NA,nrow(fea)-nrow(dat)))
    x <- seq(1, length(y), 1)/100
    ts <- data.frame(date,y,x,xm,xf,xw,xe)
  }
  else if (type=="active") {
    date <- fea$date_id
    xm <- fea$date_month
    xe <- fea$extrawork
    if (fact=="active_count") {
      xp <- fea$push3/1000
    } else if (fact=="android_active_count") {
      xp <- fea$push2/1000
    } else if (fact=="iphone_active_count") {
      xp <- fea$push1/1000
    }
    xw <- cbind(xw1=fea$week_1,xw2=fea$week_2,xw3=fea$week_3,xw4=fea$week_4,xw5=fea$week_5,xw6=fea$week_6,xw7=fea$week_7)
    y <- c(dat[,fact],rep(NA,nrow(fea)-nrow(dat)))
    x <- seq(1, length(y), 1)/100
    ts <- data.frame(date,y,x,xm,xp,xw,xe)    
  }
  return (ts)
}

activate_model <- function(y, x, xm, xf, xw, poly.order) {
  #采用线性权重提高近期样本点的重要性
  if (poly.order==1){
    nlmod <- nls(y ~ a0+a1*x+b1*xm+c1*exp(-c2*xf)+
                   d2*xw[,2]+d3*xw[,3]+d4*xw[,4]+d5*xw[,5]+d6*xw[,6],
                 start=list(a0=1,a1=1,b1=1,c1=1,c2=1,d2=1,d3=1,d4=1,d5=1,d6=1))
  } else if (poly.order==2) {
    nlmod <- nls(y ~ a0+a1*x+a2*I(x^2)+b1*xm+c1*exp(-c2*xf)+
                   d2*xw[,2]+d3*xw[,3]+d4*xw[,4]+d5*xw[,5]+d6*xw[,6],
                 start=list(a0=1,a1=1,a2=1,b1=1,c1=1,c2=1,d2=1,d3=1,d4=1,d5=1,d6=1))
  } else if (poly.order==3) {
    nlmod <- nls(y ~ a0+a1*x+a2*I(x^2)+a3*I(x^3)+b1*xm+c1*exp(-c2*xf)+
                   d2*xw[,2]+d3*xw[,3]+d4*xw[,4]+d5*xw[,5]+d6*xw[,6],
                 start=list(a0=1,a1=1,a2=1,a3=1,b1=1,c1=1,c2=1,d2=1,d3=1,d4=1,d5=1,d6=1))
  } else if(poly.order==4) {
    nlmod <- nls(y ~ a0+a1*x+a2*I(x^2)+a3*I(x^3)+a4*I(x^4)+b1*xm+c1*exp(-c2*xf)+
                   d2*xw[,2]+d3*xw[,3]+d4*xw[,4]+d5*xw[,5]+d6*xw[,6],
                 start=list(a0=1,a1=1,a2=1,a3=1,a4=1,b1=1,c1=1,c2=1,d2=1,d3=1,d4=1,d5=1,d6=1))
  } else if(poly.order==5) {
    nlmod <- nls(y ~ a0+a1*x+a2*I(x^2)+a3*I(x^3)+a4*I(x^4)+a4*I(x^5)+b1*xm+c1*exp(-c2*xf)+
                   d2*xw[,2]+d3*xw[,3]+d4*xw[,4]+d5*xw[,5]+d6*xw[,6],
                 start=list(a0=1,a1=1,a2=1,a3=1,a4=1,a5=1,b1=-0.02,c1=0.38,c2=0.60,d2=-0.08,d3=-0.10,d4=-0.11,d5=-0.11,d6=-0.08))
  }
  return (nlmod)
}

active_model <- function(y, x, xm, xp, xw, poly.order) {
  #采用线性权重提高近期样本点的重要性
  if (poly.order==1){
    nlmod <- nls(y ~ a0+a1*x+b1*xm+c1*xp+d1*xw[,1]+d2*xw[,7],
                 start=list(a0=1,a1=1,b1=1,c1=1,d1=1,d2=1))
  } else if (poly.order==2) {
    nlmod <- nls(y ~ a0+a1*x+a2*I(x^2)+b1*xm+c1*xp+d1*xw[,1]+d2*xw[,7],
                 start=list(a0=1,a1=1,a2=1,b1=1,c1=1,d1=1,d2=1))
  } else if (poly.order==3) {
    nlmod <- nls(y ~ a0+a1*x+a2*I(x^2)+a3*I(x^3)+b1*xm+c1*xp+d1*xw[,1]+d2*xw[,7],
                 start=list(a0=1,a1=1,a2=1,a3=1,b1=1,c1=1,d1=1,d2=1))
  } else if(poly.order==4) {
    nlmod <- nls(y ~ a0+a1*x+a2*I(x^2)+a3*I(x^3)+a4*I(x^4)+b1*xm+c1*xp+d1*xw[,1]+d2*xw[,7],
                 start=list(a0=1,a1=1,a2=1,a3=1,a4=1,b1=1,c1=1,d1=1,d2=1))
  } else if(poly.order==5) {
    nlmod <- nls(y ~ a0+a1*x+a2*I(x^2)+a3*I(x^3)+a4*I(x^4)+a5*I(x^5)+b1*xm+c1*xp+d1*xw[,1]+d2*xw[,7],
                 start=list(a0=1,a1=1,a2=1,a3=1,a4=1,a5=1,b1=1,c1=1,d1=1,d2=1))
  }
  return (nlmod)
}

#
model_fit <- function(ts.fit, poly.order, type="activate") {
  if (type=="activate") {
    xm <- ts.fit[,"xm"]
    xf <- ts.fit[,"xf"]
    #xe <- ts.fit[,"xe"]
    xw <- cbind(ts.fit[,"xw1"],ts.fit[,"xw2"],ts.fit[,"xw3"],ts.fit[,"xw4"],ts.fit[,"xw5"],ts.fit[,"xw6"],ts.fit[,"xw7"])
    x <- ts.fit[,"x"]
    y <- log(ts.fit[,"y"])

    #采用线性权重提高近期样本点的重要性
    nlmod <- activate_model(y,x,xm,xf,xw,poly.order)
    #nlmod.fit <- predict(nlmod)
  } else if (type=="active") {
    xm <- ts.fit[,"xm"]
    xp <- ts.fit[,"xp"]
    #xe <- ts.fit[,"xe"]
    xw <- cbind(ts.fit[,"xw1"],ts.fit[,"xw2"],ts.fit[,"xw3"],ts.fit[,"xw4"],ts.fit[,"xw5"],ts.fit[,"xw6"],ts.fit[,"xw7"])
    x <- ts.fit[,"x"]
    y <- log(ts.fit[,"y"])
    nlmod <- active_model(y,x,xm,xp,xw,poly.order)
  }
  parm <- summary(nlmod)$parameters
  x.predict <- x[length(x)] + 1:3/100
  nlmod.predict <- rep(parm[1,1], 3)
  for (i in c(1:poly.order)) {
    nlmod.predict <- nlmod.predict+parm[i+1,1]*x.predict^i
  }
  base0 <- nlmod.predict[1]
  diff0 <- (nlmod.predict[3]-nlmod.predict[1])/2
  parm <- c(base0,diff0,as.numeric(parm[(poly.order+2):nrow(parm),1]))
  return (parm)
}

model_predict <- function(ts.predict, poly.order, parm, type="activate") {
  if (type == "activate") {
    xm <- ts.predict[,"xm"]
    xf <- ts.predict[,"xf"]
    xw <- cbind(ts.predict[,"xw1"],ts.predict[,"xw2"],ts.predict[,"xw3"],
                ts.predict[,"xw4"],ts.predict[,"xw5"],ts.predict[,"xw6"])
    x <- ts.predict[,"x"]
    nlmod.predict <- rep(parm[1],nrow(ts.predict))
    for (k in 2:length(nlmod.predict)) {
      nlmod.predict[k] <- nlmod.predict[1]+parm[2]*(k-1)
    }
    nlmod.predict <- (nlmod.predict+parm[3]*xm+parm[4]*exp(-parm[5]*xf))
    for (j in c(6:10)) {
      nlmod.predict <- nlmod.predict+parm[j]*xw[,j-4]
    }
  } else if (type == "active") {
    xm <- ts.predict[,"xm"]
    xp <- ts.predict[,"xp"]
    xw <- cbind(ts.predict[,"xw1"],ts.predict[,"xw2"],ts.predict[,"xw3"],
                ts.predict[,"xw4"],ts.predict[,"xw5"],ts.predict[,"xw6"])
    x <- ts.predict[,"x"]
    nlmod.predict <- rep(parm[1],nrow(ts.predict))
    for (k in 2:length(nlmod.predict)) {
      nlmod.predict[k] <- nlmod.predict[1]+parm[2]*(k-1)
    }
    xp[which(xp<0)] <- rnorm(length(xp[which(xp<0)]),mean=parm[7],sd=parm[8])
    nlmod.predict <- (nlmod.predict+parm[3]*xm+parm[4]*xp)
    nlmod.predict <- (nlmod.predict+parm[5]*xw[,1]+parm[6]*xw[,2])
  }
  return (nlmod.predict) 
}

model_select <- function(factFrame, featFrame, fitRange,testRange,testLength=30,type="activate",fact="activate_count") {

  fitRange <- as.Date(as.character(fitRange),"%Y%m%d")
  testRange <- as.Date(as.character(testRange),"%Y%m%d")
  
  # Parameters Need to be fitted
  fitLenRange <- c(210:90)
  testLenRange <- c(30:26)
  polyOrderRange <- c(1:5)

  best_nmae <- 99999
  best_parm <- c(-1,-1,-1)
  for (testLen in testLenRange) {
    testStartDate <- testRange[2]-testLen+1
    testEndDate <- testRange[2]
    for (fitLen in fitLenRange) {
      fitStart <- as.numeric(format(testStartDate - fitLen,"%Y%m%d"))
      fitEnd <- as.numeric(format(testStartDate - 1,"%Y%m%d"))
      testStart <- as.numeric(format(testStartDate,"%Y%m%d"))
      testEnd <- as.numeric(format(testEndDate,"%Y%m%d"))
      
      ts.fit <- ts_prepare(factFrame,featFrame,c(fitStart,fitEnd),type=type,fact=fact)
      ts.predict <- ts_prepare(factFrame,featFrame,c(testStart,testEnd),type=type,fact=fact)
      # x is the polynominal value
      ts.predict$x <- ts.predict$x+nrow(ts.fit)/100
      
      for (polyOrder in polyOrderRange){
        tryCatch({
          parm <- model_fit(ts.fit,polyOrder,type)
          #xp <- c(ts.fit$xp,ts.predict$xp)
          xp <- c(ts.predict$xp)
          mu <- mean(xp[which(xp>0)])
          sigma <- sd(xp[which(xp>0)])
          ts.test <- model_predict(ts.predict,polyOrder,c(parm,mu,sigma),type)
          mae <- abs((ts.predict$y-exp(ts.test))/ts.predict$y)
          weight <- ceiling((1:length(mae))/7)
          nmae <- sum(mae*weight/sum(weight))
          if (nmae < best_nmae) {
            best_nmae <- nmae
            best_parm <- c(fitLen, testLen, polyOrder, best_nmae,parm)
            #print (c(fitLen, testLen, polyOrder))
          }
          #print (nmae)
          #plot(ts.fit$x,log(ts.fit$y), type="l",xlim=c(0,(nrow(ts.fit)+testLen+1)/100))
          #lines(seq((nrow(ts.fit)+1)/100,(nrow(ts.fit)+testLen+1)/100,0.01),log(ts.predict$y),lwd=2)
          #lines(seq((nrow(ts.fit)+1)/100,(nrow(ts.fit)+testLen+1)/100,0.01),ts.res,col="red",lwd=2)        
        }, error = function(e){
          #print (c(-1,-1,testLen,fitLen))
        })
      }
    }
  }
  #print (best_nmae)
  #print (best_parm)
  return (best_parm)
}

args = commandArgs(TRUE)
#args = c("D://R work place//product_predict",20140330,20140331,20140429,"baiduboxapp_detail_1.csv","baiduboxapp_feature_1.csv")
workpath <- args[1]
basetime <- args[2]
predict_start_date <- args[3]
predict_end_date <- args[4]
datfile <- args[5]
feafile <- args[6]

#全局常量
graph <- TRUE
test_range <- 30
fit_start_date <- 20120901
fit_end_date <- format(as.Date(basetime,"%Y%m%d")-test_range,"%Y%m%d")
test_start_date <- format(as.Date(basetime,"%Y%m%d")-test_range+1,"%Y%m%d")
test_end_date <- format(as.Date(basetime,"%Y%m%d")-1,"%Y%m%d")

#获取训练预测数据
setwd(workpath)
factFrame <- read.csv(datfile,header=TRUE)
featFrame <- read.csv(feafile, header=TRUE)
fitRange <- as.numeric(c(fit_start_date,fit_end_date))
testRange <- as.numeric(c(test_start_date,basetime))
predictRange <- as.numeric(c(predict_start_date, predict_end_date))

#
name <- c("all_activate","android_activate","iphone_activate","all_active","android_active","iphone_active")
type <- c("activate","activate","activate","active","active","active")
fact <- c("activate_count","android_activate_count","iphone_activate_count2","active_count","android_active_count","iphone_active_count")

sink("baiduboxapp_all_detail_predict.out")
for (i in 1:length(name)) {
  parm <- model_select(factFrame,featFrame,fitRange,testRange,testLength=test_range,type=type[i],fact=fact[i])
  ts.history <- ts_prepare(factFrame,featFrame,c(fitRange[1],testRange[2]),type=type[i],fact=fact[i])
  ts.predict <- ts_prepare(factFrame,featFrame,predictRange,type=type[i],fact=fact[i])
  ts.predict$x <- ts.predict$x+nrow(ts.history)/100
  
  predict.parm <- parm[5:length(parm)]
  predict.parm[1] <- predict.parm[1]+predict.parm[2]*parm[2]
  nmae <- parm[4]
  if (type[i]=="active") {
    xp <- c(ts.history$xp[(nrow(ts.history)-7):nrow(ts.history)],ts.predict$xp)
    mu <- mean(xp[which(xp>0)])
    sigma <- sd(xp[which(xp>0)])/3
    predict.parm <- c(predict.parm,mu,sigma)
  }
  ts.res <- model_predict(ts.predict,parm[3],predict.parm,type=type[i])
  #print (exp(ts.res))
  cat (c(nmae,exp(ts.res)))
  cat ("\n")
  if (graph) {
    png("test.png",width=700, height=500)
    plot(ts.history$x,ts.history$y,type="l",xlim=c(0,ts.predict$x[nrow(ts.predict)]))
    lines(ts.predict$x,ts.predict$y,col="blue",lwd=2)
    lines(ts.predict$x,exp(ts.res),col="red",lwd=2)
    dev.off()
  }
}
sink()
