# Predict Channel Product KPI using ARIMA model

# Clear the work space
rm(list=ls(all=TRUE))
ls()

activate_arima <- function(df, factor, range=12, graph=FALSE) {
  # Define config variables
  title <- paste(factor," predict",sep="")
  # Fitting ARIMA Model
  dfts <- ts(df[,factor], start=1, frequency=1)
  dftsarima <- auto.arima(dfts, trace = TRUE, stationary = FALSE, seasonal = FALSE)
  dftsfore <- forecast.Arima(dftsarima, h = range, level=c(68,95))
  if (graph) {
    par(mfrow=c(1,1))
    plot(dftsfore, main=title)
  }

  # Conform Predict Results
  month <- date_trans(df$newdate[nrow(df)], range, c(0,12,2))
  average = ceiling(dftsfore$mean[1:length(dftsfore$mean)])
  lw_68 = ceiling(dftsfore$lower[1:length(dftsfore$mean),1])
  hi_68 = ceiling(dftsfore$upper[1:length(dftsfore$mean),1])
  lw_95 = ceiling(dftsfore$lower[1:length(dftsfore$mean),2])
  hi_95 = ceiling(dftsfore$upper[1:length(dftsfore$mean),2])

  return (data.frame(month,average=average,lw_68,hi_68,lw_95,hi_95,stringsAsFactors=FALSE))
}

accumulate_arima <- function(df, factor, acc, range=12, graph=FALSE) {
  period <- c(0,12,2)
  d <- activate_arima(df, "activate_count", range, graph=FALSE)
  month <- d$month

  accfore <- c(acc+d$average[1]*dayofmonth(month[1]), c(2:range)*0)
  if (period[3] == 2) {
    halfmonth=as.numeric(substr(month[1],7,8))
    days <- 15*(halfmonth%%2) + (dayofmonth(month[1])-15)*(halfmonth-1)
    accfore <- c(acc+d$average[1]*days, c(2:range)*0)
  }
  for (i in c(2:range)) {
    if (period[3] == 2) {
      halfmonth=as.numeric(substr(month[i],7,8))
      days <- 15*(halfmonth%%2) + (dayofmonth(month[i])-15)*(halfmonth-1)
      accfore[i] <- accfore[i-1] + d$average[i]* days
    }
  }
  if (graph) {
    par(mfrow=c(1,1))
    plot(c(1:range), accfore, type="l")
  }
  return (data.frame(month,average=accfore,stringsAsFactors=FALSE))
}

dayofmonth <- function(yearmonth,split=0,part=0) {
  year = substr(yearmonth,1,4)
  month = substr(yearmonth,5,6)
  map = c(31,28,31,30,31,30,31,31,30,31,30,31)
  if (as.numeric(year)%%400 == 0 || as.numeric(year)%%100!=0 && as.numeric(year)%%4==0) {
     map[2] = 29
  }
  if (split==0) {
    return (map[as.numeric(month)])
  } else if (split>part) {
    #return (trunc(map[as.numeric(month)]/split))
    return (30/split)
  } else {
    cn <- trunc(30/split)
    return (map[as.numeric(month)]-cn*(split-1))
  }
}

agg_predict <- function(df, factor, grade, range) {
  # Aggregate the KPI to Week/month
  if (factor=="activate_count") {
    df_agg <- inst_aggregate(df[,c("date_id", factor)], grade=grade, agg_type="mean")
    d <- activate_arima(df_agg, factor, range, graph=TRUE)
  } else if (factor=="active_count") {
    df_agg <- inst_aggregate(df[,c("date_id", factor)], grade=grade, agg_type="mean")
    d <- activate_arima(df_agg, factor, range, graph=TRUE)
    print (df_agg)
    print (d)
  } else if (factor=="accumulate_count") {
    df_agg <- inst_aggregate(df[,c("date_id", "activate_count")], grade=grade, agg_type="mean")  
    accumulate <- df$accumulate_count[nrow(df)]
    d <- accumulate_arima(df_agg, "activate_count", accumulate, range, graph=TRUE)
    names(df_agg)[names(df_agg)=="activate_count"]="accumulate_count"
  }
  if (substr(df_agg$newdate[nrow(df_agg)],7,8)=="01") {
    last=df_agg[nrow(df_agg),]
    lastfactor <- last[1,factor]
    if (factor=="accumulate_count") {
      lastfactor <- accumulate
    }
    lastdate <- as.numeric(as.character(last$newdate))
    d <- data.frame(month=c(lastdate,d$month[-nrow(d)]),average=c(lastfactor,d$average[-nrow(d)]))
  }
  return (d)
}

agg_merge <- function(matrix) {
  first <- 1
  for (i in c(1:nrow(matrix))) {
    if (as.numeric(substr(matrix[i,1],7,8)) == 2) {
      lastmon <- c(as.numeric(substr(matrix[i-1,1],1,6)))
      mon <- c(as.numeric(substr(matrix[i,1],1,6)))

      if (first==1) {
        month <- c(mon)
        activate_count <- c(matrix[i-1,2]*dayofmonth(lastmon,2,1)+matrix[i,2]*dayofmonth(mon,2,2))
        active_count <- c(matrix[i,3])
        accumulate_count <- c(matrix[i-1,4]+matrix[i,2]*dayofmonth(mon,2,2))
        first <- 0
      } else {
        month <- c(month,mon)
        activate_count <- c(activate_count,matrix[i-1,2]*dayofmonth(lastmon,2,1)+matrix[i,2]*dayofmonth(mon,2,2))
        active_count <- c(active_count,matrix[i,3])
        accumulate_count <- c(accumulate_count,matrix[i-1,4]+matrix[i,2]*dayofmonth(mon,2,2))
      }
    }
  }
  return (cbind(month,activate_count,active_count,accumulate_count))
}

# Load neccessary Lib
library('forecast');

# Load Argument Lib
args = commandArgs(TRUE)
workpath <- args[1]
basetime <- args[2]
datfile1 <- args[3]
datfile2 <- args[4]
datfile3 <- args[5]
datfile4 <- args[6]

# Set runtime path
setwd(workpath)

# Load Source File
source("/home/yupeng01/channel_predict/KPI/script/r/common.R")

# Global variables 
product <- c("baiduboxapp", "baidubrowser","baiduinput","baiduappsearch")
product_name <- c("百度框", "百度浏览器", "百度输入法", "百度手机助手")
file_name <- c(datfile1,datfile2,datfile3,datfile4)
factor <- c("activate_count","active_count","accumulate_count")
grade <- "halfmonth"
predict_start_date=20140401
predict_time_range=24


for (i in c(1:length(product))) {
  # Load data source
  KPI <- read.csv(file_name[i], header=TRUE)

  d1 <- agg_predict(KPI, factor[1], grade, predict_time_range)
  d2 <- agg_predict(KPI, factor[2], grade, predict_time_range)
  d3 <- agg_predict(KPI, factor[3], grade, predict_time_range)

  d <- agg_merge(cbind(d1$month,d1$average,d2$average,d3$average))
  p <- cbind(product=rep(product_name[i],nrow(d)))

  if (i==1) {
    ret1 <- d
    ret2 <- p
  } else {
    ret1 <- rbind(ret1, d)
    ret2 <- rbind(ret2, p)
  }
  ret <- cbind(ret2,ret1)
  print (cbind(ret1,ret2))
}

write.csv(ret, "channel_product_predict.out")

