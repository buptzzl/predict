# Common Function Definition

inst_aggregate <- function(dat, grade='day', agg_type='mean') {
  date <- as.Date(paste(substr(dat$date,1,4),substr(dat$date,5,6),substr(dat$date,7,8),sep='-'))
  date_f <- switch(grade,
         day = "%Y-%m-%d",
         week = "%Y-%W",
         halfmonth = "%Y%m%d",
         month = "%Y-%m",
         year = "%Y"
  )
  fieldname <- names(dat)[names(dat)!="date"]
  agg_date <- format(date, date_f)
  if (grade == "halfmonth") {
    halfmonth <- function(str) {
      yearmonth <- substr(str,1,6)
      day <- "02"
      if (as.numeric(substr(str,7,8)) <= 15) {
        day <- "01"
      }
      return (paste(yearmonth,day,sep=""))
    }
    agg_date <- unlist(lapply(as.list(agg_date),halfmonth))
  }
  dat[grade] <- agg_date
  newdate <- unique(agg_date)
  dat2 <- data.frame(newdate)
  for (col in fieldname) {
    dat2[col] <- as.numeric(tapply(dat[,col],dat[,grade],agg_type,simpify=FALSE))
  }
  return (dat2)
}
#test <- inst_aggregate(KPI[1:nrow(KPI),c("date","activate","active")],grade='week')

date_trans <- function(start, range, period=c(0,12,31)) {
  year <- as.numeric(substr(start, 1, 4))
  month <- as.numeric(substr(start, 5, 6))
  day <- as.numeric(substr(start, 7, 8))
  trans <- c(1:range)
  for (i in 1:range) {
    if ((day+1)%%period[3]==1) {
      dayup <- 1
      day <- 1
    } else {
      dayup <- 0
      day <- day + 1
    }

    if (month==period[2] && (month+dayup)%%period[2]==1) {
      monthup <- 1
      month <- 1
    } else {
      monthup <- 0
      month <- month + dayup
    }
    year <- year + monthup
    trans[i] <- as.numeric(year*10000+month*100+day)
  }
  return (trans)
}

inst_curve_fit <- function(y, x, fun, gragh=FALSE) {
  x_star <- switch(fun,
                   line = x,
                   log = log(x)
  )
  fit <- lm(y~x_star)
  R_Square <- round(as.numeric(summary(fit)["adj.r.squared"]),2)
  MAE <- round(sum(abs(as.numeric(unlist(summary(fit)["residuals"]))))/sum(y),3)
  SD <- round(sd(as.numeric(unlist(summary(fit)["residuals"]))))
  if (gragh) {
    plot(y)
    lines(fit$coefficients[1]+fit$coefficients[2]*x_star)
    title(paste("Adj-R-Square:",R_Square,"MAE:",MAE,"SD:",SD))
  }
  return (fit)
}

#effect <- inst_curve_fit(test$activate,c(1:nrow(test)),"log", TRUE)

inst_curve_predict <- function(fit, y, x, range, fun, arima=FALSE) {
  x_hat <- c((length(y)+1):(length(y)+range))
  y_hat <- switch(fun,
                  line = fit$coefficients[1]+fit$coefficients[2]*x_hat,
                  log = fit$coefficients[1]+fit$coefficients[2]*log(x_hat)
                  )
  if(arima) {
    ydiffts <- as.numeric(unlist(summary(fit)["residuals"]))
    ydiffts <- ts(ydiffts, start=1, frequency=1)
    arima1 <- auto.arima(ydiffts, trace = TRUE, stationary = FALSE, seasonal = TRUE)
    ydifffore <- forecast.Arima(arima1, h = range, level=c(68,95))
    y_hat <- y_hat + ydifffore$mean
  }
  plot(c(x,x_hat),c(y,y_hat))
  lines(x_hat, y_hat)
}
#inst_curve_predict(fit_log, y, x, range=12, 'log', FALSE)


inst_dlm_predict <- function(y_train, x_train, prange, reg=FALSE) {
  # Construct SSM Model
  ssm1 <- function(parm, x.mat) {
    dlm <- dlmModPoly(order = 2, m0=c(y_train[1],0),dV = (parm[1]), dW = (parm[2:3])) + 
      dlmModSeas(7, dV = 0, dW = c(0,rep(0,5)), C0 = 1e+2*diag(6)) + 
      dlmModReg(X=x.mat, addInt=FALSE, m0=rep(0,ncol(x_train)), dV=0, dW=rep(parm[4],ncol(x_train)), C0=(1e+12)*diag(nrow=ncol(x_train)))
  }
  ssm2 <- function(parm) {
    dlm <- dlmModPoly(order = 2, m0=c(y_train[1],0),dV = (parm[1]), dW = (parm[2:3])) + 
      dlmModSeas(7, dV = 0, dW = c(0,rep(0,5)), C0 = 1e+2*diag(6))
  } 
  
  # MLE, train using specified days
  if (reg==FALSE){
    fit <- dlmMLE(y_train, rep(0,4), build=ssm2,  lower=rep(1e-6, 5))
    Mod <- ssm2(fit$par)
  } else {
    fit <- dlmMLE(y_train, rep(0,4), x.mat=x_train, build=ssm1,  lower=rep(1e-6, 5))
    Mod <- ssm1(fit$par,x_train)
  }
  print (paste("convergence:",fit$convergence))
  #y_padd <- c(y_train, rep(NA, prange))
  y_padd <- y_train
  ModFilt <- dlmFilter(y_padd, Mod)
  print (ModFilt$mod$FF)
  
  return (ModFilt)
}