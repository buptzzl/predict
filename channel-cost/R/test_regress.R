require(car)
require(TSA)
require(MASS)


check_model = function (lm.mode, 
                        ckc_lst=list(p.value=0, RES=c(), R2=c(), R2.adj=c(), acf_chk=c(), p.normal=0)) {
  model_ = lm.mode;
  p.valu = pf(model_$fstatistic[1], model_$fstatistic[2], model_$fstatistic[3], lower.tail=F);
  if (p.valu >= 0.05) {
    #print ("coefficience may be zero.")
    ckc_lst$p.value = p.value;
  }
  
  RSE = model_$sigma
  ckc_lst$RES = RSE;
  
  R2 = model_$r.squared
  R2.adj = model_$adj.ar.squared
  ckc_lst$R2 = R2; ckc_lst$R2.adj = R2.adj;
  
  acf. = acf(rstudent(model_), plot=F) #TSA
  acf.shel = 2/sqrt(acf.$n.used)
  if (length (acf.$acf > acf.shel) > 0 || length (acf.$acf < -1*acf.shel)) {
    #print ("acf test failed.")
    ckc_lst$acf.chk = length(acf.$acf > acf.shel) + length(acf.$acf < -1*acf.shel)
  }
  
  test.norm.shap = shapiro.test (rstandard(model_))
  print (paste("residual norm test: p.value=", test.norm.shap$p.value))
    #dat = data.frame (nw=res.lm$lm.dat$model$nw, et=res.lm$lm.dat$model$et, od=res.lm$lm.dat$model$od)
    #boxcovx (nw ~ 0 + od + +et, data=dat)  # 尝试对拟合值变换
  }
  
  test.outer = outlierTest(model_)
  test.outer.idx = names(test.outer$p)
  print (paste("outliertest:", test.outer$p))
  
  return(ckc_lst)
}