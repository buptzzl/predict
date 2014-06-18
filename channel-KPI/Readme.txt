
# 预测结果解释
样本数据区间： 2013-01-01/2014-06-04 

browser.lm.pdf 
基于时间特征进行拟合。黑色为源数据，其他颜色为不同特征组合训练出的模型的拟合效果。

browser.lm.final.pdf 
增加当前TOP3 + 历史TOP4-10的渠道数据 作为特征，训练出的模型的拟合效果。

browser.lm.finalPredict@Residuals.pdf
对样本基于距离预测时间的前后进行加权后的拟合与预测效果。

browser.lm.final.EffeTrain@Predict@Residuals.pdf
对TOP3 + TOP4-10 的渠道，进行后半段部分拟合后，其结果作为特征的一部分，训练出的模型的拟合与预测效果。






