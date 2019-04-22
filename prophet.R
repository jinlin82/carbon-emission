###问题1：prophet函数中的yearly.seasonality="TRUE","auto","FALSE",以及“Fourier terms”有什么区别
###问题2：Fourier terms中的x(t),t是否需要从1开始重新编号
###问题3：simulated historical forecast是如何处理误差的 
rm=list()
history=read.csv("E:/github_repo/carbon-emission/data/updata/guangdong.csv")
#help("as.Date")

history$date=as.Date(history$date,"%Y/%m/%d")
str(history)
colnames(history)=c("ds","y")
#arma模型
##平稳性检验
library(tseries)
pp.test(history$y)#10%的显著性水平下平稳


##自相关
par(mfrow=c(2,1)) 

acf(history$y,main="",xlab="滞后期",ylab="ACF")#画自相关图
title(main = "(a)the ACF of dealprice",cex.main=0.95)

pacf(history$y,main="",xlab="滞后期",ylab="PACF",las=1)#画偏自相关图
title(main="(b)the PACF of dealprice",cex.main=0.95)

##模型拟合
m1=arima(history$y,order=c(1,0,1))

##获取残差序列
resid=m1$residuals
plot(resid)

par(mfrow=c(2,1))
rt.square<-resid^2
acf(rt.square,main="",xlab="lag(c)",ylab="ACF",las=1)#画自相关图
title(main = "(c)the ACF of resi Square",cex.main=0.95)
pacf(rt.square,main="",xlab="Lag(d)",ylab="PACF",las=1)#画偏自相关图
title(main = "(d)the PACF of resi Square",cex.main=0.95)

##ARCH效应检验
library(FinTS)
ArchTest(resid,lag=12)  #存在ARCH效应

library(fGarch)
m2=garchFit(~arma(1,1)+garch(1,1),data=history$y)
pre=predict(m2,n.ahead=10)
#?seq.Date
pre$ds=seq.Date(as.Date("2019/04/02"),as.Date("2019-04-11"),by="day")
pre=as.data.frame(pre[,c(4,1)])
colnames(pre)=c("ds","value")
fitvalue=data.frame(history$ds,m2@fitted)
colnames(fitvalue)=c("ds","value")
arma.his=rbind(fitvalue,pre)#将拟合值与预测值合并
##
par(mfrow=c(1,1))
plot(arma.his,lty=1,type="b",pch=16,xlab="日期",ylab="碳价/元",xaxt="n",cex.lab=0.8,cex.axis=0.7,ylim=c(11,19))
points(history$y,lty=2,type="b",pch=4)
axis(1,at=c(1,100,200,300,400,500),labels = c("2017/1/3","2017/6/5","2017/10/30","2018/3/27","2018/8/16","2019/1/15"),cex.axis=0.7)
legend("topleft",c("预测值","真实值"),pch=c(16,4),lty=c(1,2),bty="n",cex=0.7)

#prophet模型
library(prophet)
m3=prophet(history)
str(history)
library(stringr)
?str_sub
date=as.character(history$ds)
date
str_sub(date[1],6,7)
help("prophet")
#m31=prophet(history,yearly.seasonality = TRUE,)
plot(history$ds,history$y)
m31=prophet(history,yearly.seasonality = TRUE,weekly.seasonality=TRUE,daily.seasonality = FALSE)
m31$component.modes
future31=make_future_dataframe(m31,periods = 10)
forecast31=predict(m31,future31)
plot(m31,forecast31)
prophet_plot_components(m31,forecast31)

#Fourier terms
N=c(1:10)
n=c(1,2,3)
x1=2*pi*N/365.25
x2=2*pi*n/7
xt.yearly=c(cos(x1),sin(x1))
xt.weekly=c(cos(x2),sin(x2))
#预测值对比
str(forecast)
predictvalue=cbind(pre,forecast[c(550:559),19],forecast31[c(550:559),19])
colnames(predictvalue)=c("ds","armapre","propre")
  
#cross-validation
as.Date("2017/01/03","%Y/%m/%d")+105
crossday=cross_validation(m31,180,units="days")
write.csv(crossday,"E:/github_repo/carbon-emission/data/updata/crossday.csv")
crossday=read.csv("E:/github_repo/carbon-emission/data/updata/crossday.csv")
perme=performance_metrics(crossday,metrics="mape",rolling_window = 0.1)
plot_cross_validation_metric(crossday,metric = "mape",rolling_window = 0.1)
plot_forecast_component(m31,forecast31,forecast31$weekly,uncertainty = TRUE,plot_cap = FALSE)
plot(m31,forecast31)+add_changepoints_to_plot(m31, threshold = 0.01, cp_color = "red",cp_linetype = "dashed", trend = TRUE)
#as.Date("2019/04/01","%Y/%m/%d")-90
#as.Date("2019/02/15","%Y/%m/%d")-as.Date("2018/11/19","%Y/%m/%d")
#as.Date("2019/01/01","%Y/%m/%d")-90*0.5+90
#as.Date("2018/11/17","%Y/%m/%d")-as.Date("2018/07/05","%Y/%m/%d")
?c
#as.Date("2017/09/28","%Y/%m/%d")-as.Date("2017/07/06","%Y/%m/%d")
#as.Date("2018/02/04","%Y/%m/%d")-as.Date("2017/11/25","%Y/%m/%d")


#节假日数据（文中无假日效应）
library(dplyr)
playoffs <- data_frame(
  holiday = 'playoff',
  ds = as.Date(c('2008-01-13', '2009-01-03', '2010-01-16',
                 '2010-01-24', '2010-02-07', '2011-01-08',
                 '2013-01-12', '2014-01-12', '2014-01-19',
                 '2014-02-02', '2015-01-11', '2016-01-17',
                 '2016-01-24', '2016-02-07')),
  lower_window = 0,
  upper_window = 1
)
superbowls <- data_frame(
  holiday = 'superbowl',
  ds = as.Date(c('2010-02-07', '2014-02-02', '2016-02-07')),
  lower_window = 0,
  upper_window = 1
)
holidays <- bind_rows(playoffs, superbowls)
