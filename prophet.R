###问题1：prophet函数中的yearly.seasonality="TRUE","auto","FALSE",以及“Fourier terms”有什么区别
###问题2：Fourier terms中的x(t),t是否需要从1开始重新编号
###问题3：simulated historical forecast是如何处理误差的 
rm(list=ls())
history=read.csv("E:/github_repo/carbon-emission/data/updata/guangdong2.csv")
#help("as.Date")

history$date=as.Date(history$date,"%Y/%m/%d")
str(history)
colnames(history)=c("ds","y")
#arma模型
##平稳性检验
library(tseries)
pp.test(history$y)
dh=diff(history$y)
pp.test(dh)

##自相关
par(mfrow=c(2,1)) 

acf(dh,main="",xlab="滞后期",ylab="ACF")#画自相关图
title(main = "(a)the ACF of dealprice",cex.main=0.95)

pacf(dh,main="",xlab="滞后期",ylab="PACF",las=1)#画偏自相关图
title(main="(b)the PACF of dealprice",cex.main=0.95)

##模型拟合
Box.test(dh,lag=10,type="Ljung")
m1.1=arima(history$y,order=c(1,1,1))
m1.0=arima(history$y,order=c(1,1,0))
m1.2=arima(history$y,order=c(2,1,0))
m1.3=arima(history$y,order=c(0,1,1))
m1.4=arima(history$y,order=c(0,1,2))
Box.test(m1.1$residuals,lag=10,type="Ljung")
Box.test(m1.0$residuals,lag=10,type="Ljung")
Box.test(m1.2$residuals,lag=10,type="Ljung")
Box.test(m1.3$residuals,lag=10,type="Ljung")
Box.test(m1.4$residuals,lag=10,type="Ljung")
##获取残差序列
resid=m1.1$residuals
plot(resid)
auto.arima(history$y)
plot(predict(m1.1,n.ahead=10)$pred)
#正态性检验
jarque.bera.test(resid)
shapiro.test(resid)
#异常值检验
#summary(history$y)
#boxplot(history$y)
#hist(resid,main=NULL,breaks = 50)
#qqnorm(resid,main =NULL)
#qqline(resid)
which(abs(resid)>3)
resid[431]
resid0=resid[-434]
Box.test(resid0,lag=10,type="Ljung")
ArchTest(resid0,lag=12) 

#jarque.bera.test(resid)
#shapiro.test(resid)
plot(history$y)
hist(resid0,main=NULL,breaks = 50)
qqnorm(resid0,main =NULL)
qqline(resid0)
##ARCH效应检验
library(FinTS)
ArchTest(resid,lag=12)  #存在ARCH效应

par(mfrow=c(1,1))
plot(resid)
rt.square<-resid^2
acf(rt.square,main="",xlab="lag(c)",ylab="ACF",las=1)#画自相关图
title(main = "(c)the ACF of resi Square",cex.main=0.95)
pacf(rt.square,main="",xlab="Lag(d)",ylab="PACF",las=1)#画偏自相关图
title(main = "(d)the PACF of resi Square",cex.main=0.95)

library(fGarch)
help(package="fGarch")
fit2.1=garchFit(formula = ~arma(1,1)+garch(1,1),data=dh,cond.dist = c("norm"))
resid1=fit2.1@residuals
plot(resid1)
#which(abs(resid1)>3)
#resid1[430]
#resid1[433]

Box.test(resid1,lag=10,type="Ljung")
ArchTest(resid1,lag=12) 
?garchFit
library(rugarch)
?ugarchspec
garch_mod=ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model=list(armaOrder=c(1,1)))
fit.2=ugarchfit(spec=garch_mod,data=dh)
resid2=fit.2@fit$residuals
Box.test(resid2,lag=10,type="Ljung")
Box.test(resid2^2,lag=10,type="Ljung")
ArchTest(resid2,lag=20) 
#########################################################指数平滑法###############
library(forecast)
fit2=ets(history$y)
pre2=predict(fit2,n.ahead=10)

str(m1.1)
#预测
arima=predict(m1.1,n.ahead=10)$pred
ets=predict(fit2,n.ahead=10)[["mean"]]
#?seq.Date
pre=as.data.frame(cbind(arima,ets))
ds=seq.Date(as.Date("2019/04/02"),as.Date("2019-04-11"),by="day")
pre=cbind(ds,pre)
str(m1.1$residuals)
str(fitvalue1)
resid1=data.frame(history$ds,as.numeric(m1.1$residuals))
fitvalue1=data.frame(history$ds,as.numeric(fitted(m1.1)))
colnames(fitvalue1)=c("ds","value1")
resid2=as.data.frame(as.numeric(fit2[["residuals"]]))
value2=as.data.frame(as.numeric(fit2[["fitted"]]))
resid=cbind(resid1,resid2)
colnames(resid)=c("ds","arima","ets")
fitvalue=cbind(fitvalue1,value2)
colnames(fitvalue)=c("ds","arima","ets")
value=rbind(fitvalue,pre)#将拟合值与预测值合并
##
par(mfrow=c(1,1))
plot(value$arima,lty=1,type="b",pch=10,xlab="日期",ylab="碳价/元",xaxt="n",cex.lab=0.8,cex.axis=0.7,ylim=c(11,22))
lines(history$y,lty=1,type="b",pch=4,col="blue")
lines(value$ets,lty=0,type="b",pch=5,col="red")
axis(1,at=c(1,100,200,300,400,500),labels = c("2017/1/3","2017/6/5","2017/10/30","2018/3/27","2018/8/16","2019/1/15"),cex.axis=0.7)
legend("topleft",c("预测值","真实值"),pch=c(10,4),lty=c(1,2),bty="n",cex=0.7)

par(mfrow=c(1,1))
plot(resid$arima,lty=1,type="b",pch=10,xlab="日期",ylab="residuals",xaxt="n",cex.lab=0.8,cex.axis=0.7)
lines(resid$ets,lty=2,type="b",pch=5,col="red")
axis(1,at=c(1,100,200,300,400,500),labels = c("2017/1/3","2017/6/5","2017/10/30","2018/3/27","2018/8/16","2019/1/15"),cex.axis=0.7)
legend("topleft",c("arima","ets"),pch=c(10,5),lty=c(1,2),bty="n",cex=0.7)
plot(resid$ets)
which(abs(resid$ets)>3)
#################################################################prophet模型
library(prophet)
m3=prophet(history)
#m31=prophet(history,yearly.seasonality = TRUE,)
plot(history$ds,history$y)
m31=prophet(history,yearly.seasonality = TRUE,weekly.seasonality=TRUE,daily.seasonality = FALSE)
m31$changepoints
str(m31)
future31=make_future_dataframe(m31,periods = 10)
forecast31=predict(m31,future31)
plot(m31,forecast31)
prophet_plot_components(m31,forecast31)
?prophet
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
help(package="prophet")
?prophet
rm(list=ls())
monday=read.csv("E:/github_repo/carbon-emission/data/updata/monday.csv")
tuesday=read.csv("E:/github_repo/carbon-emission/data/updata/tuesday.csv")
Wednesday=read.csv("E:/github_repo/carbon-emission/data/updata/Wednesday.csv")
Thurday=read.csv("E:/github_repo/carbon-emission/data/updata/Thurday.csv")
Friday=read.csv("E:/github_repo/carbon-emission/data/updata/Friday.csv")
plot(monday$dealprice,type="b",lty=0,col="red",ylim=c(12,20))
lines(tuesday$dealprice,type="b",lty=0,col="black",ylim=c(12,20))
lines(Wednesday$dealprice,type="b",lty=0,col="green",ylim=c(12,20))
lines(Thurday$dealprice,type="b",lty=0,col="blue",ylim=c(12,20))
lines(Friday$dealprice,type="b",lty=0,col="yellow",ylim=c(12,20))

guangdong2=read.csv("E:/github_repo/carbon-emission/data/updata/guangdong2.csv")
guangdong2$weekday=factor(guangdong2$weekday)
library(ggplot2)
head(guangdong2)
ggplot(data=guangdong2,mapping=aes(x=guangdong2$date,y=guangdong2$dealprice,colour=guangdong2$weekday))+geom_point(size=2,ylim=c(11,20))