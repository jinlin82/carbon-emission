###问题1：prophet函数中的yearly.seasonality="TRUE","auto","FALSE",以及“Fourier terms”有什么区别
###问题2：Fourier terms中的x(t),t是否需要从1开始重新编号
###问题3：simulated historical forecast是如何处理误差的 
rm=list()
history=read.csv("E:/github_repo/carbon-emission/data/updata/guangdong.csv")
#help("as.Date")

history$data=as.Date(history$data,"%Y/%m/%d")
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
future=make_future_dataframe(m3,periods = 10)
forecast=predict(m3,future)
plot(m3,forecast)
prophet_plot_components(m3,forecast)
#help("prophet")
m3$growth
m3$changepoints
#m31=prophet(history,yearly.seasonality = TRUE,)
m31=prophet(history,yearly.seasonality = TRUE,weekly.seasonality=TRUE,daily.seasonality = TRUE)
future31=make_future_dataframe(m31,periods = 10)
forecast31=predict(m31,future31)
plot(m31,forecast31)
prophet_plot_components(m31,forecast)

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
  

    
