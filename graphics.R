rm(list=ls())
## setwd("E:/paper&project/paper/journal/carbon-emission")
library(data.table)
Data <- read.csv("./data/guangdong.csv",header = T,sep=",")
setcolorder(Data,c("AQIIndex","ClosePrice","Europe_change","Global_GasPrice","Eco_rmb","JDWeather","cer_rmb_diff","TAN180_Change","Deal"))
#network
predict_nnet <- read.csv("./data/predict_nnet_61.csv",header = T,sep=",")
predict_nnet <- as.vector(predict_nnet$V1)
par(mar=c(5,7,4,2)+0.1, oma=c(0, 0, 2, 0))
plot(predict_nnet[1:367],col="red",type="b",pch=16,xlab="日期",ylab="碳价/元",xaxt="n",cex.lab=2,cex.axis=1.5,ylim=c(11,19))
points(Data[1:357,9],col="blue",type="b",pch=4)
axis(1,at=c(1,50,100,150,200,250,300,350),labels = c("2017/1/4","2017/3/23","2017/6/9","2017/8/21","2017/11/6",
                                                     "2018/1/7","2018/4/9","2018/6/21"),cex.axis=1.5)
legend("topright",c("预测值","真实值"),pch=c(16,4),col=c("red","blue"),bty="n",cex=2,x.intersp=0.25,y.intersp=0.5)

#arma
arma <- read.csv("./data/arima_result.csv",header = T,sep=",")
plot(arma$x,col="red",type="b",pch=16,xlab="日期",ylab="碳价/元",xaxt="n",ylim = c(11,19),cex.lab=2,cex.axis=1.5)
points(Data[1:357,9],col="blue",type="b",pch=4)
axis(1,at=c(1,50,100,150,200,250,300,350),labels = c("2017/1/4","2017/3/23","2017/6/9","2017/8/21","2017/11/6",
                                                     "2018/1/7","2018/4/9","2018/6/21"),cex.axis=1.5)
legend("topright",c("预测值","真实值"),pch=c(16,4),col=c("red","blue"),bty="n",cex=2,x.intersp=0.25,y.intersp=0.5)

#prophet
prophet_model <- read.csv("./data/prophet.csv",header = T,sep=",")
plot(prophet_model$x,col="red",type="b",pch=16,xlab="日期",ylab="碳价/元",xaxt="n",ylim=c(11,19),cex.lab=2,cex.axis=1.5)
points(Data[1:357,9],col="blue",type="b",pch=4)
axis(1,at=c(1,50,100,150,200,250,300,350),labels = c("2017/1/4","2017/3/23","2017/6/9","2017/8/21","2017/11/6",
                                                     "2018/1/7","2018/4/9","2018/6/21"),cex.axis=1.5)
legend("topright",c("预测值","真实值"),pch=c(16,4),col=c("red","blue"),bty="n",cex=2,x.intersp=0.25,y.intersp=0.5)

#MAPE
accuracy <- read.csv("./data/MAPE.csv",header =T,sep=",")
par(mar=c(5,7,4,2)+0.1, oma=c(0, 0, 2, 0))
plot(accuracy[,1],type="b",ylim=c(0,10),
     xlab="预测期",ylab="MAPE",cex.lab=1,cex.axis=1.,pch=18,lwd=1.5,cex=1.)
lines(accuracy[,2],type="b",col="red",pch=16,lwd=1.5,cex=1.)
lines(accuracy[,3],type="b",col="blue",pch=4,lwd=1.5,cex=1.)
legend("topright",c("灰色BP","Prophet","ARMA"),pch=c(18,16,4),
       col=c("black","red","blue"),bty="n",cex=1.,x.intersp=0.25,y.intersp=0.5)  

