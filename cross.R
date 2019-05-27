#history2=read.csv("E:/github_repo/carbon-emission/data/updata/guangdong2.csv")
#colnames(history2)=c("ds","y","weekday")
#history2$ds=as.Date(history2$ds,"%Y/%m/%d")
#library(prophet)
#history=history2[,c(1,2)]
#m <- prophet(weekly.seasonality=FALSE, changepoint.prior.scale = 0.05)
#m <- add_seasonality(m, name='weaking', period=5, fourier.order=3)
#m <- fit.prophet(m, history)
#crossday=cross_validation(m,90,units="days")#运行时间略长
#perform=performance_metrics(crossday,rolling_window = 0.1)
#############arima和ets的cross对象#################
rm(list=ls())
####1切割点产生################
generate_cutoffs <- function(df, horizon, initial, period) {
  # Last cutoff is (latest date in data) - (horizon).
  cutoff <- max(df$ds) - horizon
  tzone <- attr(cutoff, "tzone")  # Timezone is wiped by putting in array
  result <- c(cutoff)
  while (result[length(result)] >= min(df$ds) + initial) {
    cutoff <- cutoff - period
    # If data does not exist in data range (cutoff, cutoff + horizon]
    if (!any((df$ds > cutoff) & (df$ds <= cutoff + horizon))) {
      # Next cutoff point is 'closest date before cutoff in data - horizon'
      if (cutoff > min(df$ds)) {
        closest.date <- max(df$ds[df$ds <= cutoff])
        cutoff <- closest.date - horizon
      }
      # else no data left, leave cutoff as is, it will be dropped.
    }
    result <- c(result, cutoff)
  }
  result <- utils::head(result, -1)
  if (length(result) == 0) {
    stop(paste(
      'Less data than horizon after initial window.',
      'Make horizon or initial shorter.'
    ))
  }
  # Reset timezones
  attr(result, "tzone") <- tzone
  message(paste(
    'Making', length(result), 'forecasts with cutoffs between',
    result[length(result)], 'and', result[1]
  ))
  return(rev(result))
}
###################求arima的cross#########################################
cross_validation2 <- function(
  model, horizon, units, period = NULL, initial = NULL) {
  df=model$history
  horizon.dt <- as.difftime(horizon, units = "days")
  initial.dt <- as.difftime(3 * horizon, units = "days")
  period.dt <- as.difftime(0.5 * horizon, units ="days")
  cutoffs <- generate_cutoffs(df, horizon.dt, initial.dt, period.dt)
  predicts <- data.frame()
  for (i in 1:length(cutoffs)) {
    cutoff <- cutoffs[i]
    history.c <- dplyr::filter(df, ds <= cutoff)#产生拟合数据
    df.predict <- dplyr::filter(df, ds > cutoff, ds <= cutoff + horizon.dt)#真实值
    m2<- arima(history.c$y,order=c(1,1,1))#模型拟合
    yhat <- forecast::forecast(m2, length(df.predict[,1]))#预测值
    df.c <- cbind(df.predict, yhat)[,c("ds","y","Point Forecast","Lo 95","Hi 95")]
    colnames(df.c)=c("ds",'y','yhat','yhat_lower', 'yhat_upper')#选用95%置信区间
    df.c$cutoff <- cutoff# Merge yhat, y, and cutoff.
    predicts <- rbind(predicts, df.c)
  }
  return(predicts)
}
###################求ets的cross#########################################
#i=2;model=m;horizon=90
cross_validation3 <- function(
  model, horizon, units, period = NULL, initial = NULL) {
  df=model$history
  horizon.dt <- as.difftime(horizon, units = "days")
  initial.dt <- as.difftime(3 * horizon, units = "days")
  period.dt <- as.difftime(0.5 * horizon, units ="days")
  cutoffs <- generate_cutoffs(df, horizon.dt, initial.dt, period.dt)
  predicts <- data.frame()
  for (i in 1:length(cutoffs)) {
    cutoff <- cutoffs[i]
    history.c <- dplyr::filter(df, ds <= cutoff)#产生拟合数据
    df.predict <- dplyr::filter(df, ds > cutoff, ds <= cutoff + horizon.dt)#真实值
    m2<- ets(history.c$y)#模型拟合
    yhat <- forecast::forecast(m2, length(df.predict[,1]))#预测值
    df.c <- cbind(df.predict, yhat)[,c("ds","y","Point Forecast","Lo 95","Hi 95")]
    colnames(df.c)=c("ds",'y','yhat','yhat_lower', 'yhat_upper')#选用95%置信区间
    df.c$cutoff <- cutoff# Merge yhat, y, and cutoff.
    predicts <- rbind(predicts, df.c)
  }
  return(predicts)
}

arimacrossday=cross_validation2(m,horizon = 90,initial = 270,period = 45)

etscrossday=cross_validation3(m,horizon = 90,initial = 270,period = 45)

arimaperform=performance_metrics(arimacrossday,rolling_window = 0.1)
etsperform=performance_metrics(etscrossday,rolling_window = 0.1)

plot_cross_validation_metric(arimacrossday,metric = "mape",rolling_window =0.1)
plot_cross_validation_metric(etscrossday,metric = "mape",rolling_window =0.1)
plot_cross_validation_metric(crossday,metric = "mape",rolling_window =0.1)


########################计算mape###############################
rolling_mean_by_h <- function(x, h, w, name) {
  # Aggregate over h
  df <- data.frame(x=x, h=h)
  df2 <- df %>%
    dplyr::group_by(h) %>% 
    dplyr::summarise(mean = mean(x), n = dplyr::n())
  
  xm <- df2$mean
  ns <- df2$n
  hs <- df2$h
  
  res <- data.frame(horizon=c())
  res[[name]] <- c()
  # Start from the right and work backwards
  i <- length(hs)
  while (i > 0) {
    # Construct a mean of at least w samples
    n <- ns[i]
    xbar <- xm[i]
    j <- i - 1
    while ((n < w) & (j > 0)) {
      # Include points from the previous horizon. All of them if still less
      # than w, otherwise just enough to get to w.
      n2 <- min(w - n, ns[j])
      xbar <- xbar * (n / (n + n2)) + xm[j] * (n2 / (n + n2))
      n <- n + n2
      j <- j - 1
    }
    if (n < w) {
      # Ran out of horizons before enough points.
      break
    }
    res.i <- data.frame(horizon=hs[i])
    res.i[[name]] <- xbar
    res <- rbind(res.i, res)
    i <- i - 1
  }
  return(res)
}

#df=crossday;w=66;x=ape;h=df$horizon;name="mape"
mape2 <- function(df, w) {
  df$horizon <- df$ds - df$cutoff
  df <- df[order(df$horizon),]
  ape <- abs((df$y - df$yhat) / df$y)
  if (w < 0) {
    return(data.frame(horizon = df$horizon, mape = ape))
  }
  return(rolling_mean_by_h(x = ape, h = df$horizon, w = w, name = 'mape'))
}

arimamape=mape2(arimacrossday,66)
prophetmape=mape2(crossday,66)
etsmape=mape2(etscrossday,66)
plot(etsmape,type="l",xlab="Horizon(days)",ylab="mape",ylim=c(0,0.2),lty=1)
lines(prophetmape,type="p",lty=2)
lines(arimamape,type="l",lty=3)
legend("topright",c("etsmape","prophetmape","arimamape"),cex=0.7,lty=c(1,2,3),x.intersp=0.25,y.intersp=0.5)
