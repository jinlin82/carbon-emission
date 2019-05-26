#############arima和ets的cross对象#################
rm(list=ls())
####1.切割点产生################
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

cross_validation2 <- function(
  a,model, horizon, units, period = NULL, initial = NULL) {
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
    if("a"=="arima"){#函数不能直接比较
      m2<- a(history.c$y,order=c(1,1,1))#模型拟合
    }else{
      m2=a(history.c$y)
    }
    yhat <- forecast(m2, length(df.predict[,1]))#预测值
    df.c <- cbind(df.predict, yhat)[,c(1,2,6,9,10)]
    colnames(df.c)=c("ds",'y','yhat','yhat_lower', 'yhat_upper')#选用95%置信区间
    df.c$cutoff <- cutoff# Merge yhat, y, and cutoff.
    predicts <- rbind(predicts, df.c)
  }
  return(predicts)
}

arimacrossday=cross_validation2(arima,m,horizon = 90,initial = 270,period = 45)
etscrossday=cross_validation2(ets,m,horizon = 90,initial = 270,period = 45)

arimaperform=performance_metrics(arimacrossday,rolling_window = 0.1)
etsperform=performance_metrics(etscrossday,rolling_window = 0.1)

plot_cross_validation_metric(arimacrossday,metric = "mape",rolling_window =0.1)
plot_cross_validation_metric(etscrossday,metric = "mape",rolling_window =0.1)
plot_cross_validation_metric(crossday,metric = "mape",rolling_window =0.1)
