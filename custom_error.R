rel_med <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err<-as.numeric(median(abs((labels)-(preds))/(labels)))
  return(list(metric = "error", value = err))
}
rel_mean <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err<-as.numeric(mean(abs((labels)-(preds))/exp(labels)))
  return(list(metric = "error", value = err))
}

MAE2 <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err<-as.numeric(median(abs((labels)-(preds))))
  return(list(metric = "error", value = err))
}
