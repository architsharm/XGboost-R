fairloss <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  c=2
  x<-(-labels+preds)
  grad <- c*x/(abs(x)+c)
  hess <- c^2/(abs(x)+c)^2
  
  threshold<-1
  var=runif(1,0,1)
  
  grad = (abs(x)<threshold )*grad - (abs(x)>=threshold )*var 
  hess = (abs(x)<threshold )*hess + (abs(x)>=threshold ) 
  return(list(grad = grad, hess = hess))
}

huberloss <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  h=1
  d<-preds-labels
  scale = 1 + (d / h)^2
  scale_sqrt = sqrt(scale)
  
  grad <- d / scale_sqrt
  hess <- 1 / scale / scale_sqrt
  
  threshold<-1
  var=runif(1,0,1)
  grad = (abs(d)<threshold )*grad - (abs(d)>=threshold )*var 
  hess = (abs(d)<threshold )*hess + (abs(d)>=threshold ) 
  return(list(grad = grad, hess = hess))
}

logcosh <- function(preds, dtrain) {
  
  labels <- getinfo(dtrain, "label")
  x<-preds-labels
  grad <- tanh(x)
  hess <- 1/(cosh(x)^2)
  
  threshold<-1
  var=runif(1,0,1)
  grad = (abs(x)<threshold )*grad - (abs(x)>=threshold )*var 
  hess = (abs(x)<threshold )*hess + (abs(x)>=threshold ) 
  return(list(grad = grad, hess = hess))
}
