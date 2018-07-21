library(caTools)
library(xgboost)
library(caret)
library(mlbench)
library(tidyverse)
library(plyr)
library(ggplot2)
library(MLmetrics)
library(readr)
library(corrplot)
library(rsconnect)
library(Matrix)

source("custom_loss.R")
source("custom_error.R")
data1<-read.csv("Data.csv",stringsAsFactors = F)

type<-unique(data1$type)%>%as.list()
data12<-data.frame()
for(i in type){
  temp<-filter(data1,data1$type==i)
  l<-boxplot.stats(temp$area)$stats[1]
  u<-boxplot.stats(temp$area)$stats[5]
  temp<-filter(temp,!(temp$area<l|temp$area>u))
  data12<-rbind(data12,temp)
}


#Area-BHK outliers 
bhk<-unique(data12$bhk)%>%as.list()
data121<-data.frame()
for(i in bhk){
  temp<-filter(data12,data12$bhk==i)
  l<-boxplot.stats(temp$area)$stats[1]
  u<-boxplot.stats(temp$area)$stats[5]
  temp<-filter(temp,!(temp$area<l|temp$area>u))
  data121<-rbind(data121,temp)
}

data2<-data121

# Find correlated variables so that not to confuse model
t3<-select(data2,-rate,-source,-price,-city,-lat,-lon)
t1<-select(data2,rate)
input_cor<-cor(t3)
highlyCorrelated <- findCorrelation(input_cor, cutoff=0.75)
highlyCorrelated1<-names(select(t3,highlyCorrelated))

# Find feature impportance using correlation/linear regression with the output
t5<-select(t3,-highlyCorrelated)
correlationMatrix <- cor(t5,t1)
names<-rownames(correlationMatrix)%>%as.data.frame()
t2<-unlist(correlationMatrix)%>%as.data.frame()
t2<-cbind(names,t2)
names(t2)<-c("names","rate")
t2$names<-as.factor(t2$names)
t2$rate<-abs(t2$rate)
t<-t2[order(t2$rate),]
t$names <- factor(t$names, levels = t$names[order(t$rate)])

#cutoff point in plot comes to be at alightly greater than 0.05  
selectedvariables<-filter(t,t$rate>0.1)
selectedvariables<-as.character(selectedvariables$names)

# Xgboost Model

# Split datasets, use city column to equally divide in various cities
smp_size<-floor(nrow(data3)*0.7)
trainind<-sample(seq_len(nrow(data3)),size=smp_size)
train<-data3[trainind,]
test<-data3[-trainind,]
trainl<-rate[trainind]
testl<-rate[-trainind]


evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err<-as.numeric(median(abs(exp(labels)-exp(preds))/exp(labels)))
  #err <- as.numeric(sum(labels != (preds > 0)))/length(labels)
  return(list(metric = "error", value = err))
}





#custommodel<-xgboost(data=as.matrix(data3),label=unlist(rate),nrounds=6000,early_stopping_rounds = 10,eval_metric=evalerror,maximize=F,min_child_weight=5)
custommodel1<-xgboost(data=as.matrix(train),label=unlist(trainl),nrounds=500,early_stopping_rounds = 10,eval_metric=evalerror,maximize=F,min_child_weight=5)

#pred<-predict(custommodel,as.matrix(test))
#pred1<-predict(custommodel1,as.matrix(test))

#summary(abs(exp(pred)-exp(testl))/exp(testl))
#summary(abs(exp(pred1)-exp(testl))/exp(testl))

xgb.save(custommodel,"model_final")