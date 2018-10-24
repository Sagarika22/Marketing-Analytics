datascience<-read.csv("/Users/sagarikadutta/Downloads/data_science_challenge_samp_18.csv")

View(datascience)
str(datascience)
summary(datascience)

#Changing from factor to numeric of  total spend
datascience$total_spend<-as.numeric(datascience$total_spend)/100
options(digits=9)
datascience$total_spend<-as.character(datascience$total_spend)
datascience$total_spend<-as.numeric(datascience$total_spend)


#removing column from data where date is more than 03-26-2016
datascience$order_date<-as.Date(datascience$order_date)
install.packages("dplyr")
library(dplyr)
datascience<-datascience[datascience$order_date>="2015-03-20" & datascience$order_date<="2016-03-26",]
View(datascience)

#ordeering the dataset
datascience<-datascience[order(datascience$cust_id,datascience$order_date),]

#generating the new feature as date difference that is difference between minimum date and given date
library(dplyr)
datascience<-datascience%>% mutate(order_date = as.Date(order_date, "%m/%d/%Y"))
mindate<-datascience %>% group_by(cust_id) %>% summarise(min_date=min(order_date))
View(mindate)
datascience<-merge(datascience,mindate,by="cust_id")
order_date<-as.POSIXct(datascience$order_date)
min_date<-as.POSIXct(datascience$min_date)
diffmindate<-difftime(datascience$min_date,datascience$order_date)
View(diffmindate)
diffmindate<-(-1)*diffmindate/(60*60*24)
datascience<-cbind(datascience,diffmindate)
datascience$diffmindate<-as.numeric(datascience$diffmindate)

#generating week number on the basis of problem (Sun-Sat)
install.packages("lubridate")
library(lubridate)
weeknumber<-epiweek(datascience$order_date)
datascience<-cbind(datascience,weeknumber)
library(dplyr)
datascience<-datascience %>% group_by(cust_id) %>%
  mutate(WeekDifference = c(0, diff(weeknumber)))


#finding the latest week difference 
install.packages("sqldf")
library(sqldf)
frequency<-sqldf("SELECT cust_id, count(*) as 'frequencyofvisit'
from datascience group by cust_id")
datascience<-merge(datascience,frequency,by="cust_id")
recency<-sqldf("SELECT cust_id,round((372-(diffmindate))/7,0) as 'recent week' from datascience")
datanew<-cbind.data.frame(datascience,recency)
View(datanew)


#checking missing values,infinites and NaNs
sum(sapply(datanew,is.na))
sum(sapply(datanew,is.nan))
sum(sapply(datanew,is.infinite))


#defining the target variable:based on data if they visit next week then 1 else 0
datanew$newcolumn<-ifelse(datanew$WeekDifference==1 | datanew$WeekDifference==-52,1,0)
colnames(datanew)[colnames(datanew)=="newcolumn"] <- "Targetv"

#finding correlations
cor(datanew$Targetv,datanew$frequencyofvisit)
cor(datanew$Targetv,datanew$diffmindate)

#boosting the model
set.seed(3421)
index=sample(2,nrow(datanew),replace=TRUE,prob=c(0.6,0.4))
train=datanew[index==1,]
test=datanew[ind==2,]
install.packages("gbm")
library("gbm")
gbmModel = gbm(formula =Targetv~frequencyofvisit+train$`recent week`+diffmindate,
               distribution = "bernoulli",
               data = train,
               n.trees = 50000,
               shrinkage = .01,
               n.minobsinnode =10)

gbmTrainPredictions = predict(object = gbmModel,
                              newdata = train,
                              n.trees = 1500,
                              type = "response")

summary(gbmModel, plot = FALSE)
summary(gbmTrainPredictions)
gbm.perf(gbmModel)
#probability model
library(nnet)
prob.model=multinom(formula= Targetv~units_purchased+total_spend+datanew$`recent week`+frequencyofvisit,data=datanew)

coeff=summary(prob.model)$coefficients
std=summary(prob.model)$standard.errors
print(coeff)
print(coeff/std)

#model with the only customers who visited next week and purchased less than 3
nm=which(datanew$Targetv==1 & datanew$units_purchased<=3)
summary(datanew[nm,])

#new model on the above criteria
datanew.model=lm(formula=Targetv~frequencyofvisit+units_purchased+`recent week`,data= datanew[nm,])
summary(datanew.model)

#boosting the model
set.seed(3421)
index=sample(2,nrow(datanew),replace=TRUE,prob=c(0.6,0.4))
train=datanew[index==1,]
test=datanew[ind==2,]
install.packages("gbm")
library("gbm")
gbmModel = gbm(formula =Targetv~frequencyofvisit+train$`recent week`+diffmindate,
               distribution = "bernoulli",
               data = train,
               n.trees = 50000,
               shrinkage = .01,
               n.minobsinnode =10)

gbmTrainPredictions = predict(object = gbmModel,
                              newdata = train,
                              n.trees = 1500,
                              type = "response")

summary(gbmModel, plot = FALSE)
summary(gbmTrainPredictions)
gbm.perf(gbmModel)


