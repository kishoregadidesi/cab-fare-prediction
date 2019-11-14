# cab-fare-prediction
we have to automate the cab fare amount based on time and distance


rm(list=ls())
setwd('C:/sample files ds')
getwd()
df= read.csv("train_cab.csv")
head(df,10)
class(df$fare_amount)
df$fare_amount=  as.numeric(levels(df$fare_amount))[df$fare_amount]

# To check how the data is distributed

hist(df$fare_amount)


#.....................missingvalueanalysis..................................#
missingval= data.frame(apply(df,2,function(x){sum(is.na(x))}))
missingval$colname= row.names(missingval)
row.names(missingval)= NULL
names(missingval)[1]= 'values'
missingval = missingval[,c(2,1)]
missingval$percent = (missingval$values/nrow(df))*100
missingval= missingval[order(-missingval$values),]

df$fare_amount[is.na(df$fare_amount)]= median(df$fare_amount,na.rm = T)
df$passenger_count[is.na(df$passenger_count)]= median(df$passenger_count,na.rm = T)
#after imputing the misiing values with mean,median, knn i found median is better to replace missing values


#.......................... outlier analysis...................................#

#visualisation

library(ggplot2)
ggplot(df, aes(x=df$fare_amount, y= df$fare_amount))+
  geom_boxplot(fill= "cornsilk", outlier.colour="red", outlier.size = 3)+
  guides(fill= FALSE)+theme_bw()+theme(text=element_text(size=10))

ggplot(df, aes(x= fare_amount, y= passenger_count))+
  geom_boxplot(fill= "cornsilk", outlier.colour="red", outlier.size = 3)+
  guides(fill= FALSE)+theme_bw()+theme(text=element_text(size=10))

ggplot(df, aes(x= fare_amount, y= pickup_longitude))+
  geom_boxplot(fill= "cornsilk", outlier.colour="red", outlier.size = 3)+
  guides(fill= FALSE)+theme_bw()+theme(text=element_text(size=10))

ggplot(df, aes(x= fare_amount, y= pickup_latitude))+
  geom_boxplot(fill= "cornsilk", outlier.colour="red", outlier.size = 3)+
  guides(fill= FALSE)+theme_bw()+theme(text=element_text(size=10))

ggplot(df, aes(x= fare_amount, y= dropoff_longitude))+
  geom_boxplot(fill= "cornsilk", outlier.colour="red", outlier.size = 3)+
  guides(fill= FALSE)+theme_bw()+theme(text=element_text(size=10))

ggplot(df, aes(x= fare_amount, y= dropoff_latitude))+
  geom_boxplot(fill= "cornsilk", outlier.colour="red", outlier.size = 3)+
  guides(fill= FALSE)+theme_bw()+theme(text=element_text(size=10))


#removing outliers from the data according to my understanding

df= df[which(!df$fare_amount>=200),]
df= df[which(!df$fare_amount<2),]
df= df[which(!df$passenger_count>6),]
df= df[which(!df$pickup_longitude>-1),]
df= df[which(!df$pickup_latitude>45),]
df= df[which(!df$dropoff_longitude>-60),]
df= df[which(!df$dropoff_latitude<1),]
df= df[which(!df$pickup_datetime==43),]


# extracting data from date and lat,lon variables

df$month=format(as.Date(df$pickup_datetime),"%m")
df$year= format(as.Date(df$pickup_datetime),"%y")
df$day= format(as.Date(df$pickup_datetime),'%w')
df$hour= format(as.POSIXct(strptime(df$pickup_datetime,"%Y-%m-%d %H:%M:%S UTC", tz="")),format= "%H")

earthDist= function(lon1,lat1,lon2,lat2){
  rad= pi/180
  a1= lat1*rad
  a2= lon1*rad
  b1= lat2*rad
  b2= lon2*rad
  dlon= b2-a2
  dlat= b1-a1
  a= (sin(dlat/2))^2 +cos(a1)*cos(b1)* (sin(dlon/2))^2
  c= 2*atan2(sqrt(a),sqrt(1-a))
  r= 6378.1
  d= r*c
  return(d)
}
df$distance=earthDist(df$pickup_longitude, df$pickup_latitude,df$dropoff_longitude, df$dropoff_latitude)

# To check the relation between two variables

ggplot(df, aes_string(x= df$distance, y= df$fare_amount))+
  geom_bar(stat= 'identity',fill= "DarkslateBlue", colour= "black", position= 'dodge' ,size= 10)+ theme_bw()+
  scale_x_continuous(breaks= scales::pretty_breaks(15))+
  scale_y_continuous(breaks= scales::preety_breaks(10))+
  theme(text= element_text(size=15))



ggplot(df, aes_string(x= df$day, y= df$fare_amount))+
  geom_point(colour= df$hour,size=4)+
  scale_y_continuous(breaks= scales::pretty_breaks(15))+
  xlab("distance")+ylab("fare amount")+
  scale_colour_discrete(name= 'hour')+
  theme_bw()



#.............................Feature selection..............................#

df$day= as.factor(df$day)
df$passenger_count= as.factor(df$passenger_count)
df$month = as.factor(df$month)
df$year = as.factor(df$year)
df$hour = as.factor(df$hour)

# correlation test for contiuous variables
numeric_index= sapply(df, is.numeric)
numeric_data= df[,numeric_index]

library(corrgram)
corrgram(df[,numeric_index],order= F, upper.panel = panel.pie, text.panel = panel.txt, main= 'correlation plot')

#anova test for categorical variables
factor_index= sapply(df, is.factor)
factor_data= df[,factor_index]
factor_data= cbind(factor_data, df$fare_amount)
names(factor_data)[7]= 'fare_amount'

anova(lm(fare_amount~ passenger_count+month+year+day+hour, factor_data))


# After extracting the data from 'datetime, latitude, longitue' variables, we can remove them from the data.
df = subset(df, select=-c(pickup_datetime,pickup_latitude,pickup_longitude,dropoff_longitude,dropoff_latitude))


#......................................machine learning.........................#



train_index= sample(1:nrow(df), 0.8*nrow(df))
train = df[train_index,]
validate= df[-train_index,]

#.................................linear regression....................................#
library(usdm)
library(sp)
library(raster)
vif(df[,-1])
vifcor(numeric_data[,-1],th= 0.9)


lm_model= lm(fare_amount~., train)
summary(lm_model)
pred_lr= predict(lm_model, validate[,-1])

mape= function(y, ypred){
  mean(abs((y-ypred)/y)*100)
}

mape(validate[,1], pred_lr)

rmse= function(y, ypred){
  sqrt(mean((y-ypred)^2))
}

rmse(pred_lr,validate[,1])

# mape     = 34.11153
# accuracy = 65.88847
# rmse     = 7.524407

#...............................decision tree regressor..........................#


library(rpart)
library(MASS)

fit= rpart(fare_amount~., data=train, method= 'anova')
predict_dt= predict(fit, validate[,-1])



mape(validate[,1],predict_dt)

rmse(validate[,1],predict_dt)

# mape     = 26.13592
# accuracy = 73.86408
# rmse     = 5.901162
  
#............................. random forest regressor..............................#

library(randomForest)

rf= randomForest(fare_amount~., train, importance= TRUE, ntree= 200)  
predict_rf= predict(rf,validate[,-1])  

mape(validate[,1], predict_rf)  
rmse(predict_rf,validate[,1])

# mape     = 24.00311
# accuracy = 75.9969
# rmse     = 6.02356


rf= randomForest(fare_amount~., train, importance= TRUE, ntree= 300)  
predict_rf= predict(rf,validate[,-1])  

mape(validate[,1], predict_rf)  
rmse(predict_rf,validate[,1])

# mape     = 20.01568
# accuracy = 79.98432
# rmse     = 5.620021



#..........................................knn..........................................#

library(class)
knn_pr = knn(train[,1:7], validate[,1:7], train$fare_amount, k= 23)
sum(diag(knn_pr))/nrow(validate)

# mape     = 57.8414
# accuracy = 42.1586



# test data 

test = read.csv("test.csv")
test$month= format(as.Date(test$pickup_datetime),"%m")
test$year= format(as.Date(test$pickup_datetime), "%y")
test$day = format(as.Date(test$pickup_datetime), "%w")
test$hour= format(as.POSIXct(strptime(test$pickup_datetime,"%Y-%m-%d %H:%M:%S UTC", tz="")),format= "%H")

earthDist= function(lon1,lat1,lon2,lat2){
  rad= pi/180
  a1= lat1*rad
  a2= lon1*rad
  b1= lat2*rad
  b2= lon2*rad
  dlon= b2-a2
  dlat= b1-a1
  a= (sin(dlat/2))^2 +cos(a1)*cos(b1)* (sin(dlon/2))^2
  c= 2*atan2(sqrt(a),sqrt(1-a))
  r= 6378.1
  d= r*c
  return(d)
}
test$distance= earthDist(test$pickup_longitude, test$pickup_latitude,test$dropoff_longitude, test$dropoff_latitude)


test$day= as.factor(test$day)
test$passenger_count= as.factor(test$passenger_count)
test$month = as.factor(test$month)
test$year = as.factor(test$year)
test$hour = as.factor(test$hour)


test= subset(test, select= -c(pickup_datetime,pickup_latitude,pickup_longitude,dropoff_longitude,dropoff_latitude))


wer= df
wer$fare_amount = NULL
test= rbind(wer[1,],test)
test= test[-1,]


new_rf= randomForest(fare_amount~.,df,importance = TRUE,ntree= 300)
pred_rff= predict(new_rf ,test[,1:6])


write.csv(pred_rff, "fare_amount.csv", row.names= T)
