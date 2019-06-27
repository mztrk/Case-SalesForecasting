#Uploading datasets

library(readr)
article_attributes <- read_delim("C:/Users/mustafa.ozturk/Desktop/Özel/Adidas/Germany/article_attributes.txt", 
                                 ";", escape_double = FALSE, trim_ws = TRUE)
View(article_attributes)

library(readr)
sales <- read_delim("C:/Users/mustafa.ozturk/Desktop/Özel/Adidas/Germany/sales.txt", 
                    ";", escape_double = FALSE, trim_ws = TRUE)
View(sales)



#Merging two tables
 library(data.table)
 dt1 <- data.table(article_attributes, key = "article") 
 dt2 <- data.table(sales, key = "article,")
 joined.dt1.dt.2 <- dt1[dt2]


##Distinct count article
aggregate(data.frame(count = sales$article), list(value = sales$article), length)

aggregate(data.frame(count = joined.dt1.dt.2$article), list(value = joined.dt1.dt.2$article), length)

a <- aggregate(data.frame(count = sales$article), list(value = sales$article), length)
b <- aggregate(data.frame(count = joined.dt1.dt.2$article), list(value = joined.dt1.dt.2$article), length)
c <- aggregate(data.frame(count = article_attributes$article), list(value = article_attributes$article), length)
merge(a,c,by=value, by.x = "value",by.y = "value")

##Distinct count week
aggregate(data.frame(count = sales$retailweek), list(value = sales$retailweek), length)

##Omiting NA's
OmitNAData <- na.omit(joined.dt1.dt.2)


##Converting Week

install.packages("lubridate")
OmitNAData$week <- lubridate::week(OmitNAData$retailweek)

##Converting Year

OmitNAData$year <- lubridate::year(OmitNAData$retailweek)

#Train Test Validate data splitting

TrainData <- OmitNAData[ which(OmitNAData$retailweek <='2016-08-21'), ]
TestData <- merge(x = (OmitNAData[ which(OmitNAData$retailweek <='2016-12-25'), ]), y = TrainData, by = "article", all.x = TRUE)
ValidateData <- merge(x = OmitNAData , y = TestData, by = "article", all.x = TRUE)


##Categorical to numeric

TrainData$productgroup <- as.factor(TrainData$productgroup)
TrainData$country <- as.factor(TrainData$country)
TrainData$category <- as.factor(TrainData$category)
TrainData$style <- as.factor(TrainData$style)
TrainData$gender <- as.factor(TrainData$gender)
TestData$productgroup <- as.factor(TestData$productgroup)
TestData$country <- as.factor(TestData$country)
TestData$category <- as.factor(TestData$category)
TestData$style <- as.factor(TestData$style)
TestData$gender <- as.factor(TestData$gender)
ValidateData$productgroup <- as.factor(ValidateData$productgroup)
ValidateData$country <- as.factor(ValidateData$country)
ValidateData$category <- as.factor(ValidateData$category)
ValidateData$style <- as.factor(ValidateData$style)
ValidateData$gender <- as.factor(ValidateData$gender)
Forecast$productgroup <- as.factor(Forecast$productgroup)
Forecast$country <- as.factor(Forecast$country)
Forecast$category <- as.factor(Forecast$category)
Forecast$style <- as.factor(Forecast$style)
Forecast$gender <- as.factor(Forecast$gender)

# ---------------------Time Series-------------------------



TrainDataGer <- subset(TrainData, country =="Germany")
TrainDataGerShoes <- subset(TrainDataGer, productgroup == "SHOES")
myts <- ts(TrainDataGerShoes$sales, start=c(2014, 52), end=c(2016, 32), frequency=52) 
myts_1 <- ts(TrainData$sales, start=c(2014, 52), end=c(2016, 32), frequency=52)



# plot series
plot(myts)

# Seasonal decomposition
newts_1 <- log(myts_1)
fit <- stl(newts_1, s.window="seasonal")
plot(fit)


#Result *** No Seasonality

# Selecting variables


install.packages("leaps")

library(leaps)

BestSubGroup <- regsubsets(sales ~ratio+promo1+promo2+productgroup+category+style+gender+rgb_r_main_col+rgb_g_main_col+rgb_b_main_col+rgb_r_sec_col+rgb_g_sec_col+rgb_b_sec_col+week+year, data = TrainData, nvmax = 20)
summary(BestSubGroup)
plot(summary(BestSubGroup)$cp, xlab = "# of variables", ylab = "Cp")
points(which.min(summary(BestSubGroup)$cp), which.min(summary(BestSubGroup)$cp), pch = 20, col = "red")
coef(BestSubGroup, which.min(summary(BestSubGroup)$cp)) #Important Variables 
plot(BestSubGroup,scale="Cp")


summary(lm(sales ~ ratio+ promo1+ promo2+ productgroup + category +gender + rgb_r_main_col +rgb_g_main_col +year , data = TrainData))

summary(lm(sales ~ ratio+ promo1+ promo2+ productgroup + category +gender  +year , data = TrainData))

#--------------------Decision Tree--------------------------------------
# Classification Tree with rpart
install.packages('rpart')

library(rpart)

# grow tree 
DecTree <- rpart(salescat ~ratio+ promo1+ promo2+ productgroup + category+gender + rgb_r_main_col +rgb_g_main_col +year,method="class", data=TrainData)

printcp(DecTree) # display the results 
plotcp(DecTree) # visualize cross-validation results 
summary(DecTree) # detailed summary of splits

# plot tree 
plot(DecTree, uniform=TRUE, 
     main="Classification Tree for TrainDataa")
text(DecTree, use.n=TRUE, all=TRUE, cex=.8)



#----------------------Random Forrest Modelling--------------------
set.seed(1)

library(randomForest)
RF_Train=randomForest(sales ~ ratio+ promo1+ promo2+ productgroup + category +gender + rgb_r_main_col +rgb_g_main_col +year ,data=TrainData,importance=TRUE,na.action=na.omit) #random forrest modelling took like 3 hrs
summary(RF_Train)
plot(RF_Train)
print(RF_Train) 
print(importance(RF_Train,type = 2))

predict(RF_Train,TestData,type="response", norm.votes=TRUE, predict.all=TRUE, proximity=FALSE, nodes=FALSE)

predict(RF_Train,TestData,type="response", norm.votes=FALSE, predict.all=TRUE, proximity=FALSE, nodes=FALSE)

#Random Forrest Modelling v2

set.seed(1)

library(randomForest)
RF_Train_V2=randomForest(sales ~ ratio+ promo1+ promo2  +year + week ,data=TrainData,importance=TRUE,na.action=na.omit) #random forrest modeli 15 dakika surdu 
summary(RF_Train_V2)
plot(RF_Train_V2)
print(RF_Train_V2) 
print(importance(RF_Train_V2,type = 2))


#Random Forrest Modelling v3

set.seed(1)

library(randomForest)
RF_Train_V3=randomForest(sales ~ ratio+ promo1+ promo2  +year + week ,data=TrainData,importance=TRUE,na.action=na.omit) #random forrest modeli 15 dakika surdu 
summary(RF_Train_V3)
plot(RF_Train_V3)
print(RF_Train_V3) 
print(importance(RF_Train_V3,type = 2))



# Sales to categorical

quantile(TrainData$sales,.95)

summary(sales)

TrainData$salescat <- cut(TrainData$sales,breaks = c(Min(TrainData$sales), quantile(TrainData$sales,.25),quantile(TrainData$sales,.50),quantile(TrainData$sales,.75),quantile(TrainData$sales,.95),Inf ),  labels = c("1-10", "11-26","27-65","66-215","216 +"))


#Categorical Random Forest

set.seed(1)

library(randomForest)
RF_Train_V4=randomForest(salescat ~ ratio+ promo1+ promo2  +year + week ,data=TrainData,importance=TRUE,na.action=na.omit) #random forrest modeli 15 dakika surdu 
summary(RF_Train_V4)
plot(RF_Train_V4)
print(RF_Train_V4) 
print(importance(RF_Train_V4,type = 2))


# Categorical Random Forest v2
set.seed(1)

library(randomForest)
RF_Train_V5=randomForest(salescat ~ratio+ promo1+ promo2+ productgroup + category +gender + rgb_r_main_col +rgb_g_main_col +year,data=TrainData,importance=TRUE,na.action=na.omit) #random forrest modeli 15 dakika surdu 
summary(RF_Train_V5)
plot(RF_Train_V5)
print(RF_Train_V5) 
print(importance(RF_Train_V5,type = 2)
      

#-------------Final Model --------------


Traindata$ForecastedSales <- predict(RF_Train,TrainData,type="response", norm.votes=TRUE, predict.all=TRUE, proximity=FALSE, nodes=FALSE)
TestData$ForecastedSales <- predict(RF_Train,TestData,type="response", norm.votes=FALSE, predict.all=TRUE, proximity=FALSE, nodes=FALSE)
ValidateData$ForecastedSales <- predict(RF_Train,ValidateData,type="response", norm.votes=FALSE, predict.all=TRUE, proximity=FALSE, nodes=FALSE)


##-------------Results----------
#Forecasting new sales 
#I did use forecast.csv file which include randomise ratio between 0-1.

Forecast$sales <- predict(RF_Train,Forecast,type="response", norm.votes=FALSE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)

aggregate(Forecast$sales, by=list(Forecast$country), FUN=sum)

#Identifiying What is driving sales,

coef(BestSubGroup, which.min(summary(BestSubGroup)$cp)) #Important Variables


# Determine which type of promotion has stronger impact on sales

AllData <- na.omit(joined.dt1.dt.2)

BestSubGroup_1 <- regsubsets(sales ~promo1+promo2+ratio, data = AllData, nvmax = 20) 
summary(BestSubGroup_1)
plot(summary(BestSubGroup_1)$cp, xlab = "# of variables", ylab = "Cp")
points(which.min(summary(BestSubGroup_1)$cp), which.min(summary(BestSubGroup_1)$cp), pch = 20, col = "red")
coef(BestSubGroup_1, which.min(summary(BestSubGroup_1)$cp)) #Important Variables 




