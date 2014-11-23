BIKE-SHARING-REGRESSION-ANALYSIS-AND-ANNOVA-
============================================
### BIKE DATA TESTING Regression Analysis and ANNOVA ### 

setwd("C:\\Users\\Nitesh Kumar\\Documents\\Kaggle\\Bike Sharing")

##IMPORTING DATA ##

sample<-read.csv("F:/BIKE DATA/sampleSubmission.csv")
train<-read.csv("F:/BIKE DATA/train.csv")
test<-read.csv("F:/BIKE DATA/test.csv")


cor(train[,6:10])
names(train)

##scatter plot matrix/graph#####

pairs(train[,2:12])

summary(train[,2:12])

### Regression model - BASE MODEL ###
model1<-lm(count~season+holiday+workingday+weather+temp+atemp+humidity+windspeed,data=train)

summary(model1)

summary.aov(model1)
## removing the atemp####

model2<-update(model1,~.-atemp)
summary(model2)
summary.aov(model2)
### comparing model

anova(model1,model2)

##removing holiday####

model3<-update(model2,~.-holiday)
summary(model3)
summary.aov(model3)

##removing workingday###
model4<-update(model3,~.-workingday)
summary(model4)
summary.aov(model4)

### VIF CALCULATION ####
library(car)
vif(model1)

### DROPING TEMP ND ATEMP AS IT HAS HIGH VIF VALUE (GREATER THAN 5 )####

model5<-update(model4,~.-temp)
summary(model5)
vif(model5)

model6<-update(model5,~.-atemp)
summary(model6)
vif(model6)

model7<-update(model6,~.-weather)
summary(model7)

###USE OF AIC TO DETERMINE THE BEST  MODEL###

step(model1,direction="backward")


### random model test ####

modelx<-lm(count~temp+humidity+windspeed,data=train)
summary(modelx)
modelx1<-update(modelx,~.-windspeed)
summary(modelx1)
