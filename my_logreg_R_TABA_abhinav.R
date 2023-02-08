# Import data for Logistic Regression
mydata<- read.csv("C:/Users/abhin/Desktop/NCI 2022/Stats/TABA/Default.csv")

# check for missing values
sum(is.na(mydata))

# Descriptive Stats
str(mydata)
names(mydata)[names(mydata)=="ï..gender"] <-"gender"

# Typecasting categorical features
# mydata$gender<-as.factor(mydata$gender)
# mydata$retire<-as.factor(mydata$retire)
# mydata$default<-as.factor(mydata$default)
# mydata$marital<-as.factor((mydata$marital))
# mydata$homeown<-as.factor(mydata$homeown)

# final summary and structure
# summary(mydata)
# str(mydata)
dim(mydata)
# Intial plots got from SPSS except corr plots i.e. pairs()
# hist(mydata$age)
# boxplot(mydata$age)
# correlation plots
# 
# num_data<-mydata[,c(2,3,5:8)]
# head(num_data)

# We can see here the IV's are highly correlated with each other, so 
# need to remove them by applying transformations or using PCA. 
# pairs(num_data, panel = panel.smooth) 

# fitting the logreg model
# attach(mydata)
logreg1<- glm(default~.,data = mydata, family = binomial)
summary(logreg1)
# check residuals and outliers
sum(rstandard(logreg1)>2.5) # 11 i.e. 0.4 %
sum(rstandard(logreg1)>2)# 43 i.e. 1.5 % 
# so no residuals problem

sum(cooks.distance(logreg1)>1)

logreg2<- glm(default~marital+gender+homeown+age+ed+income+creddebt+othdebt,data = mydata, family = binomial)
summary(logreg2)

# partial likelihood ratio test - compare models
library(lmtest)
lrtest(logreg1,logreg2)
# fitness is similar for both so we can select parsimonious model

#Check Linearity of the Logit
LoLTestModel<-glm(default~age+age:log(age)+ed+ed:log(ed)+income+income:log(income)
                  +creddebt+creddebt:log(creddebt)+othdebt+othdebt:log(othdebt),
                  data = mydata, family=binomial)
summary(LoLTestModel)
# this shows to transform income, creddebt and othdebt into log
logreg1_1 <- update(logreg1, ~.+log(income)+log(creddebt)+log(othdebt))
summary(logreg1_1)
# check residuals and outliers
sum(rstandard(logreg1_1)>2.5) # 7 i.e. 0.2 % < 1%
sum(rstandard(logreg1_1)>2)# 46 i.e. 1.6 % < 5 %
# so no residuals problem

sum(cooks.distance(logreg1_1)>1)
# for Checking Multicollinearity we can run a Linear Reg model and assess VIF with same predictors

# raw_data<- read.csv("C:/Users/abhin/Desktop/NCI 2022/Stats/TABA/Default.csv")
# summary(raw_data)
library(car) # for vif
linfit<- lm(mydata$default~.+log(income)+log(creddebt)+log(othdebt)
            ,data = mydata)
vif(linfit)
linfit2<- lm(mydata$default~.-gender-retire-marital-homeown,data = mydata)
vif(linfit2)

# model building
logreg1_2 <-update(logreg1_1,~.-gender)
summary(logreg1_2)

logreg1_3 <-update(logreg1_2,~.-marital)
summary(logreg1_3)

logreg1_4 <-update(logreg1_3,~.-income)
summary(logreg1_4)

logreg1_5 <-update(logreg1_4,~.-retire)
summary(logreg1_5)

logreg1_6 <-update(logreg1_5,~.-othdebt)
summary(logreg1_6)

# checking for effects of possible interactions
lr1 <- update(logreg1,~.+retire:income)
summary(lr1)

lr2 <- update(logreg1,~.+marital:othdebt)
summary(lr2)

lr3 <- update(logreg1,~.+othdebt:income)
summary(lr3)

lr4 <- update(logreg1,~.+homeown:retire)
summary(lr4)

# lr5 <- update(logreg1,~.+homeown:marital)
# summary(lr5)
# This is the final proposed total features for model
final_model <- update(logreg1,~.+retire:income+marital:othdebt+othdebt:income+homeown:retire)
summary(final_model)

# check residuals and outliers
sum(rstandard(final_model)>2.5) # 7 i.e. 0.2 % < 1%
sum(rstandard(final_model)>2)# 46 i.e. 1.6 % < 5 %
# so no residuals problem
sum(cooks.distance(final_model)>1)
# for Checking Multicollinearity we can run a Linear Reg model and assess VIF with same predictors
# model building
final_model1<- update(final_model,~.-gender)
summary(final_model1)

final_model2<- update(final_model1,~.-marital)
summary(final_model2)

final_model3<- update(final_model2,~.-income:othdebt)
summary(final_model3)

# check residuals and outliers
sum(rstandard(final_model3)>2.5) # 11 i.e. 0.4 % < 1%
sum(rstandard(final_model3)>2)# 43 i.e. 1.5 % < 5 %
# so no residuals problem
sum(cooks.distance(final_model3)>1)

testmodel4<-update(final_model3,~.-retire:homeown)
summary(testmodel4)

testmodel5<-update(testmodel4,~.-retire)
summary(testmodel5)
testmodel6<-update(testmodel5,~.-income:retire)
summary(testmodel6)
# compare model
lrtest(logreg1_1,logreg1_2)
lrtest(logreg1_2,logreg1_3)
lrtest(logreg1_3,logreg1_4)
lrtest(logreg1_4,logreg1_5)
lrtest(logreg1_5,logreg1_6)

lrtest(final_model2,final_model3)


# odds ratio
coef(logreg1_6)
exp(coef(logreg1_6))


#Examine predicted probabilities and classifications
pred_probs<-predict(logreg1_6, type="response")
pred_probs
pred_class<-ifelse(pred_probs<0.5, "0","1")
pred_class

#Use Table function to create confusion matrix
table(mydata$default,pred_class)
names(logreg1_6)
#confusionMatrix function in caret package
library(caret)
library(e1071)
default<-as.factor(mydata$default)
pred_classf<-as.factor(pred_class)
confusionMatrix(default,pred_classf,positive = "1")

#PseudoRSquared Statistics
library(DescTools)
PseudoR2(logreg1_6, which = "all")


# hoslem test
library(ResourceSelection)
hl <- hoslem.test(mydata$default,fitted(logreg1_6),g =10 )
hl

# plots for classification
plot(pred_probs, jitter(mydata$default,0.5),cex = 0.5, ylab = "Jittered default outcome")
library(Deducer)
rocplot(logreg1_6)
#------------------------------------------------------------
# check confusionmatrix for final_model3
#Examine predicted probabilities and classifications
pred_probs<-predict(final_model3, type="response")
pred_probs
pred_class<-ifelse(pred_probs<0.5, "0","1")
pred_class

#Use Table function to create confusion matrix
table(mydata$default,pred_class)
names(final_model3)
#confusionMatrix function in caret package
library(caret)
library(e1071)
default<-as.factor(mydata$default)
pred_classf<-as.factor(pred_class)
confusionMatrix(default,pred_classf,positive = "1")

# plots for classification
plot(pred_probs, jitter(mydata$default,0.5),cex = 0.5, ylab = "Jittered default outcome")
library(Deducer)
rocplot(final_model3)

#PseudoRSquared Statistics
library(DescTools)
PseudoR2(final_model3, which = "all")


# hoslem test
library(ResourceSelection)
hl <- hoslem.test(mydata$default,fitted(final_model3),g =10 )
hl

coef(final_model3)
exp(coef(final_model3))


