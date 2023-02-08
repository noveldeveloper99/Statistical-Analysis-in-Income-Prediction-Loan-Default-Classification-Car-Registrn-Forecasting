findata <- read.csv("C:/Users/abhin/Desktop/NCI 2022/Stats/ca/IncomeData.csv")
# head(findata)
# tail(findata,2)
# summary(findata)
colnames(findata)[1]<-"age"
sum(is.na(findata))
# correlation plots
# pairs(findata)


# casting cat. variables
findata$edcat<-as.factor(findata$edcat)
findata$default<-as.factor(findata$default)
findata$jobsat<- as.factor(findata$jobsat)
findata$homeown<- as.factor(findata$homeown)
summary(findata)
# cor(findata, method = "pearson") # correlation for Cat. vars. doesnt work
#dummy for jobsat as has 5 levels
library(fastDummies)
findata <- dummy_cols(findata,select_columns = c("edcat","jobsat"))
findata<-findata[,c(-3,-9)] # removed jobsat and edcat
summary(findata)
# subsetting removing cat. vars for cor()
# fin_num <- findata[, -c(3,8:10)]
# head(fin_num)
# cor(fin_num, method = "pearson")
# pairs(fin_num, panel = panel.smooth)

str(findata) #structure function
attach(findata)

names(findata)
# attach(findata)
# findata[, "edcat_5"]==1
# model building - base model statistics
mymodel<- lm(log(income)~.-edcat_5-jobsat_5, findata)
summary(mymodel)
par(mfrow=c(2,2))
plot(mymodel)
vif(mymodel)# shows high multi-corr between edcat and yrsed and a little age/ address
durbinWatsonTest(mymodel) # autocorrelation
ncvTest(mymodel) # test of homoscedasticity
max(cooks.distance(mymodel)) # influential outliers
min(cooks.distance(mymodel))
influencePlot(mymodel)
# mymodel_1<- update(mymodel,~.-address-edcat+I(age^2)+I(yrsempl^2+(address)^2)-
#                      yrsempl-I(cars+carvalue))

# mymodel_1<- update(mymodel, ~.-edcat-age-yrsed-yrsempl-address-carvalue+I(yrsempl^2+address^0.5)+I(age^2)+I(yrsed^2)+log(carvalue)+I(jobsat:default))

mymodel_1<- update(mymodel, ~.-edcat_1-edcat_2-edcat_3-edcat_4) # removed edcat

summary(mymodel_1)
par(mfrow=c(2,2))
plot(mymodel_1)
vif(mymodel_1)
durbinWatsonTest(mymodel_1) # shows high multi-corr between edcat and yrsed and a little age/ address
ncvTest(mymodel_1)
max(cooks.distance(mymodel_1))
min(cooks.distance(mymodel_1))
influencePlot(mymodel_1)

# fixes non-linearity between predictors and response
# mymodel_2<- update(mymodel_1, ~.-age+I(age^2)- yrsed+I(yrsed^2)-yrsempl+I(yrsempl^2)-address-carvalue+I(address^0.5)+log(carvalue))
mymodel_2<-update(mymodel_1,~.-age+I(age^2)- yrsed +I(yrsed^2)-yrsempl+I(yrsempl)- address+I(address^0.5) -carvalue+I(log(carvalue)) )
# mymodel_2<-update(mymodel_1,~.- yrsed +I(yrsed^2)-yrsempl+I(yrsempl)- address+I(address^0.5) -carvalue+I(log(carvalue)) )
summary(mymodel_2)
par(mfrow=c(2,2))
plot(mymodel_2)
vif(mymodel_2)
durbinWatsonTest(mymodel_2) # shows multi-corr between creddebt and othdebt; and age and address
ncvTest(mymodel_2)
max(cooks.distance(mymodel_2))
min(cooks.distance(mymodel_2))
influencePlot(mymodel_2)

mymodel_3<-update(mymodel_2,~.+I(age^2+address))
# mymodel_2<-update(mymodel_1,~.- yrsed +I(yrsed^2)-yrsempl+I(yrsempl)- address+I(address^0.5) -carvalue+I(log(carvalue)) )
summary(mymodel_3)
par(mfrow=c(2,2))
plot(mymodel_3)
vif(mymodel_3)
durbinWatsonTest(mymodel_3) # shows multi-corr between creddebt and othdebt; and age and address
ncvTest(mymodel_3)
max(cooks.distance(mymodel_3))
min(cooks.distance(mymodel_3))
influencePlot(mymodel_3)

mymodel_4<-update(mymodel_3,~.-cars-homeown-jobsat_1-jobsat_2-jobsat_3-jobsat_4)
# mymodel_2<-update(mymodel_1,~.- yrsed +I(yrsed^2)-yrsempl+I(yrsempl)- address+I(address^0.5) -carvalue+I(log(carvalue)) )
summary(mymodel_4)
par(mfrow=c(2,2))
plot(mymodel_4)
vif(mymodel_4)
durbinWatsonTest(mymodel_4) 
ncvTest(mymodel_4)

# no influential outliers found with 1 or greater cooks distance
max(cooks.distance(mymodel_4))
min(cooks.distance(mymodel_4))
# cooks.distance( mymodel_4)
influencePlot(mymodel_4, scale = 3, main =" Influence Plot")

# checking final model for improvement in ncvTest>0.05
mymodel_5<-update(mymodel_4,~.-I(age^2)-I(address^0.5))
# mymodel_2<-update(mymodel_1,~.- yrsed +I(yrsed^2)-yrsempl+I(yrsempl)- address+I(address^0.5) -carvalue+I(log(carvalue)) )
summary(mymodel_5)
par(mfrow=c(2,2))
plot(mymodel_5)
vif(mymodel_5)
durbinWatsonTest(mymodel_5) 
ncvTest(mymodel_5)
max(cooks.distance(mymodel_5))
min(cooks.distance(mymodel_5))
influencePlot(mymodel_5)

#---------------------------------------------------------------------

nu<- findata[-c(3647, 4261),]
summary(nu)

mymodel<- lm(log(income)~.-edcat_5-jobsat_5, data= nu)
summary(mymodel)
par(mfrow=c(2,2))
plot(mymodel)
vif(mymodel)
durbinWatsonTest(mymodel) # shows high multi-corr between edcat and yrsed and a little age/ address
ncvTest(mymodel)
max(cooks.distance(mymodel))
min(cooks.distance(mymodel))
influencePlot(mymodel)

# mymodel_1<- update(mymodel,~.-address-edcat+I(age^2)+I(yrsempl^2+(address)^2)-
#                      yrsempl-I(cars+carvalue))

# mymodel_1<- update(mymodel, ~.-edcat-age-yrsed-yrsempl-address-carvalue+I(yrsempl^2+address^0.5)+I(age^2)+I(yrsed^2)+log(carvalue)+I(jobsat:default))

mymodel_1<- update(mymodel, ~.-edcat_1-edcat_2-edcat_3-edcat_4) # removed edcat

summary(mymodel_1)
par(mfrow=c(2,2))
plot(mymodel_1)
vif(mymodel_1)
durbinWatsonTest(mymodel_1) # shows high multi-corr between edcat and yrsed and a little age/ address
ncvTest(mymodel_1)

# fixes non-linearity between predictors and response
# mymodel_2<- update(mymodel_1, ~.-age+I(age^2)- yrsed+I(yrsed^2)-yrsempl+I(yrsempl^2)-address-carvalue+I(address^0.5)+log(carvalue))
mymodel_2<-update(mymodel_1,~.-age+I(age^2)- yrsed +I(yrsed^2)-yrsempl+I(yrsempl)- address+I(address^0.5) -carvalue+I(log(carvalue)) )
# mymodel_2<-update(mymodel_1,~.- yrsed +I(yrsed^2)-yrsempl+I(yrsempl)- address+I(address^0.5) -carvalue+I(log(carvalue)) )
summary(mymodel_2)
par(mfrow=c(2,2))
plot(mymodel_2)
vif(mymodel_2)
durbinWatsonTest(mymodel_2) # shows multi-corr between creddebt and othdebt; and age and address
ncvTest(mymodel_2)

mymodel_3<-update(mymodel_2,~.+I(age^2+address))
# mymodel_2<-update(mymodel_1,~.- yrsed +I(yrsed^2)-yrsempl+I(yrsempl)- address+I(address^0.5) -carvalue+I(log(carvalue)) )
summary(mymodel_3)
par(mfrow=c(2,2))
plot(mymodel_3)
vif(mymodel_3)
durbinWatsonTest(mymodel_3) # shows multi-corr between creddebt and othdebt; and age and address
ncvTest(mymodel_3)
max(cooks.distance(mymodel_3))
min(cooks.distance(mymodel_3))
influencePlot(mymodel_3)

mymodel_4<-update(mymodel_3,~.-jobsat_1-jobsat_2-jobsat_3-jobsat_4-cars-homeown)
# mymodel_2<-update(mymodel_1,~.- yrsed +I(yrsed^2)-yrsempl+I(yrsempl)- address+I(address^0.5) -carvalue+I(log(carvalue)) )
summary(mymodel_4)
par(mfrow=c(2,2))
plot(mymodel_4)
vif(mymodel_4)
durbinWatsonTest(mymodel_4) 
ncvTest(mymodel_4)

# no influential outliers found with 1 or greater cooks distance
max(cooks.distance(mymodel_4))
min(cooks.distance(mymodel_4))
# cooks.distance( mymodel_4)
influencePlot(mymodel_4, scale = 3, main =" Influence Plot")



#---------------------------------





