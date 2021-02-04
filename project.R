insurance <- read.csv("insurance.csv")
#convert categorical columns to factor
insurance$sex <- factor(insurance$sex)
insurance$smoker <- factor(insurance$smoker)
insurance$region <- factor(insurance$region)
insurance$sex <- as.numeric(factor(insurance$sex, levels=c("male","female"), labels=c(0,1)))
insurance$smoker <- as.numeric(factor(insurance$smoker, levels=c("yes","no"), labels=c(0,1)))
insurance$region <- as.numeric(factor(insurance$region, levels=c("northwest","northeast","southeast","southwest"), labels=c(1,2,3,4)))
plot(insurance)
par(mfrow=c(2,3))
plot(age,charges,main = "Age vs Charges",xlab = "Age",ylab="Charges",col = "blue")
plot(sex,charges,main = "Sex vs Charges",xlab = "Sex: Male = 0, Female = 1",ylab = "Charges", col = "red")
plot(bmi,charges,main = "Bmi vs Charges",xlab = "Bmi",ylab="Charges", col = "magenta")
plot(children,charges, main = "Children vs Charges",xlab = "Children",ylab="Charges",col = "orange")
plot(smoker,charges, main = "Smoker vs Charges",xlab = "Smoker: Yes = 0, No = 1", ylab = "Charges",col = "steelblue")
plot(region,charges, main = "Region vs Charges",xlab = "Region: Northwest=1 Northeast=2,Southeast=3,Southwest=4",ylab ="Charges", col = "hotpink4")
par(mfrow=c(1,1))
cor(insurance)

#Train Test Split 
set.seed(101)
n = nrow(insurance)
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
train = insurance[trainIndex ,]
test = insurance[-trainIndex ,]
#train the model
insurance.lm <- lm(charges~.,data=train)
summary(insurance.lm)
#test
true_test <- test$charges
pred <- predict(insurance.lm, test)
#plot of predicted vs true values
plot(pred,true_test, xlab="Predicted",ylab="Actual",pch=19,col="blue")
abline(a=0,b=1)
#distribution plot of residuals
hist(resid(insurance.lm),main="Distribution of Residuals",xlab="Residuals",horizontal = T,col="darkseagreen")
#plot of residuals vs predicted values
yres <- resid(insurance.lm)
ypred <- predict(insurance.lm)
plot(ypred,yres,ylab="Residuals",xlab="Predicted Values",main="Residuals vs. Predicted Values",pch=19,col="dimgrey")
abline(h=0)
#normality plot
insurance.stdres = rstandard(insurance.lm)
qqnorm(insurance.stdres)
qqline(insurance.stdres)


##Muhammad Code
#Histogram for the charges column
hist(insurance$charges,col ="Blue",xlab="Charges")
summary(insurance$charges)

is.null(insurance)
summary(insurance)
colSums(is.na(insurance))

cor(insurance[c("age", "bmi", "children", "charges")])

pairs(insurance[c("age", "bmi", "children", "charges")],main="ScatterPlot Matrix")

plot(insurance$age,insurance$charges,col="blue",xlab="Age",ylab="Charges")

library(ggplot2)

ggplot(insurance, aes(age, charges, colour = smoker,"Age vs Charges Scatter plot with smokers")) + geom_point() 

ggplot(insurance, aes(age, charges, colour = sex,"Age vs Charges Scatter plot with smokers")) + geom_point() 

ggplot(insurance, aes(age, charges, colour = region,"Age vs Charges Scatter plot with smokers")) + geom_point() 

i=1
for (i in 1:length(insurance$bmi)){
  if (insurance$bmi[i]>30){
    insurance$Obese[i]="Obese"
  }
  else 
  {insurance$Obese[i]="Not Obese"}
  i=i+1
}


ggplot(insurance, aes(age, charges, colour = Obese,"Age vs Charges Scatter plot with smokers")) + geom_point() 

ggplot(insurance, aes(age, charges, colour = children,"Age vs Charges Scatter plot with smokers")) + geom_point() 

ggplot(insurance, aes(bmi, charges,colour=smoker)) + geom_point() 

ggplot(insurance, aes(region, charges,colour=region)) + geom_violin()+geom_boxplot(width=0.1)

ggplot(insurance, aes(sex, charges,colour=sex)) + geom_violin()+geom_boxplot(width=0.1)

ggplot(insurance, aes(smoker, charges,colour=smoker)) + geom_violin()+geom_boxplot(width=0.1)

ggplot(insurance, aes(Obese, charges,colour=Obese)) + geom_violin()+geom_boxplot(width=0.1)

i=0
insurance['status']=NA
insurance$status=c(1:length(insurance$bmi))
for (i in 1:length(insurance$bmi)){
  if (insurance$smoker[i]== "yes" ){
    if (insurance$Obese[i]=="Obese"){
      insurance$status[i]="Smoker and obese"}
    else
    {insurance$status[i]="Smoker or Obese"}
  }else if (insurance$Obese[i]=="Obese")
  {insurance$status[i]="Smoker or Obese"
  }else {insurance$status[i]="Neither"}
  i=i+1
}

ggplot(insurance, aes(age, charges, colour = status)) + geom_point() 









#Load data and convert categ variables to factors
insurance <- read.csv('insurance.csv", header = TRUE, stringsAsFactors = TRUE)
str(insurance)
attach(insurance)

#y distribution is right-skewed= mean>median
#1st violation of lin reg= normal dist for dependent variable - try to correct with model
summary(insurance$charges)
hist(insurance$charges)

#cor for numeric variables - look at initial relationships
cor(insurance[c('age','bmi','children','charges')])

#scatterplot of num variables - look for trends, not clouds
pairs(insurance[c("age", "bmi", "children", "charges")])

#scatter plot with cor- explain ovals
install.packages('psych')
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

#create full model- explain why intercept should be reasonably ignored. interpret (ex. other variables held constant)
full_mod<- lm(charges~., data = insurance)
#explain dummy coding
full_mod

###EVALUATE MODEL PERFORMANCE
#look at residuals, stars, r2
summary(full_mod)

###IMPROVE MODEL
#add polynomial for age
insurance$age2 <- insurance$age^2

#turn bmi into binary indicator var b/c only care if normal or obese, not actual values
insurance$bmi30 <- ifelse(insurance$bmi > 30, 1, 0)

#Add interaction between smokers and bmi over 30

#Add all together for improved model
imp_model <- lm(charges~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)

#better r2
summary(imp_model)

#heatmap code
heatmap(as.matrix(insurance[c('age','bmi','children','charges')]),scale='column',Colv = NA, Rowv = NA)
