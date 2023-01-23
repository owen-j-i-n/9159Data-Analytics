###########################################
#MDA9160 Final Project
#Authors: Runze Yi, Xianglin Jin
#Importing data/library and data preparation
#data = read.csv('Life Expectancy Data.csv')
data = read.csv('https://raw.githubusercontent.com/owen-j-i-n/9159Data/main/LifeExpectancyData.csv')

library(ggplot2)
library(lmtest)
library(MASS)
library(faraway)
library(Metrics)
library(vtable)
library(corrplot)


missing_counts <- data.frame(feature = factor(names(data)),
                             counts=sapply(data, function(x) sum(is.na(x))))

#distribution of missing values
ggplot(missing_counts,
       aes(x=reorder(feature, -counts), y=counts, fill=counts)) +
  geom_bar(stat="identity") +
  ggtitle("Missing counts in each feature") +
  xlab("Feature") + ylab("Missing count") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))+
  scale_fill_continuous(trans = 'reverse')

missing.rows = dim(data)[1] -  dim(na.omit(data))[1]
sprintf("Dataset size: [%s]", toString(dim(data)))
sprintf("Missing rows: %s (%s%%)", missing.rows, round((missing.rows*100)/dim(data)[1], 2))

missings_df <- data.frame(type=c("missing", "non-missing") ,count = c(missing.rows,  dim(na.omit(data))[1]))

ggplot(missings_df, aes(fill=type, y="", x=count)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Missing vs Non-missing row counts") +
  xlab("Missing count") + ylab("") +
  theme(text = element_text(size = 18))+
  scale_fill_brewer(palette="Set1")

#43.87% of the data contains missing rows. If data are just supplied with the median or mean, information
#is actually lost rather than gained since we have no knowledge of how the null values appeard and how
#the data is collected.
#Therefore we might just as well remove them.


#The below commented code replaces null with median/mean,
#but we chose to not use that since the performance and assumptions
#are worse than just removing them
#To see how it performs with replacing missing values,
#just uncomment the following code and run from start again.
# par(mfrow=c(2,7))
# boxplot(data$Life.expectancy,
#         ylab = "Life Expectancy",
#         main = "Boxplot of Life Expectancy",
#         col= "#FF6666",
#         outcol="#FF6666")
# boxplot(data$Adult.Mortality,
#         ylab = "Adult Mortality",
#         main = "Boxplot of Adult Mortality",
#         col= "#FF6666",
#         outcol="#FF6666")
# boxplot(data$Alcohol,
#         ylab = "Alcohol",
#         main = "Boxplot of Alcohol",
#         col= "#008080",
#         outcol="#008080")
# boxplot(data$Hepatitis.B,
#         ylab = "Hepatitis B",
#         main = "Boxplot of Hepatitis B",
#         col= "#FF6666",
#         outcol="#FF6666")
# boxplot(data$BMI,
#         ylab = "BMI",
#         main = "Boxplot of BMI",
#         col= "#008080",
#         outcol="#008080")
# boxplot(data$Polio,
#         ylab = "Polio",
#         main = "Boxplot of Polio",
#         col= "#FF6666",
#         outcol="#FF6666")
# boxplot(data$Total.expenditure,
#         ylab = "Total Expenditure",
#         main = "Boxplot of Total Expenditure",
#         col= "#FF6666",
#         outcol="#FF6666")
# boxplot(data$Diphtheria,
#         ylab = "Diphteria",
#         main = "Boxplot of Diphteria",
#         col= "#FF6666",
#         outcol="#FF6666")
# boxplot(data$GDP,
#         ylab = "GDP",
#         main = "Boxplot of GDP",
#         col= "#FF6666",
#         outcol="#FF6666")
# boxplot(data$Population,
#         ylab = "Population",
#         main = "Boxplot of Population",
#         col= "#FF6666",
#         outcol="#FF6666")
# boxplot(data$thinness..1.19.years,
#         ylab = "Thinness 1-19 years",
#         main = "Boxplot of Thinness for 1-19 years old",
#         col= "#FF6666",
#         outcol="#FF6666")
# boxplot(data$thinness.5.9.years,
#         ylab = "Thinness 5-9 years",
#         main = "Boxplot of Thinness for 5-9 years old",
#         col= "#FF6666",
#         outcol="#FF6666")
# boxplot(data$Income.composition.of.resources,
#         ylab = "Income Composition",
#         main = "Boxplot of Income Composition",
#         col= "#008080",
#         outcol="#008080")
# boxplot(data$Schooling,
#         ylab = "Schooling",
#         main = "Boxplot of Schooling",
#         col= "#FF6666",
#         outcol="#FF6666")
# boxplot(data$Year,
#         ylab = "Year",
#         main = "Boxplot of Year",
#         col= "#008080",
#         outcol="#008080")
# #Only alcohol, BMI , Year and Income.composition.of.resources shows few or no outliers
# 
# Life.expectancy_median <- median(data$Life.expectancy,  na.rm = TRUE)
# Adult.Mortality_median <- median(data$Adult.Mortality,  na.rm = TRUE)
# Hepatitis.B_median <- median(data$Hepatitis.B,  na.rm = TRUE)
# Polio_median <- median(data$Polio,  na.rm = TRUE)
# Diphtheria_median <- median(data$Diphtheria,  na.rm = TRUE)
# Total.expenditure_median <- median(data$Total.expenditure,  na.rm = TRUE)
# GDP_median <- median(data$GDP,  na.rm = TRUE)
# Population_median <- median(data$Population,  na.rm = TRUE)
# thinness..1.19.years_median <- median(data$thinness..1.19.years,  na.rm = TRUE)
# thinness.5.9.years_median <- median(data$thinness.5.9.years,  na.rm = TRUE)
# Schooling_median <- median(data$Schooling,  na.rm = TRUE)
# Year_median <- median(data$Year, na.rm = TRUE)#did not use mean for Year
# #as it's going to be transformed into categorical
# 
# Alcohol_mean <- mean(data$Alcohol,  na.rm = TRUE)
# BMI_mean <- mean(data$BMI,  na.rm = TRUE)
# Income.composition.of.resources_mean <- mean(data$Income.composition.of.resources,  na.rm = TRUE)
# 
# 
# 
# data$Life.expectancy[is.na(data$Life.expectancy)] <- Life.expectancy_median
# data$Adult.Mortality[is.na(data$Adult.Mortality)] <- Adult.Mortality_median
# data$Hepatitis.B[is.na(data$Hepatitis.B)] <- Hepatitis.B_median
# data$Polio[is.na(data$Polio)] <- Polio_median
# data$Diphtheria[is.na(data$Diphtheria)] <- Diphtheria_median
# data$Total.expenditure[is.na(data$Total.expenditure)] <- Total.expenditure_median
# data$GDP[is.na(data$GDP)] <- GDP_median
# data$Population[is.na(data$Population)] <- Population_median
# data$thinness..1.19.years[is.na(data$thinness..1.19.years)] <- thinness..1.19.years_median
# data$thinness.5.9.years[is.na(data$thinness.5.9.years)] <- thinness.5.9.years_median
# data$Schooling[is.na(data$Schooling)] <- Schooling_median
# #means
# data$Alcohol[is.na(data$Alcohol)] <- Alcohol_mean
# data$BMI[is.na(data$BMI)] <- BMI_mean
# data$Income.composition.of.resources[is.na(data$Income.composition.of.resources)] <- Income.composition.of.resources_mean
# data$Year[is.na(data$Year)] <- Year_median
# 




data$Year=as.factor(data$Year)#change it from numeric to dummy, as the difference might not be equal
st(data)

data=data[rowSums(is.na(data)) == 0,]#remove null entries
unique(data$Country)
data2=subset(data, select = -c(Country,Status,Year))#remove categorical data for scatter plot matrix

copy = data

ggplot(data, aes(x=Life.expectancy)) + 
  geom_density(alpha=.3, fill="red", color="red", size=1.5)+
  geom_vline(aes(xintercept=mean(Life.expectancy)), size=1)+
  ggtitle("Distribution density of Life.expectancy") +
  theme(text = element_text(size = 18))

#The data is not in normal shape, with a mean of approximately 70
st(data)
st(data2)

pairs(data2)#scatter plot matrix
#none of the predictors have a linear relationship with the response variable

###########################################
#Checking model assumption with plain model
plain_lm = lm(Life.expectancy~.,data=data)#plain model with all predictors
summary(plain_lm)
#Multiple R-squared:  0.9679,	Adjusted R-squared:  0.9643


#Residual plot , the Linearity assumption might hold but EV assumption clearly violated
plot(fitted(plain_lm), resid(plain_lm), pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Residual plot")
abline(h = 0, col = "darkorange", lwd = 2)

#QQ plot, normality assumption clearly violated
qqnorm(resid(plain_lm), pch = 20, main = "QQ Plot")  
qqline(resid(plain_lm), col = "dodgerblue", lwd = 2)

bptest(plain_lm)#2.889e-08
shapiro.test(resid(plain_lm))#2.2e-16



lm_cd = cooks.distance(plain_lm)#cooks distance to find influential points

inf_i = which(lm_cd > 4/length(lm_cd))
length(lm_cd[inf_i])
data=data[-inf_i,]

inf_lm = lm(Life.expectancy~.,data=data)
summary(inf_lm)
#Multiple R-squared:  0.991,	Adjusted R-squared:  0.99 

#Residual plot - The linearity assumption might hold but EV assumption is clearly violated
plot(fitted(inf_lm), resid(inf_lm), pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Residual plot")
abline(h = 0, col = "darkorange", lwd = 2)

#QQ plot, normality assumption clearly violated
qqnorm(resid(inf_lm), pch = 20, main = "QQ Plot")  
qqline(resid(inf_lm), col = "dodgerblue", lwd = 2)

bptest(inf_lm)#2.2e-16
shapiro.test(resid(inf_lm))#2.2e-16





b = boxcox(inf_lm,lambda = seq(0, 2, 1/10))#choose a lambda of 1.5

lambda = 1.5
lm_transf = lm(((Life.expectancy^(lambda)-1)/(lambda))~.,data=data)
summary(lm_transf)
#Multiple R-squared:  0.9908,	Adjusted R-squared:  0.9897  

#Residual plot - Linearity looks good and Equal Variance looks bad
plot(fitted(lm_transf), resid(lm_transf), pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Residual plot")
abline(h = 0, col = "darkorange", lwd = 2)

#QQ plot, normality looks bad but a bit better than before
qqnorm(resid(lm_transf), pch = 20, main = "QQ Plot")  
qqline(resid(lm_transf), col = "dodgerblue", lwd = 2)


bptest(lm_transf)#2.534e-11
shapiro.test(resid(lm_transf))#2.2e-16


#######################################
#Repeat the model assumptions test after removing Country
data = copy


plain_lm = lm(Life.expectancy~.-Country,data=data)#plain model with all predictors
summary(plain_lm)#R2 dropped by more than 0.1
#Multiple R-squared:  0.839,	Adjusted R-squared:  0.8356


#Residual plot , both assumptions clearly violated
plot(fitted(plain_lm), resid(plain_lm), pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Residual plot")
abline(h = 0, col = "darkorange", lwd = 2)

#QQ plot, normality assumption looks violated, but looks significantly better than before
qqnorm(resid(plain_lm), pch = 20, main = "QQ Plot")  
qqline(resid(plain_lm), col = "dodgerblue", lwd = 2)

bptest(plain_lm)#2.2e-16
shapiro.test(resid(plain_lm))#7.87e-08, a lot better



lm_cd = cooks.distance(plain_lm)#cooks distance to find influential points

inf_i = which(lm_cd > 4/length(lm_cd))
length(lm_cd[inf_i])
data=data[-inf_i,]

inf_lm = lm(Life.expectancy~.-Country,data=data)
summary(inf_lm)
#Multiple R-squared:  0.8747,	Adjusted R-squared:  0.8719


#Residual plot - both assumptions are clearly violated
plot(fitted(inf_lm), resid(inf_lm), pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Residual plot")
abline(h = 0, col = "darkorange", lwd = 2)

#QQ plot, normality looks close to good
qqnorm(resid(inf_lm), pch = 20, main = "QQ Plot")  
qqline(resid(inf_lm), col = "dodgerblue", lwd = 2)

bptest(inf_lm)#2.2e-16
shapiro.test(resid(inf_lm))#0.003814#much better





b = boxcox(inf_lm,lambda = seq(0, 2, 1/10))#choose a lambda of 0.8

lambda = 0.8
lm_transf = lm(((Life.expectancy^(lambda)-1)/(lambda))~.-Country,data=data)

#Residual plot - both assumptions are clearly violated
plot(fitted(lm_transf), resid(lm_transf), pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Residual plot")
abline(h = 0, col = "darkorange", lwd = 2)

#QQ plot, normality looks close to good
qqnorm(resid(lm_transf), pch = 20, main = "QQ Plot")  
qqline(resid(lm_transf), col = "dodgerblue", lwd = 2)


bptest(lm_transf)#2.2e-16
shapiro.test(resid(lm_transf))#0.001902

#All three assumptions are violated before or after removing influential points
#transforming model or repeating the same after removing the Country
#wait to see more of this after removing some more predictors
##############################################
#Using VIF, variable selection to choose useful predictors
data = copy


model = lm(Life.expectancy~.,data=data)
summary(model)
#Multiple R-squared:  0.9679,	Adjusted R-squared:  0.9643

model2 = lm(Life.expectancy~.-Country,data=data)
summary(model2)
#Multiple R-squared:  0.839,	Adjusted R-squared:  0.8356

anova(model2, model)
#P value:2.2e-16, Country is significant

model3 = lm(Life.expectancy~.-Year,data=data)
anova(model3, model)
#P value:2.2e-16, Year is significant

model4 = lm(Life.expectancy~.-Status,data=data)
anova(model4, model)
# This predictor is completely irrevalent, thus dropped
data=subset(data, select = -c(Status))#remove categorical data for scatter plot matrix
copy = data

mod.linear <- lm(Life.expectancy~ .-Country-Year, data)
vifs <- data.frame(vif(mod.linear))

ggplot(vifs, aes(y=vif.mod.linear., x=row.names(vifs))) + 
  geom_bar(aes(fill=vif.mod.linear.>5),stat="identity")+
  scale_y_continuous(trans = "sqrt",  breaks = c(5, 10, 50, 100))+
  geom_hline(yintercept = 5, colour = "red") + 
  ggtitle("VIF per feature") +
  xlab("Featurs") + ylab("VIF") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))+
  scale_fill_brewer(palette="Dark2")

vif(model2)# Country is a non-identifable parameters
which(vif(model2)>10)#remove infant.deaths first, that one is the largest among these 4

vif_model = lm(Life.expectancy~.-Country-infant.deaths,data=data)
which(vif(vif_model)>10)#remove GDP

vif_model = lm(Life.expectancy~.-Country-infant.deaths-GDP,data=data)
which(vif(vif_model)>10)#all below 10 now.

vifs <- data.frame(vif(vif_model))
ggplot(vifs, aes(y=vif.vif_model., x=row.names(vifs))) + 
  geom_bar(aes(fill=vif.vif_model.>10),stat="identity")+
  scale_y_continuous(trans = "sqrt",  breaks = c(5, 10, 50, 100))+
  geom_hline(yintercept = 10, colour = "red") + 
  ggtitle("VIF per feature") +
  xlab("Featurs") + ylab("VIF") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))+
  scale_fill_brewer(palette="Dark2")

data=subset(data, select = -c(GDP,infant.deaths))#remove the two from data
copy = data

model_vif = lm(Life.expectancy~.,data=data)#model after removing correlation
summary(model_vif)
#Multiple R-squared:  0.9676,	Adjusted R-squared:  0.9641

model_vif2 = lm(Life.expectancy~.-Country,data=data)#if we further exclude Country
summary(model_vif2)
#without Country
#Multiple R-squared:  0.8313,	Adjusted R-squared:  0.8281

###################################################
#Applying backward/forward/stepwise selection
#using model_vif meaning including Country in the selection process
step_AIC = step(model_vif, direction = "both", trace = 0)
length(step_AIC$coefficients)#155 coefficients, including dummy and intercept

back_AIC = step(model_vif, direction = "backward", trace = 0)
length(back_AIC$coefficients)#155 coefficients, including dummy and intercept

for_AIC = step(model_vif, direction = "forward", trace = 0)
length(for_AIC$coefficients)#164 coefficients, including dummy and intercept

which(step_AIC$coefficients!=back_AIC$coefficients)#step and back are the same
which(for_AIC$coefficients!=model_vif$coefficients)#for and vif are the same

#step and back has the same model, forward model is the same as the model_vif



back_BIC = step(model_vif,direction="backward",trace=0, k = log(nrow(data)))
length(back_BIC$coefficients)#151 coefficients, including dummy and intercept

for_BIC = step(model_vif,direction="forward",trace=0, k = log(nrow(data)))
length(for_BIC$coefficients)#164 coefficients, including dummy and intercept

step_BIC = step(model_vif,direction="both",trace=0, k = log(nrow(data)))
length(step_BIC$coefficients)#151 coefficients, including dummy and intercept

which(back_BIC$coefficients!=step_BIC$coefficients)#back and step are the same
which(for_BIC$coefficients!=model_vif$coefficients)#for and vif are the same


length(model_vif$coefficients)#164 coefficients
summary(model_vif)
length(step_AIC$coefficients)#155 coefficients
summary(step_AIC)
length(step_BIC$coefficients)#151 coefficients
summary(step_BIC)

#H0: the parameters removed from the stepwise(AIC) process are not significant.
D_stat_AIC = deviance(step_AIC) - deviance(model_vif)
D_stat_AIC #8.834864
1-pchisq(D_stat_AIC,9)#0.4526554; the parameters removed are insignificant

#H0: the parameters removed from the stepwise(BIC) process are not significant.
D_stat_BIC = deviance(step_BIC) - deviance(model_vif)
D_stat_BIC #43.07532
1-pchisq(D_stat_BIC,12)#2.192626e-05, some of the parameters removed are significant



#######################################
#Cross validation
# k fold cross validation
k = 5
RMSE_AIC = RMSE_BIC = RMSE_vif = numeric(k)

# Shuffle the data (randomization)
# First randomly order integers 1,...,n (using the sample function)
set.seed(1)


n = nrow(data)

#Create k equally size folds
folds <- cut(1:n,breaks=k,labels=FALSE)

#Perform a k-fold cross validation
for(i in 1:k)
{
  # Find the indices for test data
  test_index = which(folds==i)
  
  # Obtain training/test data
  test_data = data[test_index, ]
  training_data = data[-test_index, ]
  
  #CV can't work if these categorical data are added
  kcv_AIC <- update(step_AIC, . ~ .-Country-Year, data = training_data)
  kcv_BIC <- update(step_BIC, . ~ .-Country-Year, data = training_data)
  kcv_vif <- update(model_vif, . ~ . -Country-Year, data = training_data)
  
  # Obtain RMSE on the 'test' data
  resid_AIC = test_data$Life.expectancy - predict(kcv_AIC, newdata=test_data) 
  RMSE_AIC[i] = sqrt(sum(resid_AIC^2)/nrow(test_data)) 
  
  resid_BIC = test_data$Life.expectancy - predict(kcv_BIC, newdata=test_data) 
  RMSE_BIC[i] = sqrt(sum(resid_BIC^2)/nrow(test_data))
  
  resid_vif = test_data$Life.expectancy - predict(kcv_vif, newdata=test_data) 
  RMSE_vif[i] = sqrt(sum(resid_vif^2)/nrow(test_data))
  
}


# Chooses fit_quad 
mean(RMSE_AIC)#4.624697
mean(RMSE_BIC)#4.82167
mean(RMSE_vif)#3.957291
#choose the model before variable selection, just after VIF

#if we add year back
step_AIC_year = update(step_AIC, . ~ .-Country, data = data)
step_BIC_year = update(step_BIC, . ~ .-Country, data = data)
model_vif_year = update(model_vif, . ~ . -Country, data = data)

#PRESS if we add back Year
sum((resid(step_AIC_year)/(1-hatvalues(step_AIC_year)))^2)/n#17.75981
sum((resid(step_BIC_year)/(1-hatvalues(step_BIC_year)))^2)/n#20.48571
sum((resid(model_vif_year)/(1-hatvalues(model_vif_year)))^2)/n#13.6213


#if we add country back
step_AIC_country = update(step_AIC, . ~ .-Year, data = data)
step_BIC_country = update(step_BIC, . ~ .-Year, data = data)
model_vif_country = update(model_vif, . ~ .-Year, data = data)

#PRESS if we add back Country, all Inf, overfitted too hard
sum((resid(step_AIC_country)/(1-hatvalues(step_AIC_country)))^2)/n#Inf
sum((resid(step_BIC_country)/(1-hatvalues(step_BIC_country)))^2)/n#Inf
sum((resid(model_vif_country)/(1-hatvalues(model_vif_country)))^2)/n#Inf


#if we add country back
step_AIC_both = update(step_AIC, . ~ ., data = data)
step_BIC_both = update(step_BIC, . ~ ., data = data)
model_vif_both = update(model_vif, . ~ ., data = data)

#PRESS if we add both back, all Inf, overfitted too hard
sum((resid(step_AIC_both)/(1-hatvalues(step_AIC_both)))^2)/n#Inf
sum((resid(step_BIC_both)/(1-hatvalues(step_BIC_both)))^2)/n#Inf
sum((resid(model_vif_both)/(1-hatvalues(model_vif_both)))^2)/n#Inf


#PRESS if we don't add back anything
step_AIC = update(step_AIC, . ~ .-Country-Year, data = data)
step_BIC = update(step_BIC, . ~ .-Country-Year, data = data)
model_vif = update(model_vif, . ~ .-Country-Year, data = data)


sum((resid(step_AIC)/(1-hatvalues(step_AIC)))^2)/n#18.02758
sum((resid(step_BIC)/(1-hatvalues(step_BIC)))^2)/n#20.43165
sum((resid(model_vif)/(1-hatvalues(model_vif)))^2)/n#13.74599

#Choose the model after VIF with year but not country, as it has a lower PRESS
#which is model_vif_year

#######################################################
#Checking model assumption again with chosen model

summary(model_vif_year)
#Multiple R-squared:  0.8313,	Adjusted R-squared:  0.8281 


#Residual plot , both assumptions clearly violated
plot(fitted(model_vif_year), resid(model_vif_year), pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Residual plot")
abline(h = 0, col = "darkorange", lwd = 2)

#QQ plot, normality assumption looks violated
qqnorm(resid(model_vif_year), pch = 20, main = "QQ Plot")  
qqline(resid(model_vif_year), col = "dodgerblue", lwd = 2)

bptest(model_vif_year)#2.2e-16
shapiro.test(resid(model_vif_year))#2.102e-08



lm_cd = cooks.distance(model_vif_year)#cooks distance to find influential points

inf_i = which(lm_cd > 4/length(lm_cd))
length(lm_cd[inf_i])
data=data[-inf_i,]

inf_lm = lm(Life.expectancy~.-Country,data=data)
summary(inf_lm)
#Multiple R-squared:  0.8733,	Adjusted R-squared:  0.8707 

#Residual plot - both assumptions clearly violated
plot(fitted(inf_lm), resid(inf_lm), pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Residual plot")
abline(h = 0, col = "darkorange", lwd = 2)

#QQ plot, normality assumption looks rather okay
qqnorm(resid(inf_lm), pch = 20, main = "QQ Plot")  
qqline(resid(inf_lm), col = "dodgerblue", lwd = 2)

bptest(inf_lm)#2.2e-16
shapiro.test(resid(inf_lm))#0.01018



b = boxcox(inf_lm,lambda = seq(0, 2, 1/10))#choose a lambda of 0.8

lambda = 0.8
lm_transf = lm(((Life.expectancy^(lambda)-1)/(lambda))~.-Country,data=data)

#Residual plot - Linearity looks good and Equal Variance looks bad
plot(fitted(lm_transf), resid(lm_transf), pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Residual plot")
abline(h = 0, col = "darkorange", lwd = 2)

#QQ plot, normality looks bad but a bit better than before
qqnorm(resid(lm_transf), pch = 20, main = "QQ Plot")  
qqline(resid(lm_transf), col = "dodgerblue", lwd = 2)


bptest(lm_transf)#2.2e-16
shapiro.test(resid(lm_transf))#0.009024

#Based on model assumption's test results, we will remove the influential points
#but not transform the data

final_model = lm(Life.expectancy~.-Country,data=data)
summary(final_model)
#Multiple R-squared:  0.8733,	Adjusted R-squared:  0.8707 

resid = data$Life.expectancy - predict(final_model) 
RMSE = sqrt(sum(resid^2)/nrow(data)) #2.920846

round(1533*0.3)
test_index = sample(1533,460,rep=FALSE)
test_set = data[test_index,]
training_set = data[-test_index, ]
eval_final_model = lm(Life.expectancy~.-Country,data=training_set)

resid = training_set$Life.expectancy - predict(eval_final_model) 
RMSE_train = sqrt(sum(resid^2)/nrow(data)) #2.442954

resid = test_set$Life.expectancy - predict(eval_final_model, newdata = test_set) 
RMSE_test = sqrt(sum(resid^2)/nrow(data)) #1.62088

summary(eval_final_model)
#Multiple R-squared:  0.875,	Adjusted R-squared:  0.8713 