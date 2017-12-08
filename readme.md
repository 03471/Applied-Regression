#STAT 5310
#Semester Project

#Work on 2 datasets: one is diabetes(faraway); one of your choice
packages <- c('faraway','car','stats', 'MASS','lmtest')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), dependencies = TRUE)  
}
require(faraway)
require(car)
require(stats)
require(MASS)
require(lmtest)

#A. (90 pts) From each of the data sets(2) perform the followings:
#__________________________________________________________________________________________________
#Dataset: Diabetes
data(diabetes)
attach(diabetes)
#------------------------------------------------------------------------------------------------
#######################
#Inspect the dataset
#######################

"(a) Explain briefly the data (all variables): the variables, any variable needed to be taken out."
"
Description
403 African Americans were interviewed in a study to understand the prevalence of obesity, diabetes,
and other cardiovascular risk factors in central Virginia.

Usage
data(diabetes)

Format
A data frame with 403 observations on the following 19 variables.
id Subject ID
chol Total Cholesterol
stab.glu Stabilized Glucose
hdl High Density Lipoprotein
ratio Cholesterol/HDL Ratio
glyhb Glycosolated Hemoglobin ***
location County - a factor with levels Buckingham Louisa
age age in years
gender a factor with levels male female
height height in inches
weight weight in pounds
frame a factor with levels small medium large
bp.1s First Systolic Blood Pressure

"

#Details
#Glycosolated hemoglobin greater than 7.0 is usually taken as a positive diagnosis of diabetes
#glyhb
#

#id column should be taken out
diabetes.omitted <- na.omit(diabetes)
diabetes.reduced <- diabetes.omitted[ , -which(names(diabetes) %in% c("id","location","gender","frame"))]
diabetes.fit <- lm(chol ~ . -id, data=diabetes.omitted)

boxplot(chol~hip, data = diabetes.reduced)

#------------------------------------------------------------------------------------------------
"(b) Missing data (Remove the NA's. 

Do a summary If factors are present, do boxplots, and summaries with the respect to factors."
summary(diabetes.reduced)

lapply(diabetes.reduced, function(x){
  if(is.factor(x)){
    summary.factor(x, maxLevels = 5, sumstat = TRUE)  
  }
})
#data is clean no factors present

#help(boxplot.matrix)

boxplot(diabetes.reduced, main = "diabetes boxplot", notch = TRUE, col = 1:7)
#see outliers for chol, hdl, weight, time.ppn

#------------------------------------------------------------------------------------------------
"(c) Do a scatterplot of the data: notice any unusual patterns."

pairs(~ ., data=diabetes.reduced, main="Diabetes Scatterplot Matrix", cex=.01)

scatterplot(glyhb~ratio, data=diabetes.reduced, 
            xlab="Weight of Diabetes", 
            ylab="Miles Per Gallon",             
            main="Enhanced Scatter Plot")
names(diabetes.reduced)
"chol     stab.glu hdl      ratio    glyhb    location age      gender  
height   weight   frame    bp.1s    bp.1d    waist    hip      time.ppn"

scatterplot(chol~hip, data=diabetes.reduced, 
            xlab="hip", 
            ylab="chol",             
            main="Enhanced Scatter Plot")

scatterplot(chol~glyhb, data=diabetes.reduced, 
            xlab="glyhb", 
            ylab="chol",             
            main="Enhanced Scatter Plot")



#------------------------------------------------------------------------------------------------
"(d) Correlation matrix (including the response): notice any high correlations(>.7)"

diabetes.cor <- cor(diabetes.reduced, method = "pearson", use = "complete.obs")
lapply(diabetes.cor, function(x){
  print(x)
})

#highlight correlation
diabetes.cor[abs(diabetes.cor) < 0.7] <- NA
round(diabetes.cor, digits = 2)

#------------------------------------------------------------------------------------------------

#######################
#A tentative full model
#######################

"
(e) Depending on the application, choose the response variable, which is continuous. Build a full
  linear model
"

"from Faraway's book and explanation of Diabetes data
Glycosolated hemoglobin 'glyhb' greater than 7.0 is usually taken as a positive diagnosis of diabetes
"
#add response to data and remove  'glyhb'

diabetes.fit <- lm(chol ~ . -id, data=diabetes.omitted)
summary(diabetes.fit)

options(scipen=999)
2.2e-16

#
#------------------------------------------------------------------------------------------------
"
(f) Check the F-test, t-test of coefficient estimates: significance of coefficients estimates
"
#As the p-value is much less than 0.05, 
#we reject the null hypothesis that ?? = 0. 
#Hence there is a significant relationship between the variables 
#in the linear regression model of the data set faithful.

"
Residual standard error: 21.91 on 111 degrees of freedom
Multiple R-squared:  0.8167,	Adjusted R-squared:  0.7869 
F-statistic: 27.47 on 18 and 111 DF,  p-value: < 0.00000000000000022
"

#------------------------------------------------------------------------------------------------
#######################
#Reduced Model
#######################
#------------------------------------------------------------------------------------------------
"(g) Build a reduced model, using step() Perform an anova() procedure."
diabetes.fit.step <- step(diabetes.fit)
anova(diabetes.fit, diabetes.fit.step)
"
#Hnull : diabetes.fit and diabetes.fit.step are significantly same
#Halt : diabetes.fit and diabetes.fit.step are significanly different
#0.86 pvalue >> alpha do not reject null hypothesis
124 56875 -13   -3605.1 0.5779 0.8673
"
#------------------------------------------------------------------------------------------------
"(h) Perform a multicollinarity test (using vif())"

#initial model
vif(diabetes.fit)
#optimized model
vif(diabetes.fit.step)

#from both initial and optimized model all predictors are below threshold 10

#------------------------------------------------------------------------------------------------
"(k) Perform powerTranform() and boxcox, to see any transformations needed

Write the Regression Equation
lm(formula = chol ~ hdl + ratio + glyhb + height + bp.2d, data = diabetes.omitted)

If there are indicator variable, give interpretations.
"
diabetes.powerTransform <- powerTransform(cbind(chol ,bp.2d, glyhb , height ,hdl , ratio)~1, diabetes.reduced)
summary(diabetes.powerTransform)

diabetes.fit.transformed <- lm(log(chol)~bp.2d+I(glyhb^-1)+I(height^1) + log(hdl) + log(ratio), data=diabetes.omitted)
summary(diabetes.fit.transformed)

#------------------------------------------------------------------------------------------------
#######################
#Residual Analysis
#######################
#------------------------------------------------------------------------------------------------
"(l) Check residual requirements: constant variance plot, normal distribution check"

res <- diabetes.fit.transformed$residuals

#check the normality
hist(res)
qqnorm(res)


#check the normality
qqline(res)
rs <- rstandard(diabetes.fit.transformed)  ## standardised residuals
qqnorm(rs)  ## fine
yy <- quantile(rs, c(0.05, 0.95))
xx <- qnorm(c(0.05, 0.95))
slope <- diff(yy)/diff(xx)
int <- yy[1L] - slope * xx[1L]
abline(as.numeric(int), as.numeric(slope), col='red')  


shapiro.test(res)
#Hnull: normal Halt: not normal pvalue <= alpha
#this test is unreliable sometimes

dwtest(diabetes.fit.transformed, alternative = "two.sided")

"	Durbin-Watson test
data:  diabetes.fit.transformed
DW = 1.9215, p-value = 0.655
alternative hypothesis: true autocorrelation is not 0"

#------------------------------------------------------------------------------------------------
"(m) Take out all outlyers. Form new dataset if necessary. 
   Build the reduced model as above on the new dataset."

cooks <- cooks.distance(diabetes.fit.transformed)
plot(cooks, pch="*", cex=2, main="Diabetes by Cooks distance")
h <- 2*(5+1)/130
abline(h = h, col="red")
abline(h = 4*mean(cooks, na.rm=T), col="blue")
text(x=1:length(cooks)+5, y=cooks, offset = 0.5,family="A",cex=0.6, pos=2, labels=ifelse(cooks>4*mean(cooks, na.rm=T),names(cooks),""), col="red") 

diabetes.omitted.cooks <- diabetes.omitted[cooks<h,] 
dim(diabetes.omitted.cooks)

diabetes.fit.transformed <- lm(log(chol)~bp.2d+I(glyhb^-1)+I(height^1) + log(hdl) + log(ratio), data=diabetes.omitted.cooks)
summary(diabetes.fit.transformed)

#cross check normality
res <- diabetes.fit.transformed$residuals

#check the normality
hist(res)
qqnorm(res)


#check the normality
qqline(res)
rs <- rstandard(diabetes.fit.transformed)  ## standardised residuals
qqnorm(rs)  ## fine
yy <- quantile(rs, c(0.05, 0.95))
xx <- qnorm(c(0.05, 0.95))
slope <- diff(yy)/diff(xx)
int <- yy[1L] - slope * xx[1L]
abline(as.numeric(int), as.numeric(slope), col='red')

#------------------------------------------------------------------------------------------------
#######################
#Prediction
#######################
#------------------------------------------------------------------------------------------------
"Make 2 predictions and prediction intervals. 
 Predictor values should be in the range or out of range"

summary(diabetes.omitted.cooks$bp.2d)
"
> summary(diabetes.omitted.cooks$bp.2d)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
60.00   84.00   92.00   92.67  100.00  124.00 
"

diabetes.newdata.outrange <- data.frame(bp.2d =150, glyhb=mean(glyhb), height=mean(height), hdl=mean(hdl), ratio=mean(ratio))
diabetes.prediction.outrange <- predict(diabetes.fit.transformed, diabetes.newdata.outrange, se=T,interval = "prediction")
exp(5.484417) 
#predicted chol=240.9085

diabetes.newdata.inrange <- data.frame(bp.2d =92, glyhb=mean(glyhb), height=mean(height), hdl=mean(hdl), ratio=mean(ratio))
diabetes.prediction.inrange <- predict(diabetes.fit.transformed, diabetes.newdata.inrange, se=T,interval = "prediction")
exp(5.483396) 
#predicted chol=240.6626

#------------------------------------------------------------------------------------------------
###################################################################################################
###################################################################################################
###################################################################################################
#__________________________________________________________________________________________________

#Dataset: Boston
data(Boston)
attach(Boston)
str(Boston)
summary(Boston)
#------------------------------------------------------------------------------------------------
#######################
#Inspect the dataset
#######################

"(a) Explain briefly the data (all variables): the variables, any variable needed to be taken out."
"Description
The Boston data frame has 506 rows and 14 columns.
Usage
Boston
Format
This data frame contains the following columns:
crim
per capita crime rate by town.
zn
proportion of residential land zoned for lots over 25,000 sq.ft.
indus
proportion of non-retail business acres per town.
chas
Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
nox
nitrogen oxides concentration (parts per 10 million).
rm
average number of rooms per dwelling.
age
proportion of owner-occupied units built prior to 1940.
dis
weighted mean of distances to five Boston employment centres.
rad
index of accessibility to radial highways.
tax
full-value property-tax rate per \$10,000.
ptratio
pupil-teacher ratio by town.
black
1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
lstat
lower status of the population (percent).
medv
median value of owner-occupied homes in \$1000s."

summary(Boston)
#predictors is good nothing needs to be removed
boston.omitted <- na.omit(Boston)
boston.raw <- Boston
names(boston.raw)
#------------------------------------------------------------------------------------------------
"(b) Missing data (Remove the NA's. Do a summary
If factors are present, do boxplots, and summaries with the respoect to factors."

summary(boston.raw)

lapply(boston.raw, function(x){
  if(is.factor(x)){
    summary.factor(x, maxLevels = 5, sumstat = TRUE)  
  }
})

#data is clean no factors present

boxplot(boston.raw, main = "boston boxplot", notch = TRUE, col = 1:7)
#see outliers for crim, zn, black

#------------------------------------------------------------------------------------------------
"(c) Do a scatterplot of the data: notice any unusual patterns."
pairs(~ ., data=boston.raw, main="Boston Scatterplot Matrix", cex=.01)

scatterplot(medv ~ lstat, data=boston.raw, 
            xlab="lstat", 
            ylab="medv",             
            main="Enhanced Scatter Plot")

scatterplot(medv ~ rad, data=boston.raw, 
            xlab="rad", 
            ylab="medv",             
            main="Enhanced Scatter Plot")

scatterplot(medv ~ indus, data=boston.raw, 
            xlab="rad", 
            ylab="medv",             
            main="Enhanced Scatter Plot")
#------------------------------------------------------------------------------------------------
"(d) Correlation matrix (including the response): notice any high correlations(>.7)"
boston.raw.cor <- cor(boston.raw, method = "pearson", use = "complete.obs")
lapply(boston.raw.cor, function(x){
  print(x)
})

#highlight correlation
boston.raw.cor[abs(boston.raw.cor) < 0.7] <- NA
round(boston.raw.cor, digits = 2)

#------------------------------------------------------------------------------------------------

#######################
#A tentative full model
#######################

"
(e) Depending on the application, choose the response variable, which is continuous. Build a full
linear model
"
boston.fit <- lm(medv ~ ., data=boston.omitted)
summary(boston.fit)

#------------------------------------------------------------------------------------------------
"
(f) Check the F-test, t-test of coefficient estimates: significance of coefficients estimates
"
#As the p-value is much less than 0.05, 
#we reject the null hypothesis that ?? = 0. 
#Hence there is a significant relationship between the variables 
#in the linear regression model of the data set faithful.

"
Residual standard error: 4.745 on 492 degrees of freedom
Multiple R-squared:  0.7406,	Adjusted R-squared:  0.7338 
F-statistic: 108.1 on 13 and 492 DF,  p-value: < 0.00000000000000022
"

#------------------------------------------------------------------------------------------------
#######################
#Reduced Model
#######################
#------------------------------------------------------------------------------------------------
"(g) Build a reduced model, using step() Perform an anova() procedure."

boston.fit.step <- step(boston.fit)
anova(boston.fit, boston.fit.step)
summary(boston.fit.step)

"
Analysis of Variance Table

Model 1: medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat
Model 2: medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio +  black + lstat
Res.Df   RSS Df Sum of Sq      F Pr(>F)
1    492 11079                           
2    494 11081 -2   -2.5794 0.0573 0.9443

#Hnull : boston.fit and boston.fit.step are significantly same
#Halt : boston.fit and boston.fit.step are significanly different
#0.9443 pvalue >> alpha do not reject null hypothesis
Res.Df   RSS Df Sum of Sq      F Pr(>F)
1    492 11079                           
2    494 11081 -2   -2.5794 0.0573 0.9443
"

#------------------------------------------------------------------------------------------------
"(h) Perform a multicollinarity test (using vif())"
#initial model
vif(boston.fit)
#optimized model
vif(boston.fit.step)
#from both initial and optimized model all predictors are below threshold 10
#------------------------------------------------------------------------------------------------
"(k) Perform powerTranform() and boxcox, to see any transformations needed
Write the Regression Equation

#medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + black + lstat

If there are indicator variable, give interpretations.
"


boston.powerTransform <- powerTransform(cbind(medv , crim , zn , chas , nox , rm , dis , rad , tax , ptratio , black , lstat)~1, family="yjPower",boston.raw)
summary(boston.powerTransform)

boston.fit.transformed <- lm(I(medv^.33) ~ I(crim^-0.74) +sqrt(zn)+ sqrt(chas)+I(nox^-5.04)+rm+I(dis^-0.50)+I(rad^0.33)+I(tax^0.76)+I(ptratio^3.54)+I(black^3.76)+I(lstat^0.13), data=boston.omitted)
summary(boston.fit.step)
summary(boston.fit.transformed)
anova(boston.fit.step,boston.fit.transformed)

res <- boston.fit.step$residuals

#check the normality
hist(res)

rs <- rstandard(boston.fit.step)  ## standardised residuals
qqnorm(rs)  ## fine
yy <- quantile(rs, c(0.05, 0.95))
xx <- qnorm(c(0.05, 0.95))
slope <- diff(yy)/diff(xx)
int <- yy[1L] - slope * xx[1L]
abline(as.numeric(int), as.numeric(slope), col='red')  

#------------------------------------------------------------------------------------------------
#######################
#Residual Analysis
#######################
#------------------------------------------------------------------------------------------------
"(l) Check residual requirements: constant variance plot, normal distribution check"

res <- boston.fit.transformed$residuals

#check the normality
hist(res)

rs <- rstandard(boston.fit.transformed)  ## standardised residuals
qqnorm(rs)  ## fine
yy <- quantile(rs, c(0.05, 0.95))
xx <- qnorm(c(0.05, 0.95))
slope <- diff(yy)/diff(xx)
int <- yy[1L] - slope * xx[1L]
abline(as.numeric(int), as.numeric(slope), col='red')  



shapiro.test(res)
#Hnull: normal Halt: not normal pvalue <= alpha
#this test is unreliable sometimes

dwtest(boston.fit.transformed, alternative = "two.sided")

"	Durbin-Watson test
data:  boston.fit.transformed
DW = 1.006, p-value < 0.00000000000000022
alternative hypothesis: true autocorrelation is not 0"

#------------------------------------------------------------------------------------------------
"(m) Take out all outlyers. Form new dataset if necessary. 
Build the reduced model as above on the new dataset."

cooks <- cooks.distance(boston.fit.transformed)
summary(boston.fit.transformed)
plot(cooks, pch="*", cex=2, main="Boston by Cooks distance")
dim(boston.omitted)
h <- 2*(11+1)/506
abline(h = h, col="red")
abline(h = 4*mean(cooks, na.rm=T), col="blue")
text(x=1:length(cooks)+3, y=cooks, offset = 0.5,family="A",cex=0.6, pos=2,labels=ifelse(cooks>4*mean(cooks, na.rm=T),names(cooks),""), col="red") 

boston.omitted.cooks <- boston.omitted[cooks<h,] 
dim(boston.omitted.cooks)

boston.fit.transformed <- lm(I(medv^.33) ~ I(crim^-0.74) +sqrt(zn)+ sqrt(chas)+I(nox^-5.04)+rm+I(dis^-0.50)+I(rad^0.33)+I(tax^0.76)+I(ptratio^3.54)+I(black^3.76)+I(lstat^0.13), data=boston.omitted.cooks)
summary(boston.fit.transformed)

boston.fit.transformed <- lm(I(medv^.33) ~ crim + chas+ nox +rm+I(dis^-0.50)+I(rad^0.33)+I(tax^0.76)+I(ptratio^3.54)+ black +I(lstat^0.13), data=boston.omitted.cooks)
summary(boston.fit.transformed)

res <- boston.fit.transformed$residuals
#cross-check the normality
hist(res)
rs <- rstandard(boston.fit.transformed)  ## standardised residuals
qqnorm(rs)  ## fine
yy <- quantile(rs, c(0.05, 0.95))
xx <- qnorm(c(0.05, 0.95))
slope <- diff(yy)/diff(xx)
int <- yy[1L] - slope * xx[1L]
abline(as.numeric(int), as.numeric(slope), col='red')  

#------------------------------------------------------------------------------------------------
#######################
#Prediction
#######################
#------------------------------------------------------------------------------------------------
"Make 2 predictions and prediction intervals. 
Predictor values should be in the range or out of range"

summary(boston.raw$crim)

boston.newdata.outrange <- data.frame(crim=95, chas=mean(boston.raw$chas), nox=mean(boston.raw$nox), rm= mean(boston.raw$rm), dis=mean(boston.raw$dis), rad=mean(boston.raw$rad), tax= mean(boston.raw$tax), ptratio=mean(boston.raw$ptratio), black=mean(boston.raw$black), lstat=mean(boston.raw$lstat))
boston.prediction.outrange <- predict(boston.fit.transformed, boston.newdata.outrange, se=T,interval = "prediction")
boston.prediction.outrange
#1.857069
exp(1.857069)

boston.newdata.inrange <- data.frame(crim=3.61352, chas=mean(boston.raw$chas), nox=mean(boston.raw$nox), rm= mean(boston.raw$rm), dis=mean(boston.raw$dis), rad=mean(boston.raw$rad), tax= mean(boston.raw$tax), ptratio=mean(boston.raw$ptratio), black=mean(boston.raw$black), lstat=mean(boston.raw$lstat))
boston.prediction.inrange <- predict(boston.fit.transformed, boston.newdata.inrange, se=T,interval = "prediction")
boston.prediction.inrange
#2.687827
exp(2.687827)
#------------------------------------------------------------------------------------------------

#__________________________________________________________________________________________________

#B. (10 pts) From the dataset: gala Use R, Calculate directly

#(a) the variance-covariance matrix
data(gala)
attach(gala)
str(gala)
#identiy matrix
I <- rep(1,30)
#identity matrix
I <- diag(30)
M <- I-(1/30)* I %*% t(I)
M
Z <- as.matrix(gala)
MZ <- M %*% Z
MZ
cov <- 1/29 * t(MZ) %*% (MZ)
cov
#the calculated covarience, the diagonal is the variance 
round(cov,2)

#the covarience from the predefined function
var <- diag(cov)

#(b) calculate correlation matrix
sd_recip <- sqrt(var)^ -1
D <- diag(sd_recip)
D
corr<- D %*% cov %*% D
corr

#__________________________________________________________________________________________________
