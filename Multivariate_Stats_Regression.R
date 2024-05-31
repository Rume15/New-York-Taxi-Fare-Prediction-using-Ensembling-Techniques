#=======================================================
#MULTIVARIATE STATISTICS#
#=======================================================
#=========================================
#Correlation
#The correlation coefficient measures how 
#strongly two variables are related (linearly)
#========================================
library(mlbench)
library(ggplot2)
data("PimaIndiansDiabetes2")
# Use scatter plot to visualize the relationships
# Plot pressure vs mass
ggplot(PimaIndiansDiabetes2, aes(x=mass, y=pressure)) +
  geom_point()
#plot triceps vs mass
ggplot(PimaIndiansDiabetes2, aes(x=mass, y=triceps,color=diabetes)) +
  geom_point()
# plot the scatterplot of mass and triceps 
# together with their arithmetic means
ggplot(PimaIndiansDiabetes2, aes(x=mass, y=triceps, color=pregnant)) +
  geom_point() +
  geom_vline(xintercept=mean(PimaIndiansDiabetes2$mass, na.rm=TRUE), colour="red",
             size=1.2) +
  geom_hline(yintercept=mean(PimaIndiansDiabetes2$triceps, na.rm=TRUE), colour="blue",
             size=1.2) +
  annotate("text", label="mean of mass", angle=90, x=34, y=75,
           size=4, colour="red") +
  annotate("text", label="mean of triceps", x=55, y=25, size=4,
           colour="blue")
#Majority of the points are in upper right and lower 
#left indicating positive correlation
#==========================================================
#Compute covariance
#===========================================================
cov(PimaIndiansDiabetes2$mass, PimaIndiansDiabetes2$pressure, 
    use="pairwise.complete.obs")
cov(PimaIndiansDiabetes2$mass, PimaIndiansDiabetes2$triceps,
    use="pairwise.complete.obs")
#==========================================================
# Compute the Pearson coefficient
# of linear correlation
#===========================================================
cor(PimaIndiansDiabetes2$mass, PimaIndiansDiabetes2$pressure, 
    use="pairwise.complete.obs")

cor(PimaIndiansDiabetes2$mass, PimaIndiansDiabetes2$triceps, 
    use="pairwise.complete.obs")

# We can observe that mass and pressure have a
# weaker positive linear relationship than mass and triceps

#Compute all pairwise linear corelations for the numeric data
cor(PimaIndiansDiabetes2[, -9], use="pairwise.complete.obs")
# Pairwise correlations can be visualized in a 
# friendlier representation by using the corrplot() function
library(corrplot)
corrplot(cor(PimaIndiansDiabetes2[,-9],
             use="pairwise.complete.obs"))
#Plot pairwise scatter plots for the first 3 variables
plot(PimaIndiansDiabetes2[,c(1,2,3)])
#Plot pairwise scatter plots for the all numerical variables
pairs(PimaIndiansDiabetes2[,-9])

#We now illustrate correlations using some 
#scatter plots and some toy data
#Different types of correlations
#================================
#CASE I: no association
#================================
set.seed(1234)
par(mfrow=c(2,2))
X <- rnorm(20, 0, .5) ; Y <- rnorm(20, 0, .02)
plot(X, Y, axes = FALSE, xlim = c(-1, 1), ylim = c(-1, 1),
       xlab = '', ylab = '',main="no association") ; abline(h = 0) ; abline(v = 0)
cor(X,Y)
#=============================================
#CASE II positive association (top right):
#============================================= 
set.seed(1234)
 X <- seq( -1, 1, length = 20) ; Y <- X + rnorm(X, 0, .25)
 plot(X, Y, axes = FALSE, xlim = c(-1, 1), ylim = c(- 2, 2),
         xlab = '', ylab = '', main="positive linear association") ; abline(h = 0) ; abline(v = 0)
cor(X,Y) 
#==================================
# CASE III: #negative association
#==================================
Y <- -2*X + rnorm(X, 0, .25)
plot(X, Y, axes = FALSE, xlim = c(-1, 1), ylim = c(-2, 2),
          xlab = '', ylab = '',main='negative linear association') 
abline(h = 0) ; abline(v = 0)
cor(X,Y)
#=====================================================
#CASE IV  quadratic association
#====================================================


Y <- -.5 + 2*X * X + rnorm(X, 0, .25)
plot(X, Y, axes = FALSE, xlim = c(-1, 1), ylim = c(-2, 2),
       xlab = '', ylab = '',main="quadratic association") ; abline(h = 0) ; abline(v = 0)
cor(X,Y)

#================================
#SIMPLE LINEAR REGRESION
#=============================== 
#Data for age in years and maximum heart rate for 15 people

x = c(18,23,25,35,65,54,34,56,72,19,23,42,18,39,37)
y = c(202,186,187,180,156,169,174,172,153,199,193,174,198,183,178)
# make a scatter plot 
plot(x,y)
lm.result=lm(y~x) 
#plot the regression line
abline(lm.result)
#Check computed values for the bivariate regression
summary(lm.result)
#Extract only the coefficients
coef(lm.result)
#OR
lm.result[['coefficients']]
#Individual coefficients
#intercept
coef(lm.result)[1]
#coef of x
coef(lm.result)['x']
#Extract only the residuals
lm.Residuals<- resid(lm.result)
#OR
lm.result[['residuals']]
#get fitted valuescoef(lm.result)[1]
lm.fitted<-fitted(lm.result)
#get the standard errors
lm.SE<-coefficients(summary(lm.result))
#extract standard error for x
coefficients(summary(lm.result))['x','Std. Error']
#OR
coefficients(summary(lm.result))[2,2]
#Get predicted values for a 50,60 year old
predict(lm.result,data.frame(x= c(50,60)))
#confidence bound
predict(lm.result,data.frame(x=sort(x)), level=.9, interval="confidence")
#Scatter plot with fitted and confidence bound
plot(x,y,col="red")
#plot fitted line
abline(lm.result)
#extract upper and lower bounds
ci.lwr <- predict(lm.result,data.frame(x=sort(x)),
                 level=.9,interval="confidence")[,2]
ci.upper <-predict(lm.result,data.frame(x=sort(x)),
                   level=.9,interval="confidence")[,3]
#plot confidence bands on the same plot
points(sort(x), ci.lwr,type="l") # or use lines()
points(sort(x), ci.upper,type="l")
#lower bound can also be plotted using
#curve(predict(lm.result,data.frame(x=x),interval="confidence")[,3],add=T)


#Multiple Linear Regression with known answer
x = 1:10
y = sample(1:100,10)
#construct the linear equation without noise
z = x+y
#Regress z on x and y
lm(z ~ x+y)
#Add some noise to the data
z = x+y + rnorm(10,0,2) # now sigma = 2
lm(z ~ x+y)

# add more noise, now sigma = 10
z = x+y + rnorm(10,0,10) # now sigma = 2
lm(z ~ x+y)

#Explicitly make the intercept zero
lm(z ~ x+y -1)
# Get coefficients, residuals and other stats
summary(lm(z ~ x+y))
#Get residuals
Mult.resid<-summary(lm(z ~ x+y))
#OR
Mult.resid[['residuals']]
Mult.resid$residuals
#Get coefficients with std errors
Mult.resid[['coefficients']]
#Regress triceps on mass
model <- lm(triceps~mass, data=PimaIndiansDiabetes2)
model$coefficients
# Intercept)        mass 
# -3.3734852   0.9894821 
#Get more info using summary
summary(model)

# We can plot the regression with the following code 
ggplot(PimaIndiansDiabetes2, aes(x=mass, y=triceps)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE)

#predict the value for triceps when mass is 30,
predict(model, data.frame(mass=30), interval="confidence")
#install.packages("psych")

library(psych)
 
#Produce Scatter plots in pairs with correlations 
#and histograms
pairs.panels(PimaIndiansDiabetes2[, 1:8],
             method="pearson", hist.col="seashell",
             density=TRUE, lm=TRUE)
#====================================================
#MULTIPLE LINEAR REGRESSION
#====================================================

#Regress triceps on mass,glucose and pressure
model <- lm(triceps~mass + glucose + pressure, data=PimaIndiansDiabetes2)
summary(model)
#Use mass as the dependent and all others as predictors
model <- lm(mass ~., data=PimaIndiansDiabetes2)
summary(model)

#delete the nonsignificant variables in the model 
model <- lm(mass ~. -glucose - pedigree - age,
            data=PimaIndiansDiabetes2)
summary(model)
#Observe change in r-squared value

#======================================
#LINEAR REGRESSION 2
#======================================
library(foreign)

#bm <- read.xport('BMX_C.XPT')
# save(bm, file = 'bm.rda')

load('bm.rda')
#locate the variables of interest
which(names(bm)%in%c('BMXWT', 'BMXHT', 'BMXARML'))
#paired scatter plot for weight, standing height and upper arm length
pairs(bm[, c(9,15,22)], labels=c("weight","height","upper arm length"))
#height and upper arm length seem to be linearly related.
#The other relationships seem to be polynomial

#Estimating regression coefficients
bm2<-bm[,c(9,15,22)]
#Extract only the complete cases
bm2<-bm2[complete.cases(bm2),]
pairs(bm2, labels=c("weight","height","upper arm length"))
#extract height(X1) and upper arm length (Y1)
Y1 <- bm2[,3] ; X1 <- bm2[,2]
plot(X1,Y1)
#Use a small sample of the data to improve the visualisation

set.seed(12345)
Data.Sample<-sample(1:nrow(bm2),10)
X<-X1[Data.Sample]; Y<-Y1[Data.Sample]
pairs(bm2[Data.Sample, ], labels=c("weight","height","upper arm length"))
plot(X,Y)
log.X <- log(X) ; log.Y <-log(Y)
#plot the transformed data
plot(log.X,log.Y)
#Generate a linear model
model <- lm(log.Y ~ log.X)
summary(model)
round(coefficients(model), 3)
#access predicted values
model$fitted.values
#predict log upper arm length for different values of log Height
predict(model, data.frame(log.X=c(4.2,4.7,5.1)), interval="confidence")

#plot with the regression line
plot(log.X, log.Y, xlim = c(min(log.X, na.rm=TRUE), max(log.X,na.rm=TRUE)),
       xlab='log(height)', ylab='log(upper arm length)')
abline(model,col="red", lwd=3)
#connect each expected value with a vertical line to 
#its corresponding log.Y value:
for(i in 1:length(log.X))
lines(c(log.X[i], log.X[i]),
          c(log.Y[i], model$fitted.values[i]))
# #Plot points and their fitted values
points(log.X, model$fitted.values, pch = 19,col="grey")
#add text for fitted and actual values
text(log.X, model$fitted.values,
         labels = round(model$fitted.values, 3), pos = 4)
text(log.X, log.Y, labels=round(log.Y, 1),
    pos = 4)
#============================================
#Linear regression PART 2
#============================================

# generate variables x and y
set.seed(42)
x <- rnorm(100)
e <- rnorm(100, mean = 0, sd = 7)
y <- 5 + 15 * x + e
plot(x,y)
#Fit the regression model
lm(y~x)
#output is coefficients only
# Coefficients:
#   (Intercept)            x  
# 4.381       15.190  
#If the variables are in a data frame
df <- data.frame(x, y)
head(df)
#We can use the dataframe option
Simple.reg<-lm(y~x, data=df)
#generate some results of the model
summary(Simple.reg)
#predict y when x=90 and when x=2
predict(Simple.reg, data.frame(x=c(2)))

#==========================================
#MULTIPLE REGRESSION
#==========================================

#GENERATE values for the variables
set.seed(42)
u <- rnorm(100)
v <- rnorm(100, mean = 3, sd = 2)
w <- rnorm(100, mean = -3, sd = 1)
e <- rnorm(100, mean = 0, sd = 5)
#generate values for y
y <- 5 + 4 * u + 3 * v + 2 * w + e
#Fit the regression of y on u,v,w
lm(y ~ u + v + w)
#Results are the coefficients
#Put data in a data frame and use the data parameter in the call
df <- data.frame(y, u, v, w)
head(df)
lm(y~u+v+w, data=df)
#alternatively we can use
lm(y~., data=df)
#====================================
#Getting the regression statistics
#===================================
#Save the regression model in a variable, say m :
m <- lm(y ~ u + v + w)

#==================================================
summary(m)

#lm(formula = y ~ u + v + w)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.3832 -1.7601 -0.3115  1.8565  6.9840 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   4.7703     0.9691   4.922 3.55e-06 ***
#   u             4.1733     0.2597  16.069  < 2e-16 ***
#   v             3.0132     0.1484  20.311  < 2e-16 ***
#   w             1.9052     0.2665   7.150 1.71e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.66 on 96 degrees of freedom
# Multiple R-squared:  0.8851,	Adjusted R-squared:  0.8815 
# F-statistic: 246.6 on 3 and 96 DF,  p-value: < 2.2e-16

#Model coefficients (point estimates)
coef(m)
#Confidence intervals for model coefficients
confint(m)
#Model residuals
resid.reg<-resid(m)
#We expect residuals to have a normal distribution
hist(resid.reg, freq = FALSE)

#Residual sum of squares
deviance(m)
#ANOVA table
anova(m)
#Variable selection
#Generate some data
set.seed(4)
n <- 150
x1 <- rnorm(n)
x2 <- rnorm(n, 1, 2)
x3 <- rnorm(n, 3, 1)
x4 <- rnorm(n,-2, 2)
e <- rnorm(n, 0, 3)
#generate a response variable with only x1 and x3
y <- 4 + x1 + 5 * x3 + e
#fit model
full.model <- lm(y ~ x1 + x2 + x3 + x4)
summary(full.model)

#eliminate insignificant variables
reduced.model <- step(full.model, direction="backward")
#only x1 and x3 remain in the model
summary(reduced.model)

#Plotting Regression Residuals
#install.packages("broom")
library(broom)
library(ggplot2)
augmented_m <- augment(m)
ggplot(augmented_m, aes(x = .fitted, y = .resid)) +
  geom_point()
#Can also do the same with the plot function
plot(m, which = 1)

#Diagnosing a regression model
m <- lm(y ~ x1 + x3)
par(mfrow = (c(2, 2))) # this gives us a 2x2 plot
plot(m)

#Outlier test
library(car)
outlierTest(m)
#Identify influential observations
influence.measures(m)
#Observation 81 is starred
