df<-read.table("sugarcontent.txt")
View(df)
library(ggplot2)
#Plot sugar content along the x-axis
plot_df <- ggplot(data = df, aes(x = sugar_content, y = 0)) + 
  geom_point() + 
  geom_text(aes(label = sugar_content), size = 2.5, vjust = 2, hjust = 0.5)
#Display plot
plot_df
#The maximal margin separator is at the midpoint of the two extreme points in each cluster.
mm_separator <- (8.9 + 10)/2 

#create data frame containing the maximum margin separator
separator <- data.frame(sep = mm_separator)

#add ggplot layer 
plot_sep <- plot_df + geom_point(data = separator, aes(x = mm_separator, y = 0), color = "blue", size = 4)

#display plot
plot_sep
#++++++++++++++++++++++++++++++++++++++++++++
#We now consider a two dimensional case
#++++++++++++++++++++++++++++++++++++++++++++
#set seed
set.seed(42)
#set number of data points. 
n <- 600
#Generate data frame with two uniformly distributed predictors lying between 0 and 1.
df <- data.frame(x1 = runif(n,0,1), 
                 x2 =runif(n,0,1))
View((df))
#classify data points depending on location
# Use the line x2=1.4(x1) as the separator.
df$y <- factor(ifelse(df$x2 - 1.4*df$x1 < 0, -1, 1), 
               levels = c(-1, 1))

ggplot(df,aes(x=x1,y=x2, color=y))+
  geom_point()
#View(df)
#set margin
delta <- 0.07

# retain only those points that lie outside the margin
df1 <- df[abs(1.4*df$x1 - df$x2) > delta, ]

#build plot
plot_margins <- ggplot(data = df1, aes(x = x1, y = x2, color = y)) + geom_point() + 
  scale_color_manual(values = c("red", "blue")) + 
  geom_abline(slope = 1.4, intercept = 0)+
  geom_abline(slope = 1.4, intercept = 0.07, linetype = "dashed") +
  geom_abline(slope = 1.4, intercept = -0.07, linetype = "dashed")
plot_margins

#==================================
#Linear SVM
#==================================
#We split the data into training and testing sets. 80/20
# Set the upper bound for the length of the training set
sample_size <- floor(0.8 * nrow(df))
# Assign rows to training set randomly
train <- sample(seq_len(nrow(df1)), size = sample_size)
# Yield training and test sets
trainset <- df1[train, ]
testset <- df1[-train, ]

#load the e1071 package
library(e1071)

#build svm model, setting required parameters
svm_model<- svm(y ~ ., 
                data = trainset, 
                type = "C-classification", 
                kernel = "linear", 
                scale = FALSE)
#list components of model
names(svm_model)

#list values of the SV, index and rho
#extract support vectors
svm_model$SV
#Extract indices of the support vectors
svm_model$index
# negative y-intercept
svm_model$rho 

#compute training accuracy
pred_train <- predict(svm_model, trainset)
mean(pred_train == trainset$y)

#compute test accuracy
pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)

#load ggplot
library(ggplot2)
#==========================================
#VISUALISING THE SVM
#==========================================

#build scatter plot of training dataset
scatter_plot <- ggplot(data = trainset, aes(x = x1, y = x2, color = y)) + 
  geom_point() + 
  scale_color_manual(values = c("red", "blue"))

#add plot layer marking out the support vectors 
layered_plot <- 
  scatter_plot + geom_point(data = trainset[svm_model$index, ], aes(x = x1, y = x2), color = "purple", size = 4, alpha = 0.5)

#display plot
layered_plot
#===================================================
#ADD DECISION BOUNDARY TO THE PLOT
#===================================================
#calculate slope and intercept of decision boundary from weight vector and svm model
# Build weight vector
w <- t(svm_model$coefs)%*% svm_model$SV
w
slope_1 <--w[1]/ w[2]
slope_1
# Calculate intercept and save it to a variable
intercept_1 <- svm_model$rho / w[2]
intercept_1

#build scatter plot of training dataset
scatter_plot <- ggplot(data = trainset, aes(x = x1, y = x2, color = y)) + 
  geom_point() + scale_color_manual(values = c("red", "blue"))
#add decision boundary
plot_decision <- scatter_plot + geom_abline(slope = slope_1, intercept = intercept_1) 
#add margin boundaries


plot_margins <- plot_decision + 
  geom_abline(slope = slope_1, intercept = intercept_1 - 1/w[2], linetype = "dashed")+
  geom_abline(slope = slope_1, intercept = intercept_1 + 1/w[2], linetype = "dashed")
#display plot
plot_margins

#Inbuilt function for visualising boundaries
#load required library
library(e1071)

#build svm model
svm_model<- svm(y ~ ., data = trainset, type = "C-classification", 
      kernel = "linear", scale = FALSE)

#plot decision boundaries and support vectors for the training data
plot(x = svm_model, data = trainset)

#==============================================
#TUNING LINEAR SVMs
#==============================================
#build svm model, cost = 1
svm_model_1 <- svm(y ~ .,
                   data = trainset,
                   type = "C-classification",
                   cost = 1,
                   kernel = "linear",
                   scale = FALSE)

#print model details
svm_model_1
#build svm model, cost = 100
svm_model_100 <- svm(y ~ .,
                     data = trainset,
                     type = "C-classification",
                     cost = 100,
                     kernel = "linear",
                     scale = FALSE)

#print model details
svm_model_100
#add decision boundary and margins for cost = 1 
#to training data scatter plot
w_1 <- t(svm_model_1$coefs)%*% svm_model_1$SV
slope_1 <--w_1[1]/ w_1[2]
# Calculate intercept and save it to a variable
intercept_1 <- svm_model_1$rho / w_1[2]
train_plot_with_margins <- scatter_plot + 
  geom_abline(slope = slope_1, intercept = intercept_1) +
  geom_abline(slope = slope_1, intercept = intercept_1-1/w_1[2], linetype = "dashed")+
  geom_abline(slope = slope_1, intercept = intercept_1+1/w_1[2], linetype = "dashed")

#display plot
train_plot_with_margins

#add decision boundary and margins for cost = 100 to training data scatter plot
w_100 <- t(svm_model_100$coefs)%*% svm_model_100$SV
slope_100 <--w_100[1]/ w_100[2]
# Calculate intercept and save it to a variable
intercept_100 <- svm_model_100$rho / w_1[2]

train_plot_with_margins <-scatter_plot + 
  geom_abline(slope = slope_100, intercept = intercept_100, color = "goldenrod") +
  geom_abline(slope = slope_100, intercept = intercept_100-1/w_100[2], linetype = "dashed", color = "goldenrod")+
  geom_abline(slope = slope_100, intercept = intercept_100+1/w_100[2], linetype = "dashed", color = "goldenrod")

#display plot 
train_plot_with_margins


#Multiclass data 
#We use synthetic data that is known to be linearly separable

trainset<-read.table("trainset_multiclass.txt")
View(trainset)
testset<-read.table("testset_multiclass.txt",sep="")
#load library and build svm model
library(e1071)
svm_model<- 
  svm(y ~ ., data = trainset, type = "C-classification", 
      kernel = "linear", scale = FALSE)

#compute training accuracy
pred_train <- predict(svm_model, trainset)
mean(pred_train == trainset$y)

#compute test accuracy
pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)

#plot the SVM model
plot(svm_model, trainset)

#We use the iris data set to illustrate cross validation
# build linear SVMs for 100 distinct training/test 
# partitions of the iris dataset. You will then 
# evaluate the performance of your model by 
# calculating the mean accuracy and standard deviation. 
accuracy=c()
for (i in 1:100){
  #assign 80% of the data to the training set
  sample_size <- floor(0.8* nrow(iris))
  train <- sample(seq_len(nrow(iris)), size = sample_size)
  trainset <- iris[train, ]
  testset <- iris[-train, ]
  #build model using training data
  svm_model <- svm(Species~ ., data = trainset, 
                   type = "C-classification", kernel = "linear")
  #calculate accuracy on test data
  pred_test <- predict(svm_model, testset)
  accuracy[i] <- mean(pred_test == testset$Species)
}
head(accuracy,15)
mean(accuracy) 
sd(accuracy)
#========================================
#Polynomial kernel
#=========================================
#Generate a 2d radially separable data set
#set number of variables and seed
n <- 400
set.seed(1)

#Generate data frame with two uniformly distributed predictors, x1 and x2
df <- data.frame(x1 = runif(n, min = -1, max = 1), 
                 x2 = runif(n, min = -1, max = 1))

#We want a circular boundary. Set boundary radius 
radius <- 0.8
radius_squared <- radius^2

#create dependent categorical variable, y, with value -1 or 1 depending on whether point lies
#within or outside the circle.
df$y <- factor(ifelse(df$x1^2 + df$x2^2 < radius_squared, -1, 1), levels = c(-1, 1))
#load ggplot
library(e1071)
library(ggplot2)

#build scatter plot, distinguish class by color
scatter_plot <- ggplot(data = df, aes(x = x1, y = x2, color = y)) + 
  geom_point() +
  scale_color_manual(values = c("red", "blue"))

#display plot
scatter_plot

#=============================================
#LINEAR SVMS ON RADIALLY SEPARABLE DATA
#============================================
sample_size <- floor(0.8 * nrow(df))
set.seed(7)
# Assign rows to training set randomly
train <- sample(seq_len(nrow(df)), size = sample_size)
#Split data into training and testing set
trainset<-df[train,]
testset<-df[-train,]
#Use default cost of 1
#default cost mode;
svm_model_1 <- svm(y ~ ., data = trainset, 
                   type = "C-classification", cost = 1, kernel = "linear")
#training accuracy
pred_train <- predict(svm_model_1, trainset)
#mean(pred_train == trainset$y)
#test accuracy
pred_test <- predict(svm_model_1, testset)
mean(pred_test == testset$y)

#cost = 100 model
svm_model_2 <- svm(y ~ ., data = trainset, 
                   type = "C-classification", cost = 100, 
                   kernel = "linear")
#accuracy
pred_train <- predict(svm_model_2, trainset)
mean(pred_train == trainset$y)
pred_test <- predict(svm_model_2, testset)
mean(pred_test == testset$y)

#===================================================
#CROSS VALIDATION
#=================================================
# Print average accuracy and standard deviation
accuracy <- rep(NA, 100)
set.seed(2)

# Calculate accuracies for 100 training/test partitions
accuracy=c()
for (i in 1:100){
  sample_size <- floor(0.8 * nrow(df))
  train <- sample(seq_len(nrow(df)), size = sample_size)
  trainset <- df[train, ]
  testset <- df[-train, ]
  svm_model <- svm(y ~ ., data = trainset, type = "C-classification", kernel = "linear")
  pred_test <- predict(svm_model, testset)
  accuracy[i] <- mean(pred_test == testset$y)
}
# Print average accuracy and standard deviation
mean(accuracy)
sd(accuracy)

#==============================================
#USING KERNELS
#================================================
# WE REVISIT THE RADIALLY SEPARATED DATA
#================================================
df <- data.frame(x1 = runif(n, min = -1, max = 1), 
                 x2 = runif(n, min = -1, max = 1))

#We want a circular boundary. Set boundary radius 
radius <- 0.8
radius_squared <- radius^2

#create dependent categorical variable, y, with value -1 or 1 depending on whether point lies
#within or outside the circle.
df$y <- factor(ifelse(df$x1^2 + df$x2^2 < radius_squared, -1, 1), levels = c(-1, 1))

#transform data
df1 <- data.frame(x1sq = df$x1^2, x2sq = df$x2^2, y = df$y)

#plot data points in the transformed space
plot_transformed <- ggplot(data = df1, aes(x = x1sq, y = x2sq, color = y)) + 
  geom_point()+ guides(color = "none") + 
  scale_color_manual(values = c("red", "blue"))

#add decision boundary and visualize
plot_decision <- plot_transformed + geom_abline(slope = -1, intercept = 0.64)
plot_decision
# THE TRANSFORMED DATA IS NOW LINEARLY SEPARABLE.
#SVM with ploynomial kernel
svm_model<- 
  svm(y ~ ., data = trainset, type = "C-classification", 
      kernel = "polynomial", degree = 2)

#measure training and test accuracy
pred_train <- predict(svm_model, trainset)
mean(pred_train == trainset$y)
pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)

#plot
plot(svm_model, trainset)

#+++++++++++++++++++++++++++++++++
#Tuning SVM parameters
#++++++++++++++++++++++++++++++++++
#tune model
tune_out <- 
  tune.svm(x = trainset[, -3], y = trainset[, 3], 
           type = "C-classification", 
           kernel = "polynomial", degree = 2, cost = 10^(-1:2), 
           gamma = c(0.1,1,10), coef0 = c(0.1, 1, 10))

#list optimal values
tune_out$best.parameters$cost
tune_out$best.parameters$gamma
tune_out$best.parameters$coef0

#Build tuned model with optimal parameter values
svm_model <- svm(y~ ., data = trainset, type = "C-classification", 
                 kernel = "polynomial", degree = 2, 
                 cost = tune_out$best.parameters$cost, 
                 gamma = tune_out$best.parameters$gamma, 
                 coef0 = tune_out$best.parameters$coef0)

#Calculate training and test accuracies
pred_train <- predict(svm_model, trainset)
mean(pred_train == trainset$y)
pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)

#plot model
plot(svm_model, trainset)
#==========================================
#COMPLEX DECISION BOUNDARIES
#==========================================

#==========================================
#RBF Kernel (Radial basis function)
#==========================================
#number of data points
n <- 1000
#set seed
set.seed(1)

#create dataframe
df <- data.frame(x1 = rnorm(n, mean = -0.5, sd = 1), 
                 x2 = runif(n, min = -1, max = 1))
#set radius and centers
radius <- 0.8
center_1 <- c(-0.8, 0)
center_2 <- c(0.8, 0)
radius_squared <- radius^2

#create binary classification variable
df$y <- factor(ifelse((df$x1-center_1[1])^2 + (df$x2-center_1[2])^2 < radius_squared|
                        (df$x1-center_2[1])^2 + (df$x2-center_2[2])^2 < radius_squared, -1, 1),
               levels = c(-1, 1))
# Load ggplot2
library(ggplot2)

# Plot x2 vs. x1, colored by y
scatter_plot<- ggplot(data = df, aes(x = x1, y = x2, color = y)) + 
  # Add a point layer
  geom_point() + 
  scale_color_manual(values = c("red", "blue")) +
  # Specify equal coordinate scales
  coord_fixed()

scatter_plot 
library(e1071)
#Split data into training and testing sets
set.seed(22)
sample_size<-floor(0.8*nrow(df))
# Assign rows to training set randomly
train <- sample(seq_len(nrow(df)), size = sample_size)
trainset<-df[train,]
testset<-df[-train,]

#build linear model for comparison purposes
svm_model<- 
  svm(y ~ ., data = trainset, type = "C-classification", 
      kernel = "linear")

#accuracy
pred_train <- predict(svm_model, trainset)
mean(pred_train == trainset$y)
pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)

#plot model against testset
plot(svm_model, testset)

#build model
svm_model<- 
  svm(y ~ ., data = trainset, type = "C-classification", 
      kernel = "polynomial", degree = 2)

#accuracy
pred_train <- predict(svm_model, trainset)
mean(pred_train == trainset$y)
pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)

#Try polynomial kernel on the complex data set
#plot model

plot(svm_model, data=trainset)

# Cross validate using polynomial kernel
#create vector to store accuracies and set random number seed
accuracy <- rep(NA, 100)
set.seed(2)

#calculate accuracies for 100 training/test partitions
#using the polynomial kernel
for (i in 1:100){
  sample_size <- floor(0.8 * nrow(df))
  train <- sample(seq_len(nrow(df)), size = sample_size)
  trainset <- df[train, ]
  testset <- df[-train, ]
  svm_model<- svm(y ~ ., data = trainset, type = "C-classification", kernel = "polynomial", degree = 2)
  pred_test <- predict(svm_model, testset)
  accuracy[i] <- mean(pred_test == testset$y)
}

#print average accuracy and standard deviation
mean(accuracy)
sd(accuracy)
#create vector to store accuracies and set random number seed
accuracy <- rep(NA, 100)
set.seed(2)

#USINNG THE RADIAL KERNEL
#calculate accuracies for 100 training/test partitions
for (i in 1:100){
  sample_size <- floor(0.8 * nrow(df))
  train <- sample(seq_len(nrow(df)), size = sample_size)
  trainset <- df[train, ]
  testset <- df[-train, ]
  svm_model<- svm(y ~ ., data = trainset, type = "C-classification", kernel = "radial")
  pred_test <- predict(svm_model, testset)
  accuracy[i] <- mean(pred_test == testset$y)
}
#print average accuracy and standard deviation
mean(accuracy)
sd(accuracy)

#tune model
tune_out <- tune.svm(x = trainset[, -3], y = trainset[, 3], 
                     gamma = 5*10^(-2:2), 
                     cost = c(0.01, 0.1, 1, 10, 100), 
                     type = "C-classification", kernel = "radial")

#build tuned model withe RBF
svm_model <- svm(y~ ., data = trainset, type = "C-classification", kernel = "radial", 
                 cost = tune_out$best.parameters$cost, 
                 gamma = tune_out$best.parameters$gamma)

#calculate test accuracy
pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)   
plot(svm_model,trainset)
