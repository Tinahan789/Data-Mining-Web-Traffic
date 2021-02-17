################################################################################
################################################################################

# PROJECT 2 Code
# Tina Giagnoni

################################################################################
################################################################################

setwd("D:/Data Mining")
# read in the training data
train=read.csv("training.csv")
is.data.frame(train)
ncol(train)
nrow(train)

################################################################################
################################################################################

# PREPROCESSING

################################################################################
################################################################################


library(dplyr)

# check for NAs
na_count=sapply(train, function(y) sum(is.na(y)))
na_count=data.frame(na_count)
na_count
#remove observations with NA
train1=na.omit(train)

#output a vector of the number of different levels or categories in each feature
categories=function(D=data, c=column) {
  cat=length(unique(D[,c]))
  print(cat)
}
D=train1
n=length(train1)
p <- 1
for (i in 1:n) {
  p[i] <- categories(D,i)
}
#find which columns have only one entry type
which(p==1)
#remove columns where the entire column is one level
train1=train1[,which(p>1)]

# What are the features of train1?
colnames(train1)
# summarize the features
summary(train1)
#find binary factors
c1=train1[sample(nrow(train1), 5000),]
apply(train1,2,function(x) { all(x %in% 0:1) })
#change some features to factor
train1$Label = as.factor(train1$Label)
train1$Protocol = as.factor(train1$Protocol)
train1$Fwd.PSH.Flags = as.factor(train1$Fwd.PSH.Flags)
train1$Bwd.PSH.Flags = as.factor(train1$Bwd.PSH.Flags)
train1$FIN.Flag.Cnt = as.factor(train1$FIN.Flag.Cnt)
train1$SYN.Flag.Cnt = as.factor(train1$SYN.Flag.Cnt)
train1$RST.Flag.Cnt = as.factor(train1$RST.Flag.Cnt)
train1$PSH.Flag.Cnt = as.factor(train1$PSH.Flag.Cnt)
train1$ACK.Flag.Cnt = as.factor(train1$ACK.Flag.Cnt)
train1$URG.Flag.Cnt = as.factor(train1$URG.Flag.Cnt)
train1$CWE.Flag.Count = as.factor(train1$CWE.Flag.Count)
train1$ECE.Flag.Cnt = as.factor(train1$ECE.Flag.Cnt)
#Labeling of AM/PM was inconsistient, so we discard timestamp
train1=train1 %>% select(-Timestamp)
# make the label a 1 (ddos) 0 (benign) binary
levels(train1$Label) <- c(0,1)
str(train1)
summary(train1)

################################################################################
################################################################################

# Feature selection and dimensionality reduction

################################################################################
################################################################################

# randomly select 300,000 observations for training, and the remaining for testing
set.seed(123)
s <- sample(seq_len(nrow(train1)), size = 300000, replace = FALSE)
tr<-train1[s,]
te <- train1[-s,]

# Begin PCA
#packages for scree plot
library(FactoMineR)
library(factoextra)
#select numeric variables
t=tr %>% select_if(is.numeric)
#convert integers to numeric
#t=sapply(t, as.numeric)
#t=as.data.frame(sapply(t, as.numeric))
# Flow.Pkts.s and Flow.Byts.s has infinite values, so we will need to remove it from the PCA
t=t %>% select(-Flow.Pkts.s, -Flow.Byts.s)
ncol(t)
# Check for normality gives a singular matrix
#library(MVN)
#s1 <- sample(seq_len(nrow(t)), size = 1000, replace = FALSE)
#t1<-t[s1,]
#mvn(t1, multivariatePlot = "qq")

#probably not normally distributed, so using PCA is not justified, but let's do it anyway!!!
#principal component analysis
pr=prcomp(t, scale. = TRUE)
summary(pr)
#scree plot
fviz_screeplot(pr,addlabels=TRUE,ncp = 20)

#select variables that are factors
f=tr %>% select_if(is.factor)
f$Flow.Pkts.s=tr$Flow.Pkts.s
f$Flow.Byts.s=tr$Flow.Byts.s
str(f)
#use tree to select factor features
library(rpart)
library(rpart.plot)
tree=rpart(Label~.,data=f, method = "class")
rpart.plot(tree)
pred=predict(tree, te, type = "class")
library(caret)
confusionMatrix(pred, te$Label)
#select the features ACK flag count, CWE flag count, Flow.Pkts.s, Flow.Byts.s


################################################################################
################################################################################

# Model Building

################################################################################
################################################################################

#Create training and testing sets from training.csv
set.seed(123)
#training set size set at 100000 samples, the test set will be all remaining samples
s <- sample(seq_len(nrow(train1)), size = 100000, replace = FALSE)
tr<-train1[s,]
te <- train1[-s,]

# get principal components
t=tr %>% select_if(is.numeric)
t=t %>% select(-Flow.Pkts.s, -Flow.Byts.s)
pr=prcomp(t, scale. = TRUE)

#project test data onto same space from PCA as training data
tes=te %>% select_if(is.numeric)
tes=tes %>% select(-Flow.Pkts.s, -Flow.Byts.s)
project.b = predict(pr, tes)

#package for ROC curves
library(pROC)

################################################################################
#Random Forest
################################################################################

library(randomForest)

# 18 principal components
# training set
train.for = data.frame(pr$x[,1:18], 
                      ACK.Flag.Cnt=tr$ACK.Flag.Cnt, 
                      CWE.Flag.Count=tr$CWE.Flag.Count,  
                      Label=tr$Label)
# train the model
forest=randomForest(Label~., train.for)
# confusion matrix for the random forest
forest$confusion
# error plot for the forest with number of trees used in the ensemble
plot(forest)
# test set
test.for = data.frame(project.b[,1:18], 
                      ACK.Flag.Cnt=te$ACK.Flag.Cnt, 
                      CWE.Flag.Count=te$CWE.Flag.Count,  
                      Label=te$Label)
#if we want to use a test set the same size as training
s <- sample(seq_len(nrow(test.for)), size = 100000, replace = FALSE)
test.for=test.for[s,]
# use test data to make label predictions
pred.for=predict(forest, test.for)
#confusion matrix with statistics
confusionMatrix(pred.for, test.for$Label)
# F1
F_meas(pred.for, test.for$Label)
# ROC curve
roc.for <- roc(response = as.numeric(test.for$Label), 
               predictor = as.vector(as.numeric(pred.for)))
plot(roc.for, col="red", lwd=3, main="ROC curve for random forest", legacy.axes = TRUE)
#Area under the curve
auc(roc.for)

# Repeat with 8 principal components
# training set
train.for = data.frame(pr$x[,1:8], 
                       ACK.Flag.Cnt=tr$ACK.Flag.Cnt, 
                       CWE.Flag.Count=tr$CWE.Flag.Count,  
                       Label=tr$Label)
forest=randomForest(Label~., train.for)
forest$confusion
plot(forest)
#test set
test.for = data.frame(project.b[,1:8], 
                      ACK.Flag.Cnt=te$ACK.Flag.Cnt, 
                      CWE.Flag.Count=te$CWE.Flag.Count,  
                      Label=te$Label)
pred.for=predict(forest, test.for)
confusionMatrix(pred.for, test.for$Label)
F_meas(pred.for, test.for$Label)

################################################################################
#Logistic Regression
################################################################################

library(nnet)

# 18 principal components
#training set
train.log=data.frame(pr$x[,1:18], 
                    ACK.Flag.Cnt=as.numeric(tr$ACK.Flag.Cnt), 
                    CWE.Flag.Count=as.numeric(tr$CWE.Flag.Count), 
                    Label=tr$Label)
# build logistic regression model
logit=multinom(Label~., family = binomial,data=train.log)
#test set
test.log=data.frame(project.b[,1:18], 
                    ACK.Flag.Cnt=as.numeric(te$ACK.Flag.Cnt), 
                    CWE.Flag.Count=as.numeric(te$CWE.Flag.Count), 
                    Label=te$Label)
#if we want to use a test set the same size as training
s <- sample(seq_len(nrow(test.log)), size = 100000, replace = FALSE)
test.log=test.log[s,]
# make predictions
pred.log=predict(logit, test.log)
#confusion matrix with statistics
confusionMatrix(as.factor(pred.log), as.factor(test.log$Label))
# F1
F_meas(pred.log, test.log$Label)
# ROC curve
roc.log <- roc(response = as.numeric(test.log$Label), 
               predictor = as.vector(as.numeric(pred.log)))
plot(roc.log, col="red", lwd=3, main="ROC curve for logistic regression",  legacy.axes = TRUE)
# Area under the curve
auc(roc.log)

# 8 principal components
#training set
train.log=data.frame(pr$x[,1:8], 
                     ACK.Flag.Cnt=as.numeric(tr$ACK.Flag.Cnt), 
                     CWE.Flag.Count=as.numeric(tr$CWE.Flag.Count), 
                     Label=tr$Label)
logit=multinom(Label~., family = binomial,data=train.log)
#test set
test.log=data.frame(project.b[,1:8], 
                    ACK.Flag.Cnt=as.numeric(te$ACK.Flag.Cnt), 
                    CWE.Flag.Count=as.numeric(te$CWE.Flag.Count), 
                    Label=te$Label)
pred.log=predict(logit, test.log)
confusionMatrix(as.factor(pred.log), as.factor(test.log$Label))
F_meas(pred.log, test.log$Label)

################################################################################
#SVM
################################################################################

library(e1071)

# 18 principal components
#training set
train.svm=data.frame(pr$x[,1:18], 
                     ACK.Flag.Cnt=as.numeric(tr$ACK.Flag.Cnt), 
                     CWE.Flag.Count=as.numeric(tr$CWE.Flag.Count), 
                     Label=tr$Label)
#create svm model
svmmodel=svm(Label~., train.svm)
#test set
test.svm=data.frame(project.b[,1:18], 
                    ACK.Flag.Cnt=as.numeric(te$ACK.Flag.Cnt), 
                    CWE.Flag.Count=as.numeric(te$CWE.Flag.Count), 
                    Label=te$Label)
#if we want to use a test set the same size as training
s <- sample(seq_len(nrow(test.svm)), size = 100000, replace = FALSE)
test.svm=test.svm[s,]
# use model to classify test data
pred.svm=predict(svmmodel, test.svm)
# confusion matrix with statistics
confusionMatrix(as.factor(pred.svm), as.factor(test.svm$Label))
# F1
F_meas(pred.svm, test.svm$Label)
#ROC curve
roc.svm <- roc(response = as.numeric(test.svm$Label), 
               predictor = as.vector(as.numeric(pred.svm)))
plot(roc.svm, col="red", lwd=3, main="ROC curve for SVM",  legacy.axes = TRUE)
# area under the curve
auc(roc.svm)

# 8 principal components
#training set
train.svm=data.frame(pr$x[,1:8], 
                     ACK.Flag.Cnt=as.numeric(tr$ACK.Flag.Cnt), 
                     CWE.Flag.Count=as.numeric(tr$CWE.Flag.Count), 
                     Label=tr$Label)
svmmodel=svm(Label~., train.svm)
#test set
test.svm=data.frame(project.b[,1:8], 
                    ACK.Flag.Cnt=as.numeric(te$ACK.Flag.Cnt), 
                    CWE.Flag.Count=as.numeric(te$CWE.Flag.Count), 
                    Label=te$Label)
pred.svm=predict(svmmodel, test.svm)
confusionMatrix(as.factor(pred.svm), as.factor(test.svm$Label))
F_meas(pred.svm, test.svm$Label)

################################################################################

#plot all three ROC curves at the same time
plot(roc.log, col="blue", lwd=3, main="ROC curve comparison",  legacy.axes = TRUE)
lines(roc.for,col="green",  legacy.axes = TRUE)
lines(roc.svm,col="orange",  legacy.axes = TRUE)
legend(0.6, 0.3, legend=c("Random Forest", "Logistic Regression", "SVM"),
       col=c("green", "blue", "orange"), lty=1, cex=1)

################################################################################
################################################################################

# Model Testing

################################################################################
################################################################################


#read test data, and clean it up
testing=read.csv("testing.csv")
# check for NAs
na_count=sapply(test, function(y) sum(is.na(y)))
na_count=data.frame(na_count)
na_count

#project numerical data using principal components
testing=testing %>% select_if(is.numeric)
testing=testing %>% select(-Flow.Pkts.s, -Flow.Byts.s)
project.b = predict(pr, testing)
#select appropriate columns
testing = data.frame(project.b[,1:18], 
                     ACK.Flag.Cnt=as.factor(testing$ACK.Flag.Cnt), 
                     CWE.Flag.Count=as.factor(testing$CWE.Flag.Count))
#classify testing data based on random forest
p=predict(forest,testing)
p=as.vector(p)
write.csv(p, file = "p.giagnoni.csv", row.names = FALSE)