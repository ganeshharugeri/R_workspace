set.seed(212)

#get/check working directory
getwd()
#set working directory
setwd("/Users/ganeshharugeri/Documents/R_workspace/input/kreditechDSTest2017/")

#Import Data Set 
#Read file from working directory
filename1 <- paste(c(getwd(),"/Validation.csv"),collapse="")
filename2 <- paste(c(getwd(),"/Training.csv"),collapse="")
# read file now
Validation <- read.csv2(filename1,sep = ";",header = 1, stringsAsFactors = 1 )
Training <- read.csv2(filename2,sep = ";",header = 1, stringsAsFactors = 1 )


#Structure of both dataset
str(Training)
str(Validation) #classlabel is absent

#Check suumary
summary(Validation)
summary(Training)

#Check sample data 
head(Validation)
head(Training)
#Check STructure
str(Validation)
str(Training)

dim(Validation)
dim(Training)

# Binding training and test data
Backup <- rbind(Training, Validation)

All_Data <- rbind(Training, Validation)
str(All_Data)

#Check for the unique values in each column
sapply(All_Data, function(x) length(unique(x)))

# issing value entries
nrowCount =nrow(All_Data)
ncol(All_Data)
#Complete rows i.e, rows without missing values
completeRows = sum(complete.cases(All_Data))

#Check for the percentage of missing row entries
prop=sum(complete.cases(All_Data))/nrow(All_Data)

#Number of missing values 
table(is.na(All_Data))

#Check for missing by column
colSums(is.na(All_Data))
colSums(is.na(Training))
colSums(is.na(Validation))

#Analyse the variable dependancy 
library(ggplot2)
head(All_Data)

#Arranging the data by column names 
temp=NULL
temp=All_Data[ , order(names(All_Data[1:21]))]
temp$v7 =NULL
temp$v9 =NULL
str(temp)
head(temp)

#Ordering as required
temp <- cbind(v7=All_Data$v7,v9=All_Data$v9,temp,classlabel = All_Data$classlabel)

dim(temp)
#Assigning back to All_Data
All_Data =NULL
All_Data <- temp
temp <- NULL

head(All_Data)
str(All_Data)



#Plotting Univariate analysis
df = data.frame(All_Data$v7)

#quick plots 
qplot(All_Data$v7,All_Data$classlabel)
qplot(All_Data$v9,All_Data$classlabel)
qplot(All_Data$v42,All_Data$classlabel)

#Advanced plots
ggplot(All_Data) + geom_bar( aes(All_Data$classlabel) )

#REMOVING v95 as it has more than 55% data is NA
All_Data$v95= NULL
#Library dplyr and tidyr is required for pipe(%>%) and fill functions
library(dplyr)
library(tidyr)


All_Data <-  All_Data %>% fill(v7,v12,v32,v33,v99,v85)

All_Data$v9 <- factor(All_Data$v9)

table(All_Data$v24)

qplot(All_Data$v24)


# Standarisation function
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
#test column
TestCol= All_Data$v20
# USe function
All_Data$v20=range01(All_Data$v20)
All_Data$v24=range01(All_Data$v24)
#All_Data$v42 = Backup$v42
All_Data$v42[is.na(All_Data$v42)] <- median(All_Data$v42,na.rm=T)
All_Data$v55[is.na(All_Data$v55)] <- median(All_Data$v55,na.rm=T)

All_Data$v42=range01(All_Data$v42)
All_Data$v55=range01(All_Data$v55)


summary(All_Data)
sapply(All_Data, function(x) length(unique(x)))

#Rename label values
library(plyr)
All_Data$classlabel<- mapvalues(All_Data$classlabel, from = c("yes.", "no."), to = c("yes", "no"))

#See the output
# col = NULL
# chk=NULL
# df=NULL


#Building models
library(randomForest)

#Splitting data
train = All_Data[1:3700,]
test=NULL
test = All_Data[3701:3900,1:20]

#randomforest
rfmodel <- randomForest(classlabel~ v68+v99+v50+v20+v53,data=train,importance=TRUE, ntrees=69)
predicted<- predict(rfmodel,test)

#Confusion matrix for randomforest 
confusionMatrix(predicted,All_Data[3701:3900,]$classlabel)

#Variable Importance
?varImpPlot
varImpPlot(rfmodel)
varImpPlot(rfmodel,sort=T,n.var=10,main = "Variable importance")


#SVM model
svmmodel<-svm(classlabel ~.,data=train)
pred<- predict(svmmodel,test)

#Accuracy test [Evaluation]
library(caret)
library(e1071)



#Confusion matrix for SVM 
confusionMatrix(pred,All_Data[3701:3900,]$classlabel)

# cbind(predicted,All_Data[3701:3900,]$classlabel)
compare=NULL
compare <- cbind(Actual=All_Data[3701:3900,]$classlabel, rfPrediction = predicted, SVMPrediction = pred)

#scatterplotmatrix(car package)
scatterplotMatrix(~ v12+v20+v24+v42+v50+v53+v97,data=train)









#Tensorflow check
library(tensorflow)

sess = tf$Session()

hello <- tf$constant('Hello, TensorFlow!')
sess$run(hello)

a <- tf$constant(10)
b <- tf$constant(32)
sess$run(a + b)
  