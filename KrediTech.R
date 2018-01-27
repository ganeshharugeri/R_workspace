#get/check working directory
getwd()
#set working directory
setwd("/Users/ganeshharugeri/Documents/R_workspace/input/kreditechDSTest2017/")

#Import Data Set 
#Read file from working directory
filename1 <- paste(c(getwd(),"/Validation.csv"),collapse="")
filename2 <- paste(c(getwd(),"/Training.csv"),collapse="")
# read file now
Validation <- read.csv(filename1,sep = ";",header = 1, stringsAsFactors = 0 )
Training <- read.csv(filename2,sep = ";",header = 1, stringsAsFactors = 0 )


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

## Setting Classlabe column for test data to NA
#Validation$classlabel <- NA
## Combining Training and Testing dataset
complete_data <- rbind(Training, Validation)
## Check data structure
str(complete_data)



#Data filling using tidyr package
str(complete_data)
summary(complete_data)

library(tidyr)
# categorical variable treatment 
complete_data_formatted <-  complete_data %>% fill(v33,v12,v7,v32,v99,v85)

#Numerical variable, mean treatment v55, v42
complete_data_formatted$v55[is.na(complete_data_formatted$v55)] <- mean(complete_data_formatted$v55,na.rm=T)
complete_data_formatted$v42[is.na(complete_data_formatted$v42)] <- mean(complete_data_formatted$v42,na.rm=T)

#speical attention v95, more NAs
# will replace NA with fill for just the base model. 
#Final hint - better to remove if it is not much of importance
complete_data_formatted <-  complete_data_formatted %>% fill(v95)
# remaining NAs with an assumption of false(f) values
complete_data_formatted$v95[is.na(complete_data_formatted$v95)] <- 'f'


## Check number of uniques values for each of the column to find out columns
##which we can convert to factors
sapply(complete_data_formatted, function(x) length(unique(x)))

#convert column v9 to factor type (from int)
complete_data_formatted$v9=as.factor(complete_data_formatted$v9)

#check format results
summary(complete_data_formatted)
str(complete_data_formatted)

#____________________________
NA_Percentage_Any_Dataset <- function(Dataset,total_rows){
  mylist =list()
  for(i in names(Dataset)){
    #print(i)
    total_NAs = sum(is.na(Dataset[[i]]))
    # print(total_NAs)
    percentage = (100* total_NAs)/total_rows
    #print(percentage)
    mylist[[i]]=percentage
    # cat('Column',i,'has',percentage,'% of NA Vaues\n')
    #cat('--------------------------------------------\n')
    
  }
  summary(mylist)
  # columnwise percentages of NAs 
  mylist
  #mylist[order(mylist[1]),]
  #  sort.list(mylist)
}

#____________________________

#Number formating german deciamals to US
asNumericFunc <- function(dataset,col_name){
  dataset[[col_name]] <- as.numeric(sub(",", ".", dataset[[col_name]], fixed = TRUE))
  return (dataset)
  }

complete_data_formatted<-asNumericFunc(complete_data_formatted,"v12")
complete_data_formatted<-asNumericFunc(complete_data_formatted,"v50")
complete_data_formatted<-asNumericFunc(complete_data_formatted,"v20")
complete_data_formatted<-asNumericFunc(complete_data_formatted,"v97")

# Model creation 
install.packages("randomForest")
library(randomForest)
complete_data_formatted1<-complete_data_formatted
complete_data_formatted1$v95<-NULL
complete_data_formatted1$v9<-NULL
library(dplyr)
complete_data_formatted1$v42<- (complete_data_formatted1$v42-min(complete_data_formatted1$v42))/(max(complete_data_formatted1$v42)-min(complete_data_formatted1$v42))
complete_data_formatted1$v53<- (complete_data_formatted1$v53-min(complete_data_formatted1$v53))/(max(complete_data_formatted1$v53)-min(complete_data_formatted1$v53))


?randomForest

rfmodel <- randomForest(classlabel ~.,data=complete_data_formatted1[1:3700,], ntrees=250)
pred<- predict(rfmodel,complete_data_formatted1[3701:3900, 1:19])

svmmodel<-svm(classlabel ~.,data=complete_data_formatted1[1:3700,])
pred<- predict(svmmodel,complete_data_formatted1[3701:3900, 1:19])

View(complete_data_formatted1[3701:3900, 1:20])

complete_data_vaild<-complete_data_formatted[3701:3900,1:22]
complete_data_vaild$predicted<-pred
##
library(caret)
library(e1071)
confusionMatrix(pred,complete_data_vaild$classlabel)
#Trial
pred
confusionMatrix(pred,complete_data_formatted1[3701:3900,20])


View(complete_data_vaild[,22:23])

