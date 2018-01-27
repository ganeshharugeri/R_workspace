#get/check working directory
getwd()
#set working directory
setwd("/Users/ganeshharugeri/Documents/R_workspace/input/")
#remove working directory path
#unlink("/Users/ganeshharugeri/Documents/R_workspace/input")
#Read file from working directory
filename <- paste(c(getwd(),"/Chronic_kidney_disease_formatted.csv"),collapse="")
#Checked how paste works, it concanates the strings from the input
#?paste
# read file now
CKD_Dataset <- read.csv(filename,sep = ";",header = 1 )
summary(CKD_Dataset)
str(CKD_Dataset)
ls()
ShowAllRowsWithMissingWBValues <- CKD_Dataset[is.na(CKD_Dataset$White_Blood_Cell_Count),]
rm(result)
str(CKD_Dataset)
head(CKD_Dataset)
summary(CKD_Dataset)
#Check for the percentage of NA values in each columns of the dataset
NA_Percentage_CKD_Dataset <- function(total_rows,totalNAs){
  percentage = (100* totalNAs)/total_rows
  return(percentage)
}
#rm(NA_Percentage_CKD_Dataset())
# Recursive function to chekc for percentage of NA values in any dataset 
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
# Getting column names 
#names(CKD_Dataset)
#Column "White_Blood_Cell_Count" is removed as it had more than 25% of NA(missing) values
CKD_Dataset$White_Blood_Cell_Count <-  NULL

# Column "Specific_Gravity" isn't varying much, hence it will affect the end result 
#CKD_Dataset$Specific_Gravity = 

# Good package to clean data 
install.packages("tidyr")
library(tidyr)
# As following columns have very few NAs we can fill them up with relative values to that the other rows(NA<10%)
NewDataSet <- CKD_Dataset %>% fill(Kidney_Disease,Anemia,Pedal_Edema,Coronary_Artery_Disease,Diabetes_Mellitus,Hypertension,Serum_Creatinine,Blood_Urea,Bacteria,Pus_Cell_clumps,Blood_Pressure,Age)
# package to help finding correlation, covarience ang other dependencies
install.packages("ggpubr")
library("ggpubr")
# Draw a scatter plaot to find the correlation betwn Specific_Gravity and Kidney_Disease
ggscatter(NewDataSet, x = "Specific_Gravity", y = "Kidney_Disease", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Specific_Gravity", ylab = "Kidney_Disease")


#Handeling Sodium NA values 
NewDataSet$Sodium[is.na(NewDataSet$Sodium)] <- median(NewDataSet$Sodium,na.rm=T)

#Fill NA values with their corresponding mean of their column
for (i in c("Specific_Gravity","Hemoglobin","Packed_Cell_Volume","Appetite","Albumin","Sugar","Pus_Cell","Blood_Glucose","Potassium")){
  NewDataSet[[i]][is.na(NewDataSet[[i]]mo)] <- mean(NewDataSet[[i]],na.rm=T)
 # NewDataSet[,i]=as.factor(titanic_data[,i])
}

## Splitting training and test data
train_CKD <- NewDataSet[1:300,]
test_CKD <- NewDataSet[301:397,]

#Model
model_CKD <- glm(Kidney_Disease ~.,family=binomial(link='logit'),data=train_CKD)

