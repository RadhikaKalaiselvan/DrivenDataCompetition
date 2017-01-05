#This file cleans the pump it up dataset and has the code for the following classifiers
# 1. Random Forest
# 2. Bagging
# 3. Boosting
# 4. Decision Trees
# 5. Logistic Regression
# 6. Naive Bayes

#Load Libraries

library("randomForest")
library("rpart")
library("ggplot2")
library("e1071")
library("class")
library("rpart")
library("ipred")
library("adabag")
library("ada")
library("gbm")
library("ggplot2")
library("randomForest")
library("caret")

#Set seed
set.seed(42)

#Load the given data sets
trainData<- read.csv("http://s3.amazonaws.com/drivendata/data/7/public/4910797b-ee55-40a7-8668-10efd5c1b960.csv")
trainLabels <- read.csv("http://s3.amazonaws.com/drivendata/data/7/public/0bf8bc6e-30d0-4c50-956a-603fc693d966.csv")
cat("No. of training dataset ",nrow(trainData))

#There 3 classes in the given dataset
# 1. functional
# 2. Non functional
# 3. Functional needs repair
# Convert the class to numeric values.
class=ifelse(trainLabels$status_group=="functional",1,
             ifelse(trainLabels$status_group=="non functional",2,3))

#Merge the numeric values of class and remove the status group column of the training dataset
trainLabels=data.frame(trainLabels,class)
trainLabels=trainLabels[-2]

#Merge the training labels with the training dataset

train<-merge(trainData,trainLabels)

#Load test dataset
testData<-read.csv("http://s3.amazonaws.com/drivendata/data/7/public/702ddfc5-68cd-4d1d-a0de-f5f566f76d91.csv")
testData$class<-""



#Merging given training dataset and training  data with status group as empty string, since
#we perform preprocessing on all the dataset
#entireDataSet is training dataset + test dataset with class as emoty string
entireDataSet<- rbind(train,testData)

#The location of tanzania should have latitude lower than 0 to -15 and 
# longitude should be greater than 30 to 40.
#summary(entireDataSet$latitude)
#hist(entireDataSet$latitude)
#length(entireDataSet$latitude[entireDataSet$latitude>=0])
#length(entireDataSet$latitude[entireDataSet$latitude>=-1])
#Latitude has valid data

#Lets check for longitude
#summary(entireDataSet$longitude)
#hist(entireDataSet$longitude)
cat("No of rows with zero longitude ",nrow(entireDataSet[entireDataSet$longitude==0,]))

# Therefore longitude has missing values.
# We can derive the missing longitude from region.
# we use aggregate function to compute the mean of longitude and set it to the missing values.
longitudeValueSummary <- aggregate(longitude~region,data=entireDataSet[(entireDataSet$longitude!=0),], FUN=mean)
cat("Finding the mean value of longitude based on region ...")
print(longitudeValueSummary)

#We add a column called correctedLongitude to update the mean longitude values

entireDataSet$correctedLongitude<-entireDataSet$longitude

#The following values are obtained from the longitudeValueSummary
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Arusha"]<-36.55407
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Dar es Salaam" ]<-39.21294
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Dodoma" ]<-36.04196
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Iringa" ]<-34.89592
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Kagera" ]<-31.23309
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Kigoma" ]<-30.21889
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Kilimanjaro" ]<-37.50546
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Lindi" ]<-38.98799
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Manyara" ]<-35.92932
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Mara" ]<-34.15698
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Mbeya" ]<-33.53351
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Morogoro" ]<-37.04678
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Mtwara" ]<-39.38862
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Mwanza" ]<-33.09477
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Pwani" ]<-38.88372
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Rukwa" ]<-31.29116
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Ruvuma" ]<-35.72784
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Shinyanga" ]<-33.24037
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Singida" ]<-373950
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Tabora" ]<-32.87830
entireDataSet$correctedLongitude[entireDataSet$longitude==0&entireDataSet$region=="Tanga"]<-38.50195

cat(" After populating mean values, no. of rows with 0 longitude values = ",
nrow(entireDataSet[entireDataSet$correctedLongitude==0,]))

cat("Summary of amount_tsh")
summary(entireDataSet$amount_tsh)

#table(entireDataSet$class[entireDataSet$amount_tsh>10000],
#      entireDataSet$amount_tsh[entireDataSet$amount_tsh>10000])
#hist(entireDataSet$amount_tsh[entireDataSet$amount_tsh>10000])

#Status group is not indicated by amount_tsh>10000, therefore we reduce the values to 10000
entireDataSet$amount_tsh[entireDataSet$amount_tsh>=10000]<-10000

#Process gps_height
cat(" No. of rows with gps_height as 0 ",nrow(entireDataSet[entireDataSet$gps_height<=0,]))
#hist(entireDataSet$gps_height)

#We add a flag attribute to set -1 if gps_height is 0
entireDataSet$heightFlag <- 0
entireDataSet$heightFlag[entireDataSet$gps_height==0]<-1

#We can use the decision trees to predict the values for gps_height
corrected_gps_height<-rpart(gps_height~latitude+correctedLongitude,
                data=entireDataSet[(entireDataSet$gps_height!=0),],method="anova")
entireDataSet$gps_height[entireDataSet$heightFlag==1]<-predict(corrected_gps_height,
                                        entireDataSet[(entireDataSet$heightFlag==1),])
head(entireDataSet$correctedLongitude)
#After predicting gps_height
#cat(" No. of rows with gps_height as 0 after populating values using decision trees",
#nrow(entireDataSet[entireDataSet$gps_height==0,]))

#We calculate the age of the pump as the difference of the maximum construction year and current year
maxyear=max(entireDataSet$construction_year)
entireDataSet$pumpage<-maxyear-entireDataSet$construction_year

# If the pump age was zero we populate the mean age
entireDataSet$ageFlag<-0
entireDataSet$ageFlag[entireDataSet$pumpage==maxyear]<-1
meanage<-mean(entireDataSet$pumpage[entireDataSet$pumpage!=maxyear])
entireDataSet$pumpage[entireDataSet$pumpage==maxyear]<- round(meanage,digits = 0)


#Population
#hist(entireDataSet$population)
cat(" No. of rows with zero population value ",nrow(entireDataSet[entireDataSet$population==0,]))

# we pass the mean value of all the population with value as 0
populationMean<-mean(entireDataSet$population[entireDataSet$population!=0])
entireDataSet$population[entireDataSet$population==0]<-round(populationMean,digits = 0)

#Removing all the unknown values in the entire dataset
#entireDataSet$permit<-as.character(entireDataSet$permit)
#entireDataSet$permit[entireDataSet$permit==""]<-"unknown"
#entireDataSet$public_meeting<- as.character(entireDataSet$public_meeting)
#entireDataSet$public_meeting[entireDataSet$public_meeting==""]<-"unknown"
#entireDataSet$scheme_management<-as.character(entireDataSet$scheme_management)
#entireDataSet$scheme_management[entireDataSet$scheme_management==""]<-"unknown"

# Reducing the factor levels of the extraction_type since the proportion is very minimal
prop.table(table(entireDataSet$waterpoint_type_group))
entireDataSet$waterpoint_type_group[entireDataSet$waterpoint_type_group=="improved spring"]<-"other"
entireDataSet$waterpoint_type_group[entireDataSet$waterpoint_type_group=="dam"]<-"other"
entireDataSet$waterpoint_type_group[entireDataSet$waterpoint_type_group=="cattle trough"]<-"other"

prop.table(table(entireDataSet$scheme_management))
entireDataSet$scheme_management[entireDataSet$scheme_management=="Trust"] <- "Other"
entireDataSet$scheme_management[entireDataSet$scheme_management=="SWC"] <-"Other"
entireDataSet$scheme_management[entireDataSet$scheme_management=="None"] <-"Other"
entireDataSet$scheme_management[entireDataSet$scheme_management=="unknown"]<-"Other"

prop.table((table(entireDataSet$extraction_type_class)))
entireDataSet$extraction_type_class[entireDataSet$extraction_type_class=="wind-powered"] <- "other"
entireDataSet$extraction_type_class[entireDataSet$extraction_type_class=="rope pump"] <- "other"

#Funder - reduce factor levels
NUM_LEVELS_FUNDER = 10 

#Funder will have this many + 1 levels
funderNames <- names(summary(entireDataSet$funder)[1:NUM_LEVELS_FUNDER])
funder <- factor(entireDataSet$funder, levels=c(funderNames, "Other"))
funder[is.na(funder)] <- "Other"
entireDataSet$funder <- funder

#Installer - reduce factor levels
NUM_LEVELS_INSTALLER = 10 

#Installer will have this many + 1 levels
installerNames <- names(summary(entireDataSet$installer)[1:NUM_LEVELS_INSTALLER])
installer <- factor(entireDataSet$installer, levels=c(installerNames, "Other"))
installer[is.na(installer)] <- "Other"
entireDataSet$installer <- installer

#wpt_name - remove. Too many levels
entireDataSet<-entireDataSet[, -which(names(entireDataSet) == "wpt_name")]

#num_private - remove.
entireDataSet <-entireDataSet[, -which(names(entireDataSet) == "num_private")]

#subvillage - remove. Too many levels
entireDataSet <-entireDataSet[, -which(names(entireDataSet) == "subvillage")]

#region_code - remove, looks like a proxy for region
entireDataSet <-entireDataSet[, -which(names(entireDataSet) == "region_code")]

#district_code - remove, may also be a proxy for region.
entireDataSet <-entireDataSet[, -which(names(entireDataSet) == "district_code")]

#lga - remove, may also be a proxy for region.
entireDataSet <-entireDataSet[, -which(names(entireDataSet) == "lga")]

#ward,scheme_name - remove. Too many levels
entireDataSet <-entireDataSet[, -which(names(entireDataSet) == "ward")]
entireDataSet <- entireDataSet[, -which(names(entireDataSet) == "scheme_name")]
#recorded_by - remove. Constant
entireDataSet <- entireDataSet[, -which(names(entireDataSet) == "recorded_by")]

#Convert every String attribute to Numeric values 

entireDataSet$funder = as.numeric( entireDataSet$funder)
entireDataSet$installer = as.numeric( entireDataSet$installer)
entireDataSet$basin = as.numeric( entireDataSet$basin)
entireDataSet$region = as.numeric( entireDataSet$region)
entireDataSet$extraction_type = as.numeric( entireDataSet$extraction_type)
entireDataSet$extraction_type_group = as.numeric( entireDataSet$extraction_type_group)
entireDataSet$extraction_type_class = as.numeric( entireDataSet$extraction_type_class)
entireDataSet$scheme_management = as.numeric( entireDataSet$scheme_management)
entireDataSet$permit  = as.numeric( entireDataSet$permit)
entireDataSet$management = as.numeric( entireDataSet$management)
entireDataSet$management_group = as.numeric( entireDataSet$management_group)
entireDataSet$waterpoint_type_group = as.numeric( entireDataSet$waterpoint_type_group)
entireDataSet$waterpoint_type= as.numeric( entireDataSet$waterpoint_type)
entireDataSet$source_class = as.numeric( entireDataSet$source_class)
entireDataSet$source = as.numeric( entireDataSet$source)
entireDataSet$source_type =  entireDataSet$source_type
entireDataSet$source_type = as.numeric( entireDataSet$source_type)
entireDataSet$water_quality = as.numeric( entireDataSet$water_quality)
entireDataSet$public_meeting = as.numeric( entireDataSet$public_meeting)
entireDataSet$quality_group = as.numeric( entireDataSet$quality_group)
entireDataSet$quantity = as.numeric( entireDataSet$quantity)
entireDataSet$quantity_group = as.numeric( entireDataSet$quantity_group)
entireDataSet$payment = as.numeric( entireDataSet$payment)
entireDataSet$payment_type = as.numeric( entireDataSet$payment_type)

######################End of preprocessing #########################################
#Split the entireDataSet into training and test data

endOfTrainData<- nrow(trainData)
endOfEntireData<-nrow(entireDataSet)
 
#Split the 80% of dataset into training and the rest as test
TrainingData<- entireDataSet[1:endOfTrainData, c(
  "id","region","amount_tsh","gps_height","correctedLongitude","latitude","population"
  ,"public_meeting" ,"scheme_management","permit" ,"pumpage","extraction_type_class","management_group"
  ,"quality_group","quantity_group","source_type" ,"source_class" ,"waterpoint_type_group" ,"class"
)
]

TestData<- entireDataSet[(endOfTrainData+1):endOfEntireData, c(
  "id","region","amount_tsh","gps_height","correctedLongitude","latitude","population"
  ,"public_meeting" ,"scheme_management","permit" ,"pumpage","extraction_type_class","management_group"
  ,"quality_group","quantity_group"  ,"source_type" ,"source_class" ,"waterpoint_type_group" ,"class"
)
]
sampleTrain<-TrainingData
#sampleTest<-sample(1:nrow(TrainingData),size = 0.8*nrow(TrainingData))
#cleanTrainingData<-TrainingData[sampleTest,]
#cleanTestData<-TrainingData[-sampleTest,]


#Classifiers
######################################################################################
#Randomforest
ntrees<-500
model_forest<-randomForest(as.factor(class)~latitude+correctedLongitude+amount_tsh+gps_height
                           +pumpage+population+extraction_type_class+waterpoint_type_group+
                           quantity_group,
                        data = sampleTrain,importance = TRUE,
                         ntree = ntrees,nodesize = 5)
predicted_class<-predict(model_forest,TestData)
#rf_table<-table(predicted_class,cleanTestData$class)
#confusionMatrix(predicted_class,cleanTestData$class)

#Navie Bayes
naive = naiveBayes(as.factor(class)~.,cleanTrainingData )
predict_naive = predict(naive,cleanTestData)
table(predict_naive,cleanTestData$class)
confusionMatrix(predict_naive,cleanTestData$class)
#plot(predict_naive, cleanTestData$class, main="Naive Bayes")


#Bagging
formula <- as.formula(paste("as.factor(",colnames(cleanTrainingData)[length(cleanTrainingData)],") ~","." ))
bag <- ipred::bagging(formula, data=cleanTrainingData, boos = TRUE,mfinal=10,
                      control = rpart.control(cp = 0)) 
predictedClass<-predict(bag,cleanTestData)
confusionMatrix(predictedClass,cleanTestData$class)



#Boosting

gb = gbm.fit(cleanTrainingData[,-19],cleanTrainingData[,19],n.trees=1,verbose = FALSE,shrinkage=0.001 ,bag.fraction = 0.3 ,interaction.depth = 1, n.minobsinnode = 1, distribution = "gaussian")  
predict_boosting <- predict(gb,cleanTestData[,-19],n.trees = 1)
xtab<-table(predict_boosting,cleanTestData$class)
accuracy <- (sum(diag(xtab)) / sum(xtab))*100.0
cat("Accuracy ",accuracy)
precision <- diag(xtab) / rowSums(xtab)
precision <- mean(precision)
cat("Precision",precision)
recall <- (diag(xtab) / colSums(xtab))
recall <- mean(recall)
cat("Recall", recall)
FScore <- (2*precision*recall)/(precision+recall)
FScore <- mean(FScore)
cat("FScore",FScore)

#Works upto this
###################
#Decision Tree

DecisionTree.model<-rpart(as.factor(class)~.,data=cleanTrainingData,
                          method='class',parms=list(split='information'),minsplit=2,minbucket=1)
cpVal<- DecisionTree.model$cptable[which.min(DecisionTree.model$cptable[,"xerror"]),"CP"]
prune_model=prune(DecisionTree.model,best=10,cp=cpVal)
test_prediction=predict(DecisionTree.model,cleanTestData,type = "class")
confusionMatrix(test_prediction,cleanTestData$class)


################ Logistic Regression

cleanTrainingData$class <- as.factor(cleanTrainingData$class)
lr <- glm(cleanTrainingData$class~.,data=cleanTrainingData,family=binomial())
predicted<-predict(lr,cleanTestData)
predicted <- ifelse(predicted > 0.5,1,0)
str(cleanTrainingData)
table(predicted,cleanTestData$class)

########## KNN
model.knn <- knn(cleanTrainingData[,1:(18)],cleanTestData[,1:(18)],cleanTrainingData[,19],k=200)
tb_name <- table("Predictions" = model.knn, Actual = cleanTestData[,19])
accuracy <- (sum(diag(tb_name)) / sum(tb_name))*100.0
cat("KNN accuracy ",accuracy)









