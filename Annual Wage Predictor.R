
#Loading working directory
getwd()

#Loading the required libraries

library(caret)
library(corrplot)
library(doParallel)
library(dplyr)
library(earth)
library(elasticnet)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(mice)
library(parallel)
library(rpart.plot)
library(VIM)
library(purrr) #keep function
library(tidyr)
library(gbm) #Boosting

#Loading all the data from .csv file
annualWageData <- read.csv(file = "D:\\ENGR122\\Project\\AnnualWageData.csv")

#Glimpse of data with some sample values
str(annualWageData)

#picking only 40,000 rows due to limited resources of machine
#annualWageData <- head(annualWageData,40000)

newlyCreatedID <-
  paste(annualWageData$SERIALNO, "_", annualWageData$SPORDER, "_", annualWageData$PUMA)
annualWageData = annualWageData[-c(1:3)]

annualWageData = cbind(newlyCreatedID, annualWageData)

#remove rows where WAGP is 0
annualWageData <- subset(annualWageData, annualWageData$WAGP != 0)

#remove rows where INDP is NA< represents people with age 16 or below
annualWageData <- subset(annualWageData, !is.na(annualWageData$INDP))

#remove rows where DDRS is NA< represents people with age 5 or below
annualWageData <- subset(annualWageData, !is.na(annualWageData$DDRS))

#remove rows where AGEP < 10
annualWageData <- subset(annualWageData, annualWageData$AGEP > 10)

saveRDS(annualWageData, "D:\\ENGR122\\Project\\annualWageData.rds")

#separate out numerical and categorical variable
numericalData <- annualWageData[, c(2:4, 6, 28:29, 39, 56:57, 59, 67:68, 72)]
#[1] "ADJINC" "PWGTP"  "AGEP"   "CITWP"  "INTP"   "JWMNP"  "MARHYP" "OIP"    "PAP"    "RETP"   "WAGP"  
#[12] "WKHP"   "YOEP"
#keeping 100000 rows
#numericalData <- numericalData[1:100000, ]
saveRDS(numericalData, "D:\\ENGR122\\Project\\numericalData.rds")

categoricalData <- annualWageData[-c(2:4, 6, 28:29, 39, 56:57, 59, 67:68, 72)]

#removing categorical variables based on data understanding
categoricalData <-
  categoricalData[-c(
    5, 6, 7, 8, 10, 11, 14, 15, 16, 17:23, 24, 25, 27, 31, 32, 35:43, 46:48, 49, 53, 57:65, 68:72, 74, 75, 79:84, 87, 89, 91:93, 95, 96, 97, 98:112
  )]
str(categoricalData)
#categoricalData <- categoricalData[1:100000, ]
saveRDS(categoricalData, "D:\\ENGR122\\Project\\categoricalData.rds")

#finding categorical columns having NAs and need to be imputed
x <- unlist(lapply(categoricalData, function(x)
  any(is.na(x))))

#find numerical columns having NAs and need to be imputed
y <- unlist(lapply(numericalData, function(x)
  any(is.na(x))))

#imputing NA values using Mice package https://www.rdocumentation.org/packages/mice/versions/3.3.0
set.seed(2)

#Reference https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4701517/
aggr_plot <-
  aggr(
    annualWageData,
    col = c('navyblue', 'red'),
    numbers = TRUE,
    sortVars = TRUE,
    labels = names(data),
    cex.axis = .7,
    gap = 3,
    ylab = c("Missing Data", "Patttern")
  )

  
#md.pattern():  
#left column shows number of rows for each type of missing combination
#Right most column shows count of Na's as each unique combination of missing values
#For example if left most value is 11 and right most is 3 (with some kinf of combination of 0s and 1s) then we can say
#that for the said combination there are 3 missing values or NAs and there are 11 such rows.
#therefore the sum of left most column should be number of total records and sum of right most colunm should
#be total number of NA's across all the columns and rows.

md.pattern(numericalData)
md.pattern(categoricalData)

#Code to parallelize on windows
z=1:4
system.time(lapply(z,function(x) Sys.sleep(1)))
cl<-makeCluster(4,type="SOCK")
system.time(clusterApply(cl, z,function(x) Sys.sleep(1)))
                         
                         
micedNumericalData <-
  mice(numericalData[c(4, 6, 7, 13)],
       m = 2,
       maxit = 2,
       meth = 'pmm')
micedNumericalData <- mice::complete(micedNumericalData, 1)
saveRDS(micedNumericalData, "D:\\ENGR122\\Project\\micedNumericalData.rds")

#Reference : https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
#https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/

micedCategoricalData_col_5_6 <-
  mice(categoricalData[c(5, 6)],
       m = 2,
       maxit = 2
       )
micedCategoricalData_col_5_6 <- mice::complete(micedCategoricalData_col_5_6, 1)
saveRDS(micedCategoricalData_col_5_6,
        "D:\\ENGR122\\Project\\micedCategoricalData_col_5_6.rds")


micedCategoricalData_col_7_10_11_13 <-
  mice(categoricalData[c(7, 10, 11, 13)],
       m = 2,
       maxit = 2
       )
micedCategoricalData_col_7_10_11_13 <-
  mice::complete(micedCategoricalData_col_7_10_11_13, 1)
saveRDS(micedCategoricalData_col_7_10_11_13,
        "D:\\ENGR122\\Project\\micedCategoricalData_col_7_10_11_13.rds")

micedCategoricalData_col_17_22 <-
  mice(categoricalData[c(17, 22)],
       m = 2,
       maxit = 1	   
       )
micedCategoricalData_col_17_22 <-
  mice::complete(micedCategoricalData_col_17_22, 1)
saveRDS(micedCategoricalData_col_17_22,
        "D:\\ENGR122\\Project\\micedCategoricalData_col_17_22.rds")


micedCategoricalData_col_8_25_30_31 <-
  mice(categoricalData[c(8, 25, 30, 31)],
       m = 2,
       maxit = 2
	   )
micedCategoricalData_col_8_25_30_31 <- mice::complete(micedCategoricalData_col_8_25_30_31, 1)
saveRDS(micedCategoricalData_col_8_25_30_31,
        "D:\\ENGR122\\Project\\micedCategoricalData_col_8_25_30_31.rds")



categoricalData$MIGSP[is.na(categoricalData$MIGSP)] <- 0
MIGSP<- categoricalData$MIGSP

#Merging all categoricalData with imputations
completedMicedCategoricalData <-
  cbind(
    micedCategoricalData_col_5_6,
    micedCategoricalData_col_7_10_11_13,
    micedCategoricalData_col_8_25_30_31,
    micedCategoricalData_col_17_22,
    MIGSP
  )
completedCategoricalData <-
  cbind(completedMicedCategoricalData, categoricalData[, c(1:4, 9, 12, 14, 15, 16, 18, 19, 20, 21, 23, 24, 27, 28, 29, 32)])
saveRDS(completedCategoricalData, "D:\\ENGR122\\Project\\completedCategoricalData.rds")

completedNumericalData <-
  cbind(micedNumericalData, numericalData[, c(1, 2, 3, 5, 8, 9, 10, 11, 12)])
saveRDS(completedNumericalData, "D:\\ENGR122\\Project\\completedNumericalData.rds")

allImputedData <- cbind(completedNumericalData, completedCategoricalData)
saveRDS(allImputedData, "D:\\ENGR122\\Project\\allImputedData.rds")


#Removing data that is near zero variance
NearZeroNumerical <-
  nearZeroVar(numericalData,
              allowParallel = TRUE,
              saveMetrics = TRUE)
NearZeroNumerical
micedNumericalData_withoutZeroVar <-
  completedNumericalData[-c(5, 8, 9, 10, 11)]

#'CITWP' 'JWMNP' 'MARHYP' 'YOEP' 'PWGTP' 'AGEP' 'WAGP' 'WKHP'
colnames(micedNumericalData_withoutZeroVar)


allCombinedData <-
  cbind(micedNumericalData_withoutZeroVar, completedCategoricalData)
#22nd column is newlyCreatedID which we generated as a primary key, removing that from data
allCombinedData_withoutnewlyCreatedID <- allCombinedData[-22]
saveRDS(allCombinedData_withoutnewlyCreatedID, "D:\\ENGR122\\Project\\processedData.rds")

# calculate correlation matrix
correlationMatrix <- cor(allCombinedData_withoutnewlyCreatedID,use="complete.obs")
corrplot(correlationMatrix)
# summarize the correlation matrix
print(correlationMatrix)

# Attributes which are highly co-related (ideally > 0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75,names=TRUE)
# print indexes of highly correlated attributes
print(highlyCorrelated)

#removing highly correlated variables
allCombinedData_withoutnewlyCreatedID <- allCombinedData_withoutnewlyCreatedID[ , !(names(allCombinedData_withoutnewlyCreatedID) %in% highlyCorrelated)]
colnames(allCombinedData_withoutnewlyCreatedID)

#Removing Income to Poverty ratio Recode: 'POVPIP'
colnames(allCombinedData_withoutnewlyCreatedID[17])
allCombinedData_withoutnewlyCreatedID <- allCombinedData_withoutnewlyCreatedID[, -c(17)]

#Using earth package to find important variables.
#https://www.rdocumentation.org/packages/earth/versions/4.6.3
m.earth <-
  earth(WAGP ~ ., data = allCombinedData_withoutnewlyCreatedID)
ev <- evimp(m.earth)

plot(ev,
     cex.legend = 1,
     x.legend = nrow(x),
     y.legend = x[1, "nsubsets"])

importantVariablesData <- allCombinedData_withoutnewlyCreatedID[, c( 6, 21, 22, 32, 33, 34, 35, 28, 29, 7, 8)]
colnames(importantVariablesData)
#'AGEP' 'CIT' 'COW' 'ESR' 'INDP' 'MSP' 'RAC1P' 'SCHL' 'SEX' 'WAGP' 'WKHP'


#Filtering wages for range $20,000 to $200,000
importantVariablesData <- subset(importantVariablesData, importantVariablesData$WAGP > 20000)
importantVariablesData <- subset(importantVariablesData, importantVariablesData$WAGP < 200000)

importantVariablesData %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap( ~ key, scales = "free") +
  geom_histogram()
  
boxplot_column_1 <-
  qplot(
    x = importantVariablesData$WAGP,
    y = importantVariablesData[, 8],
    geom = "boxplot" ,
    xlab = "",
    ylab = "SCHL"
  )
boxplot_column_2 <-
  qplot(
    x = importantVariablesData$WAGP,
    y = importantVariablesData[, 11],
    geom = "boxplot" ,
    xlab = "",
    ylab = "WKHP"
  )
boxplot_column_3 <-
  qplot(
    x = importantVariablesData$WAGP,
    y = importantVariablesData[, 1],
    geom = "boxplot" ,
    xlab = "",
    ylab = "AGEP"
  )
boxplot_column_4 <-
  qplot(
    x = importantVariablesData$WAGP,
    y = importantVariablesData[, 4],
    geom = "boxplot" ,
    xlab = "",
    ylab = "ESR"
  )
boxplot_column_5 <-
  qplot(
    x = importantVariablesData$WAGP,
    y = importantVariablesData[, 5],
    geom = "boxplot" ,
    xlab = "",
    ylab = "INDP"
  )
boxplot_column_6 <-
  qplot(
    x = importantVariablesData$WAGP,
    y = importantVariablesData[, 9],
    geom = "boxplot" ,
    xlab = "",
    ylab = "SEX"
  )
boxplot_column_7 <-
  qplot(
    x = importantVariablesData$WAGP,
    y = importantVariablesData[, 6],
    geom = "boxplot" ,
    xlab = "",
    ylab = "MSP"
  )
boxplot_column_8 <-
  qplot(
    x = importantVariablesData$WAGP,
    y = importantVariablesData[, 2],
    geom = "boxplot" ,
    xlab = "",
    ylab = "CIT"
  )
boxplot_column_9 <-
  qplot(
    x = importantVariablesData$WAGP,
    y = importantVariablesData[, 7],
    geom = "boxplot" ,
    xlab = "",
    ylab = "RAC1P"
  )

grid.arrange(
  boxplot_column_1,
  boxplot_column_2,
  boxplot_column_3,
  boxplot_column_4,
  boxplot_column_5,
  boxplot_column_6,
  boxplot_column_7,
  boxplot_column_8,
  boxplot_column_9,
  ncol = 9
)


#Train Test Split
wage <- importantVariablesData$WAGP
# x <- importantVariablesData[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)] #only first 16 columns (predictors)

inTrain <- createDataPartition(y = wage, p = .70, list = FALSE)
trainData <- importantVariablesData[inTrain, ]
testData <- importantVariablesData[-inTrain, ]
na.omit(trainData)
na.omit(testData)
summary(trainData)
summary(testData)

#Regular Linear Regression
#CV
control <- trainControl(method = "cv", number = 5)

lm.train <- train(
  WAGP ~ .,
  data = trainData,
  method = "lm",
  tuneLength = 4,
  preProcess = c("scale", "center"),
  trControl = control
)
summary(lm.train)
lm.predict <- predict(lm.train, testData)
RMSE(lm.predict, testData$WAGP)
#34508.983351235

#Lasso
#install.packages("elasticnet")
lasso.train <- train(
  WAGP ~ .,
  data = trainData,
  method = "lasso",
  tuneLength = 4,
  preProcess = c("scale", "center"),
  trControl = control
)
lasso.train
lasso.predict <- predict(lasso.train, testData)

RMSE(lasso.predict, testData$WAGP)

#Stopping parallel cluster
stopCluster(cl)

models_train <- list(
  "lm" = lm.train,
  "lasso" = lasso.train
)

data.resamples1 <- resamples(models_train)

summary(data.resamples1)

### VISUALIZING TRAIN PERFORMANCE ###
bwplot(data.resamples1, metric = "RMSE")
bwplot(data.resamples1, metric = "Rsquared")

### Resamples for Test Error(RMSE) and Rsquared ###
postResample(pred = lm.predict, obs = testData$WAGP)
postResample(pred = lasso.predict, obs = testData$WAGP)