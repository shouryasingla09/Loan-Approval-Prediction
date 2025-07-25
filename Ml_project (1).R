data <- read.csv("trains.csv")
head(data)
dim(data)
# line of code removes all underscores from the column names
names(data) <- gsub("_", "", names(data))
names(data)
#names(data): Gets the column names of the data frame data
# gsub("_", "", ...): Replaces all underscores with an empty string (i.e., removes them).
#Creates a new column called TotalIncome in data
data$TotalIncome <- data$ApplicantIncome+data$CoapplicantIncome
colnames(data)
colSums(is.na(data))
# we find that numeric columns like LoanAmount has 22 null values, LoanAmountTerm has 14 null values and credit history has 50 null values
# to find for character columns
# 1. Identify character columns
charactercolumns <- sapply(data, is.character)
names(charactercolumns)
# 2. Subset to only character columns in the dataset
charactersubset <- data[,charactercolumns]
head(charactersubset)  
# 3. Count NAs in each character column
NAvalues <- colSums(is.na(charactersubset))
NAvalues
# 4. Show only those character columns where NA > 0
NAvalues[NAvalues> 0]
# checking uniques value
unique(data$Gender)
unique(data$Married)
unique(data$Dependents)
unique(data$Education)
unique(data$SelfEmployed)
unique(data$CreditHistory)
unique(data$PropertyArea)
unique(data$LoanStatus)
# it can be a case where Na values replaced by empty strings
# we can replace these empty strings with NA values and then fill NA values with appropriate values
data$LoanID[data$LoanID==""] <- NA
data$Gender[data$Gender==""] <- NA
data$Married[data$Married==""] <- NA
data$Dependents[data$Dependents==""] <- NA
data$Education[data$Education==""] <- NA
data$SelfEmployed[data$SelfEmployed==""] <- NA
data$PropertyArea[data$PropertyArea==""] <- NA
data$LoanStatus[data$LoanStatus==""] <- NA

# to find for character columns
# 1. Identify character columns
charactercolumns <- sapply(data, is.character)
names(charactercolumns)
# 2. Subset to only character columns in the dataset
charactersubset <- data[,charactercolumns]
head(charactersubset)  
# 3. Count NAs in each character column
NAvalues <- colSums(is.na(charactersubset))
NAvalues
# 4. Show only those character columns where NA > 0
NAvalues[NAvalues> 0]

# SO WE ARE GETTING THESE NULL VALUES

# SINCE DIFFERENT METHOD TO FILL VALUES USING Forward fill, Mode Imputation , Random Fill, Predictive Imputation 
#since we are working with a survey or tabular dataset (like loan prediction, credit scoring).
#Goal is to avoid biasing the data.
# we will use Mode method 
mostgender <- names(which.max(table(data$Gender)))
mostmarried <- names(which.max(table(data$Married)))
mostselfemployed <- names(which.max(table(data$SelfEmployed)))
mostDependents <- names(which.max(table(data$Dependents)))
table(data$Gender)
table(data$Married)
table(data$SelfEmployed)
table(data$Dependents)
data$Gender[is.na(data$Gender)] <- mostgender
data$Married[is.na(data$Married)] <- mostmarried
data$SelfEmployed[is.na(data$SelfEmployed)] <- mostselfemployed 
data$Dependents[is.na(data$Dependents)] <- mostDependents


# to find for character columns
# 1. Identify character columns
charactercolumns <- sapply(data, is.character)
names(charactercolumns)
# 2. Subset to only character columns in the dataset
charactersubset <- data[,charactercolumns]
head(charactersubset)  
# 3. Count NAs in each character column
NAvalues <- colSums(is.na(charactersubset))
NAvalues
# 4. Show only those character columns where NA > 0
NAvalues[NAvalues> 0]

data
# Now my target is fill or remove null values in LoanAMOUNT,LoanAmount term, credit history
max(data$TotalIncome)

#  Making a Histogram 
hist(data$TotalIncome,freq=FALSE,col = "red", main ="Fitting KDE over Total Income",xlab = "Total Income")

# Add kernel density line
lines(density(data$TotalIncome),col="green",lwd = 2)

#The TotalIncome distribution is highly right-skewed, with a large concentration of data at lower income levels and a long tail extending to higher incomes
mean(data$TotalIncome)
quantile(data$TotalIncome,0.75)
quantile(data$TotalIncome,0.95)
quantile(data$TotalIncome,0.99)
quantile(data$TotalIncome,0.50)

#A Kernel Density Estimate (KDE) is a smooth curve that estimates the probability density function (PDF) of a continuous variable
#Offering a better sense of how data is distributed.
#Showing peaks, spread, skewness more clearly.

library(e1071)
skewness(data$TotalIncome) #5.605953
#Tail on the right (right-skewed)
range(data$TotalIncome) #  1442 81000 giving the min and max value
#This large range suggests high variability — some people earn very little, while others earn significantly more
IQR(data$TotalIncome) #3355.75
#A value of ₹3,355.75 means that most middle-income individuals earn within a range of 3,355.75.
sd(data$TotalIncome) #6458.664
var(data$TotalIncome) #  41714339
# suggests that income values are widely dispersed
table(data$LoanAmount)

# plot
# Remove NAs if present
loanamount <- na.omit(data$LoanAmount)
c(min(loanamount),max(loanamount)) #9 700
# plot histogram 
hist(loanamount,freq = FALSE,breaks = 30,xlim = c(0, 800),col = "cyan", main = "Distribution of Loan Amount",xlab = "Loan Amount")
# Add density curve
lines(density(loanamount), col = "red", lwd = 2)
# Done KDE 

amt <- loanamount[loanamount > 30 & loanamount < 260]
meanamt <- mean(amt)
meanamt #129.1368
# filling out null values in loan amount with its mean amount
data$LoanAmount[is.na(data$LoanAmount)] <- meanamt
skewness(loanamount) #2.663998
skewness(amt) #0.5341201
# since we have remove extreme outliers hence it has now become more symmetrical in nature 
# hence we prefer to use mean over median
table(data$LoanAmountTerm)
loanamountterm <- na.omit(data$LoanAmountTerm)
skewness(loanamountterm) #-2.350615
# since this data is skewed hence we use median for the same
medianterm <- median(loanamountterm ) #360
data$LoanAmountTerm[is.na(data$LoanAmountTerm)] <- medianterm

table(data$CreditHistory)
str(data$CreditHistory)
# since it is a binary variable classified and categorised as 0 and 1 we will use max function to fill null values

mosthistory <- names(which.max(table(data$CreditHistory)))
data$CreditHistory[is.na(data$CreditHistory)] <- mosthistory
colSums(is.na(data))
# now all null values are filled
# now checking correlation between data

# 1. Select only numeric columns
numericdata <- data[sapply(data, is.numeric)]
# 2. Calculate correlation matrix
cormatrix <- cor(numericdata, use = "complete.obs")
library(corrplot)
# 4. Plot the heatmap with correlation values
corrplot(cormatrix, method = "color", addCoef.col = "black", number.cex = 0.7,
         tl.col = "black", tl.srt = 45)
cormatrix
#Since Loan Amount and Total Income have a positive correlation of 0.62033216 it shows that people are applying for the amount which they can pay easily by adding 
# applicant income and coapplicant income together which is Totalincome.
# so there chance of default a loan decreasing and loan status is likely to be Yes. They will be provided with loan.
# one finding made if loanamount =< total income loan status will be Yes

# now based on the dataset we are finding key insights on what factor it is noticed whether to give a loan or not 
# we will be comparing to find who are eligible to get a loan
# based on past data set we will train the dataset, validate it and then test the dataset


head(data)
# there is no more requirement of LoanID,ApplicantIncome,CoapplicantIncome
library(dplyr)
data <- data %>% select(-ApplicantIncome, -CoapplicantIncome, -LoanID)
head(data)

table(data$Gender)
table(data$Gender[data$LoanStatus=="Y"])
table(data$Gender[data$LoanStatus=="Y"])/table(data$Gender)
table(data$Gender[data$LoanStatus=="Y"])/614
# Males get more loan status than female
barplot(table(data$Gender[data$LoanStatus=="Y"])/614,main="loan approved  as per gender",col=c("blue","orange"))

table(data$Married)
table(data$Married[data$LoanStatus=="Y"])
table(data$Married[data$LoanStatus=="Y"])/table(data$Married)
table(data$Married[data$LoanStatus=="Y"])/614
barplot(table(data$Married[data$LoanStatus=="Y"])/614,main="loan approved  as per marital status",col=c("blue","orange"))
# married people get more loan status than unmarried one

table(data$Dependents)
table(data$Dependents[data$LoanStatus=="Y"])
table(data$Dependents[data$LoanStatus=="Y"])/table(data$Dependents)
table(data$Dependents[data$LoanStatus=="Y"])/614
barplot(table(data$Dependents[data$LoanStatus=="Y"])/614,main="loan approved  as per number of dependents",col=c("blue","orange","green","red"))
# people having 0 dependents are getting more loan status than those with dependents


table(data$Education)
table(data$Education[data$LoanStatus=="Y"])
table(data$Education[data$LoanStatus=="Y"])/table(data$Education)
table(data$Education[data$LoanStatus=="Y"])/614
barplot(table(data$Education[data$LoanStatus=="Y"])/614,main="loan approved  as per educational status",col=c("blue","orange"))
# Graduates are getting more loan status than not graduates

table(data$SelfEmployed)
table(data$SelfEmployed[data$LoanStatus=="Y"])
table(data$SelfEmployed[data$LoanStatus=="Y"])/table(data$SelfEmployed)
table(data$SelfEmployed[data$LoanStatus=="Y"])/614
barplot(table(data$SelfEmployed[data$LoanStatus=="Y"])/614,main="loan approved  as per service status",col=c("blue","orange"))
# people who are from service sector get more loan status than those self employed one

table(data$CreditHistory)
table(data$CreditHistory[data$LoanStatus=="Y"])
table(data$CreditHistory[data$LoanStatus=="Y"])/table(data$CreditHistory)
table(data$CreditHistory[data$LoanStatus=="Y"])/614
barplot(table(data$CreditHistory[data$LoanStatus=="Y"])/614,main="loan approved  as per those meeting credit history guidelines",col=c("blue","orange"))

# Those who are meeting credit history guidelines are getting more loan status

table(data$PropertyArea)
table(data$PropertyArea[data$LoanStatus=="Y"])
table(data$PropertyArea[data$LoanStatus=="Y"])/table(data$PropertyArea)
table(data$PropertyArea[data$LoanStatus=="Y"])/614
barplot(table(data$PropertyArea[data$LoanStatus=="Y"])/614,main="loan approved  as per property area",col=c("blue","orange","green"))
# those from semiurban area are getting more loan status

# Converting categorical variables to numeric using factor()
# Gender :Male = 1, Female = 2
data$Gender         <- as.numeric(factor(data$Gender))
# Married: No = 1, Yes = 2
data$Married        <- as.numeric(factor(data$Married))
# Dependents: "0" = 1, "1" = 2, "2" = 3, "3+" = 4
data$Dependents     <- as.numeric(factor(data$Dependents))
# Education: Graduate = 1, Not Graduate = 2
data$Education      <- as.numeric(factor(data$Education))
# SelfEmployed: No = 1, Yes = 2
data$SelfEmployed   <- as.numeric(factor(data$SelfEmployed))
# CreditHistory: 0.0 = 1, 1.0 = 2
data$CreditHistory  <- as.numeric(factor(data$CreditHistory))
# PropertyArea: Rural = 1, Semiurban = 2, Urban = 3
data$PropertyArea   <- as.numeric(factor(data$PropertyArea))
# LoanStatus: N = 0, Y = 1
data$LoanStatus <- ifelse(data$LoanStatus == "Y", 1, 0)
head(data)
# Check structure after conversion
str(data)
# prefering 
# gender =1
# married =1
#dependent =1
# educated= 1
#self employed =1
# credit history =2
# property area =2
# loan status =1


library(dplyr)

# Set seed for reproducibility
set.seed(42)

# Split indices for 80% training
trainindex <- sample(1:nrow(data), 0.8 * nrow(data))

# Create train and test sets
train <- data[trainindex, ]
test <- data[-trainindex, ]

# Separate target and features
ytrain <- train$LoanStatus
xtrain <- train %>% select(-LoanStatus)

ytest <- test$LoanStatus
xtest <- test %>% select(-LoanStatus)
nrow(train) # 491
nrow(test) #123

#a decision tree model, which is not affected by feature scale. That’s because:
#It only checks whether values are greater or less than thresholds.
#It does not compute distances or rely on gradients.
#So for Decision Trees, Scaling is NOT required


library(rpart)

# Make sure LoanStatus is a factor (required for classification)
train$LoanStatus <- as.factor(train$LoanStatus)

# Fit decision tree model
model <- rpart(LoanStatus ~ ., data = train, method = "class")
library(rpart.plot)
rpart.plot(model)
summary(model)
# Predict on test data
predictions <- predict(model, newdata = test, type = "class")

# Confusion matrix
table(Predicted = predictions, Actual = test$LoanStatus)

# Accuracy
mean(predictions == test$LoanStatus)

cm <- table(Predicted = predictions, Actual = test$LoanStatus)

# Extract counts
TP <- cm["1", "1"]  # True Positive
TN <- cm["0", "0"]  # True Negative
FP <- cm["1", "0"]  # False Positive
FN <- cm["0", "1"]  # False Negative

# Compute metrics
precision <- TP / (TP + FP)
precision 
recall <- TP / (TP + FN)
recall
F1 <- 2 * precision * recall / (precision + recall)
F1
type1error <- FP / (FP + TN)  # False Positive Rate
type1error
type2error <- FN / (FN + TP)  # False Negative Rate
type2error

# Now fitting the Randomforest Model
library(randomForest)

# Train Random Forest
rfmodel <- randomForest(LoanStatus ~ ., data = train, ntree = 100)
# Predict on test data
rfpredictions <- predict(rfmodel, newdata = test)

# Confusion Matrix
table(Predicted = rfpredictions, Actual = test$LoanStatus)
# Accuracy
mean(rfpredictions == test$LoanStatus)

# Confusion Matrix
cmrf <- table(Predicted = rfpredictions, Actual = test$LoanStatus)

# Extract counts
TPrf <- cmrf["1", "1"]  # True Positives
TNrf <- cmrf["0", "0"]  # True Negatives
FPrf <- cmrf["1", "0"]  # False Positives
FNrf <- cmrf["0", "1"]  # False Negatives

# Compute metrics
precisionrf <- TPrf / (TPrf + FPrf)
precisionrf
recallrf <- TPrf / (TPrf + FNrf)
recallrf
f1rf <- 2 * precisionrf * recallrf / (precisionrf + recallrf)
f1rf
type1errorrf <- FPrf / (FPrf + TNrf)  # False Positive Rate
type1errorrf 
type2errorrf <- FNrf / (FNrf + TPrf)  # False Negative Rate
type2errorrf

# now scaling the data and apply the logistic regression model

#Compute mean and sd from training data
trainmeans <- apply(xtrain, 2, mean)
trainsds <- apply(xtrain, 2, sd)

# Scale train and test data using training parameters
xtrainscaled <- as.data.frame(scale(xtrain, center = trainmeans, scale = trainsds))
xtestscaled <- as.data.frame(scale(xtest, center = trainmeans, scale = trainsds))

# Add target back to training and test sets
trainscaled <- cbind(xtrainscaled, LoanStatus = ytrain)
testscaled <- cbind(xtestscaled, LoanStatus = ytest)
head(trainscaled)

# Logistic Regression on Scaled Data
logisticmodel <- glm(LoanStatus ~ ., data = trainscaled, family = binomial)

# Predict probabilities on test data
probabilities <- predict(logisticmodel, newdata = testscaled, type = "response")

# Convert probabilities to class labels (threshold = 0.5)
predictedlabels <- ifelse(probabilities > 0.5, 1, 0)

# Accuracy
accuracy <- mean(predictedlabels == testscaled$LoanStatus)
accuracy 
# Confusion Matrix
cmlogistic <- table(Predicted = predictedlabels, Actual = testscaled$LoanStatus)
cmlogistic
TP <- cmlogistic["1", "1"]
TN <- cmlogistic["0", "0"]
FP <- cmlogistic["1", "0"]
FN <- cmlogistic["0", "1"]

precision <- TP / (TP + FP)
precision
recall <- TP / (TP + FN)
recall 
F1 <- 2 * precision * recall / (precision + recall)
F1 
type1error <- FP / (FP + TN)
type1error
type2error <- FN / (FN + TP)
type2error 




# Importing test data 
testing <- read.csv("test.csv")
names(testing) <- gsub("_", "", names(testing))

# Creating a column of TotalIncome
testing$TotalIncome <- testing$ApplicantIncome + testing$CoapplicantIncome

# Replace blank values with NA
testing$Gender[testing$Gender == ""] <- NA
testing$Married[testing$Married == ""] <- NA
testing$Dependents[testing$Dependents == ""] <- NA
testing$Education[testing$Education == ""] <- NA
testing$SelfEmployed[testing$SelfEmployed == ""] <- NA
testing$PropertyArea[testing$PropertyArea == ""] <- NA

# Filling missing categorical values with their mode values
testing$Gender[is.na(testing$Gender)] <- mostgender
testing$Married[is.na(testing$Married)] <- mostmarried
testing$SelfEmployed[is.na(testing$SelfEmployed)] <- mostselfemployed
testing$Dependents[is.na(testing$Dependents)] <- mostDependents

#Filling missing numerical values with mean or median
testing$LoanAmount[is.na(testing$LoanAmount)] <- meanamt
testing$LoanAmountTerm[is.na(testing$LoanAmountTerm)] <- medianterm
testing$CreditHistory[is.na(testing$CreditHistory)] <- mosthistory

# Droping out not useful columns
testing <- testing %>% select(-LoanID, -ApplicantIncome, -CoapplicantIncome)

# Converting categorical variables into factors 
testing$Gender         <- as.numeric(factor(testing$Gender))
testing$Married        <- as.numeric(factor(testing$Married))
testing$Dependents     <- as.numeric(factor(testing$Dependents))
testing$Education      <- as.numeric(factor(testing$Education))
testing$SelfEmployed   <- as.numeric(factor(testing$SelfEmployed))
testing$CreditHistory  <- as.numeric(factor(testing$CreditHistory))
testing$PropertyArea   <- as.numeric(factor(testing$PropertyArea))

# Scaling testing data for logistic regression
testingscaled <- as.data.frame(scale(testing, center = trainmeans, scale = trainsds))

# Predicting values using trained models
# Decision Tree
dt_preds <- predict(model, newdata = testing, type = "class")

# Random Forest
rf_preds <- predict(rfmodel, newdata = testing)

# Logistic Regression
logistic_probs <- predict(logisticmodel, newdata = testingscaled, type = "response")
logistic_preds <- ifelse(logistic_probs > 0.5, 1, 0)

table(dt_preds)
table(rf_preds)
table(logistic_preds)

