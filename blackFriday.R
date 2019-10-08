setwd("C:/Vignesh/Studies/Fall 2018/Adv Stats IDS 575/Project/Black Friday")
BlackFriday <- read.csv(file="BlackFriday.csv", header=TRUE, sep=",")
# lets see the structure of dataset
str(BlackFriday)
nrow(BlackFriday)
ncol(BlackFriday)

# check the missing values
sapply(BlackFriday, function(x) sum(is.na(x)))

# check the datatype for each column
typeof(BlackFriday$User_ID)
typeof(BlackFriday$Product_ID)
typeof(BlackFriday$Gender)
typeof(BlackFriday$Age)
typeof(BlackFriday$Occupation)
typeof(BlackFriday$City_Category)
typeof(BlackFriday$Stay_In_Current_City_Years)
typeof(BlackFriday$Marital_Status)
typeof(BlackFriday$Product_Category_1)
typeof(BlackFriday$Marital_Status)
typeof(BlackFriday$Product_Category_1)
typeof(BlackFriday$Product_Category_2)
typeof(BlackFriday$Product_Category_3)
typeof(BlackFriday$Purchase)

# Converting columns to Respective datatype
BlackFriday$Gender<-as.factor(BlackFriday$Gender)
BlackFriday$User_ID<-as.numeric(BlackFriday$User_ID)
BlackFriday$Age<- as.factor(BlackFriday$Age)
BlackFriday$City_Category<-as.factor(BlackFriday$City_Category)
BlackFriday$Stay_In_Current_City_Years<-as.factor(BlackFriday$Stay_In_Current_City_Years)
BlackFriday$Marital_Status<-as.factor(BlackFriday$Marital_Status)
BlackFriday$Product_Category_1<-as.factor(BlackFriday$Product_Category_1)
BlackFriday$Product_Category_2<-as.factor(BlackFriday$Product_Category_2)
summary(BlackFriday)

# Filling the empty values with Zeros
i <- sapply(BlackFriday, is.factor)
BlackFriday[i] <- lapply(BlackFriday[i], as.character) # Convert factors to character variables
BlackFriday[is.na(BlackFriday)] <- 0 # Replace NA with 0, as shown in Example 1
BlackFriday[i] <- lapply(BlackFriday[i], as.factor) # Convert character columns back to factors
str(BlackFriday)
View(BlackFriday)

# Finding Duplicate data
unique(BlackFriday)

# Get unique values in each column
rapply(BlackFriday,function(x)length(unique(x)))

# Group the age values into 7 groups
levels(BlackFriday$Age)[1] <- 1
levels(BlackFriday$Age)[2] <- 2
levels(BlackFriday$Age)[3] <- 3
levels(BlackFriday$Age)[4] <- 4
levels(BlackFriday$Age)[5] <- 5
levels(BlackFriday$Age)[6] <- 6
levels(BlackFriday$Age)[7] <- 7

#check the levels of Age
levels(BlackFriday$Age)

# Lets check the stay in current City, replace +4 with 4
levels(BlackFriday$Stay_In_Current_City_Years)
levels(BlackFriday$Stay_In_Current_City_Years)[5] <- 4
levels(BlackFriday$Stay_In_Current_City_Years)

# Lets do statistics
mean(BlackFriday$Purchase)
median(BlackFriday$Purchase)
sd(BlackFriday$Purchase)
plot(BlackFriday$Purchase)

# Lets do some Bivariate analysis
# Relationship Between Gender Vs Purchase
GenderTable<-table(BlackFriday$Gender)
GenderTable


# Lets determine the relationship between input and output variable
# Purchase and Gender
GenderPurchaseAov<-aov(BlackFriday$Purchase~BlackFriday$Gender, data=BlackFriday)
summary(GenderPurchaseAov)

# The result shows the P-value is less than 0.5 so gender influences the Purchase cost

# Purchase and Age
AgePurchaseAov <-aov(BlackFriday$Purchase~BlackFriday$Age, data=BlackFriday)
summary(AgePurchaseAov)
# The result shows the P-value is less than 0.5 so Age influences the Purchase cost

# Purchase and Martial status
MartialStatus <-aov(BlackFriday$Purchase~BlackFriday$Marital_Status, data=BlackFriday)
summary(MartialStatus)
# The result shows the P-value is less than 0.5 so Martial Status influences the Purchase cost

# Purchase and Occupation
OccupationAov <- aov(BlackFriday$Purchase~BlackFriday$Occupation , data=BlackFriday)
summary(OccupationAov)
# The result shows the P-value is less than 0.5 so Occupation influences the Purchase cost

# Purchase and currentstay in city
CurrentCityAov <- aov(BlackFriday$Purchase~BlackFriday$Stay_In_Current_City_Years, data=BlackFriday)
summary(CurrentCityAov)
# The result shows the P-value is less than 0.5 so Martial Status influences the Purchase cost

# Purchase and CityCategory
Cityaov <- aov(BlackFriday$Purchase~BlackFriday$City_Category, data=BlackFriday)
summary(Cityaov)

# Purchase and category 1
Category1aov <- aov(BlackFriday$Purchase~BlackFriday$Product_Category_1, data=BlackFriday)
summary(Category1aov)

# Purchase and category 2
Category2aov <- aov(BlackFriday$Purchase~BlackFriday$Product_Category_2, data=BlackFriday)
summary(Category2aov)

# Purchase and category 3
Category3aov <- aov(BlackFriday$Purchase~BlackFriday$Product_Category_3, data=BlackFriday)
summary(Category3aov)

#######################################################################################################
########################################################################################################
library(ggplot2)
# check if any NA
sapply(BlackFriday, function(x)all(any(is.na(x))))
# check number of NA
apply(BlackFriday, 2, function(x) sum(is.na(x)))
# check other columns to get a feel for the data
summary(BlackFriday)
# table(unlist(df$Age))
# table(unlist(df$Gender))
# table(unlist(df$City_Category))
# table(unlist(df$Stay_In_Current_City_Years))
# table(unlist(df$Marital_Status))
# graph individual purchases
ggplot(BlackFriday, aes(x = BlackFriday$Purchase)) + geom_histogram(binwidth = 100)
# aggregate purchases by User_ID and graph
aggpur <- aggregate(Purchase ~ User_ID, BlackFriday, sum)
ggplot(aggpur, aes(x = aggpur$Purchase)) + geom_histogram(binwidth = 10000) + xlim(0,5000000)
# purchases per user
pur_per_usr <- data.frame(table(BlackFriday$User_ID))

ggplot(pur_per_usr, aes(x = pur_per_usr$Freq)) + geom_histogram(binwidth = 1) + xlim(0,300)

# create new dataframe with total purchases only
tot_pur <- merge(x = aggpur, y = BlackFriday, by = "User_ID", all = TRUE)
View(tot_pur)
tot_pur_BF <- tot_pur[-which(duplicated(tot_pur$User_ID)), ]
View(tot_pur_BF)

tot_pur_BF[ ,c('User_ID', 'Product_ID', 'Product_Category_1', 'Product_Category_2', 'Product_Category_3', 'Purchase.y')] <- list(NULL)
colnames(tot_pur_BF)[colnames(tot_pur_BF)=="Purchase.x"] <- "Purchased"

# Occupation and Marital_Status should be factors - the Occupation is a fixed integer from 1 to 20 indicating the occupation of the customer
tot_pur_BF$Occupation <- as.factor(tot_pur_BF$Occupation)
tot_pur_BF$Marital_Status <- as.factor(tot_pur_BF$Marital_Status)

View(tot_pur_BF)
# get training and test data (80/20)
rownum <- sample(1:nrow(tot_pur_BF),size = 0.8*nrow(tot_pur_BF))
train <- tot_pur_BF[rownum,]
test <- tot_pur_BF[-rownum,]

# Baseline model - predict the mean of the training data
base_mean <- mean(train$Purchased)
base_mean

# Evaluate RMSE and MAE on the testing data
RMSE_base <- sqrt(mean((base_mean-test$Purchased)^2))
RMSE_base

MAE_base <- mean(abs(base_mean-test$Purchased))
MAE_base

# linear model
lm_model <- lm(Purchased ~., data = train)
summary(lm_model)
plot(lm_model)
## there are some non linear relations between Purchase amount and other two variables
## The residuals are not completely normally distributed is another concern. We can remove the tails of these outliers which have high variance
## Our prediction may vary as the scale location plot is not horizontal
## Even though there is no point beyond Cook's distance there are some rows which are affecting the
#regression model, So we neede to remove these outliers 
lm_pred <- predict(lm_model, test)
lm_model_rmse <- sqrt(mean((lm_pred-test$Purchased)^2))
lm_model_rmse
lm_model_mae <- mean(abs(lm_pred-test$Purchased))
lm_model_mae

# random forest
library(randomForest)
rf_model <- randomForest(Purchased ~., data = train)
plot(rf_model)
rf_pred <- predict(rf_model, test)
rf_model_rmse <- sqrt(mean((rf_pred-test$Purchased)^2))
rf_model_mae <- mean(abs(rf_pred-test$Purchased))

# stepwise regression
base_model <- glm(Purchased ~ 1, data = train)
whole_model <- glm(Purchased ~ ., data = train)
step_model <- step(base_model, scope = list(lower = base_model, upper = whole_model), direction = "forward")
stepwise_purchase_prediction <- predict(step_model)
swr_rmse <- sqrt(mean((stepwise_purchase_prediction-train$Purchased)^2))
swr_mae <- mean(abs(stepwise_purchase_prediction-train$Purchased))

# rpart Decision Tree
library(rpart)
rpart_model <- rpart(Purchased ~., train)
plot(rpart_model)
rpart_pred <- predict(rpart_model, test)
rpart_rmse <- sqrt(mean((rpart_pred-test$Purchased)^2))
rpart_mae <- mean(abs(rpart_pred-test$Purchased))

# gxboost

# SVM

# caret package and running k-fold cross validation
# 
# trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE)
# 
# fitControl <- trainControl(## 10-fold CV
#   method = "cv",
#   number = 10,
#   verboseIter = TRUE)
# 
# set.seed(825)
# gbmFit1 <- train(Purchased ~ ., data = tot_pur_BF, 
#                  method = "lm", 
#                  trControl = fitControl)
# gbmFit1
# 
# kfold_model <- train(Purchased ~ ., tot_pur_BF,method = "lm",trControl)
# 
# kfold_prediction <- predict(kfold_model, tot_pur_BF)
# kfold_model_rmse <- sqrt(mean((kfold_prediction-tot_pur_BF$Purchased)^2))
# kfold_model_mae <- mean(abs(kfold_prediction-tot_pur_BF$Purchased))
# 
# ggplot(test, aes(x = lm_model, y = Purchased)) + geom_point(color = "blue", alpha = 0.7) + geom_abline(color = "red") + ggtitle("Linear Model Prediction vs. Real values")
# ggplot(test, aes(x = rf_prediction, y = Purchased)) + geom_point(color = "blue", alpha = 0.7) + geom_abline(color = "red") + ggtitle("Random Forest Prediction vs. Real values")
# ggplot(train, aes(x = stepwise_purchase_prediction, y = Purchased)) + geom_point(color = "blue", alpha = 0.7) + geom_abline(color = "red") + ggtitle("Stepwise Regression Prediction vs. Real values")
# ggplot(test, aes(x = rpart_prediction, y = Purchased)) + geom_point(color = "blue", alpha = 0.7) + geom_abline(color = "red") + ggtitle("rpart Prediction vs. Real values")

# boxplots
# library(ggplot2)
# ggplot2(aes(y = Purchased, x = Gender), data = tot_pur_BF) + geom_boxplot()
# ggplot2(aes(y = Purchased, x = Age), data = tot_pur_BF) + geom_boxplot()
# ggplot2(aes(y = Purchased, x = Occupation), data = tot_pur_BF) + geom_boxplot()
# ggplot2(aes(y = Purchased, x = City_Category), data = tot_pur_BF) + geom_boxplot()
# ggplot2(aes(y = Purchased, x = Stay_In_Current_City_Years), data = tot_pur_BF) + geom_boxplot()
# ggplot2(aes(y = Purchased, x = Marital_Status), data = tot_pur_BF) + geom_boxplot()
# ggplot2(data = tot_pur_BF, aes(x = "", y = Purchased)) + geom_boxplot()
