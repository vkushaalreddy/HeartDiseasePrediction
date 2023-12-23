library(dplyr)
library(tidyr)
library(forcats)
library(Metrics)
library(glmnet)
library(caret)
library(earth)
library(rio)
library(ggplot2)
library(modeltools)
library(class)
library(earth)
library(MLmetrics)
library(nnet)
library(reshape2)

data = import("C:\\Users\\valla\\Desktop\\Shiva All\\Shiva\\DSA IDA\\HW1\\HW\\heartData.csv")

View(data)

str(data)

summary(is.na(data))

NA_DF <- data.frame(matrix(ncol = 3, nrow = 0))
NA_DF

#provide column names
for(i in 1:ncol(data)){
  num_na = sum(is.na(data[[i]]))
  if(num_na > 0){
    NA_DF <- rbind(NA_DF, c(colnames(data)[i], num_na, round(num_na/nrow(data)*100 , 2)))
  }
}

colnames(NA_DF) <- c('column_Name', 'Num_NA', '%_NA')
NA_DF

#---------------------------------
# Numeric Data Quality Report
#---------------------------------

# fetching the numeric data from the train set
train_dataNumeric<- data %>%  dplyr::select(where(is.numeric))

glimpse(train_dataNumeric)
summary(train_dataNumeric)

#As R does not have a method for extracting only Q1 or Q3, user-defined function is written to extract them.
# A function “quantile” in R is used in the above code where 2nd element/value of quantile is
#being stored in Q1 and 4th element/value of quantile is being stored in Q3.

Q1<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[2]
}
Q3<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[4]
}

# This code accepts a numerical vector x as an input parameter and then returns a vector where the
#first element is the length of the input vector (i.e., the number of observations), the second element
#is the number of unique values, the third is the number of missing values, the fourth is the mean
#value of non-missing numeric, etc
myNumericSummary <- function(x){
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm=TRUE),
    min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
    max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}

numericSummary <- train_dataNumeric %>%dplyr::summarise(across(everything(),myNumericSummary))
glimpse(numericSummary)

# column bind some labels to our summary statistics
numericSummary <-cbind(
  stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd"),
  numericSummary)
glimpse(numericSummary)

# pivoting the data and adding a couple more computed values: percent missing and percent unique fields
numericSummaryFinal <- numericSummary %>%
  pivot_longer("BMI":"SleepTime", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*missing/n,
         unique_pct = 100*unique/n) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())
numericSummaryFinal

options(digits=3)
options(scipen=99)
numericSummaryFinal %>% knitr::kable()


#---------------------------------
# Categorical Data Quality Report
#---------------------------------

train_dataFactor<- data %>% dplyr::select(where(is.character)) %>% transmute_all(as.factor)

glimpse(train_dataFactor)

mode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

factorCount <- function(v){
  count <- fct_count(v)
  return(length(count$f))
}

myFactorSummary <- function(x){
  c(length(x), n_distinct(x), sum(is.na(x)),mode(x),factorCount(x))
}

factorSummary <- train_dataFactor %>% dplyr::summarise(across(everything(),~myFactorSummary(.)))
glimpse(factorSummary)

factorSummary <-cbind(
  stat=c("n","unique","missing","Mode", "Factor Count"),
  factorSummary)
glimpse(factorSummary)

factorySummaryFinal <- factorSummary %>%
  pivot_longer("Smoking":"HeartDisease", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*as.numeric(missing)/as.numeric(n),
         unique_pct = 100*as.numeric(unique)/as.numeric(n)) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())
factorySummaryFinal

factorySummaryFinal %>% knitr::kable()

table(data$GenHealth,data$HeartDisease)

#Outliers and Skewness

library("dlookr")
data %>%
  select(BMI:HeartDisease) %>%
  plot_na_pareto(col = "blue", main = "Pareto Chart for Finding the % of NAs in each column")

par(mfrow=c(4,4))
li <- data %>% dplyr::select(c("BMI",	"Smoking",	"AlcoholDrinking",	"Stroke",	"PhysicalHealth",	"MentalHealth",	"DiffWalking",	"Sex","Race",	"Diabetic",	"PhysicalActivity",	"GenHealth",	"SleepTime",	"Asthma",	"KidneyDisease"	,"SkinCancer"))
data %>% plot_outlier(diagnose_outlier(data) %>% filter(outliers_ratio >= 10) %>% select(variables) %>% unlist())

par(mfrow = c(2,2))
for(i in colnames(train_dataNumeric)){
  skewnessVal <- skewness(train_dataNumeric[[i]],na.rm=TRUE)
  hist(train_dataNumeric[[i]],main=skewnessVal,xlab=i)
}


#----------------------------Data Preparation-------------------------------------------------#

data <- data %>%
  mutate(across(-AgeCategory, ~ case_when(
    . == "Yes" ~ 1,
    . == "No" ~ 0,
    . == "Male" ~ 1,
    . == "Female" ~ 0,
    . == "No, borderline diabetes" ~ 0,
    . == "Yes (during pregnancy)" ~ 1,
    . == "Poor" ~ 1,
    . == "Fair" ~ 2,
    . == "Good" ~ 3,
    . == "Very good" ~ 4,
    . == "Excellent" ~ 5,
    . == "White" ~ 1,
    . == "Black" ~ 2,
    . == "Asian" ~ 3,
    . == "American Indian/Alaskan Native" ~ 4,
    . == "Other" ~ 5,
    . == "Hispanic" ~ 6,
    TRUE ~ as.numeric(as.character(.))
  )))

#Convert 'Diabetic' column to integer type
data$Diabetic <- as.integer(data$Diabetic)


#Define the one-hot encoding transformation
transformer <- dummyVars(~ AgeCategory, data = data)

#Apply the transformation
transformed_data <- predict(transformer, newdata = data)
transformed_train_data <- as.data.frame(transformed_data)

dataReal <- bind_cols(transformed_train_data, data)

dataReal <- dataReal %>% select(-AgeCategory)

set.seed(283)
sample <- sample(c(TRUE, FALSE), nrow(dataReal), replace=TRUE, prob=c(0.8,0.2))
train  <- dataReal[sample, ]
test   <- dataReal[!sample, ]

#Print shapes
cat('Shape of training feature:', dim(train), '\n')
cat('Shape of testing feature:', dim(test), '\n')
predData <- subset(test,select = -c(HeartDisease))

View(train)

#-------------------------------Visualizations---------------------------------------------------#

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create a new column for labels
data$Category <- ifelse(data$HeartDisease == 1, "HeartDisease", "Normal")

# Create the plot
ggplot(data = data, aes(x = BMI, fill = Category)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("HeartDisease" = "red", "Normal" = "#fccc79")) +
  labs(title = 'Distribution of Body Mass Index', x = 'BodyMass', y = 'Frequency') +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))


# Load necessary library
library(ggplot2)

# Create the plot
ggplot(data = data, aes(x = SleepTime, fill = Category)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("HeartDisease" = "red", "Normal" = "#fccc79")) +
  labs(title = 'Distribution of SleepTime values', x = 'SleepTime', y = 'Frequency') +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))


ggplot(data = data, aes(x = PhysicalHealth, fill = Category)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("HeartDisease" = "red", "Normal" = "#fccc79")) +
  labs(title = 'Distribution of PhysicalHealth state for the last 30 days', 
       x = 'PhysicalHealth', 
       y = 'Frequency') +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))


ggplot(data = data, aes(x = MentalHealth, fill = Category)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("HeartDisease" = "red", "Normal" = "#fccc79")) +
  labs(title = 'Distribution of MentalHealth state for the last 30 days', 
       x = 'MentalHealth', 
       y = 'Frequency') +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))

correlation <- cor(train)

# Now you can execute your command
correlation_with_heart_disease <- abs(correlation['HeartDisease', ])
# Calculate the absolute values of the correlations with 'HeartDisease', excluding its own correlation
correlation_with_heart_disease <- abs(correlation['HeartDisease', ])
correlation_with_heart_disease <- correlation_with_heart_disease[-which(names(correlation_with_heart_disease) == "HeartDisease")]

# Convert to a dataframe for ggplot
correlation_df <- data.frame(
  Feature = names(correlation_with_heart_disease),
  Correlation = correlation_with_heart_disease
)


# Create the plot
ggplot(correlation_df, aes(x = reorder(Feature, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = 'Distribution of Correlation of Features with Heart Disease', x = '', y = 'Correlation') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(data, aes(x = Diabetic, fill = Category)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 15) +
  scale_fill_manual(values = c("Normal" = "#fccc79", "HeartDisease" = "red")) +
  labs(title = "Distribution of Cases with Yes/No HeartDisease based on previous exposure to Diabetic", 
       x = "Diabetic", 
       y = "Frequency") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))


ggplot(data, aes(x = AgeCategory, fill = Category)) +
  geom_bar(position = "identity") +
  scale_fill_brewer(palette = "YlOrBr") +
  labs(title = "Distribution of Cases with Yes/No HeartDisease According to AgeCategory", 
       x = "AgeCategory", 
       y = "Frequency") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))


data1 <- select_if(data, is.numeric)

correlation <- round(cor(data1), 2)

# Melt the correlation matrix for ggplot2
melted_corr <- melt(correlation)

# Create the heatmap
ggplot(data = melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "brown", mid = "orange", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  labs(x = "", y = "", title = "Correlation Heatmap") +
  geom_text(aes(label = value), color = "black", size = 3) +
  coord_fixed()



#-------------------------------------------MODELLING----------------------------------------------------------#

#MARS Model - Model 1


marsModel <- earth(HeartDisease ~.,data = train,
                   degree=10,
                   nk=180,
                   pmethod="cv",
                   nfold=8,
                   ncross=5)
pred_train_MARS <- predict(marsModel, newdata=predData ,type = 'class')

conf_table_cart_MARS <- table(pred_train_MARS, test$HeartDisease )
conf_cart_MARS<-confusionMatrix(as.factor(pred_train_MARS), as.factor(test$HeartDisease))
print(conf_cart_MARS)


#XGBoost - Model 2

train$HeartDisease <- as.factor(train$HeartDisease)
test$HeartDisease <- as.factor(test$HeartDisease)

trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
tune_grid <- expand.grid(nrounds = 2000,
                         max_depth = 5,
                         eta = 0.05,
                         gamma = 0.01,
                         colsample_bytree = 0.5,
                         min_child_weight = 0,
                         subsample = 0.75)

xgb_fit <- train(HeartDisease ~ ., data = train, method = "xgbTree",
                 trControl = trctrl,
                 preProc = c("center", "scale"),
                 tuneGrid = tune_grid,
                 tuneLength = 20) 

pred_train_xgbm = predict(xgb_fit, predData)
conf_table_cart_xgbm <- table(pred_train_xgbm, test$HeartDisease)
conf_cart_xgbm <- confusionMatrix(as.factor(pred_train_xgbm), as.factor(test$HeartDisease))
print(conf_cart_xgbm)


#Decision Tree - Model 3

fitDT<-rpart(HeartDisease~.,data=train,control=rpart.control(cp=0.001),xval=10)
pred_train_DT <- predict(fitDT, newdata=predData ,type = 'class')
conf_table_cart_DT <- table(pred_train_DT, as.factor(test$HeartDisease))
conf_cart_DT <-confusionMatrix(as.factor(pred_train_DT), as.factor(test$HeartDisease))
print(conf_cart_DT)


#GBM - Gaussian Model 4

traincontrol <- trainControl(method = "repeatedcv", number = 10, repeats = 4, 
                             verboseIter = FALSE, allowParallel = TRUE)

gbm_clf1 <- train(HeartDisease ~ ., data = train, method = "gbm", maximize = FALSE, trControl = traincontrol, 
                  tuneGrid = expand.grid(n.trees = c(100, 200, 500), interaction.depth = c(12),
                                         shrinkage = c(0.01), n.minobsinnode = c(8)), verbose = FALSE)
pred_train_gbm = predict(gbm_clf1, predData)
conf_table_cart_gbm_1 <- table(pred_train_gbm, test$HeartDisease )
conf_cart_gbm_1<-confusionMatrix(as.factor(pred_train_gbm), as.factor(test$HeartDisease))



#Multinominal Logistic  Regression Model 5 

ss<-train
ss$BMI<-as.factor(ss$Occupation)
ss$HeartDisease<-as.factor(ss$HeartDisease)
log_reg <- multinom(HeartDisease~., data=ss)
pred_train_log <- predict(log_reg, newdata=predData,type = 'class')
conf_table_cart_log <- table(pred_train_log, test$HeartDisease )
conf_cart_log<-confusionMatrix(as.factor(pred_train_log), as.factor(test$HeartDisease))
print(conf_cart_log)


#--------------------------------------------------------------------------------------------------------------#