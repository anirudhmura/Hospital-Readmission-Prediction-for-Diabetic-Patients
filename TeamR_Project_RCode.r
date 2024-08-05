## 1. Loading the relevant libraries
library(tree)
library("randomForest")
library(dplyr)
library(caret)
library(purrr)
library(class)

## 2. Reading the data
diabetic_data_og <- read.csv('diabetic_data_og.csv')

## 3. Data exploration
# 3.1 Finding missing values
sapply(diabetic_data_og, function(x) paste(round((sum(x == '?')/length(x))*100,
                                                 2), "%", sep = ""))

# 3.2 Checking number of unique values in all columns
rapply(diabetic_data_og,function(x)length(unique(x)))

# 3.3 Choosing first patient encounter and dropping the rest
diabetic_data_og <- diabetic_data_og %>% 
  group_by(patient_nbr) %>% 
  mutate(readmitted_n = ifelse(readmitted[row_number() + 1] == '<30', 1, 0),
         readmitted_n = c(readmitted_n[-n()], 0))

diabetic_data_og <- diabetic_data_og[!duplicated(diabetic_data_og$patient_nbr),]
diabetic_data_og <- within(diabetic_data_og, readmitted_n[readmitted == '<30'] <- 1)

# 3.4 Reducing and grouping feature values for diagnoses
diabetic_data_og <- diabetic_data_og %>%  
  mutate(diag_1_n = case_when(diag_1 %in% 390:459 | diag_1 == 785 ~ "circulatory",
                         diag_1 %in% 460:520 | diag_1 == 786 ~ "respiratory",
                         diag_1 %in% 520:580 | diag_1 == 787 ~ "digestive",
                         diag_1 == 250 ~ "diabetes",
                         diag_1 %in% 800:1000 ~ "injury",
                         diag_1 %in% 710:740 ~ "musculoskeletal",
                         diag_1 %in% 580:630 | diag_1 == 788 ~ "genitourinary",
                         diag_1 %in% 140:240 ~ "neoplasms",
                         diag_1 %in% 630:680 ~ "pregnecy",
                         grepl("[VE]", diag_1) ~ "other"))

diabetic_data_og <- diabetic_data_og %>%  
  mutate(diag_2_n = case_when(diag_2 %in% 390:459 | diag_2 == 785 ~ "circulatory",
                              diag_2 %in% 460:520 | diag_2 == 786 ~ "respiratory",
                              diag_2 %in% 520:580 | diag_2 == 787 ~ "digestive",
                              diag_2 == 250 ~ "diabetes",
                              diag_2 %in% 800:1000 ~ "injury",
                              diag_2 %in% 710:740 ~ "musculoskeletal",
                              diag_2 %in% 580:630 | diag_2 == 788 ~ "genitourinary",
                              diag_2 %in% 140:240 ~ "neoplasms",
                              diag_2 %in% 630:680 ~ "pregnecy",
                              grepl("[VE]", diag_2) ~ "other"))

diabetic_data_og <- diabetic_data_og %>%  
  mutate(diag_3_n = case_when(diag_3 %in% 390:459 | diag_3 == 785 ~ "circulatory",
                              diag_3 %in% 460:520 | diag_3 == 786 ~ "respiratory",
                              diag_3 %in% 520:580 | diag_3 == 787 ~ "digestive",
                              diag_3 == 250 ~ "diabetes",
                              diag_3 %in% 800:1000 ~ "injury",
                              diag_3 %in% 710:740 ~ "musculoskeletal",
                              diag_3 %in% 580:630 | diag_3 == 788 ~ "genitourinary",
                              diag_3 %in% 140:240 ~ "neoplasms",
                              diag_3 %in% 630:680 ~ "pregnecy",
                              grepl("[VE]", diag_3) ~ "other"))

# 3.5 Handling NA values in diagnoses
diabetic_data_og["diag_1_n"][is.na(diabetic_data_og["diag_1_n"])] <- 'other'
diabetic_data_og["diag_2_n"][is.na(diabetic_data_og["diag_2_n"])] <- 'other'
diabetic_data_og["diag_3_n"][is.na(diabetic_data_og["diag_3_n"])] <- 'other'

# 3.6 Removing specific discharge dispositions
diabetic_data_og <- diabetic_data_og[!diabetic_data_og$discharge_disposition_id 
                                     %in% c(11,13,14,19,20,21), ]

# 3.7 Dropping columns with missing values over 45%, id columns, and unique values of 1
diabetic_data_1 <- diabetic_data_og[, -c(1, 2, 6, 12, 19, 20, 21, 40, 41, 45, 50)]

## 4. Random Forest Model
# 4.1 Convert target variable to factor
diabetic_data_1$readmitted_n <- factor(diabetic_data_1$readmitted_n)

# 4.2 Split data into train and test sets
set.seed(123457)
inTrain <- sample(nrow(diabetic_data_1), 0.7*nrow(diabetic_data_1))
dftrain <- data.frame(diabetic_data_1[inTrain,])
dftest <- data.frame(diabetic_data_1[-inTrain,])

# 4.3 Train Random Forest model
rf_model <- randomForest(readmitted_n ~ ., data = dftrain)
rf_model

# 4.4 Make predictions on the test data
predictions <- predict(rf_model, dftest)

# 4.5 Evaluate the accuracy of the model  
accuracy <- mean(predictions == dftest$readmitted_n)
print(paste0("Accuracy: ", accuracy*100, "%"))

# 4.6 Plotting tree results
library(ggraph)
library(igraph)

# 4.7 Function to plot a single tree from the Random Forest
tree_func <- function(final_model,tree_num) {
  # ... (function code remains the same)
}

# 4.8 Save the tree plot
ggsave(tree_func(rf_model,1),file="rf_tree.png",width=12,height=8)
plot(rf_model)

# 4.9 Plot variable importance
library(ggplot2)
var_importance <- data_frame(variable=setdiff(colnames(dftrain), "readmitted_n"),
                             importance=as.vector(importance(rf_model)))
var_importance <- arrange(var_importance, desc(importance))
var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)

p <- ggplot(var_importance, aes(x=variable, weight=importance, fill=variable))
p <- p + geom_bar() + ggtitle("Variable Importance from Random Forest Fit")
p <- p + xlab("Demographic Attribute") + ylab("Variable Importance (Mean Decrease in Gini Index)")
p <- p + scale_fill_discrete(name="Variable Name")
p + theme(axis.text.x=element_blank(),
          axis.text.y=element_text(size=12),
          axis.title=element_text(size=16),
          plot.title=element_text(size=18),
          legend.title=element_text(size=16),
          legend.text=element_text(size=12))

## 5. Logistic Regression Model for Feature Selection
# 5.1 Change nominal data columns to factors
cols.num <- c("race", "gender", "age", "admission_type_id", "discharge_disposition_id",
              "admission_source_id", "payer_code", "max_glu_serum",
              "A1Cresult", "metformin", "repaglinide", "nateglinide", "chlorpropamide",
              "glimepiride", "acetohexamide", "glipizide", "glyburide", "tolbutamide",
              "pioglitazone", "rosiglitazone", "acarbose", "miglitol", "troglitazone",
              "tolazamide", "insulin", "glyburide.metformin", "glipizide.metformin",
              "metformin.rosiglitazone", "metformin.pioglitazone", "change",
              "diabetesMed", "diag_1_n", "diag_2_n", "diag_3_n")
diabetic_data_1[cols.num] <- lapply(diabetic_data_1[cols.num], factor)

# 5.2 Fit logistic regression model
reg_model <- glm("readmitted_n ~ .", diabetic_data_1, family='binomial')

summary(reg_model)

## 6. Neural Network model
install.packages("neuralnet")
library(neuralnet)
library(fastDummies)

# 6.1 Removing columns not statistically or medically significant in log regression
diabetic_data_2 <- diabetic_data_1[ , !names(diabetic_data_1) %in% 
                       c("number_outpatient", "admission_type_id", "metformin",
                     "repaglinide", "nateglinide", "chlorpropamide", "glimepiride",
                     "glipizide", "glyburide", "tolbutamide", "pioglitazone",
                    "rosiglitazone", "acarbose", "miglitol", "troglitazone",
                   "tolazamide", "glyburide.metformin", "metformin.rosiglitazone")]

# 6.2 Create dummy variables for categorical features
cols.num.f <- c("race", "gender", "age", "discharge_disposition_id",
              "admission_source_id", "payer_code", "max_glu_serum",
              "A1Cresult", "acetohexamide", "insulin", "glipizide.metformin",
              "metformin.pioglitazone", "change", "diabetesMed", "diag_1_n", 
              "diag_2_n", "diag_3_n")
data <- dummy_cols(diabetic_data_2, select_columns = cols.num.f)

# 6.3 Dropping original factor variables
data <- data[, -c(1, 2, 3, 4, 5, 7, 14, 15, 16, 17, 18, 19, 20, 21, 23, 24, 25)]

# 6.4 Normalize numeric variables
normalize <- function(x) {return((x - min(x))/ (max(x) - min(x)))}
data[1:7] <- normalize(data[1:7])

# 6.5 Split data into training and testing sets
set.seed(123457)
inTrain.n <- sample(nrow(data), 0.8*nrow(data))
dftrain.n <- data.frame(data[inTrain.n,])
dftest.n <- data.frame(data[-inTrain.n,])

# 6.6 Load Keras and TensorFlow libraries
library(tensorflow)
library(keras)

# 6.7 Define and compile the neural network model
model <- keras_model_sequential() %>%
  layer_dense(units=64, activation="relu", input_shape=ncol(dftrain.n)-1) %>% 
  layer_dropout(rate = 0.005) %>%
  layer_dense(units=64, activation="relu") %>% 
  layer_dropout(rate = 0.005) %>%
  layer_dense(units=1, activation="sigmoid")

model %>% compile(
  loss="binary_crossentropy",
  optimizer="adam",
  metrics="accuracy"
)

# 6.8 Train the model
history <- model %>% fit(
  x=as.matrix(dftrain.n[, -8]),
  y=as.matrix(as.numeric(dftrain.n[, 8])),
  validation_split=0.3,
  epochs=10,
  batch_size=120
)

# 6.9 Evaluate model on test data
model %>% evaluate(
  x=as.matrix(dftest.n[, -8]),
  y=as.numeric(dftest.n$readmitted_n)
)

# 6.10 Display model summary
summary(model)

## 7. KNN Model
# 7.1 Function to encode a single column
encode_col <- function(col) {
  levels <- unique(col)
  values <- seq_along(levels)
  setNames(values, levels)[col]
}

# 7.2 Encode columns 1-10
encoded_cols <- map(diabetic_data_1[cols.num], encode_col)

# 7.3 Replace original columns with encoded columns
diabetic_data_1[cols.num] <- encoded_cols

# 7.4 Standardize numeric columns
numeric_cols <- c(7, 9, 10, 11, 12, 13, 14, 15)
diabetic_data_1[numeric_cols] <- lapply(diabetic_data_1[numeric_cols],
                                        function(x) (x - mean(x)) / sd(x))

# 7.5 Remove less significant features
diabetic_data_3 <- diabetic_data_1[ , !names(diabetic_data_1) %in% 
            c("number_outpatient", "admission_type_id", "metformin",
              "repaglinide", "nateglinide", "chlorpropamide", "glimepiride",
              "glipizide", "glyburide", "tolbutamide", "pioglitazone",
              "rosiglitazone", "acarbose", "miglitol", "troglitazone",
              "tolazamide", "glyburide.metformin", "metformin.rosiglitazone")]

# 7.6 Split the dataset into training and testing sets
set.seed(123457)
train_index <- createDataPartition(diabetic_data_3$readmitted_n, p = 0.8,
                                   list = FALSE)
X_train <- diabetic_data_3[train_index, -22]
X_test <- diabetic_data_3[-train_index, -22]
y_train <- diabetic_data_3[train_index, 22]
y_test <- diabetic_data_3[-train_index, 22]

# 7.7 Set the number of folds for cross-validation
num_folds <- 10

# 7.8 Create a KNN classifier for cross-validation
knn_cv_model <- train(x = X_train, y = y_train$readmitted_n, method = "knn",
                      tuneGrid = data.frame(k = 1:10),
                      trControl = trainControl(method = "cv", 
                                               number = num_folds))

# 7.9 Plot the cross-validation scores for different values of k
plot(knn_cv_model, xlab = "k", ylab = "Accuracy")

# 7.10 Find the best k value
best_k <- knn_cv_model$bestTune$k
print(paste("Best k value:", best_k))

# 7.11 Create a KNN classifier with the best k value
knn_model_best_k <- knn(train = X_train, test = X_test, cl = y_train$readmitted_n, k = best_k)

# 7.12 Evaluate the accuracy of the model on the testing data
accuracy_best_k <- mean(knn_model_best_k == y_test$readmitted_n)
print(paste("Accuracy with best k value:", accuracy_best_k))

# 7.13 Generate and display the confusion matrix
confusionMatrix(table(knn_model_best_k, y_test$readmitted_n))
