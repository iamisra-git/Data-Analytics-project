library(data.table)
library(psych) 
library(ggplot2)
library(pROC)

#read the train and test files and save it to a variable 
train_data<-read.csv("C:/Users/ISHRA/R/emails_spam_train.csv")
test_data<-read.csv("C:/Users/ISHRA/R/email_spam_test.csv")


# Convert 'traindata' to a data frame
train_dataframe <- as.data.frame(train_data)

# Install and load the DescTools package
install.packages("DescTools")
library(DescTools)


#separating spam emails from non-spam from train dataset
spam_emails <- as.data.frame(train_dataframe[train_dataframe$Email.Class == 1, ])
nonspam_emails <- as.data.frame( train_dataframe[train_dataframe$Email.Class == 0, ])


#check directly
missing_data_spamemails<-colSums(is.na(spam_emails))
missing_data_spamemails
#count how many elements are missing
sum(missing_data_spamemails)
missing <- names(missing_data_spamemails[missing_data_spamemails>0])
missing

# Calculate the mode of non-missing values in the HOLLA column
mode_missing_data_spamemails<- Mode(spam_emails$HOLLA, na.rm = TRUE)
# Impute missing values with the mode
spam_emails$HOLLA[is.na(spam_emails$HOLLA)] <- mode_missing_data_spamemails
# Find the total sum of occurrences of 'HOLLA' for spams
total_sum_holla <- sum(spam_emails['HOLLA'])
print(total_sum_holla)
# Calculating the frequency of a particular column just for checking purpose 
column_B <- spam_emails[, 'HOLLA']  
frequency_inspam <- table(column_B) 
print(frequency_inspam)

# Calculate the mode of non-missing values in the HOLLA column
mode_missing_data_nonspamemails <- Mode(nonspam_emails$HOLLA, na.rm = TRUE)
nonspam_emails$HOLLA[is.na(nonspam_emails$HOLLA)] <- mode_missing_data_nonspamemails
total_nonspam <- sum(nonspam_emails['HOLLA'])
print(total_nonspam)
column_B <- nonspam_emails[, 'HOLLA']  
frequency_innonspam <- table(column_B)  
print(frequency_innonspam)


# Combine spam and non-spam data frames
combined_data <- rbind(spam_emails, nonspam_emails)
combined_data <- combined_data[, -1]
spam_emails <- combined_data[, -1]
nonspam_emails  <- combined_data[, -1]
# Load required library
library(tm)

# Function to remove stop words and columns with significantly different frequency in spam and non-spam datasets
remove_dissimilar_frequency_columns <- function(spam_data, nonspam_data, combined_data, threshold = 0.75) {
  removed_columns <- c()  # Initialize vector to store removed column names
  
  # Get stop words
  stop_words <- stopwords("en")
  
  # Iterate over columns
  for (col in colnames(combined_data)) {
    # Check if the column is a stop word or if sum of frequencies is less than 10
    if (col %in% stop_words || sum(combined_data[[col]]) < 20) {
      removed_columns <- c(removed_columns, col)
      next  # Skip to the next iteration if it's a stop word or sum of frequencies is less than 10
    }
    
    # Calculate sum of frequencies in spam and non-spam datasets
    spam_freq <- sum(spam_data[[col]])
    nonspam_freq <- sum(nonspam_data[[col]])
    
    # Check if the absolute difference in total sum of frequency between spam and non-spam datasets exceeds 75% of either sum
    if (spam_freq < nonspam_freq) {
      if ((nonspam_freq - spam_freq) > 0.75 * spam_freq) {
        removed_columns <- c(removed_columns, col)
      }
    } else {
      if ((spam_freq - nonspam_freq) > 0.75 * nonspam_freq) {
        removed_columns <- c(removed_columns, col)
      }
    }
  }
  
# Remove columns with significantly different frequency and stop words from the combined dataset
  cleaned_data <- combined_data[, !(names(combined_data) %in% removed_columns), drop = FALSE]
  
  return(cleaned_data)
}

# Remove columns with significantly different frequency and stop words in combined dataset
cleaned_combined_data <- remove_dissimilar_frequency_columns(spam_emails, nonspam_emails, combined_data)
# Print cleaned data
print(cleaned_combined_data)
# Calculate total number of columns in the cleaned combined data
total_columns <- ncol(cleaned_combined_data)
# Print the total number of columns
print(total_columns)


library(ROSE)
non_spam_count <- sum(cleaned_combined_data$Email.Class == 0)
non_spam_count
# Calculate the difference between the two counts
difference <-2 * non_spam_count
# Perform oversampling using ROSE
oversampled_train <- ovun.sample(Email.Class ~ ., 
                           data = cleaned_combined_data, 
                           method = "over", 
                           N = difference)$data

oversampled_train
total_rows <- nrow(oversampled_train)
print(total_rows)

cleaned_combined_data$Email.Class <- ifelse(cleaned_combined_data$Email.Class == 0, "nonspam", "spam")

oversampled_train$Email.Class <-ifelse(oversampled_train$Email.Class == 0, "nonspam", "spam")




# Convert 'testdata' to a data frame
test_dataframe <- as.data.frame(test_data)

#perform missing for test 
#separating spam emails from non-spam from train dataset
spam_emails_test <- as.data.frame(test_dataframe[test_dataframe$Email.Class == 1, ])
nonspam_emails_test <- as.data.frame( test_dataframe[test_dataframe$Email.Class == 0, ])


# Calculate the mode of non-missing values in the HOLLA column
mode_missing_data_spamemails_test<- Mode(spam_emails_test$HOLLA, na.rm = TRUE)
# Impute missing values with the mode
spam_emails_test$HOLLA[is.na(spam_emails_test$HOLLA)] <- mode_missing_data_spamemails_test
# Calculate the mode of non-missing values in the HOLLA column
mode_missing_data_nonspamemails_test <- Mode(nonspam_emails_test$HOLLA, na.rm = TRUE)
nonspam_emails_test$HOLLA[is.na(nonspam_emails_test$HOLLA)] <- mode_missing_data_nonspamemails_test



# Combine spam and non-spam data frames
combined_data_test <- rbind(spam_emails_test, nonspam_emails_test)



# Find common column names between combined_data and test_data
common_columns <- intersect(names(cleaned_combined_data), names(combined_data_test))
test_data_filtered <- combined_data_test[, common_columns]



test_data_filtered$Email.Class <- ifelse(test_data_filtered$Email.Class == 0, "nonspam", "spam")

###model 1 
library(caret)

# Convert 'Email.Class' to a factor
oversampled_train$Email.Class <- as.factor(oversampled_train$Email.Class)

# Train logistic regression model
model1 <-  train(
  form = Email.Class ~ .,
  data = oversampled_train,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)
# Summary of the trained model
summary(model1)

predicted_probs_model1 <- predict(model1, newdata = test_data_filtered)
# Compute confusion matrix for model1
conf_matrix_model1 <- confusionMatrix(as.factor(predicted_probs_model1), as.factor(test_data_filtered$Email.Class))
conf_matrix_model1

# Print the performance table for model1
print(predicted_probs_model1)
str(predicted_probs_model1)

predicted_prob <- predict(model1, newdata = test_data_filtered , type="prob")

prob_spam <- predicted_prob$spam

true_labels <- as.factor(test_data_filtered$Email.Class)
true_labels
roc_curve_model1 <- roc(true_labels, prob_spam, levels = rev(levels(true_labels)))

# Calculate AUC
auc_model1 <- auc(roc_curve_model1)
print(paste("AUC Value:", auc_model1))

# Plot ROC curve
plot(roc_curve_model1, main="ROC Curve for Logistic Regression", col="#1c61b6")
abline(a=0, b=1, lty=2, col="red")


# Extract performance metrics for model1
accuracy_model1 <- conf_matrix_model1$overall['Accuracy']
sensitivity_model1 <- conf_matrix_model1$byClass['Sensitivity']
specificity_model1 <- conf_matrix_model1$byClass['Specificity']
pos_pred_model1 <- conf_matrix_model1$byClass['Pos Pred Value']
neg_pred_model1 <- conf_matrix_model1$byClass['Neg Pred Value']
f1_score_model1 <- conf_matrix_model1$byClass['F1']
accuracy_model1
sensitivity_model1
specificity_model1
precision_model1
# Print confusion matrix for model1
print(conf_matrix_model1)

test_data_filtered_model1<-test_data_filtered
test_data_filtered_model1$Predicted_Class<-as.factor(predicted_probs_model1)

# Filter spam and non-spam emails
spam_data_model1 <- test_data_filtered_model1[test_data_filtered_model1$Email.Class == "spam", ]
spam_data_model1
nonspam_data_model1 <- test_data_filtered_model1[test_data_filtered_model1$Email.Class == "nonspam", ]

# Count matches and mismatches for spam
spam_matches_model1 <- sum(spam_data_model1$Email.Class == spam_data_model1$Predicted_Class)
spam_matches_model1 
spam_mismatches_model1 <- sum(spam_data_model1$Email.Class != spam_data_model1$Predicted_Class)

# Count matches and mismatches for non-spam
nonspam_matches_model1 <- sum(nonspam_data_model1$Email.Class == nonspam_data_model1$Predicted_Class)
nonspam_mismatches_model1 <- sum(nonspam_data_model1$Email.Class != nonspam_data_model1$Predicted_Class)

# Create labels for the bar plot
labels_model1 <- c(paste("Spam Matches (", spam_matches_model1, ")", sep = ""),
                   paste("Spam Mismatches (", spam_mismatches_model1, ")", sep = ""),
                   paste("Non-Spam Matches (", nonspam_matches_model1, ")", sep = ""),
                   paste("Non-Spam Mismatches (", nonspam_mismatches_model1, ")", sep = ""))

# Plot a bar chart with proper indexing and labels
barplot(c(spam_matches_model1, spam_mismatches_model1, nonspam_matches_model1, nonspam_mismatches_model1),
        names.arg = labels_model1, 
        col = c("lightblue", "lightgreen", "lightblue", "lightgreen"),
        xlab = "Prediction Outcome", ylab = "Count",
        main = "Predicted vs Original Class Comparison")


####2nd MODEL
# Load required libraries
library(e1071)

# Convert 'Email.Class' to a factor
oversampled_train$Email.Class <- as.factor(oversampled_train$Email.Class)

# Train Naive Bayes model
model2 <- naiveBayes(Email.Class ~ ., data = oversampled_train)

# Summary of the trained model
summary(model2)

# Predict using the trained model on test_data_filtered
predicted_probs_model2 <- predict(model2, newdata = test_data_filtered)

# Compute confusion matrix for model2
conf_matrix_model2 <- confusionMatrix(as.factor(predicted_probs_model2), as.factor(test_data_filtered$Email.Class))
conf_matrix_model2
# Extract performance metrics for model2
accuracy <- conf_matrix_model2$overall['Accuracy']
sensitivity <- conf_matrix_model2$byClass['Sensitivity']
specificity <- conf_matrix_model2$byClass['Specificity']
pos_pred <- conf_matrix_model2$byClass['Pos Pred Value']
neg_pred <- conf_matrix_model2$byClass['Neg Pred Value']
f1_score <- conf_matrix_model2$byClass['F1']
accuracy
sensitivity
specificity
precision
f1_score
predicted_probs <- predict(naive_bayes_mod, newdata = test_data_filtered, type = "raw")
# Extract predicted probabilities for the positive class (spam)
predicted_probs_spam <- predicted_probs[, "spam"]
predicted_probs_spam
# Compute ROC curve
roc_curve_model2 <- roc(test_data_filtered$Email.Class, predicted_probs_spam)

auc_model2 <- auc(roc_curve_model2)
auc_model2
plot(roc_curve_model2, main = "ROC Curve", col = "blue", lwd = 2)
abline(a=0, b=1, lty=2, col="red")


performance_table_model2 <- data.frame(Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1 Score", "AUC"),
                                Value = c(accuracy, sensitivity, specificity, precision, f1_score, auc_model2))

print(performance_table_model2)


test_data_filtered_model2<-test_data_filtered
test_data_filtered_model2$Predicted_Class<-predicted_probs_model2

spam_data_with_model2 <- test_data_filtered_model2[test_data_filtered_model2$Email.Class == "spam", ]
nonspam_data_with_model2 <- test_data_filtered_model2[test_data_filtered_model2$Email.Class == "nonspam", ]

spam_matches_with_model2 <- sum(spam_data_with_model2$Email.Class == spam_data_with_model2$Predicted_Class)
spam_matches_with_model2
spam_mismatches_with_model2 <- sum(spam_data_with_model2$Email.Class != spam_data_with_model2$Predicted_Class)
spam_mismatches_with_model2

nonspam_matches_with_model2 <- sum(nonspam_data_with_model2$Email.Class == nonspam_data_with_model2$Predicted_Class)
nonspam_matches_with_model2
nonspam_mismatches_with_model2 <- sum(nonspam_data_with_model2$Email.Class != nonspam_data_with_model2$Predicted_Class)
nonspam_mismatches_with_model2

labels_with_model2 <- c(paste("Spam Matches (", spam_matches_with_model2, ")", sep = ""),
                        paste("Spam Mismatches (", spam_mismatches_with_model2, ")", sep = ""),
                        paste("Non-Spam Matches (", nonspam_matches_with_model2, ")", sep = ""),
                        paste("Non-Spam Mismatches (", nonspam_mismatches_with_model2, ")", sep = ""))

barplot(c(spam_matches_with_model2, spam_mismatches_with_model2, nonspam_matches_with_model2, nonspam_mismatches_with_model2),
        names.arg = labels_with_model2, 
        col = c("lightblue", "lightgreen", "lightblue", "lightgreen"),
        xlab = "Prediction Outcome", ylab = "Count",
        main = "Predicted vs Original Class Comparison")


###3rd MODEL
# Load required libraries
library(randomForest)

# Convert 'Email.Class' to a factor
oversampled_train$Email.Class <- as.factor(oversampled_train$Email.Class)

# Train Random Forest model
model3 <- randomForest(Email.Class ~ ., data = oversampled_train,trControl = trainControl(method = "cv", number = 5))

# Summary of the trained model
print(model3)

# Predict using the trained model on test_data_filtered
predicted_test_model3 <- predict(model3, newdata = test_data_filtered)

# Compute confusion matrix for model3
conf_matrix_model3 <- confusionMatrix(as.factor(predicted_test_model3), as.factor(test_data_filtered$Email.Class))
conf_matrix_model3
# Extract performance metrics for model3
accuracy_model3 <- conf_matrix_model3$overall['Accuracy']
sensitivity_model3 <- conf_matrix_model3$byClass['Sensitivity']
specificity_model3 <- conf_matrix_model3$byClass['Specificity']
pos_pred_model3 <- conf_matrix_model3$byClass['Pos Pred Value']
neg_pred_model3 <- conf_matrix_model3$byClass['Neg Pred Value']
f1_score_model3 <- conf_matrix_model3$byClass['F1']

# Print performance metrics for model3
accuracy_model3
sensitivity_model3
specificity_model3
precision_model3
f1_score_model3

# Extract predicted probabilities for the positive class (spam) for model3
predicted_probs_spam_model3 <- as.numeric(predict(model3, newdata = test_data_filtered, type = "prob")[, "spam"])

# Compute ROC curve for model3
roc_curve_model3 <- roc(test_data_filtered$Email.Class, predicted_probs_spam_model3)

# Compute AUC for model3
auc_model3 <- auc(roc_curve_model3)
auc_model3
# Plot ROC curve for model3
plot(roc_curve_model3, main = "ROC Curve for Random Forest", col = "#1c61b6")
abline(a = 0, b = 1, lty = 2, col = "red")


# Create a performance table for model3
performance_table_model3 <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1 Score", "AUC"),
  Value = c(accuracy_model3, sensitivity_model3, specificity_model3, precision_model3, f1_score_model3, auc_model3)
)

# Print the performance table for model3
print(performance_table_model3)

test_data_filtered_model3=test_data_filtered
test_data_filtered_model3$Predicted_Class<-predicted_test_model3
# Filter spam and non-spam emails
spam_data_model3 <- test_data_filtered_model3[test_data_filtered_model3$Email.Class == "spam", ]
nonspam_data_model3 <- test_data_filtered_model3[test_data_filtered_model3$Email.Class == "nonspam", ]

# Count matches and mismatches for spam
spam_matches_model3 <- sum(spam_data_model3$Email.Class == spam_data_model3$Predicted_Class)
spam_mismatches_model3 <- sum(spam_data_model3$Email.Class != spam_data_model3$Predicted_Class)

# Count matches and mismatches for non-spam
nonspam_matches_model3 <- sum(nonspam_data_model3$Email.Class == nonspam_data_model3$Predicted_Class)
nonspam_mismatches_model3 <- sum(nonspam_data_model3$Email.Class != nonspam_data_model3$Predicted_Class)

# Create labels for the bar plot
labels_model3 <- c(paste("Spam Matches (", spam_matches_model3, ")", sep = ""),
                   paste("Spam Mismatches (", spam_mismatches_model3, ")", sep = ""),
                   paste("Non-Spam Matches (", nonspam_matches_model3, ")", sep = ""),
                   paste("Non-Spam Mismatches (", nonspam_mismatches_model3, ")", sep = ""))

# Plot a bar chart with proper indexing and labels
barplot(c(spam_matches_model3, spam_mismatches_model3, nonspam_matches_model3, nonspam_mismatches_model3),
        names.arg = labels_model3, 
        col = c("lightblue", "lightgreen", "lightblue", "lightgreen"),
        xlab = "Prediction Outcome", ylab = "Count",
        main = "Predicted vs Original Class Comparison")


####4th MODEL
# Load the required library
library(class)
library(pROC)

# Extract predictors and target variable
X_train <- oversampled_train[, -ncol(oversampled_train)]  # Predictors
y_train <- oversampled_train[, ncol(oversampled_train)]   # Target variable

# Assuming 'test_data_filtered' contains your test data
# Extract predictors from the test data
X_test <- test_data_filtered[, -ncol(test_data_filtered)]  # Predictors

# Set the value of K
K <- 10  # You can adjust this value based on your requirement

# Train the KNN model
model4 <- knn(train = X_train, test = X_test, cl = y_train, k = K)
summary(model4)


# Predict probabilities for ROC curve
predicted_probs_model4 <- as.numeric(model4)

# Compute ROC curve for model4
roc_curve_model4 <- roc(test_data_filtered$Email.Class=="spam", predicted_probs_model4)

# Compute AUC for model4
auc_model4 <- auc(roc_curve_model4)

# Plot ROC curve for model4
plot(roc_curve_model4, main = "ROC Curve for K-Nearest Neighbors", col = "#1c61b6")
abline(a = 0, b = 1, lty = 2, col = "red")

# Print AUC for model4
auc_model4

# Print confusion matrix for model4
conf_matrix_model4 <- confusionMatrix(as.factor(model4), as.factor(test_data_filtered$Email.Class))
print(conf_matrix_model4)

# Extract performance metrics for model3
accuracy_model4 <- conf_matrix_model4$overall['Accuracy']
sensitivity_model4 <- conf_matrix_model4$byClass['Sensitivity']
specificity_model4 <- conf_matrix_model4$byClass['Specificity']
pos_pred_model4 <- conf_matrix_model4$byClass['Pos Pred Value']
neg_pred_model4 <- conf_matrix_model4$byClass['Neg Pred Value']
f1_score_model4 <- conf_matrix_model4$byClass['F1']

# Print performance metrics for model3
accuracy_model4
sensitivity_model4
specificity_model4
precision_model4
f1_score_model4


test_data_filtered_model4<-test_data_filtered
test_data_filtered_model4$Predicted_Class<-predicted_probs_model4
# Filter spam and non-spam emails

spam_data_model4 <- test_data_filtered_model4[test_data_filtered_model4$Email.Class == "spam", ]
nonspam_data_model4 <- test_data_filtered_model4[test_data_filtered_model4$Email.Class == "nonspam", ]

# Count matches and mismatches for spam
spam_matches_model4 <- sum(spam_data_model4$Email.Class == "spam" & spam_data_model4$Predicted_Class == 2)
spam_mismatches_model4 <- sum(spam_data_model4$Email.Class == "spam" & spam_data_model4$Predicted_Class != 2)

# Count matches and mismatches for non-spam
nonspam_matches_model4 <- sum(nonspam_data_model4$Email.Class == "nonspam" & nonspam_data_model4$Predicted_Class == 1)
nonspam_mismatches_model4 <- sum(nonspam_data_model4$Email.Class == "nonspam" & nonspam_data_model4$Predicted_Class != 1)

# Create labels for the bar plot
labels_model4 <- c(paste("Spam Matches (", spam_matches_model4, ")", sep = ""),
                   paste("Spam Mismatches (", spam_mismatches_model4, ")", sep = ""),
                   paste("Non-Spam Matches (", nonspam_matches_model4, ")", sep = ""),
                   paste("Non-Spam Mismatches (", nonspam_mismatches_model4, ")", sep = ""))

# Plot a bar chart with proper indexing and labels
barplot(c(spam_matches_model4, spam_mismatches_model4, nonspam_matches_model4, nonspam_mismatches_model4),
        names.arg = labels_model4, 
        col = c("lightblue", "lightgreen", "lightblue", "lightgreen"),
        xlab = "Prediction Outcome", ylab = "Count",
        main = "Predicted vs Original Class Comparison")







#5th MODEL
# Load required libraries
library(caret)
library(e1071)
library(pROC)

# Set up the train control
ctr <- trainControl(method = "cv", number = 5, sampling = "up")

# Convert 'Email.Class' to a factor
cleaned_combined_data$Email.Class <- as.factor(cleaned_combined_data$Email.Class)

# Train Naive Bayes model
naive_bayes_mod <- naiveBayes(Email.Class ~ ., data =cleaned_combined_data,trControl = ctr)

# Summary of the trained model
print(naive_bayes_mod)

# Predict using the trained model on test_data_filtered
predicted_test_model5 <- predict(naive_bayes_mod, newdata = test_data_filtered)


# Predict using the trained model on test_data_filtered
predicted_probs_model5 <- predict(naive_bayes_mod, newdata = test_data_filtered, type = "raw")

# Extract predicted probabilities for the positive class (spam)
predicted_probs_spam_model5 <- predicted_probs_model5[, "spam"]

# Compute ROC curve
roc_curve_model5 <- roc(test_data_filtered$Email.Class, predicted_probs_spam_model5)

# Compute AUC
auc_model5 <- auc(roc_curve_model5)
print(auc_model5)

# Plot ROC curve
plot(roc_curve_model5, main = "ROC Curve", col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "red")



# Compute confusion matrix
conf_matrix_model5<- confusionMatrix(as.factor(predicted_test_model5), as.factor(test_data_filtered$Email.Class))
print(conf_matrix_model5 )

# Extract performance metrics
accuracy_model5 <- conf_matrix$overall['Accuracy']
sensitivity_model5 <- conf_matrix$byClass['Sensitivity']
specificity_model5  <- conf_matrix$byClass['Specificity']
pos_pred_model5 <- conf_matrix_model5$byClass['Pos Pred Value']
neg_pred_model5 <- conf_matrix_model5$byClass['Neg Pred Value']
f1_score_model5  <- conf_matrix$byClass['F1']

# Combine all performance metrics into a data frame
performance_table <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1 Score"),
  Value = c(accuracy_model5 , sensitivity_model5 , specificity_model5 , precision_model5 , f1_score_model5 )
)

test_data_filtered_model5<-test_data_filtered
test_data_filtered_model5$Predicted_Class<-predicted_test_model5
# Filter spam and non-spam emails
# Filter spam and non-spam emails
spam_data_model5 <- test_data_filtered_model5[test_data_filtered_model5$Email.Class == "spam", ]
nonspam_data_model5 <- test_data_filtered_model5[test_data_filtered_model5$Email.Class == "nonspam", ]

# Display spam data
spam_data_model5 

# Count matches and mismatches for spam
spam_matches_model5 <- sum(spam_data_model5$Email.Class == spam_data_model5$Predicted_Class)
spam_mismatches_model5 <- sum(spam_data_model5$Email.Class != spam_data_model5$Predicted_Class)

# Count matches and mismatches for non-spam
nonspam_matches_model5 <- sum(nonspam_data_model5$Email.Class == nonspam_data_model5$Predicted_Class)
nonspam_mismatches_model5 <- sum(nonspam_data_model5$Email.Class != nonspam_data_model5$Predicted_Class)

# Create labels for the bar plot
labels_model5 <- c(paste("Spam Matches (", spam_matches_model5, ")", sep = ""),
                   paste("Spam Mismatches (", spam_mismatches_model5, ")", sep = ""),
                   paste("Non-Spam Matches (", nonspam_matches_model5, ")", sep = ""),
                   paste("Non-Spam Mismatches (", nonspam_mismatches_model5, ")", sep = ""))

# Plot a bar chart with proper indexing and labels
barplot(c(spam_matches_model5, spam_mismatches_model5, nonspam_matches_model5, nonspam_mismatches_model5),
        names.arg = labels_model5, 
        col = c("lightblue", "lightgreen", "lightblue", "lightgreen"),
        xlab = "Prediction Outcome", ylab = "Count",
        main = "Predicted vs Original Class Comparison")



# Create a summary table for all models
summary_table <- data.frame(
  Model = c("Logistic Regression", "Naive Bayes", "Random Forest", "K-Nearest Neighbors", "Naive Bayes (with CV)"),
  Accuracy = c(accuracy_model1, accuracy, accuracy_model3, accuracy_model4, accuracy_model5),
  Sensitivity = c(sensitivity_model1, sensitivity, sensitivity_model3, sensitivity_model4, sensitivity_model5),
  Nev= c(neg_pred_model1, neg_pred, neg_pred_model3, neg_pred_model4, neg_pred_model5),
  F1_Score = c(f1_score_model1, f1_score, f1_score_model3, f1_score_model4, f1_score_model5),
  AUC = c(auc_model1, auc_model2, auc_model3, auc_model4, auc_model5)
)

# Print the summary table
print(summary_table)

# Plot the summary table
library(ggplot2)

# Melt the summary table for plotting
library(reshape2)
melted_summary <- melt(summary_table, id.vars = "Model")

# Plot the summary table
ggplot(melted_summary, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Performance Summary of Different Models",
       x = "Model", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels and increase font size
        axis.text.y = element_text(size = 10),  # Increase font size of y-axis labels
        axis.title = element_text(size = 12),  # Increase font size of axis titles
        legend.position = "top",  # Move legend to the top
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 10),  # Increase font size of legend text
        plot.title = element_text(size = 14),  # Increase font size of plot title
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.background = element_blank()) +  # Remove panel background
  geom_text(aes(label = round(value, 2)), position = position_dodge(width = 1), vjust = -0.5, size = 3.5)  # Add data labels

# Combine ROC curves into a list
roc_curves <- list(model1 = roc_curve_model1, 
                   model2 = roc_curve_model2, 
                   model3 = roc_curve_model3, 
                   model4 = roc_curve_model4, 
                   model5 = roc_curve_model5)

# Extracting and plotting ROC curves for each model
for (i in 1:length(roc_curves)) {
  curve <- roc_curves[[i]]
  plot(curve, col = c("red", "blue", "green", "orange", "purple")[i], add = i > 1, main = "ROC Curves for Different Models")
}
legend("bottomright", legend = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"), col = c("red", "blue", "green", "orange", "purple"), lty = 1)