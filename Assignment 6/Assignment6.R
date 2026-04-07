library(ggfortify)
library(ggplot2)
library(stats)
library(caret)
library(tidyverse)
library(randomForest)
library(class)

dataset <- read.csv("C:/Users/IDWT/OneDrive - Rensselaer Polytechnic Institute/Desktop/ITWS-4600-dewiti/Assignment 6/DATA (1).csv")
colnames(dataset) <- c(
  "student_id",
  "student_age",
  "sex",
  "high_school_type",
  "scholarship_type",
  "additional_work",
  "artistic_or_sports_activity",
  "has_partner",
  "total_salary",
  "transportation_type",
  "accommodation_type",
  "mother_education",
  "father_education",
  "num_siblings",
  "parental_status",
  "mother_occupation",
  "father_occupation",
  "weekly_study_hours",
  "reading_freq_nonscientific",
  "reading_freq_scientific",
  "seminar_attendance",
  "project_impact",
  "class_attendance",
  "midterm_prep_1",
  "midterm_prep_2",
  "note_taking",
  "listening_in_class",
  "discussion_improves_success",
  "flip_classroom",
  "gpa_last_semester",
  "expected_gpa",
  "course_id",
  "grade"
)

dataset <- dataset %>%
  mutate(across(
    -c(student_id, course_id, grade),
    as.factor
  ))

final_dataset <- dataset %>%
  select(
    -student_id    
  )
# ----- 1.) EDA -----
sum(is.na(dataset))
str(dataset)

# Distribution of Final Grades
ggplot(dataset, aes(x = grade)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Final Grades",
    x = "Grade",
    y = "Number of Students"
  ) +
  theme_minimal()

# Relationship between Grades and Study Hours
ggplot(dataset, aes(x = weekly_study_hours, y = grade)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Grade vs Weekly Study Hours",
    x = "Weekly Study Hours",
    y = "Grade"
  ) +
  theme_minimal()

# Relationship between attendance and grade
ggplot(dataset, aes(x = class_attendance, y = grade)) +
  geom_boxplot(fill = "lightcoral") +
  labs(
    title = "Grade vs Class Attendance",
    x = "Class Attendance",
    y = "Grade"
  )+
  theme_minimal()


boxplot(dataset$grade, main = "Boxplot of Final Grade")

sd(final_dataset$grade)

# ----- 2.) Model Development -----

## Linear Regression
edu_regression <- lm(grade~., data = final_dataset)
summary(edu_regression)

# Cross validation
train_control <- trainControl(method = "cv", number = 10)
edu_control_regression <- train(grade~., data = final_dataset, method = "lm",  trControl = train_control)

# Cross validated results
print(edu_control_regression)
print(edu_control_regression$results)

## Classification
e.train <- sample(145,100) 
e.train

edu.train <- final_dataset[e.train,]
edu.test <- final_dataset[-e.train,] 

# kNN model
knn.predicted <- knn(edu.train[,1:31], edu.test[,1:31], edu.train[,32], k=3)
table(knn.predicted, edu.test[,32], dnn=list('predicted','actual'))

# kNN cross validation & results
knn.mod <- train(grade~., data = final_dataset, method = "knn",  trControl = train_control)
print(knn.mod)
print(knn.mod$results)

# Random forest model
rf.model <- randomForest(grade~., data=edu.train)
rf.predicted <- predict(rf.model, edu.test)
table(rf.predicted, edu.test$grade, dnn=list('predicted','actual'))

# Random forest cross validation & results
rf.mod <- train(grade~., data = final_dataset, method = "rf",  trControl = train_control)
print(rf.mod)
print(rf.mod$results)

## Metrics

# Linear regression metrics
lm_pred <- predict(edu_control_regression, final_dataset)
lm_actual <- final_dataset$grade

rmse_lm <- RMSE(lm_pred, lm_actual)
mae_lm  <- MAE(lm_pred, lm_actual)
r2_lm   <- R2(lm_pred, lm_actual)

lm_metrics <- data.frame(
  Model = "Linear Regression",
  RMSE = rmse_lm,
  MAE = mae_lm,
  R2 = r2_lm
)
lm_metrics

# kNN metrics

knn_cm <- caret::confusionMatrix(
  data = knn.predicted,
  reference = as.factor(edu.test$grade), 
  mode = "everything"             
)
knn_cm

# RF metrics
rf_pred <- as.factor(rf.predicted)
rf_true <- as.factor(edu.test$grade)

common_levels <- intersect(levels(rf_pred), levels(rf_true))

rf_pred <- factor(rf_pred, levels = common_levels)
rf_true <- factor(rf_true, levels = common_levels)

rf_matrix <- table(
  Actual = rf_true,
  Predicted = rf_pred
)

rf_matrix

n <- sum(rf_matrix)
diag_vals <- diag(rf_matrix)
row_sums <- rowSums(rf_matrix)
col_sums <- colSums(rf_matrix)

rf_accuracy <- sum(diag_vals) / n

rf_precision <- diag_vals / col_sums
rf_recall <- diag_vals / row_sums

rf_f1 <- 2 * rf_precision * rf_recall /
  (rf_precision + rf_recall)

rf_metrics <- data.frame(
  Model = "Random Forest",
  Accuracy = rf_accuracy,
  Precision = mean(rf_precision, na.rm = TRUE),
  Recall = mean(rf_recall, na.rm = TRUE),
  F1 = mean(rf_f1, na.rm = TRUE)
)

rf_metrics

