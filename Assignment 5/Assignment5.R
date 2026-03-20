library(ggfortify)
library(ggplot2)
library(stats)
library(caret)
library(tidyverse)
library(randomForest)
library(class)

nyc_dataset <- read.csv("C:/Users/IDWT/OneDrive - Rensselaer Polytechnic Institute/Desktop/Spring 2026 Classes/Data Analytics/Assignment Submissions/Assignment 5/NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")

## ----- Regression (1a) -----
# Finding all unique values in neighborhood and will choose one 
unique(nyc_dataset$NEIGHBORHOOD)
unique(nyc_dataset$EASE.MENT)


# Filtering for columns that are in Manhattan borough
nyc_filtered <- nyc_dataset
nyc_filtered$EASE.MENT <- NULL
colSums(is.na(nyc_filtered))
Borough_filtered <- nyc_filtered[nyc_filtered$BOROUGH == 1,]

# Cleaning borough subset
colSums(is.na(Borough_filtered))
cleaned_borough_filtered <- na.omit(Borough_filtered)


# Cleaning neighborhood subset
# Subset with only 3-4 neighborhoods
Neighborhood_filtered <- Borough_filtered[
  Borough_filtered$NEIGHBORHOOD %in% c("CHINATOWN", "SOHO", "EAST VILLAGE"),
]

cleaned_neighborhood_filtered <- na.omit(Neighborhood_filtered)
colSums(is.na(cleaned_neighborhood_filtered))

#  EDA 
hist(cleaned_borough_filtered$YEAR.BUILT)

summary(cleaned_borough_filtered$RESIDENTIAL.UNITS)

ggplot(
  subset(cleaned_borough_filtered, SALE.PRICE != "0"),
  aes(
    x = as.Date(SALE.DATE, format = "%m/%d/%Y"),
    y = as.numeric(gsub(",", "", SALE.PRICE))
  )
) +
  geom_point(alpha = 0.4, color = "darkorange") +
  scale_y_log10() +
  labs(
    title = "Sale Price vs Sale Date (Manhattan)",
    x = "Sale Date",
    y = "Sale Price (log10)"
  ) +
  theme_minimal()

# Sale Price outliers
ggplot(
  subset(cleaned_borough_filtered, as.numeric(gsub(",", "", SALE.PRICE)) > 0),
  aes(y = as.numeric(gsub(",", "", SALE.PRICE)))
) +
  geom_boxplot(fill = "skyblue", outlier.alpha = 0.35) +
  scale_y_log10() +
  labs(title = "Sale Price — Boxplot (log scale)",
       y = "Sale Price (log10)") +
  theme_minimal()

# ----- Regression Analysis (1b) ------
lmod <- lm(SALE.PRICE~SALE.DATE + RESIDENTIAL.UNITS, data = cleaned_borough_filtered)
lmod2 <- lm(SALE.PRICE~SALE.DATE + COMMERCIAL.UNITS, data = cleaned_borough_filtered)
lmod3 <- lm(SALE.PRICE~GROSS.SQUARE.FEET + Council.District, data = cleaned_borough_filtered)

summary(lmod)
summary(lmod2)
summary(lmod3)

## ----- Classification (2a) -----
# Model data
model_df <- cleaned_neighborhood_filtered %>%
  mutate(NEIGHBORHOOD = factor(NEIGHBORHOOD),
         SALE.PRICE = as.numeric(gsub(",", "", SALE.PRICE)),
         GROSS.SQUARE.FEET = as.numeric(gsub(",", "", GROSS.SQUARE.FEET)),
         LAND.SQUARE.FEET = as.numeric(gsub(",", "", LAND.SQUARE.FEET))) %>%
  select(NEIGHBORHOOD, SALE.PRICE, GROSS.SQUARE.FEET, LAND.SQUARE.FEET,
         RESIDENTIAL.UNITS, COMMERCIAL.UNITS, TOTAL.UNITS, YEAR.BUILT) %>%
  drop_na()


set.seed(42)
s.train <- sample(nrow(model_df), floor(0.7 * nrow(model_df)))
train_df <- model_df[s.train, ]
test_df  <- model_df[-s.train,] 

# kNN
# Scale features
pp <- preProcess(train_df[,-1], method = c("center","scale"))
X.train <- predict(pp, train_df[,-1])
X.test  <- predict(pp,  test_df[,-1])

# Fitting model
knn.pred <- knn(X.train, X.test, train_df$NEIGHBORHOOD, k = 3)

# Confusion table 
table(knn.pred, test_df$NEIGHBORHOOD, dnn = c("predicted","actual"))

## Random forest
rf.model <- randomForest(NEIGHBORHOOD ~ ., data = train_df)

rf.pred <- predict(rf.model, test_df)

table(rf.pred, test_df$NEIGHBORHOOD, dnn = c("predicted","actual"))


# Confusion Matrices
## kNN
knn_cm <- caret::confusionMatrix(
  data = knn.pred,                
  reference = test_df$NEIGHBORHOOD, 
  mode = "everything"             
)

## Random Forest
rf_cm <- caret::confusionMatrix(
  data = rf.pred,                 
  reference = test_df$NEIGHBORHOOD, 
  mode = "everything"
)

print(knn_cm)
print(rf_cm)

# ----- Questions -----

# 1b.)  For lmod and lmod2, there was not a high linear correlation between
#       Sale price and the chosen variables. The R^2 value was 0.38 and 0.37
#       respectively. lmod3 had the highest R^2 squared value of 0.99, showing
#       that Sale Price and the chosen predictors are highly correlated. 
#       For the data cleaning for the regression analysis, I made a copy of
#       the dataset and made all values in EASE.ment null. Honestly, I 
#       should of deleted this column since all of the values were null.
#       From there, I filtered for only the Manhattan borough and cleared
#       any null values. 

# 2a.) The final results of my classification models resulted in 
#      my kNN having a 69% accuracy, 50% precision, 25% recall, and 
#      33% F1 score. For the cleaning and formatting of my classification
#      data, I filtered my Borough_filtered data to only include 3 neighborhoods.
#      I did this because it was easier to use the copy I created rather than
#      making another copy. The best performing neighborhood was East Village
#      while the worst performing neighborhood was Chinatown. 

# 3.) Final conclusions of the quality of this dataset is that there was a lot of 
#     null values and trying to figure out how to subset the data for both
#     regression and classification was the most difficult part. The linear 
#     regression models went smoothly despite the low correlation, however, kNN was giving me errors 
#     based on nonidentical lengths or my target variable not being a factor.
#     Also, a lot of the variables do not seem relevant for prediction or it is
#     not clear what they mean. 