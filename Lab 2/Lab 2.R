library("ggplot2")
library("readr")

NY_House_Dataset <- read_csv("C:/Users/IDWT/OneDrive - Rensselaer Polytechnic Institute/Desktop/Spring 2026 Classes/Data Analytics/Labs/Lab 2/NY-House-Dataset.csv")

data <- NY_House_Dataset

# null_data <- sum(is.na(data))
filtered_dataset <- data[data$PRICE != max(data$PRICE),]

# ----- First linear model (Predicting Price with PropertySqFt) ----

# Finding outliers
ggplot(filtered_dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()


# Building linear model 
lmod0 <- lm(PRICE~PROPERTYSQFT, data = filtered_dataset)

summary(lmod0)

ggplot(filtered_dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

# ----- Second linear model (Predicting Price with Beds) ----
ggplot(filtered_dataset, aes(x = BEDS, y = PRICE)) +
  geom_point()

# Building linear model 
lmod0 <- lm(PRICE~BEDS, data = filtered_dataset)

summary(lmod0)

ggplot(filtered_dataset, aes(x = BEDS, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

# ----- Third linear model (Predicting Price with Bath) ----
ggplot(filtered_dataset, aes(x = BATH, y = PRICE)) +
  geom_point()

filtered_dataset <- data[data$PRICE<1900000000,]

# Building linear model 
lmod0 <- lm(PRICE~BATH, data = filtered_dataset)

summary(lmod0)

ggplot(filtered_dataset, aes(x = BATH, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

