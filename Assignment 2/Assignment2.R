library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(stats)

results <- read_csv("C:/Users/IDWT/OneDrive - Rensselaer Polytechnic Institute/Desktop/Spring 2026 Classes/Data Analytics/Assignment Submissions/EPI Results/epi_results_2024_pop_gdp.csv")

NAs <- is.na(results)
results_complete <- results[complete.cases(results), ]

# Histogram code
hist(results_complete$gdp, main = "GDP Histogram", prob=TRUE)
lines(density(results_complete$gdp,bw="SJ"))
rug(results_complete$gdp)

# Boxplot code
boxplot(gdp ~ region,
       data = results_complete,
       main = "GDP by Region",
       ylab = "GDP",
       las = 2,          # rotate x-axis labels
       cex.axis = 0.8)   # smaller text so labels fit

# Derived subsets 
results_complete_Latin_Carribean <- filter(results_complete, region == "Latin America & Caribbean")
results_complete_Asia_Pacific <- filter(results_complete, region == "Asia-Pacific")

# Derived subset boxplots
boxplot(gdp ~ region,
        data = results_complete_Latin_Carribean,
        main = "GDP for Latin America & Caribbean",
        xlab = "",
        ylab = "GDP",
        las = 2,          # rotate x-axis labels
        cex.axis = 0.8)   # smaller text so labels fit

boxplot(gdp ~ region,
        data = results_complete_Asia_Pacific,
        main = "GDP for Asia-Pacific",
        xlab = "",
        ylab = "GDP",
        las = 2,          # rotate x-axis labels
        cex.axis = 0.8)   # smaller text so labels fit

# QQ Plot between derived subsets
qqnorm(results_complete_Latin_Carribean$gdp); qqline(results_complete_Latin_Carribean$gdp)

# ----- Linear models (part 3) ------

# Linear models (since I chose GDP for previous problem, will choose EPI.New instead)

LC_model <- lm(population~EPI.new, data = results_complete_Latin_Carribean)
summary(LC_model)
plot(LC_model)

regions <- unique(results_complete$region)

chosen_var <- "EPI.new"   # change if needed

stopifnot(all(c("region", "population", "gdp", chosen_var) %in% names(results_complete)))

results_complete_min <- results_complete %>%
  dplyr::select(region, population, gdp, !!rlang::sym(chosen_var)) %>%
  dplyr::filter(stats::complete.cases(.))

min_n_scatter <- 3
min_n_diag    <- 5

n_complete <- function(df, vars) {
  sum(stats::complete.cases(df[, vars, drop = FALSE]))
}

safe_plot_lm <- function(mod, ok_for_diag) {
  if (!ok_for_diag) {
    message("Skipping residual plots: insufficient data for stable diagnostics.")
    return(invisible(NULL))
  }
  op <- par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
  on.exit(par(op), add = TRUE)
  plot(mod, id.n = 0)
}

regions <- unique(results_complete_min$region)

for (reg in regions) {
  df <- results_complete_min %>% dplyr::filter(region == reg)
  
  cat("\n==============================\n")
  cat("Region:", reg, "\n")
  cat("==============================\n")
  
  # --- 3.1.a Population vs chosen_var ---
  vars_31a <- c(chosen_var, "population")
  n31a <- n_complete(df, vars_31a)
  
  if (n31a >= min_n_scatter) {
    p1 <- ggplot(df, aes(x = .data[[chosen_var]], y = population)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = paste("Population vs", chosen_var, "—", reg),
           x = chosen_var, y = "Population")
    print(p1)
    
    m1 <- lm(as.formula(paste("population ~", chosen_var)), data = df)
    cat("\nModel: population ~", chosen_var, "\n")
    print(summary(m1))
    safe_plot_lm(m1, ok_for_diag = (n31a >= min_n_diag))
  } else {
    message(sprintf("Skipping 3.1 Population plot for '%s': only %d complete rows (need ≥ %d).",
                    reg, n31a, min_n_scatter))
  }
  
  # --- 3.1.b GDP vs chosen_var ---
  vars_31b <- c(chosen_var, "gdp")
  n31b <- n_complete(df, vars_31b)
  
  if (n31b >= min_n_scatter) {
    p2 <- ggplot(df, aes(x = .data[[chosen_var]], y = gdp)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = paste("GDP vs", chosen_var, "—", reg),
           x = chosen_var, y = "GDP")
    print(p2)
    
    m2 <- lm(as.formula(paste("gdp ~", chosen_var)), data = df)
    cat("\nModel: gdp ~", chosen_var, "\n")
    print(summary(m2))
    safe_plot_lm(m2, ok_for_diag = (n31b >= min_n_diag))
  } else {
    message(sprintf("Skipping 3.1 GDP plot for '%s': only %d complete rows (need ≥ %d).",
                    reg, n31b, min_n_scatter))
  }
  
  # --- 3.2: Two models with chosen_var as response ---
  # Model A: chosen_var ~ population
  vars_32a <- c(chosen_var, "population")
  n32a <- n_complete(df, vars_32a)
  
  if (n32a >= min_n_scatter) {
    cat("\nModel A:", chosen_var, "~ population\n")
    mA <- lm(as.formula(paste(chosen_var, "~ population")), data = df)
    print(summary(mA))
    safe_plot_lm(mA, ok_for_diag = (n32a >= min_n_diag))
  } else {
    message(sprintf("Skipping 3.2 Model A for '%s': only %d complete rows (need ≥ %d).",
                    reg, n32a, min_n_scatter))
  }
  
  # Model B: chosen_var ~ gdp
  vars_32b <- c(chosen_var, "gdp")
  n32b <- n_complete(df, vars_32b)
  
  if (n32b >= min_n_scatter) {
    cat("\nModel B:", chosen_var, "~ gdp\n")
    mB <- lm(as.formula(paste(chosen_var, "~ gdp")), data = df)
    print(summary(mB))
    safe_plot_lm(mB, ok_for_diag = (n32b >= min_n_diag))
  } else {
    message(sprintf("Skipping 3.2 Model B for '%s': only %d complete rows (need ≥ %d).",
                    reg, n32b, min_n_scatter))
  }
}

# ----- Part 4 (KNN) ----
knn_data <- bind_rows(
  results_complete_Latin_Carribean,
  results_complete_Asia_Pacific
)

# Keep only needed variables
knn_data <- knn_data %>%
  select(region, population, gdp, EPI.new) %>%
  filter(complete.cases(.))

# Make region a factor
knn_data$region <- factor(knn_data$region)


# log-transform if skewed
knn_data$population <- log10(knn_data$population)
knn_data$gdp        <- log10(knn_data$gdp)

# --------------------------
# Train/test split
# --------------------------
set.seed(6)
train_idx <- createDataPartition(knn_data$region, p = 0.7, list = FALSE)

train <- knn_data[train_idx, ]
test  <- knn_data[-train_idx, ]

# --------------------------
# Standardize predictors
# --------------------------
pre <- preProcess(train[, c("population", "gdp", "EPI.new")], method = c("center", "scale"))

train_scaled <- predict(pre, train[, c("population", "gdp", "EPI.new")])
test_scaled  <- predict(pre, test[,  c("population", "gdp", "EPI.new")])

# --------------------------
# Try several k values
# --------------------------
k_values <- c(1,3,5,7,9)

accuracies <- c()

for (k in k_values) {
  pred <- knn(train_scaled, test_scaled, cl = train$region, k = k)
  acc <- mean(pred == test$region)
  accuracies <- c(accuracies, acc)
  cat("k =", k, "accuracy =", acc, "\n")
}

# Pick best k
best_k <- k_values[which.max(accuracies)]
cat("\nBest k =", best_k, "\n")

# --------------------------
# Final model with best k
# --------------------------
final_pred <- knn(train_scaled, test_scaled, cl = train$region, k = best_k)

# Confusion matrix
cm <- table(Predicted = final_pred, Actual = test$region)
cm

# Accuracy
accuracy <- mean(final_pred == test$region)
accuracy