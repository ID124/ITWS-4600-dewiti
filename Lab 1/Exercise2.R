library(readr)
library(EnvStats)
library(nortest)


epi.data <- read_csv("C:/Users/IDWT/OneDrive - Rensselaer Polytechnic Institute/Desktop/Spring 2026 Classes/Data Analytics/Labs/Lab 1/epi_results_2024_pop_gdp.csv")

View(epi.data)

# Two variables
GDP <- epi.data$gdp
BDH <- epi.data$BDH.new

# Variable summaries
summary(GDP)
summary(BDH)

# Using rows that don't have NA's
NAs <- is.na(GDP)
GDP.complete <- GDP[!NAs]

# Variable boxplots
GDPAbove20000 <- GDP.complete[GDP.complete>20000]
boxplot(GDP.complete, GDPAbove20000, names = c("GDP","GDP>20000"))

BDHAbove50 <- BDH[BDH>50]
boxplot(BDH, BDHAbove50, names = c("BDH", "BDH>50"))

# ----- Histograms ----

# BDH histogram 
hist(BDH, prob=TRUE)
lines(density(BDH,bw="SJ"))
rug(BDH)


# GDP histogram 
hist(GDP.complete, prob=TRUE)
lines(density(GDP.compelete, bw="SJ"))
rug(BDH)





# ECDFs
plot(ecdf(BDH), do.points=FALSE, verticals=TRUE) 
plot(ecdf(GDP.complete), do.points=FALSE, verticals=TRUE) 

# QQ plots
qqnorm(BDH); qqline(BDH)
qqnorm(GDP.complete); qqline(GDP.complete)

# QQ plots against both variables
qqplot(BDH, GDP, xlab = "Q-Q plot for BDH & GDP") 

# Statistical Tests
GDP_test <- rnorm(500)
BDH_test <- rnorm(500)

hist(GDP_test)
hist(BDH_test)

shapiro.test(GDP_test)
shapiro.test(BDH_test)

ad.test(GDP_test)
ad.test(BDH_test)

ks.test(GDP_test,BDH_test)

wilcox.test(GDP_test,BDH_test)

var.test(GDP_test,BDH_test)
t.test(GDP_test,BDH_test)

hist(GDP_test, col='lightsteelblue')
hist(BDH_test, col='lightgreen', add=TRUE)


