library(MVN)
library(car)
library(ggplot2)
library(GGally)
library(dplyr)
library(biotools)
library(stats)
library(MASS)
library(readxl)


# Read the Excel file
data_check <- read_excel("E:/GitHub/sherlock-final-project/cluster_summary.xlsx", sheet='Sheet1')

# Shapiro-Wilk test and Q-Q plot for each variable
for (col in c('x1', 'x2', 'x3')) {
  shapiro_test <- shapiro.test(data_check[[col]])
  print(paste("Shapiro-Wilk test for", col, ": W =", shapiro_test$statistic, ", p =", shapiro_test$p.value))
  
  # Q-Q plot
  qqnorm(data_check[[col]], main = paste("Q-Q plot for", col))
  qqline(data_check[[col]], col = "red")
}

# Assuming data_check is your dataframe and 'kmedoids' is your grouping variable
data_multivariate <- data_check[, c('x1', 'x2', 'x3')]

# Multivariate normality test using Mahalanobis distance
result_mvn <- mvn(data = data_multivariate, mvnTest = "mardia", multivariatePlot = "qq")

# Print the result
print(result_mvn)

# Using biotools package for Box's M test
box_test <- boxM(data_check[, c('x1', 'x2', 'x3')], data_check$kmedoids)
print(box_test)

pairs(data_check[, c('x1', 'x2', 'x3')], 
      main = "Scatterplot Matrix", 
      pch = 19, 
      col = as.numeric(data_check$kmedoids))

# Using GGally
library(GGally)
ggpairs(data_check, columns = c('x1', 'x2', 'x3'), 
        aes(color = factor(kmedoids), alpha = 0.5)) +
  ggtitle("Scatterplot Matrix")

# Calculating VIF using car package

vif_data <- lm(cbind(x1, x2, x3) ~ 1, data = data_check)
vif_values <- vif(vif_data)
print(vif_data)

# MANOVA using stats package
manova_res <- manova(cbind(x1, x2, x3) ~ factor(kmedoids), data = data_check)
# Summary using Wilks' lambda
summary(manova_res, test = "Wilks")

# Individual ANOVA tests using Wilks' lambda
summary.aov(manova_res, test = "Wilks")

cor_matrix <- cor(data_multivariate)
print(cor_matrix)
