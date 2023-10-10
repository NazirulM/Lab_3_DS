
#import and view data frame
df <- read.csv("C:/Users/User/OneDrive - Universiti Teknologi PETRONAS/Documents/Churn_Train.csv")
View(df)

#required libraries
library("tidyverse")
library("dplyr")
library("cowplot")
library("dlookr")
library("psych")
library("mice")
library("missForest")
library("caret")
library("ggplot2")

#checking missing values
sum(is.na(df))

#DATA CLEANING

# Impute missing values using Mice

#checking missing values pattern
df_numeric <- df %>%
  select(Monthly.Charges, Tenure, Total.Charges)
md.pattern(df_numeric)

mice_imputed <- data.frame(
  original = df$Total.Charges,
  imputed_pmm = complete(mice(df_numeric, method =
                                "pmm"))$Total.Charges,
  imputed_cart = complete(mice(df_numeric, method =
                                 "cart"))$Total.Charges,
  imputed_lasso = complete(mice(df_numeric, method =
                                  "lasso.norm"))$Total.Charges)
mice_imputed

#checking each imputation methods via histogram
h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position =
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()

h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position =
                   "identity") +
  ggtitle("PMM-imputed distribution") +
  theme_classic()

h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position =
                   "identity") +
  ggtitle("Cart-imputed distribution") +
  theme_classic()

h4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position =
                   "identity") +
  ggtitle("Lasso-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

missForest_imputed <- data.frame(
  original = df$Total.Charges,
  imputed_missForest = missForest(df_numeric)$ximp$Total.Charges
)
missForest_imputed

g1 <- ggplot(missForest_imputed, aes(x = imputed_missForest)) +
  geom_histogram(fill = "aquamarine", color = "#000000", position =
                   "identity") +
  ggtitle("MissForest-imputed distribution") +
  theme_classic()

plot_grid(g1, nrow = 2, ncol = 2)

df$Total.Charges <- mice_imputed$imputed_pmm

# Normalize data with log scaling method
log_scale = log(as.numeric(df$Total.Charges))
table(log_scale)

g2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "beige", color = "#000000", position =
                   "identity") +
  ggtitle("mice-imputed distribution") +
  theme_classic()

plot_grid(g2, nrow = 2, ncol = 2)

correlate(df) %>%
  plot()

# transforming categorical data into numerical using one-hot encoding
new_data <- data.frame(df$Gender, df$Partner, df$Dependents, df$Phone.Service,
                       df$Multiple.Lines, df$Online.Security, df$Online.Backup,
                       df$Device.Protection, df$Tech.Support, df$Streaming.TV,
                       df$Paperless.Billing)

summary(new_data)

dmy <- dummyVars(" ~ .", data = new_data, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = new_data))
glimpse(dat_transformed)











