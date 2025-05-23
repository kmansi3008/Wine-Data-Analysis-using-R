library(tidyverse)
library(gridExtra)
library(plyr)
library(dplyr)

summarize_numeric = function(dataset) {
  
  dataset = select_if(dataset, is.numeric)
  summary.table = data.frame(Attribute = names(dataset))
  
  summary.table = summary.table %>% 
    mutate('Missing Values' = apply(dataset, 2, function (x) sum(is.na(x))),
           'Unique Values' = apply(dataset, 2, function (x) length(unique(x))),
           'Mean' = colMeans(dataset, na.rm = TRUE),
           'Min' = apply(dataset, 2, function (x) min(x, na.rm = TRUE)),
           'Max' = apply(dataset, 2, function (x) max(x, na.rm = TRUE)),
           'SD' = apply(dataset, 2, function (x) sd(x, na.rm = TRUE))
    )
  summary.table
}


summarize_character = function(dataset) {
  
  dataset = select_if(dataset, is.character)
  summary.table = data.frame(Attribute = names(dataset))
  
  summary.table = summary.table %>% 
    mutate('Missing Values' = apply(dataset, 2, function (x) sum(is.na(x))),
           'Unique Values' = apply(dataset, 2, function (x) length(unique(x))),
    )
  summary.table
}


wine_quality = read_csv('winequality-dataset.csv', show_col_types = FALSE)
wine_quality

summary(wine_quality)

# summarize data to find potential outliers, data missing, etc
summarize_character(wine_quality)
summarize_numeric(wine_quality)

# variables are ordered 
wine_quality = wine_quality %>% mutate(
  fixed_acidity_category = factor(fixed_acidity_category, levels = c("Low", "Medium", "High"), ordered = TRUE),
  alcohol_category = factor(alcohol_category, levels = c("Low", "Medium", "High"), ordered = TRUE),
  sugar_category = factor(sugar_category, levels = c("Dry", "Semi-dry", "Sweet"), ordered = TRUE)
)

sapply(wine_quality, class)
summary(wine_quality)

library(skimr)
skim(wine_quality)

#########################################
#########################################
########## Start Data Cleaning ########## 
#########################################
#########################################

#######################################
#### Fix the factors missing values ###
#######################################

# determine whether the percentage of missing values is significant 

# fixed_acidity_category
wine_quality %>%
  summarise(
    Total_Observations = n(),
    Missing_Values = sum(is.na(fixed_acidity_category)),
    Percent_Missing = (sum(is.na(fixed_acidity_category)) / n()) * 100,
    .groups = 'drop'
  )

# alcohol_category
wine_quality %>%
  summarise(
    Total_Observations = n(),
    Missing_Values = sum(is.na(fixed_acidity_category)),
    Percent_Missing = (sum(is.na(fixed_acidity_category)) / n()) * 100,
    .groups = 'drop'
  )

# sugar_category
wine_quality %>%
  summarise(
    Total_Observations = n(),
    Missing_Values = sum(is.na(sugar_category)),
    Percent_Missing = (sum(is.na(sugar_category)) / n()) * 100,
    .groups = 'drop'
  )

# impute missing values by imputing the most frequent level 

# fixed_acidity_category
wine_quality$fixed_acidity_category[is.na(wine_quality$fixed_acidity_category)] <- "Medium"

# alcohol_category
wine_quality$alcohol_category[is.na(wine_quality$alcohol_category)] <- "Medium"

# sugar_category
wine_quality$sugar_category[is.na(wine_quality$sugar_category)] <- "Dry"

summary(wine_quality)
skim(wine_quality)

#######################################
#### Fix the numeric missing values ###
#######################################

#######################################
# fixed_acidity 
wine_quality %>%
  summarise(
    Total_Observations = n(),
    Missing_Values = sum(is.na(`fixed acidity`)),
    Percent_Missing = (sum(is.na(`fixed acidity`)) / n()) * 100,
    .groups = 'drop'
  )

# impute the median value for missing values
median <- median(wine_quality$`fixed acidity`, na.rm = TRUE)
wine_quality$`fixed acidity` <- replace_na(wine_quality$`fixed acidity`, median)
summary(wine_quality)

#######################################

# volatile acidity  
wine_quality %>%
  summarise(
    Total_Observations = n(),
    Missing_Values = sum(is.na(`volatile acidity`)),
    Percent_Missing = (sum(is.na(`volatile acidity`)) / n()) * 100,
    .groups = 'drop'
  )

# impute the median value for missing values
median1 <- median(wine_quality$`volatile acidity`, na.rm = TRUE)
wine_quality$`volatile acidity` <- replace_na(wine_quality$`volatile acidity`, median1)
summary(wine_quality)

#######################################

# residual sugar 
wine_quality %>%
  summarise(
    Total_Observations = n(),
    Missing_Values = sum(is.na(`residual sugar`)),
    Percent_Missing = (sum(is.na(`residual sugar`)) / n()) * 100,
    .groups = 'drop'
  )

# impute the median value for missing values
median1 <- median(wine_quality$`residual sugar`, na.rm = TRUE)
wine_quality$`residual sugar` <- replace_na(wine_quality$`residual sugar`, median1)
summary(wine_quality)

#######################################

# citric acid 
wine_quality %>%
  summarise(
    Total_Observations = n(),
    Missing_Values = sum(is.na(`citric acid`)),
    Percent_Missing = (sum(is.na(`citric acid`)) / n()) * 100,
    .groups = 'drop'
  )

# impute the median value for missing values
median1 <- median(wine_quality$`citric acid`, na.rm = TRUE)
wine_quality$`citric acid` <- replace_na(wine_quality$`citric acid`, median1)
summary(wine_quality)

#######################################

# chlorides 
wine_quality %>%
  summarise(
    Total_Observations = n(),
    Missing_Values = sum(is.na(`chlorides`)),
    Percent_Missing = (sum(is.na(`chlorides`)) / n()) * 100,
    .groups = 'drop'
  )

# impute the median value for missing values
median1 <- median(wine_quality$`chlorides`, na.rm = TRUE)
wine_quality$`chlorides` <- replace_na(wine_quality$`chlorides`, median1)
summary(wine_quality)

#######################################

# free sulfur dioxide
wine_quality %>%
  summarise(
    Total_Observations = n(),
    Missing_Values = sum(is.na(`free sulfur dioxide`)),
    Percent_Missing = (sum(is.na(`free sulfur dioxide`)) / n()) * 100,
    .groups = 'drop'
  )

# impute the median value for missing values
median1 <- median(wine_quality$`free sulfur dioxide`, na.rm = TRUE)
wine_quality$`free sulfur dioxide` <- replace_na(wine_quality$`free sulfur dioxide`, median1)
summary(wine_quality)

#######################################

# total sulfur dioxide
wine_quality %>%
  summarise(
    Total_Observations = n(),
    Missing_Values = sum(is.na(`total sulfur dioxide`)),
    Percent_Missing = (sum(is.na(`total sulfur dioxide`)) / n()) * 100,
    .groups = 'drop'
  )

# impute the median value for missing values
median1 <- median(wine_quality$`total sulfur dioxide`, na.rm = TRUE)
wine_quality$`total sulfur dioxide` <- replace_na(wine_quality$`total sulfur dioxide`, median1)
summary(wine_quality)

#######################################

# density 
wine_quality %>%
  summarise(
    Total_Observations = n(),
    Missing_Values = sum(is.na(`density`)),
    Percent_Missing = (sum(is.na(`density`)) / n()) * 100,
    .groups = 'drop'
  )

# impute the median value for missing values
median1 <- median(wine_quality$`density`, na.rm = TRUE)
wine_quality$`density` <- replace_na(wine_quality$`density`, median1)
summary(wine_quality)

#######################################

# pH 
wine_quality %>%
  summarise(
    Total_Observations = n(),
    Missing_Values = sum(is.na(`pH`)),
    Percent_Missing = (sum(is.na(`pH`)) / n()) * 100,
    .groups = 'drop'
  )
# impute the median value for missing values
median1 <- median(wine_quality$`pH`, na.rm = TRUE)
wine_quality$`pH` <- replace_na(wine_quality$`pH`, median1)
summary(wine_quality)

#######################################


# sulphates 
wine_quality %>%
  summarise(
    Total_Observations = n(),
    Missing_Values = sum(is.na(`sulphates`)),
    Percent_Missing = (sum(is.na(`sulphates`)) / n()) * 100,
    .groups = 'drop'
  )
# impute the median value for missing values
median1 <- median(wine_quality$`sulphates`, na.rm = TRUE)
wine_quality$`sulphates` <- replace_na(wine_quality$`sulphates`, median1)
summary(wine_quality)

#######################################

# quality 
wine_quality %>%
  summarise(
    Total_Observations = n(),
    Missing_Values = sum(is.na(`quality`)),
    Percent_Missing = (sum(is.na(`quality`)) / n()) * 100,
    .groups = 'drop'
  )
# impute the median value for missing values
median1 <- median(wine_quality$`quality`, na.rm = TRUE)
wine_quality$`quality` <- replace_na(wine_quality$`quality`, median1)
summary(wine_quality)

#######################################

skim(wine_quality)
summary(wine_quality)

######################################
######################################
######## Investigate outliers ######## 
######################################
######################################

##### TOOK THIS OUT FOR REPORT #######
##### TOOK THIS OUT FOR REPORT #######
##### TOOK THIS OUT FOR REPORT #######

# Outliers in free sulfur dioxide 

ggplot(wine_quality) + geom_histogram(aes(x = `free sulfur dioxide`), binwidth = 5)

# wine_quality <- wine_quality %>%
#  filter(`free sulfur dioxide` <= quantile(`free sulfur dioxide`, 0.98))


# Outliers in total sulfur dioxide 

ggplot(wine_quality) + geom_histogram(aes(x = `total sulfur dioxide`), binwidth = 5)

# wine_quality <- wine_quality %>%
#  filter(`total sulfur dioxide` <= quantile(`total sulfur dioxide`, 0.98))

#####################################
#####################################
#####################################
######## univariate analysis s#######
#####################################
#####################################
#####################################


########################### Factors
ggplot(wine_quality) + geom_bar(aes(x = fixed_acidity_category)) + labs(title = "Fixed Acidity Categories")
ggplot(wine_quality) + geom_bar(aes(x = alcohol_category)) + labs(title = "Alcohol Category")
ggplot(wine_quality) + geom_bar(aes(x = sugar_category)) + labs(title = "Sugar Category")


########################### Numerics
ggplot(wine_quality) + geom_histogram(aes(x = `fixed acidity`)) + labs(title = "Fixed Acidity")
summarize_numeric(select(wine_quality,`fixed acidity`))

ggplot(wine_quality) + geom_histogram(aes(x = `volatile acidity`), bins = 25) + labs(title = "Volatile Category")
summarize_numeric(select(wine_quality,`volatile acidity`))

ggplot(wine_quality) + geom_histogram(aes(x = `citric acid`)) + labs(title = "Citric Acid")
summarize_numeric(select(wine_quality,`citric acid`))

ggplot(wine_quality) + geom_histogram(aes(x = `residual sugar`), bins = 25) + labs(title = "Residual Sugar")
summarize_numeric(select(wine_quality,`residual sugar`))

ggplot(wine_quality) + geom_histogram(aes(x = `chlorides`)) + labs(title = "Chlorides")
summarize_numeric(select(wine_quality,`chlorides`))

ggplot(wine_quality) + geom_histogram(aes(x = `free sulfur dioxide`)) + labs(title = "Free sulfur dioxide")
summarize_numeric(select(wine_quality,`free sulfur dioxide`))

ggplot(wine_quality) + geom_histogram(aes(x = `total sulfur dioxide`)) + labs(title = "Total sulfur dioxide")
summarize_numeric(select(wine_quality,`total sulfur dioxide`))

ggplot(wine_quality) + geom_histogram(aes(x = `density`)) + labs(title = "Density")
summarize_numeric(select(wine_quality,`density`))

ggplot(wine_quality) + geom_histogram(aes(x = `pH`)) + labs(title = "pH")
summarize_numeric(select(wine_quality,`pH`))

ggplot(wine_quality) + geom_histogram(aes(x = `sulphates`)) + labs(title = "sulphates")
summarize_numeric(select(wine_quality,`sulphates`))

ggplot(wine_quality) + geom_histogram(aes(x = `alcohol`)) + labs(title = "alcohol")
summarize_numeric(select(wine_quality,`alcohol`))

ggplot(wine_quality) + geom_bar(aes(x = `quality`)) + labs(title = "quality")
summarize_numeric(select(wine_quality,`quality`))


#####################################
#####################################
#####################################
######## bivariate analysis s#######
#####################################
#####################################
#####################################


########################### Factors

### fixed_acidity_category
ggplot(wine_quality) + geom_bar(aes(x= quality, fill = fixed_acidity_category))

# not sure which graph to use

ggplot(wine_quality) + geom_boxplot(aes(x = quality, y = fixed_acidity_category))  +
  coord_flip()

### alcohol category
ggplot(wine_quality) + geom_bar(aes(x= quality, fill = alcohol_category))

# not sure which graph to use

ggplot(wine_quality) + geom_boxplot(aes(x = quality, y = alcohol_category))  +
  coord_flip()

### sugar category
ggplot(wine_quality) + geom_bar(aes(x= quality, fill = sugar_category))
# not sure which graph to use

ggplot(wine_quality) + geom_boxplot(aes(x = quality, y = sugar_category))  +
  coord_flip()

########################### Numerics

### fixed acidity 
ggplot(wine_quality, aes(x = quality, y = `fixed acidity`)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, color = "#112446") +
  theme_minimal()

ggplot(wine_quality, aes(x = factor(quality), y = `fixed acidity`, fill = factor(quality))) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Quality", y = "Fixed Acidity", fill = "Quality")


### volatile acidity 
ggplot(wine_quality, aes(x = quality, y = `volatile acidity`)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, color = "#112446") +
  theme_minimal()

ggplot(wine_quality, aes(x = factor(quality), y = `volatile acidity`, fill = factor(quality))) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Quality", y = "Volatile Acidity", fill = "Quality")

### citric acid
ggplot(wine_quality, aes(x = quality, y = `citric acid`)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, color = "#112446") +
  theme_minimal()

ggplot(wine_quality, aes(x = factor(quality), y = `citric acid`, fill = factor(quality))) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Quality", y = "Citric Acid", fill = "Quality")

### residual sugar
ggplot(wine_quality, aes(x = quality, y = `residual sugar`)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, color = "#112446") +
  theme_minimal()

ggplot(wine_quality, aes(x = factor(quality), y = `residual sugar`, fill = factor(quality))) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Quality", y = "Residual Sugar", fill = "Quality")

### chlorides
ggplot(wine_quality, aes(x = quality, y = `chlorides`)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, color = "#112446") +
  theme_minimal()

ggplot(wine_quality, aes(x = factor(quality), y = `chlorides`, fill = factor(quality))) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Quality", y = "Chlorides", fill = "Quality")

### free sulfur dioxide
ggplot(wine_quality, aes(x = quality, y = `free sulfur dioxide`)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, color = "#112446") +
  theme_minimal()

ggplot(wine_quality, aes(x = factor(quality), y = `free sulfur dioxide`, fill = factor(quality))) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Quality", y = "Free Sulfur Dioxide", fill = "Quality")

### total sulfur dioxide 
ggplot(wine_quality, aes(x = quality, y = `total sulfur dioxide`)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, color = "#112446") +
  theme_minimal()

ggplot(wine_quality, aes(x = factor(quality), y = `total sulfur dioxide`, fill = factor(quality))) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Quality", y = "Total Sulfur Dioxide", fill = "Quality")

### density
ggplot(wine_quality, aes(x = quality, y = `density`)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, color = "#112446") +
  theme_minimal()

ggplot(wine_quality, aes(x = factor(quality), y = `density`, fill = factor(quality))) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Quality", y = "Density", fill = "Quality")

 ### pH
ggplot(wine_quality, aes(x = quality, y = `pH`)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, color = "#112446") +
  theme_minimal()

ggplot(wine_quality, aes(x = factor(quality), y = `pH`, fill = factor(quality))) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Quality", y = "pH", fill = "Quality")

### sulphates
ggplot(wine_quality, aes(x = quality, y = `sulphates`)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, color = "#112446") +
  theme_minimal()

ggplot(wine_quality, aes(x = factor(quality), y = `sulphates`, fill = factor(quality))) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Quality", y = "Sulphates", fill = "Quality")

### alcohol
ggplot(wine_quality, aes(x = quality, y = `alcohol`)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, color = "#112446") +
  theme_minimal()

ggplot(wine_quality, aes(x = factor(quality), y = `alcohol`, fill = factor(quality))) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Quality", y = "Alcohol", fill = "Quality")


################################
################################
### create a heat map
################################
################################
numeric_columns <- sapply(wine_quality, is.numeric)
numeric_data <- wine_quality[, numeric_columns]
numeric_data

ggcorrplot(cor(numeric_data))

################################
#### frequency of each category
################################
################################
ggplot(wine_quality, aes(x = quality, group = 1)) +
  geom_line(stat = "count") +
  geom_point(stat = "count", color = "red") +
  labs(title = "Frequency for Quality (3,8)", x = "Quality", y = "Frequency")

category_counts <- table(wine_quality$quality)
sum(category_counts)

category_df <- as.data.frame(category_counts)
# Calculate percentages
category_df2 <- category_df$Freq / sum(category_df$Freq) * 100

