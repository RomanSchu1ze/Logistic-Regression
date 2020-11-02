# Titanic dataset  

# Direcory ----
setwd("~/Desktop/kaggle-titanic-master/input")

# Training data ----
training.data.raw <- read.csv("train.csv", header = T, na.strings = c(""))


## Data preparation -----------------------------------------------------------

# Checking for missings ----
sapply(training.data.raw, function(x) sum(is.na(x)))

# Check how many unique values ----
sapply(training.data.raw, function(x) length(unique(x)))

# Plot missings ----
Amelia::missmap(training.data.raw, main = "Missing values vs observed")

# The variable cabin has too many missing values, we will not use it. 
# We will also drop PassengerId since it is only an index and Ticket.
# Using the subset() function we subset the original dataset selecting the 
# relevant columns only.

data <- subset(training.data.raw, select = c(2, 3, 5, 6, 7, 8, 10, 12))

# Replace age missings by mean ----
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = T)

# Checking how factor data is dummified ----
contrasts(data$Sex)
contrasts(data$Embarked)

# Remove missings, there are only two ----
data <- data[!is.na(data$Embarked), ]
rownames(data) <- NULL

# Missings left? ----
sum(is.na(data))



## Model fitting --------------------------------------------------------------

# Training and test data
train <- data[1:800, ]
test <- data[801:889, ]


j <- as.character(c(T, T, F))
if(is.character(j)) {
  j <- as.numeric(as.factor(j))
}

