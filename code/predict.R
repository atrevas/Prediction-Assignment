library(dplyr)
library(caret)
library(ggplot2)
library(scales)
library(lubridate)
library(rpart)

load_data <- function(file){
  # Build a path for the data file name
  fn <- file.path('data', file)
  
  df <- read.csv(fn, stringsAsFactor = TRUE, na.strings = c('NA', '')
                  , quote = '\"')
  return (df)
}

###############################################################################
# Load the data
###############################################################################
# Load the training data
raw_train_data <- load_data('pml-training.csv')
raw_train_data <- tbl_df(raw_train_data)

# Load the test data
raw_test_data <- load_data('pml-testing.csv')
raw_test_data <- tbl_df(raw_test_data)

###############################################################################
# First look at the raw data
###############################################################################
# How many observation and variables we have
d <- dim(raw_train_data)
df1 <- data.frame(Data = 'train',Obs = d[1], Vars = d[2])
d <- dim(raw_test_data)
df2 <- data.frame(Data = 'test',Obs = d[1], Vars = d[2])
rbind(df1, df2)

###############################################################################
# Remove variables with lots of NAs
###############################################################################
# Get the percentage of NAs values in each variable
perc_nas <- sapply(raw_train_data, function (x) sum(is.na(x)) / length(x))

# Get a list of variables with more than 90% of NAs values
high_nas <- names(perc_nas[perc_nas > 0.9])

# Remove the hight NAs variables from train data
train_data <- raw_train_data %>%
  select( - one_of(high_nas))

# Remove the hight NAs variables from test data
test_data <- raw_test_data %>%
  select( - one_of(high_nas))

###############################################################################
# Remove timestamp variables
###############################################################################
# From train data
train_data <- train_data %>%
   select(- raw_timestamp_part_1, -raw_timestamp_part_2, -cvtd_timestamp)

# From test data
test_data <- test_data %>%
   select(- raw_timestamp_part_1, -raw_timestamp_part_2, -cvtd_timestamp)

###############################################################################
# Create dummy variables
###############################################################################
# dummies <- dummyVars(~ user_name + new_window , data = train)
# 
# glimpse(tbl_df(as.data.frame(predict(dummies, newdata = train))))


###############################################################################
# Remove zero and near zero-variance predictors
###############################################################################
nzv <- nearZeroVar(train_data)

dim(train_data)
train_data <- train_data %>%
  select(-nzv)
dim(train_data)

dim(test_data)
test_data <- test_data %>%
  select(-nzv)
dim(test_data)


###############################################################################
# Create list of names of predictors variables
###############################################################################
# Create list of numeric variables 
is_num <- sapply(train_data, is.numeric)
numerics <- names(is_num[is_num == TRUE])

# Remove the X variable as it is a kind of key that is unique for each
# observation
predictors <- numerics[numerics != 'X']

###############################################################################
# Exploratory Data Analysis at the PCA
###############################################################################
# ggplot(pca_train, aes(x = PC1, colour = classe)) +
#   geom_line(stat = 'density', size = 1) +
#   ggtitle('PC1 Density Plot\n') +
#   theme(plot.title = element_text(size = 25, face = 'bold')) +
#   ylab('Density')
# 
# ggplot(pca_train, aes(x = PC2, colour = classe)) +
#   geom_line(stat = 'density', size = 1) +
#   ggtitle('PC2 Density Plot\n') +
#   theme(plot.title = element_text(size = 25, face = 'bold')) +
#   ylab('Density')
# 
# ggplot(pca_train, aes(x = PC3, colour = classe)) +
#   geom_line(stat = 'density', size = 1) +
#   ggtitle('PC3 Density Plot\n') +
#   theme(plot.title = element_text(size = 25, face = 'bold')) +
#   ylab('Density')

###############################################################################
# Train the model
###############################################################################
set.seed(123)

# Create a smaller sample for training the model
indx <- createDataPartition(train_data$classe
                    , p = 0.25
                    , list = FALSE)

small_train_data <- train_data[indx, ]

# Train the model
fit_control <- trainControl(method = 'repeatedcv'
                            , number = 4)

model_fit <- train(x = small_train_data[, predictors]
                   , y = small_train_data$classe
                   , method = 'rf'
                   , metric = 'Accuracy'
                   , trControl = fit_control)

model_fit