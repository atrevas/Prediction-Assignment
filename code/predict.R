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
raw_train <- load_data('pml-training.csv')
raw_train <- tbl_df(raw_train)

# Load the test data
raw_test <- load_data('pml-testing.csv')
raw_test <- tbl_df(raw_test)

###############################################################################
# First look at the raw data
###############################################################################
# How many observation and variables we have
d <- dim(raw_train)
df1 <- data.frame(Data = 'train',Obs = d[1], Vars = d[2])
d <- dim(raw_test)
df2 <- data.frame(Data = 'test',Obs = d[1], Vars = d[2])
rbind(df1, df2)

###############################################################################
# Remove variables with lots of NAs
###############################################################################
# Get the percentage of NAs values in each variable
perc_nas <- sapply(raw_train, function (x) sum(is.na(x)) / length(x))

# Get a list of variables with more than 90% of NAs values
high_nas <- names(perc_nas[perc_nas > 0.9])

# Remove the hight NAs variables from train data
train <- raw_train %>%
  select( - one_of(high_nas))

# Remove the hight NAs variables from test data
test <- raw_test %>%
  select( - one_of(high_nas))

###############################################################################
# Remove timestamp variables
###############################################################################
# From train data
train <- train %>%
   select(- raw_timestamp_part_1, -raw_timestamp_part_2, -cvtd_timestamp)

# From test data
test <- test %>%
   select(- raw_timestamp_part_1, -raw_timestamp_part_2, -cvtd_timestamp)

###############################################################################
# Create dummy variables
###############################################################################
# dummies <- dummyVars(~ user_name + new_window , data = train)
# 
# glimpse(tbl_df(as.data.frame(predict(dummies, newdata = train))))


###############################################################################
# Remove zero and near zer-variance predictors
###############################################################################
nzv <- nearZeroVar(train)

dim(train)
train <- train %>%
  select(-nzv)
dim(train)

dim(test)
test <- test %>%
  select(-nzv)
dim(test)


###############################################################################
# Create list of names of numeric variables
###############################################################################
# Create list of numeric variables 
is_num <- sapply(train, is.numeric)
numerics <- names(is_num[is_num == TRUE])

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
indx <- createDataPartition(train$classe
                    , p = 0.1
                    , list = FALSE)

small_train <- train[indx, ]

# Train the model
fit_control <- trainControl(method = 'repeatedcv'
                            , number = 4)
ptm <- proc.time()
model_fit <- train(classe ~ ., data = select(small_train,-user_name)
                   , method = 'rf'
                   , metric = 'Accuracy'
                   , trControl = fit_control)
proc.time() - ptm
model_fit