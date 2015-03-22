library(dplyr)
library(caret)
library(ggplot2)
library(scales)
library(lubridate)

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

##############################################################################
# Plot some bar graphs
###############################################################################
# Create a data frame for bar graphs
bar <- train %>%
  select(user_name, new_window, classe, cvtd_timestamp) %>%
  mutate(
    user_name = factor(user_name)
    , new_window = factor(new_window)
    , classe = factor(classe)
    , wle_date = as.Date(dmy_hm(cvtd_timestamp))
    )

# glimpse(bar)

# Specify breaks as a Date vector
date_breaks <- seq(as.Date(min(bar$wle_date)), as.Date(max(bar$wle_date)), by = '1 day')

# Build a bar graph showing the number of observation by the user_name variable
bar %>%
  group_by(wle_date, user_name) %>%
  summarise( count = n()) %>%
  ggplot(aes(x = wle_date, y = count, fill = user_name)) +
    geom_bar(stat = 'identity', width = 0.5, position = 'dodge') +
    scale_x_date(breaks = date_breaks, labels = date_format('%d/%m/%Y')) +
    scale_y_continuous(labels = comma) +
    xlab('Date') +
    ylab('Count') +
    ggtitle('Number of observations by date and user_name\n')
    
# Build a bar graph showing the number of observations by the classe variable
bar %>%
  group_by(wle_date, classe) %>%
  summarise( count = n()) %>%
  ggplot(aes(x = wle_date, y = count, fill = classe)) +
    geom_bar(stat = 'identity', width = 0.5, position = 'dodge') +
    scale_x_date(breaks = date_breaks, labels = date_format('%d/%m/%Y')) +
    scale_y_continuous(labels = comma) +
    xlab('Date') +
    ylab('Count') +
    ggtitle('Number of observations by date and classe\n')

# Build a bar graph showing the number of observations by the new_window variable
bar %>%
  group_by(wle_date, new_window) %>%
  summarise( count = n()) %>%
  ggplot(aes(x = wle_date, y = count, fill = new_window)) +
  geom_bar(stat = 'identity', width = 0.5, position = 'dodge') +
  scale_x_date(breaks = date_breaks, labels = date_format('%d/%m/%Y')) +
  scale_y_continuous(labels = comma) +
  xlab('Date') +
  ylab('Count') +
  ggtitle('Number of observations by date and new_window\n')


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
# Create lists of names of factor and numeric variables
###############################################################################
# Create list of factor variables
is_factor <- sapply(train, is.factor)
factors <- names(is_factor[is_factor == TRUE])

# Create list of numeric variables 
is_num <- sapply(train, is.numeric)
numerics <- names(is_num[is_num == TRUE])

###############################################################################
# Center and scale numeric variables
###############################################################################
pre <-  preProcess(train[, numerics], method = c("center", "scale"))
pre_train <- data.frame(predict(pre, train[, numerics]))
pre_train <- tbl_df(pre_train)

# Add back the factor variables
train <- bind_cols(pre_train, train[ , factors])
