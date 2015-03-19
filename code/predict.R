library(dplyr)
library(caret)
library(ggplot2)
library(scales)
library(lubridate)

###############################################################################
# Load the data
###############################################################################
# Build a path for the data file name
fn <- file.path('data', 'pml-training.csv')

# Load the training data
raw <- read.csv(fn, stringsAsFactor = FALSE, na.strings = c('NA', '')
                    , quote = '\"')
raw <- tbl_df(raw)

###############################################################################
# First look at the raw data
###############################################################################
# How many observation and variables we have
d <- dim(raw)
df <- data.frame(Obs = d[1], Vars = d[2])
df

# Take a first look at the variables
glimpse(raw)

###############################################################################
# Remove variables with lots of NAs
###############################################################################
# Get the percentage of NAs values in each variable
perc_nas <- sapply(raw, function (x) sum(is.na(x)) / length(x))

# Get a list of variables with more than 90% of NAs values
high_nas <- perc_nas[perc_nas > 0.9]

# names(high_nas)

# Remove the hight NAs variables
train <- raw %>%
  select( - one_of(names(high_nas)))

###############################################################################
# Transform original variables
###############################################################################
# Create a date column
train <- train %>%
  mutate(wle_date = dmy_hm(cvtd_timestamp))

# Remove the original timestamp variable
train <- train %>%
  select(- cvtd_timestamp)

# Define factor variables
train <- train %>%
  mutate(user_name = factor(user_name)
         , new_window = factor(new_window)
         , classe = factor(classe))

# Create list of factor variables
is_factor <- sapply(train, is.factor)
factors <- is_factor[is_factor == TRUE]

# Take a first look at the variables
glimpse(train)

# Check which variables are numeric
is_num <- sapply(train, is.numeric)
numerics <- names(is_num[is_num == TRUE])



###############################################################################
# Plot some bar graphs
###############################################################################
# Create a data frame for bar graphs
bar <- train %>%
  select(user_name, new_window, classe, wle_date) %>%
  mutate(
    user_name = factor(user_name)
    , new_window = factor(new_window)
    , classe = factor(classe)
    , wle_date = as.Date(wle_date)
    )

# glimpse(bar)

# Specify breaks as a Date vector
date_breaks <- seq(as.Date(min(train$wle_date)), as.Date(max(train$wle_date)), by = '1 day')

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
# Create dummy variables
###############################################################################
# dummies <- dummyVars(~ user_name + new_window , data = train)
# 
# glimpse(tbl_df(as.data.frame(predict(dummies, newdata = train))))


###############################################################################
# Remove zero and near zer-variance predictors
###############################################################################
# dim(wle_clean)
# nzv <- nearZeroVar(wle_clean)
# wle_clean <- wle_clean %>%
#   select(-nzv)
# dim(wle_clean)


pre_pro <-  preProcess(train[, numerics], method = c("center", "scale"))
tbl_df(data.frame(predict(pre_pro, train[, numerics])))
