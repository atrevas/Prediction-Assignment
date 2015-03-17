library(dplyr)
library(caret)
library(ggplot2)
library(scales)
library(lubridate)

# Build a path for the data file name
fn <- file.path('data', 'pml-training.csv')

# Load the training data
wle_raw <- read.csv(fn, stringsAsFactor = FALSE, na.strings = c('NA', ''), quote = '\"')
wle_raw <- tbl_df(wle_raw)

# How many observation and variables we have
d <- dim(wle_raw)
df <- data.frame(Obs = d[1], Vars = d[2])
df

# Take a first look at the variables
# glimpse(wle_raw)

# Get the percentage of NAs values in each variable
perc_nas <- sapply(wle_raw, function (x) sum(is.na(x)) / length(x))

# Get a list of variables with more than 90% of NAs values
high_nas <- perc_nas[perc_nas > 0.9]
# names(high_nas)

# Remove the hight NAs variables
wle_clean <- wle_raw %>%
  select( - one_of(names(high_nas)))

# Create a date column
wle_clean <- wle_clean%>%
  mutate(wle_date = dmy_hm(cvtd_timestamp))

# Remove the original timestamp variable
wle_clean <- wle_clean %>%
  select(- cvtd_timestamp)

# Define factor variables
wle_clean <- wle_clean %>%
  mutate(user_name = factor(user_name)
         , new_window = factor(new_window))

# Take a first look at the variables
# glimpse(wle_clean)

# Check which variables are numeric
is_num <- sapply(wle_clean, is.numeric)

num <- names(is_num[is_num == TRUE])
non_num <- names(is_num[is_num == FALSE])

# Create a data frame for bar graphs
wle_bar <- wle_clean %>%
  select(user_name, new_window, classe, wle_date) %>%
  mutate(
    user_name = factor(user_name)
    , new_window = factor(new_window)
    , classe = factor(classe)
    , wle_date = as.Date(wle_date)
    )

# glimpse(wle_bar)

# Specify breaks as a Date vector
date_breaks <- seq(as.Date(min(wle_clean$wle_date)), as.Date(max(wle_clean$wle_date)), by = '1 day')

# Build a bar graph showing the number of observation by the user_name variable
wle_bar %>%
  group_by(wle_date, user_name) %>%
  summarise( count = n()) %>%
  ggplot(aes(x = wle_date, y = count, fill = user_name)) +
    geom_bar(stat = 'identity', width = 0.5, position = 'dodge') +
    scale_x_date(breaks = date_breaks, labels = date_format('%d/%m/%Y')) +
    scale_y_continuous(labels = comma) +
    xlab('Date') +
    ylab('Count')
    
# Build a bar graph showing the number of observations by the classe variable
wle_bar %>%
  group_by(wle_date, classe) %>%
  summarise( count = n()) %>%
  ggplot(aes(x = wle_date, y = count, fill = classe)) +
    geom_bar(stat = 'identity', width = 0.5, position = 'dodge') +
    scale_x_date(breaks = date_breaks, labels = date_format('%d/%m/%Y')) +
    scale_y_continuous(labels = comma) +
    xlab('Date') +
    ylab('Count')

# Build a bar graph showing the number of observations by the new_window variable
wle_bar %>%
  group_by(wle_date, new_window) %>%
  summarise( count = n()) %>%
  ggplot(aes(x = wle_date, y = count, fill = new_window)) +
  geom_bar(stat = 'identity', width = 0.5, position = 'dodge') +
  scale_x_date(breaks = date_breaks, labels = date_format('%d/%m/%Y')) +
  scale_y_continuous(labels = comma) +
  xlab('Date') +
  ylab('Count')

    
