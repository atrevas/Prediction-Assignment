library(dplyr)
library(caret)

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
glimpse(wle_raw)

# Get the percentage of NAs values in each variable
perc_nas <- sapply(wle_raw, function (x) sum(is.na(x)) / length(x))

# Get a list of variables with more than 90% of NAs values
high_nas <- perc_nas[perc_nas > 0.9]
names(high_nas)

# Remove the hight NAs variables
wle_clean <- wle_raw %>%
  select(-one_of(names(high_nas)))

# Take a first look at the variables
glimpse(wle_clean)

