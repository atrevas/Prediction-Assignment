library(dplyr)
library(caret)

# Build a path for the data file name
fn <- file.path('data', 'pml-training.csv')

# Load the training data
wle_raw <- read.csv(fn, stringsAsFactor = FALSE, na.strings = c('NA', ''), quote = '\"')
wle_raw <- tbl_df(wle)

# How many observation and variables we have
d <- dim(wle_raw)
df <- data.frame(Obs = d[1], Vars = d[2])
df

# Take a first look at the variables
glimpse(wle_raw)


sapply(wle_raw, function (x) sum(is.na(x)) / length(x))



wle %>%
  select(user_name) %>%
  distinct()