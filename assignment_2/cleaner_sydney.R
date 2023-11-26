library(tidyverse)

rm(list=ls())

#location of folders
data_in  <- "raw_data/"
data_out <- "clean_data/"

library(tidyverse)
# Initial cleaning of the data
full_data <-read.csv(paste0(data_in,"listings.csv"))

# sample with main variables
data <- full_data %>%
  select(id,price,neighbourhood_cleansed,accommodates,name,beds, bathrooms_text)

# filter for accommodates 2 - 6
data<-data %>% filter(accommodates >= 2 & accommodates <= 6)

# check for missing values in the main variables
data %>% summarise_all(funs(sum(is.na(.))))

# Formatting columns

# Use regular expression to extract the number of bedrooms
data$bedroom_text <- str_extract(data$name, "\\b\\d+\\s*bedrooms?\\b")
data$bedroom_count <- str_extract(data$bedroom_text, "\\b\\d+\\b")
data$bedroom_count <- as.numeric(data$bedroom_count)
# Use regular expression to extract the number of baths
data$bath_count <- str_extract(data$bathrooms_text, "\\b\\d+\\b")
data$bath_count <- as.numeric(data$bath_count)
# remove certain columns
data <- data %>% select(-c(name, bathrooms_text, bedroom_text,beds))

# working with price variable

# Remove dollar sign and commas
data$price <- gsub("[\\$,]", "", data$price)
# Convert to numeric
data$price <- as.numeric(data$price)
# boxplot
boxplot(data$price)
# unusually high prices for some listings (max is 25000$ per night) I checked some of them and it is clearly mistakes.

# cumulative distribution of price table
price_table <- data %>%
  group_by(price) %>%
  summarise(count = n()) %>%
  mutate(cumulative_count = cumsum(count),
         cumulative_percent = cumulative_count / sum(count))

# remove listings with price higher than 450$ (90% of listings are below 450$)
data <- data %>% filter(price <= 450)

# log price
data$log_price <- log(data$price)

# correlation 
cor(data$price, data$accommodates)
cor(data$price, data$bedroom_count,use = "pairwise.complete.obs")
cor(data$price, data$bath_count,use = "pairwise.complete.obs")

# regress log price on accommodates, bedroom_count, bath_count
model <- lm(log_price ~ accommodates + bedroom_count + bath_count + neighbourhood_cleansed, data = data)
summary(model)

