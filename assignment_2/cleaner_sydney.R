library(tidyverse)
library(writexl)

rm(list=ls())

#location of folders
data_in  <- "raw_data/"
data_out <- "clean_data/"

# Initial cleaning of the data
data <-read.csv(paste0(data_in,"listings.csv"),sep=",", header = TRUE, stringsAsFactors = FALSE)

# check for missing values in the main variables
data %>% summarise_all(funs(sum(is.na(.))))

# dropping some of the variables
data<-read.csv(paste0(data_in,"listings.csv"))
drops <- c("neighbourhood_group_cleansed","calendar_updated","bathrooms","listing_url",
           "scrape_id","last_scraped","source","description","neighborhood_overview","picture_url",
            "host_url","host_name","host_since","host_location","host_about","host_thumbnail_url",
           "host_picture_url","host_neighbourhood","host_verifications", "neighbourhood","host_response_time","license",
           "review_scores_accuracy","review_scores_cleanliness","review_scores_checkin","review_scores_communication",
           "review_scores_location","review_scores_value","latitude","longitude", "minimum_minimum_nights",
           "minimum_nights_avg_ntm","maximum_minimum_nights","minimum_maximum_nights","maximum_maximum_nights",
           "maximum_nights_avg_ntm","calendar_last_scraped","availability_30","availability_60","availability_90","host_id",
           "availability_365")
data<-data[ , !(names(data) %in% drops)]

# Formatting columns

#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified","instant_bookable")){
  data[[binary]][data[[binary]]=="f"] <- 0
  data[[binary]][data[[binary]]=="t"] <- 1
  data[[binary]][data[[binary]]==""] <- 0
}

#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  data[[perc]]<-gsub("%","",as.character(data[[perc]]))
}

# Use regular expression to extract the number of bedrooms
data$bedroom_text <- str_extract(data$name, "\\b\\d+\\s*bedrooms?\\b")
data$bedroom_count <- str_extract(data$bedroom_text, "\\b\\d+\\b")
data$bedroom_count <- as.numeric(data$bedroom_count)
# Use regular expression to extract the number of baths
data$bath_count <- str_extract(data$bathrooms_text, "\\b\\d+\\b")
data$bath_count <- as.numeric(data$bath_count)
# remove certain columns
data <- data %>% select(-c(bathrooms_text, bedroom_text,bedrooms))

# working with price variable

# Remove dollar sign and commas
data$price <- gsub("[\\$,]", "", data$price)
# Convert to numeric
data$price <- as.numeric(data$price)# unusually high prices for some listings (max is 25000$ per night) I checked some of them and it is clearly mistakes.

# cumulative distribution of price table
price_table <- data %>%
  group_by(price) %>%
  summarise(count = n()) %>%
  mutate(cumulative_count = cumsum(count),
         cumulative_percent = cumulative_count / sum(count))

mean(data$price) # 297.628
median(data$price) #176

# Filters to see the effect of amenities on price

# Filter price is less than or equal to 650
data <- data %>% filter(price <= 650)
# filter number of accommodates 2 - 6
data <- data %>% filter(accommodates >= 2 & accommodates <= 6)

mean(data$price) # 194.68
median(data$price)# 165

#amenities
data$amenities<-gsub("\\u2019","'",data$amenities)
data$amenities<-gsub("\\u2013","",data$amenities)
data$amenities<-gsub("\\[","",data$amenities)
data$amenities<-gsub("\\]","",data$amenities)
data$amenities<-gsub('\\"',"",data$amenities)
data$amenities <- gsub("\\\\", "", data$amenities)
data$amenities <- gsub("/", "", data$amenities)

data$amenities<-as.list(strsplit(data$amenities, ","))


# group by amenities
amenities_table <- data %>%
  unnest(amenities) %>%
  group_by(amenities) %>%
  summarise(count = n(), median_price = median(price),avg_price = mean(price))

# number of amenities
data$amenities_count <- lengths(data$amenities)

top_amenities <- amenities_table %>%
  arrange(desc(count)) %>%
  filter( avg_price > 250) %>%
  head(30) %>%
  select(amenities,avg_price,count)

# create a list of amenities to check
amenities_to_check <- top_amenities$amenities
# Remove leading spaces using trimws
amenities_to_check <- trimws(amenities_to_check)
# Print the result
print(amenities_to_check)

# creating amenities variables ( I decided to create one by one for the control, looping through the list is working but some mistakes are hard to avoid)

data$private_balcony <- grepl("Private patio or balcony", data$amenities)
data$bathtub <- grepl("Bathtub", data$amenities)
data$outdoor_furniture <- grepl("Outdoor furniture", data$amenities)
data$outdoor_dining <- grepl("Outdoor dining area", data$amenities)
data$bbq <- grepl("BBQ grill", data$amenities)
data$free_dryer <- grepl("Free dryer  In unit", data$amenities)
data$baking_sheet <- grepl("Baking sheet", data$amenities)
data$ceiling_fan <- grepl("Ceiling fan", data$amenities)
data$barbecue_utensils <- grepl("Barbecue utensils", data$amenities)
data$blender <- grepl("Blender", data$amenities)
data$board_games <- grepl("Board games", data$amenities)
data$hight_chair <- grepl("High chair", data$amenities)
data$nespresso <- grepl("Nespresso", data$amenities)
data$beach_essentials <- grepl("Beach essentials", data$amenities)
data$netflix <- grepl("Netflix", data$amenities)
data$private <- grepl("private", data$amenities)
data$beach_access <- grepl("Beach access", data$amenities)
data$view <- grepl("view", data$amenities)
data$premium <- grepl("premium|Premium", data$amenities)
data$waterfront <- grepl("Waterfront", data$amenities)

# drop amenities column
data <- data %>% select(-amenities,-name)
#write csv
write.csv(data,file=paste0(data_out,"airbnb_sydney_cleaned.csv"))



