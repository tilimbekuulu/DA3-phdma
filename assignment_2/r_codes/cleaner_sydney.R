# The code was adopted from Data Analysis for Business, Economics, and Policy by Gabor Bekes and  Gabor Kezdi
# gabors-data-analysis.com 
# Please refer to all the codes used in 

library(tidyverse)
library(writexl)
library(stargazer)
library(Hmisc)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot)


rm(list=ls())

#location of folders
data_in  <- "raw_data/"
data_out <- "clean_data/"

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# Initial cleaning of the data ( switch between listings.csv and listings_june.csv correspondingly)
#data <-read.csv(paste0(data_in,"listings.csv"),sep=",", header = TRUE, stringsAsFactors = FALSE) # September 2023
data <-read.csv(paste0(data_in,"listings_june.csv"),sep=",", header = TRUE, stringsAsFactors = FALSE) # holdout june 2022


# check for missing values in the main variables
data %>% summarise_all(funs(sum(is.na(.))))

# dropping some of the variables

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
price_table<- data %>%
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

mean(data$price) # 194.6884
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
data$beach_access <- grepl("Beach access", data$amenities)
data$view <- grepl("view", data$amenities)
data$premium <- grepl("premium|Premium", data$amenities)
data$waterfront <- grepl("Waterfront", data$amenities)

# drop amenities column
data <- data %>% select(-amenities,-name)
#write csv
#write.csv(data,file=paste0(data_out,"airbnb_sydney_cleaned.csv"))

# run table 
# drop host_acceptance_rate and host_response_rate
data <- data %>% select(-c(host_acceptance_rate, host_response_rate,
                           host_listings_count,host_total_listings_count,
                           has_availability,calculated_host_listings_count_shared_rooms,
                           calculated_host_listings_count_private_rooms,calculated_host_listings_count_entire_homes,
                           last_review,first_review,maximum_nights,number_of_reviews,number_of_reviews_l30d,reviews_per_month))

# property type
table(data$property_type)
# group by property type and calculate average price
property_type_table <- data %>%
  group_by(property_type) %>%
  summarise(count = n(), median_price = median(price),avg_price = mean(price))
# Pool some property types together
data$property_type <- ifelse(data$property_type %in% c("Entire home", "Entire townhouse","Entire condo", "Entire serviced apartment
"), data$property_type, "Other")
table(data$property_type)

# create a factor variables for some property types
data <- data %>% mutate(f_property_type = factor(property_type))
table(data$f_property_type)

# room type
#Room type as factor
table(data$room_type)
data <- data %>% mutate(f_room_type = factor(room_type))

# neighborhood as factors
data$f_neighbourhood_cleansed = factor(data$neighbourhood_cleansed)


# Create numeric variables
numericals <- c("accommodates","beds","minimum_nights", "number_of_reviews_ltm",
                "review_scores_rating","calculated_host_listings_count",
                "bath_count","bedroom_count","amenities_count")
data <- data %>%
  mutate_at(vars(numericals), list("n"=as.numeric))

nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)

#############################################################
# selecting dummies
dummies <- names(data)[seq(2,4)]
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))

# rename column instant_bookable
colnames(data)[14] <- "d_instant_bookable"

# 2 part
dummies <- names(data)[seq(19,37)]
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))


# keep columns if contain d_, n_,f_, 
data <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*"), price, id)

# drop if no price
data <- data %>%
  drop_na(price) # 18709
summary(data$price)
describe(data$price)

data <- data %>%
  mutate(ln_price = log(price))

# Histograms
lnprice_g <- ggplot(data, aes(ln_price)) +
  geom_histogram(binwidth = 0.15, fill = color[3], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("Count") +
  xlab("Log price") +
  theme_bg()
lnprice_g

price_g2 <- ggplot(data, aes(price)) +
  geom_histogram(binwidth = 25, fill = color[3], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("count") +
  xlab("Price") +
  theme_bg()
price_g2

# save price_g2 in graph folder
ggsave("graphs/price_g2.png", plot = price_g2, width = 6, height = 4, units = "in", dpi = 300)



# Graphs for some of the variables
data %>%
  group_by(n_accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

n_accommodates <- ggplot(data = data, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, colour=color[3], shape=16)+
  ylim(0,800)+
  xlim(2,6)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="lm", colour=color[1], se=FALSE)+
  theme_bg()
n_accommodates

# Squares and further values to create
data <- data %>%
  mutate(n_accommodates2=n_accommodates^2, ln_accommodates=log(n_accommodates),
         ln_accommodates2=log(n_accommodates)^2)

# Regression 1: ln price and num of accommodates and squares
summary(lm(ln_price ~ n_accommodates + n_accommodates2, data=data))
summary(lm(price ~ n_accommodates + n_accommodates2, data=data))
# Regression 2: ln price and log num of accommodates
summary(lm(ln_price ~ ln_accommodates + ln_accommodates2 , data=data))

## Beds
data %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

data <- data %>%
  mutate(n_beds2=n_beds^2)
# Regression price and beds
summary(lm(ln_price ~ n_beds + n_beds2, data=data))

ggplot(data, aes(n_bath_count)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of bathrooms") +
  theme_bg()

# Pool accommodations with 0,1,2,3+ bathrooms
data %>%
  group_by(n_bath_count) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

data <- data %>%
  mutate(f_bathroom = cut(n_bath_count, c(0,1,2,3,10), labels=c(0,1,2,3), right = F) )
data %>%
  group_by(f_bathroom) %>%
  summarise(mean_price = mean(price), n = n())

# regress price on f_bathroom
summary(lm(ln_price ~ f_bathroom, data=data))

# Number of reviews (last 30 days)
table_reviews <- data %>%
  group_by(n_number_of_reviews_ltm) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews_ltm, 
                                   c(0,1,10,max(data$n_number_of_reviews_ltm)+1), labels=c(0,1,2), right = F))
data %>%
  group_by(f_number_of_reviews) %>%
  summarise(median_price = median(price) ,mean_price = mean(price) ,  n=n())
# regress price on f_number_of_reviews
summary(lm(ln_price ~ f_number_of_reviews, data=data))

## review score effect
ggplot(data = data, aes(x=n_review_scores_rating , y=price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_bg()

# add quadratic term
data <- data %>%
  mutate(n_review_scores_rating2=n_review_scores_rating^2)
# regress price on review score
summary(lm(ln_price ~ n_review_scores_rating + n_review_scores_rating2, data=data))

# minimum nights
table_nights <- data %>%
  group_by(n_minimum_nights) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# Pool and categorize the number of minimum nights: 1,2,3, 3+
data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,1126), labels=c(1,2,3), right = F))
data %>%
  group_by(f_minimum_nights) %>%
  summarise(median_price = median(price) ,mean_price = mean(price) ,  n=n())


# regress price on f_minimum_nights
summary(lm(ln_price ~ f_minimum_nights, data=data))

# Pool calculated host listings count
data <- data %>%
  mutate(f_calculated_host_listings_count= cut(n_calculated_host_listings_count, c(1,2,max(data$n_calculated_host_listings_count)+1), labels=c(1,2), right = F))

data %>%
  group_by(f_calculated_host_listings_count) %>%
  summarise(median_price = median(price) ,mean_price = mean(price) ,  n=n())

# regress price on f_calculated_host_listings_count
summary(lm(ln_price ~ f_calculated_host_listings_count, data=data))

# Pool number of amenities

## review score effect
ggplot(data = data, aes(x=n_amenities_count , y=price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Number of amenities",y="Daily price (USD)")+
  theme_bg()

# add quadratic term
data <- data %>%
  mutate(n_amenities_count2=n_amenities_count^2)
# regress price on amenities count
summary(lm(ln_price ~ n_amenities_count + n_amenities_count2, data=data))

# Pool number of amenities

# table by grouping amenities
table_amenities <- data %>%
  group_by(n_amenities_count) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

data <- data %>%
  mutate(f_amenities_count= cut(n_amenities_count, c(0,10,15,200), labels=c(1,2,3), right = F))

data %>%
  group_by(f_amenities_count) %>%
  summarise(median_price = median(price) ,mean_price = mean(price) ,  n=n())

# regress price on f_amenities_count
summary(lm(ln_price ~ f_amenities_count, data=data))

# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)

###########################################################################################################

data <- data %>% 
  mutate_if(is.character, factor)

# Quick look at data #
######################
glimpse(data)
skim(data)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# Missing values
data <- data %>%
  drop_na(price)

#imputation
data <- data %>%
  mutate(
    n_bath_count =  ifelse(is.na(n_bath_count), median(n_bath_count, na.rm = T), n_bath_count), #assume at least 1 bath
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), # bed = accommodates
    n_beds2 = ifelse(is.na(n_beds2), n_accommodates^2, n_beds2), 
    n_bedroom_count = ifelse(is.na(n_bedroom_count), 1, n_bedroom_count), # assume at least 1 bedroom
  )

# drop columns when many missing not imortant
to_drop <- c("n_review_scores_rating", "n_review_scores_rating2")
data <- data %>%
  select(-one_of(to_drop))

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

skim(data)

# Look at some descriptive statistics
#####################################

#How is the average price changing in my district by `property_type`, `room_type` 
data %>%
  group_by(f_room_type,f_property_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

Hmisc::describe(data$price)

g4 <- ggplot(data = data, aes(x = f_room_type, y = price)) +
  stat_boxplot(aes(group = f_room_type), geom = "errorbar", width = 0.3,
               color = c(color[2],color[1], color[3],color[4]), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = f_room_type),
               color = c(color[2],color[1], color[3],color[4]), fill = c(color[2],color[1], color[3],color[4]),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,550), breaks = seq(0,600,100)) +
  labs(x = "Room type",y = "Price (US dollars)")+
  theme_bg()
g4
# save g4 in graph folder
ggsave("graphs/g4.png", width = 6, height = 4, units = "in", dpi = 300)

g5 <- ggplot(data, aes(x = factor(n_accommodates), y = price,
                       fill = factor(f_property_type), color=factor(f_property_type))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c(color[2],color[1],color[3],color[4])) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1],color[3],color[4])) +
  labs(x = "Accomodates (Persons)",y = "Price (US dollars)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 650), breaks = seq(0,650,50))+
  theme_bg() +
  theme(legend.position = c(0.1,0.8))        
g5

# save g5 in graph folder
ggsave("graphs/g5.png", width = 6, height = 4, units = "in", dpi = 300)

skimr::skim(data)

# Final sample
#write.csv(data,file=paste0(data_out,"airbnb_sydney_cleaned.csv"))
write.csv(data,file=paste0(data_out,"airbnb_sydney_cleaned_june_holdout.csv"))

# check the date for control



