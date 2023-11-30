rm(list=ls())

library(tidyverse)
library(stargazer)
library(Hmisc)
library(skimr)

#location of folders
data_in  <- "raw_data/"
data_out <- "clean_data/"

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# Import the cleaned data
data <- read_csv(paste(data_out,"airbnb_sydney_cleaned.csv", sep = ""))

# run table 

# drop host_acceptance_rate and host_response_rate
data <- data %>% select(-c(host_acceptance_rate, host_response_rate,
                           host_listings_count,host_total_listings_count,
                           has_availability,calculated_host_listings_count_shared_rooms,
                           calculated_host_listings_count_private_rooms,calculated_host_listings_count_entire_homes,
                           last_review,first_review,maximum_nights))



# property type
table(data$property_type)
# group by property type and calculate average price
property_type_table <- data %>%
  group_by(property_type) %>%
  summarise(count = n(), median_price = median(price),avg_price = mean(price))

# create a factor variables for some property types
data$entire_home <- ifelse(data$property_type == "Entire home", 1, 0)
data$townhouse <- ifelse(data$property_type == "Entire townhouse", 1, 0)
data$service_apartment <- ifelse(data$property_type == "Entire serviced apartment", 1, 0)
data$entire_condo <- ifelse(data$property_type == "Entire condo", 1, 0)

sum(data$entire_home)
sum(data$townhouse)
sum(data$service_apartment)
sum(data$entire_condo)

# drop property type column
data <- data %>% select(-property_type)

# room type
#Room type as factor
table(data$room_type)
data <- data %>% mutate(f_room_type = factor(room_type))

# Rename
data$f_room_type <- factor(ifelse(data$f_room_type== "Entire home/apt", "Entire/Apt",
                                   ifelse(data$f_room_type== "Private room", "Private",
                                          ifelse(data$f_room_type== "Shared room", "Shared", "."))))

# neighborhood as factors
data$f_neighbourhood_cleansed = factor(data$neighbourhood_cleansed)

# Create numeric variables
numericals <- c("accommodates","beds","minimum_nights","number_of_reviews", "number_of_reviews_ltm",
                "number_of_reviews_l30d","review_scores_rating","calculated_host_listings_count",
               "reviews_per_month","bath_count","bedroom_count","amenities_count")
data <- data %>%
  mutate_at(vars(numericals), list("n"=as.numeric))

nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)

# dropping variables
data<-data[ , !(names(data) %in% numericals)]

# selecting dummies
dummies <- names(data)[seq(3,7)]
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))
# 2 part
dummies <- names(data)[seq(9,33)]
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

# with price info only
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
  mutate(n_accommodates2=n_accommodates^2, ln_accommodates=log(n_accommodates) ,
         ln_accommodates2=log(n_accommodates)^2)

# Regression 1: ln price and num of accommodates and squares
summary(lm(ln_price ~ n_accommodates + n_accommodates2, data=data))
# Regression 2: ln price and log num of accommodates
summary(lm(ln_price ~ ln_accommodates , data=data))
# Regression 3: ln price and num of accommodates
lm(ln_price ~ n_accommodates, data=data)



