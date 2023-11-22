library(tidyverse)

rm(list=ls())

#setting working directory
setwd("Documents/ceu/prediction_gabor/DA3-phdma/assignment_2")

#location of folders
data_in  <- "raw_data/"
data_out <- "clean_data/"

library(tidyverse)

# Initial cleaning of the data
data<-read.csv(paste0(data_in,"listings.csv"))
drops <- c("host_thumbnail_url","host_picture_url","listing_url","thumbnail_url","medium_url","picture_url","xl_picture_url","host_url","last_scraped","description", "experiences_offered", "neighborhood_overview", "notes", "transit", "access", "interaction", "house_rules", "host_about", "host_response_time", "name", "summary", "space", "host_location")
data<-data[ , !(names(data) %in% drops)]
write.csv(data,file=paste0(data_in,"airbnb_berlin_listing.csv"))
rm(data)

# opening dataset
df<-read.csv(paste0(data_in,"airbnb_berlin_listing.csv"),
             sep=",",header = TRUE, stringsAsFactors = FALSE)

#drop broken lines - where id is not a character of numbers
df$junk<-grepl("[[:alpha:]]", df$id)
df<-subset(df,df$junk==FALSE)
df<-df[1:ncol(df)-1]

#display the class and type of each columns
sapply(df, class)
sapply(df, typeof)







