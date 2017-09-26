library(dplyr)
library(pracma)

load("data.RData")
data <- tbl_df(data)

## Select columns

data_short <- data %>% select(zipcode, latitude, longitude, property_type, room_type, accommodates, bathrooms, bedrooms, beds, amenities, price, guests_included, minimum_nights, number_of_reviews, review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, review_scores_communication, review_scores_location, review_scores_value, instant_bookable, cancellation_policy)

## Calc price per person

data_short$price <- as.double(substr(paste(data_short$price), 2, 500))
data_short <- data_short %>% filter(!is.na(price))
data_short <- data_short %>% mutate(price_pp = price / guests_included)
data_short <- data_short %>% filter(!is.na(price_pp))

## Start of Zipcode as string

data_short$zipcode <- as.character(data_short$zipcode)
rexp <- "^(\\w+)\\s?(.*)$"
data_short$zip_first <- sub(rexp,"\\1",data_short$zipcode)

## Calculate the distance
my_haversine <- function(lat, long) {
  longlat = c()
  for (i in c(1:length(lat))) {
    longlat <- append(longlat, haversine(c(lat[i], long[i]), c(51.510067, -0.133869)))
  }
  return(longlat)
}
longlatlist <- my_haversine(data_short$latitude, data_short$longitude)
data_short$distance <- longlatlist

count_amenities <- function(amenities){
  count_list = c()
  for (i in c(1:length(amenities))) {
    count_list <- append(count_list, length(strsplit(as.character(amenities[i]), ",")[[1]]))
  }
  return(count_list)
}
count_list <- count_amenities(data_short$amenities)
data_short$amenities_count <- count_list

data_short <- data_short %>% filter(!is.na(review_scores_rating))
# data_short <- data_short %>% filter(price_pp < 150)
# data_short <- data_short %>% filter(number_of_reviews > 3)
# data_short <- data_short %>% filter(grepl("room$", room_type))

save(data_short, file = "data_short.RData")
