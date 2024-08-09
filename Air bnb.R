#Real estate data
#data/airbnb_price.csv This is a CSV file containing data on Airbnb listing prices and locations.

#listing_id: unique identifier of listing
#price: nightly listing price in USD
#nbhood_full: name of borough and neighborhood where listing is located
#data/airbnb_room_type.xlsx This is an Excel file containing data on Airbnb listing descriptions and room types.

#listing_id: unique identifier of listing
#description: listing description
#room_type: Airbnb has three types of rooms: shared rooms, private rooms, and entire homes/apartments
#data/airbnb_last_review.tsv This is a TSV file containing data on Airbnb host names and review dates.

#listing_id: unique identifier of listing
#host_name: name of listing host
#last_review: date when the listing was last reviewed





library(dplyr) 
library(readr)
library(readxl)
library(stringr)

# Begin coding here ...
airbnb_price <- read.csv("data/airbnb_price.csv")
airbnb_room_type <- read_xlsx("data/airbnb_room_type.xlsx")
airbnb_last_review <- read.table("data/airbnb_last_review.tsv", header = TRUE, sep = "\t")

unique(airbnb_last_review$room_type)
table(airbnb_last_review$room_type)

#Converting into  numeric
airbnb_price$price <- gsub(" dollars", "", airbnb_price$price) # Remove " dollars"
airbnb_price$price <- as.numeric(airbnb_price$price) # Convert to numeric

#Convertng into date formart
str(airbnb_last_review$last_review)
airbnb_last_review$last_review<- as.Date(airbnb_last_review$last_review, format = "%B %d %Y")


#Checking room type
head(airbnb_room_type)
unique(airbnb_room_type$room_type)
table(airbnb_room_type$room_type)
airbnb_room_type <- airbnb_room_type %>%
  mutate(room_type = case_when(
    room_type %in% c("Entire home/apt", 'entire home/apt', 'ENTIRE HOME/APT') ~ "Entire home/apt",
    room_type %in% c('private room', 'Private room', 'PRIVATE ROOM') ~ "Private room",
    room_type %in% c('shared room', 'Shared room','SHARED ROOM') ~ "Shared room",
    TRUE ~ room_type # Keep any values not matching above categories as is
  ))
table(airbnb_room_type$room_type)

#merging the datasets
merged_data<-airbnb_price  %>% full_join(airbnb_room_type, by = "listing_id")  %>% 
  full_join(airbnb_last_review, by = "listing_id")
head(merged_data)

table(merged_data$room_type)


#Creating the four variables
first_reviewed <- min (merged_data$last_review , na.rm = TRUE) 
last_reviewed <- max (merged_data$last_review, na.rm = TRUE)  
nb_private_rooms <- sum (merged_data$room_type=="Private room", na.rm = TRUE)
avg_price <- mean (merged_data$price, na.rm =  TRUE)

review_dates<-tibble( first_reviewed = first_reviewed,
                      last_reviewed = last_reviewed,
                      nb_private_rooms = nb_private_rooms,
                      avg_price = avg_price)

review_dates