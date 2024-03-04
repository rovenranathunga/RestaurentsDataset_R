install.packages("mongolite")
install.packages("dplyr")
library(mongolite)
library(dplyr)

#Specify the path of the .csv file
local_path <- "/Users/roven/Documents/Assignment/restaurants.csv"

##converting the restaurants collection into a data frame
restaurants <-read.csv(local_path)
View(restaurants)

##removing missing data from the restaurants data frame only for borough and cuisine because they are the only columsn relevant to our goal
none_missing <- subset(restaurants, borough != "Missing" & cuisine != "Not Listed/Not Applicable")
View(none_missing)

#grades is currently a list where each entry is a 4 x 3 data frame,
##here we are extracting the most recent score for each restaurant
grades <- none_missing$grades
score <- lapply(grades, "[", 1,3)
score

##adding the score that was extracted in lines 20-22 to restaurants so that each restaurant has the correlated score easilyreadily available
none_missing$rating <- score
View(none_missing)

##removing all columns from the restaurants data frame except those that are relevant
Task1_df <- dplyr::select(none_missing, name, borough, cuisine, rating)
View(Task1_df)
