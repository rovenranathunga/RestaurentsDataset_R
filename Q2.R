install.packages("mongolite")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("modelr")
install.packages("Metrics")
install.packages("remotes")
library(mongolite)
library(dplyr)
library(tidyverse)
library(modelr)
library(Metrics)


#Specify the path of the .csv file
local_path <- "/Users/roven/Documents/Assignment/restaurants.csv"

##converting the restaurants collection into a data frame
restaurants <- read.csv(local_path)

##removing missing data from the restaurants data frame only for borough and cuisine because they are the only columns relevant to our goal
none_missing <- subset(restaurants, borough != "Missing" & cuisine != "Not Listed/Not Applicable")

#grades is currently a list where each entry is a 4 x 3 data frame,
##here we are extracting the most recent score for each restaurant
grades <- none_missing$grades

score <- lapply(grades, "[", 1,3)

date <- lapply(grades, "[", 1:3,1)

for(i in 1:length(date)){
  date[[i]] <- year(date[[i]])
}


##adding the score that was extracted in lines 20-22 to restaurants so that each restaurant has the correlated score easily readily available
none_missing$rating <- score


##removing all columns from the restaurants data frame except those that are relevant
df <- dplyr::select(none_missing, name, borough, cuisine, rating)

##the following is for question 2
df2 <- dplyr::select(none_missing, name, borough, cuisine)


first_score[sapply(first_score, is.null)] <- NA
second_score[sapply(second_score, is.null)] <- NA
third_score[sapply(third_score, is.null)] <- NA
first_date[sapply(first_date, is.null)] <- NA
second_date[sapply(second_date, is.null)] <- NA
third_date[sapply(third_date, is.null)] <- NA

first_score = unlist(first_score)
second_score = unlist(second_score)
third_score = unlist(third_score)
first_date = unlist(first_date)
second_date = unlist(second_date)
third_date = unlist(third_date)

df2$first_score <- first_score
df2$second_score <- second_score
df2$third_score <- third_score
df2$first_date <- first_date
df2$second_date <- second_date
df2$third_date <- third_date

##removing missing values from df2
df2 <- df2 %>% na.omit()

##American
American <- filter(df2, cuisine == "American")
American_scores = c()
American_scores = append(American_scores, American$first_score)
American_scores = append(American_scores, American$second_score)
American_scores = append(American_scores, American$third_score)

American_dates = c()
American_dates = append(American_dates, American$first_date)
American_dates = append(American_dates, American$second_date)
American_dates = append(American_dates, American$third_date)
##

##for loop, needs to be edited after filtering for only 1 cuisine, at the moment the for loop takes too long
#doing for American first, the ratings_ code needs to be repeated for every cuisine
American_ratings_2015 <- c()
American_ratings_2014 <- c()
American_ratings_2013 <- c()
American_ratings_2012 <- c()
American_ratings_2011 <- c()

for (i in 1:length(American_scores)) {
  year = American_dates[i]
  if(year == 2015) {
    American_ratings_2015 <- append(American_ratings_2015, American_scores[i])
  }
  if(year == 2014) {
    American_ratings_2014 <- append(American_ratings_2014, American_scores[i])
  }
  if(year == 2013) {
    American_ratings_2013 <- append(American_ratings_2013, American_scores[i])
  }
  if(year == 2012) {
    American_ratings_2012 <- append(American_ratings_2012, American_scores[i])
  }
  if(year == 2011) {
    American_ratings_2011 <- append(American_ratings_2011, American_scores[i])
  }
}
##for loop

American_ratings = data.frame(matrix(nrow = 5, ncol = 0))
ratings <- c(mean(American_ratings_2015), mean(American_ratings_2014), mean(American_ratings_2013), mean(American_ratings_2012), mean(American_ratings_2011))
American_ratings$year <- years
American_ratings$average_rating <- ratings


lebronjames <- c()
for(i in 1:length(American_ratings_2015)) {
  lebronjames <- append(lebronjames, 2015)
}
for(i in 1:length(American_ratings_2014)) {
  lebronjames <- append(lebronjames, 2014)
}
for(i in 1:length(American_ratings_2013)) {
  lebronjames <- append(lebronjames, 2013)
}
for(i in 1:length(American_ratings_2012)) {
  lebronjames <- append(lebronjames, 2012)
}
for(i in 1:length(American_ratings_2011)) {
  lebronjames <- append(lebronjames, 2011)
}

michaeljordan <- c(American_ratings_2015)
michaeljordan <- append(michaeljordan, American_ratings_2014)
michaeljordan <- append(michaeljordan, American_ratings_2013)
michaeljordan <- append(michaeljordan, American_ratings_2012)
michaeljordan <- append(michaeljordan, American_ratings_2011)

newdf <- data.frame(matrix(nrow = length(lebronjames), ncol = 0))
newdf$year <- lebronjames
newdf$rating <- michaeljordan

oneway.test(rating ~ pred, data=newdf, var.equal=FALSE)

graphlmao <- ggplot(data = newdf) +
  geom_point(mapping = aes(x = year, y = rating))

sim1 <- lm(rating ~ year, data = newdf)
coef(sim1)

grid <- data_grid(newdf, year)
grid <- add_predictions(grid, sim1)

ggplot(newdf, aes(year)) +
  geom_point(aes(y = rating)) +
  geom_line(aes(y=pred), data = grid, colour = "red", size = 1)

newdf <- newdf %>% add_predictions(sim1)

rmse(newdf$rating, newdf$pred)


View(newdf)
class(newdf)
American_graph = ggplot(data = American_ratings) +
  geom_point(mapping = aes(x = year, y = average_rating))
##American

##Chinese
Chinese <- filter(df2, cuisine == "Chinese")
Chinese_scores = c()
Chinese_scores = append(Chinese_scores, Chinese$first_score)
Chinese_scores = append(Chinese_scores, Chinese$second_score)
Chinese_scores = append(Chinese_scores, Chinese$third_score)

Chinese_dates = c()
Chinese_dates = append(Chinese_dates, Chinese$first_date)
Chinese_dates = append(Chinese_dates, Chinese$second_date)
Chinese_dates = append(Chinese_dates, Chinese$third_date)
##

##for loop, needs to be edited after filtering for only 1 cuisine, at the moment the for loop takes too long
#doing for Chinese first, the ratings_ code needs to be repeated for every cuisine
chinese_ratings_2015 <- c()
chinese_ratings_2014 <- c()
chinese_ratings_2013 <- c()
chinese_ratings_2012 <- c()
chinese_ratings_2011 <- c()

for (i in 1:length(Chinese_scores)) {
  year = Chinese_dates[i]
  if(year == 2015) {
    chinese_ratings_2015 <- append(chinese_ratings_2015, Chinese_scores[i])
  }
  if(year == 2014) {
    chinese_ratings_2014 <- append(chinese_ratings_2014, Chinese_scores[i])
  }
  if(year == 2013) {
    chinese_ratings_2013 <- append(chinese_ratings_2013, Chinese_scores[i])
  }
  if(year == 2012) {
    chinese_ratings_2012 <- append(chinese_ratings_2012, Chinese_scores[i])
  }
  if(year == 2011) {
    chinese_ratings_2011 <- append(chinese_ratings_2011, Chinese_scores[i])
  }
}
##for loop

Chinese_ratings = data.frame(matrix(nrow = 5, ncol = 0))
ratings <- c(mean(chinese_ratings_2015), mean(chinese_ratings_2014), mean(chinese_ratings_2013), mean(chinese_ratings_2012), mean(chinese_ratings_2011))
Chinese_ratings$year <- years
Chinese_ratings$average_rating <- ratings

Chinese_graph = ggplot(data = Chinese_ratings) +
  geom_point(mapping = aes(x = year, y = average_rating))
##chinese

##Pizza
Pizza <- filter(df2, cuisine == "Pizza")
Pizza_scores = c()
Pizza_scores = append(Pizza_scores, Pizza$first_score)
Pizza_scores = append(Pizza_scores, Pizza$second_score)
Pizza_scores = append(Pizza_scores, Pizza$third_score)

Pizza_dates = c()
Pizza_dates = append(Pizza_dates, Pizza$first_date)
Pizza_dates = append(Pizza_dates, Pizza$second_date)
Pizza_dates = append(Pizza_dates, Pizza$third_date)
##

##for loop, needs to be edited after filtering for only 1 cuisine, at the moment the for loop takes too long
#doing for Pizza first, the ratings_ code needs to be repeated for every cuisine
Pizza_ratings_2015 <- c()
Pizza_ratings_2014 <- c()
Pizza_ratings_2013 <- c()
Pizza_ratings_2012 <- c()
Pizza_ratings_2011 <- c()

for (i in 1:length(Pizza_scores)) {
  year = Pizza_dates[i]
  if(year == 2015) {
    Pizza_ratings_2015 <- append(Pizza_ratings_2015, Pizza_scores[i])
  }
  if(year == 2014) {
    Pizza_ratings_2014 <- append(Pizza_ratings_2014, Pizza_scores[i])
  }
  if(year == 2013) {
    Pizza_ratings_2013 <- append(Pizza_ratings_2013, Pizza_scores[i])
  }
  if(year == 2012) {
    Pizza_ratings_2012 <- append(Pizza_ratings_2012, Pizza_scores[i])
  }
  if(year == 2011) {
    Pizza_ratings_2011 <- append(Pizza_ratings_2011, Pizza_scores[i])
  }
}
##for loop

Pizza_ratings = data.frame(matrix(nrow = 5, ncol = 0))
ratings <- c(mean(Pizza_ratings_2015), mean(Pizza_ratings_2014), mean(Pizza_ratings_2013), mean(Pizza_ratings_2012), mean(Pizza_ratings_2011))
Pizza_ratings$year <- years
Pizza_ratings$average_rating <- ratings

Pizza_graph = ggplot(data = Pizza_ratings) +
  geom_point(mapping = aes(x = year, y = average_rating))
##Pizza

##Italian
Italian <- filter(df2, cuisine == "Italian")
Italian_scores = c()
Italian_scores = append(Italian_scores, Italian$first_score)
Italian_scores = append(Italian_scores, Italian$second_score)
Italian_scores = append(Italian_scores, Italian$third_score)

Italian_dates = c()
Italian_dates = append(Italian_dates, Italian$first_date)
Italian_dates = append(Italian_dates, Italian$second_date)
Italian_dates = append(Italian_dates, Italian$third_date)
##

##for loop, needs to be edited after filtering for only 1 cuisine, at the moment the for loop takes too long
#doing for Italian first, the ratings_ code needs to be repeated for every cuisine
Italian_ratings_2015 <- c()
Italian_ratings_2014 <- c()
Italian_ratings_2013 <- c()
Italian_ratings_2012 <- c()
Italian_ratings_2011 <- c()

for (i in 1:length(Italian_scores)) {
  year = Italian_dates[i]
  if(year == 2015) {
    Italian_ratings_2015 <- append(Italian_ratings_2015, Italian_scores[i])
  }
  if(year == 2014) {
    Italian_ratings_2014 <- append(Italian_ratings_2014, Italian_scores[i])
  }
  if(year == 2013) {
    Italian_ratings_2013 <- append(Italian_ratings_2013, Italian_scores[i])
  }
  if(year == 2012) {
    Italian_ratings_2012 <- append(Italian_ratings_2012, Italian_scores[i])
  }
  if(year == 2011) {
    Italian_ratings_2011 <- append(Italian_ratings_2011, Italian_scores[i])
  }
}
##for loop

Italian_ratings = data.frame(matrix(nrow = 5, ncol = 0))
ratings <- c(mean(Italian_ratings_2015), mean(Italian_ratings_2014), mean(Italian_ratings_2013), mean(Italian_ratings_2012), mean(Italian_ratings_2011))
Italian_ratings$year <- years
Italian_ratings$average_rating <- ratings

Italian_graph = ggplot(data = Italian_ratings) +
  geom_point(mapping = aes(x = year, y = average_rating))
##Italian

##coffee
coffee <- filter(df2, cuisine == "Café/Coffee/Tea")
coffee_scores = c()
coffee_scores = append(coffee_scores, coffee$first_score)
coffee_scores = append(coffee_scores, coffee$second_score)
coffee_scores = append(coffee_scores, coffee$third_score)

coffee_dates = c()
coffee_dates = append(coffee_dates, coffee$first_date)
coffee_dates = append(coffee_dates, coffee$second_date)
coffee_dates = append(coffee_dates, coffee$third_date)
##

##for loop, needs to be edited after filtering for only 1 cuisine, at the moment the for loop takes too long
#doing for coffee first, the ratings_ code needs to be repeated for every cuisine
coffee_ratings_2015 <- c()
coffee_ratings_2014 <- c()
coffee_ratings_2013 <- c()
coffee_ratings_2012 <- c()
coffee_ratings_2011 <- c()

for (i in 1:length(coffee_scores)) {
  year = coffee_dates[i]
  if(year == 2015) {
    coffee_ratings_2015 <- append(coffee_ratings_2015, coffee_scores[i])
  }
  if(year == 2014) {
    coffee_ratings_2014 <- append(coffee_ratings_2014, coffee_scores[i])
  }
  if(year == 2013) {
    coffee_ratings_2013 <- append(coffee_ratings_2013, coffee_scores[i])
  }
  if(year == 2012) {
    coffee_ratings_2012 <- append(coffee_ratings_2012, coffee_scores[i])
  }
  if(year == 2011) {
    coffee_ratings_2011 <- append(coffee_ratings_2011, coffee_scores[i])
  }
}
##for loop

coffee_ratings = data.frame(matrix(nrow = 5, ncol = 0))
ratings <- c(mean(coffee_ratings_2015), mean(coffee_ratings_2014), mean(coffee_ratings_2013), mean(coffee_ratings_2012), mean(coffee_ratings_2011))
coffee_ratings$year <- years
coffee_ratings$average_rating <- ratings

coffee_graph = ggplot(data = coffee_ratings) +
  geom_point(mapping = aes(x = year, y = average_rating))
##coffee



##finding highest and lowest year for ratings
##don't need this code anymore but we could keep it to talk about it in the report
length(new$name)
new = na.omit(df2)
highest = df2$first_date[[1]]
lowest = df2$first_date[[1]]

for(i in 1:length(new$first_date)){
  if(new$first_date[[i]] > highest){
    highest = new$first_date[[i]]
  }
} ##2015 is highest year

for(i in 1:length(new$first_date)){
  if(new$first_date[[i]] < lowest){
    lowest = new$first_date[[i]]
  }
} ##2011 is lowest year
##finding highest and lowest year for ratings









#TASK 2
df$rating = as.integer(as.character(df$rating))

#finding which cuisine there are the most of (which cuisine we have the most entries for)
#done using the below command then filtering the column 'n'
View(count(df,cuisine))

#filtering the dataframe to only include the top 5 most common cuisines, as they are the one's we have chosen to graph.
c <- filter(df, cuisine == "American" | cuisine == "Chinese" | cuisine == "Pizza" | cuisine == "Italian" | cuisine == "Café/Coffee/Tea")
View(c)


##below includes tasks 3 and 4
##from the below graphs, observe each group to find out which cuisine has the highest and lowest average rating in each borough,
##^then, find out how many restaurants there are for the highest and lowest rated cuisine,
##^this let's us know how many review entries there are for each respective cuisine and if the number of entries affects the average.
#------------Bronx--------------
#smoothgeom
bronx = filter(c, borough == "Bronx")
View(bronx)



bronx_grouped <- group_by(bronx, cuisine)
bronx_average<-bronx_grouped %>% group_by(cuisine) %>% summarise(rating=mean(rating, na.rm = TRUE), count = n())
View(bronx_average)

#explain why there is such a big curve. explain the correlation (if there is any)
ggplot(data = bronx_average, aes(x = count, y = rating )) +
  geom_point() +
  geom_smooth()

table(bronx$cuisine)
#-------------------------------
#------------Brooklyn-----------
#boxplot
brooklyn = filter(c, borough == "Brooklyn")
View(brooklyn)

brooklyn_grouped <- group_by(brooklyn, cuisine)
brooklyn_average<-summarise(brooklyn_grouped, rating=mean(rating, na.rm = TRUE))
View(brooklyn_average)

ggplot(data = brooklyn_average) +
  geom_point(mapping = aes(x = cuisine, y = rating))

#compare the medians of each cuisine. note any outliers and how that may have affected the results.

box_plot <- ggplot(brooklyn,aes(x=cuisine, y=rating))
box_plot + geom_boxplot()
#-------------------------------
#------------Manhattan----------
Manhattan = filter(c, borough == "Manhattan")
View(Manhattan)

Manhattan_grouped <- group_by(Manhattan, cuisine)
Manhattan_average<-summarise(Manhattan_grouped, rating=mean(rating, na.rm = TRUE))
View(Manhattan_average)

ggplot(data = Manhattan_average) +
  geom_point(mapping = aes(x = cuisine, y = rating))

#explain that over 50% of the ratings are in the ~10 range. that there are hardly any ratings past 40
hist(Manhattan$rating)
#-----------------------------
#------------Queens-----------
Queens = filter(c, borough == "Queens")
View(Queens)

Queens_grouped <- group_by(Queens, cuisine)
Queens_average<-summarise(Queens_grouped, rating=mean(rating, na.rm = TRUE))
View(Queens_average)

ggplot(data = Queens_average) +
  geom_point(mapping = aes(x = cuisine, y = rating))
#-------------------------------
#------------Staten Island------
Staten_island = filter(c, borough == "Staten Island")
View(Staten_island)

Staten_island_grouped <- group_by(Staten_island, cuisine)
Staten_island_average<-summarise(Staten_island_grouped, rating=mean(rating, na.rm = TRUE))
View(Staten_island_average)

ggplot(data = Staten_island_average) +
  geom_point(mapping = aes(x = cuisine, y = rating))
#-------------------------------


##now finding, how many of each havg and lavg are there in each borough?
count_bronx = count(bronx,cuisine)
View(count_bronx)

ggplot(data = count_bronx, aes(x = cuisine, y = n)) +
  geom_col()
#from this I can see there are 52 Italian restaurants and 441 american restaurants in bronx

count_brooklyn = count(brooklyn,cuisine)
count_brooklyn

ggplot(data = count_brooklyn, aes(x = cuisine, y = n)) +
  geom_col()
#from this I can see there are 763 Chinese restaurants and 289 coffee shops in brooklyn

count_manhattan = count(Manhattan,cuisine)
count_manhattan

ggplot(data = count_manhattan, aes(x = cuisine, y = n)) +
  geom_col()
#from this I can see there are 510 Chinese restaurants and 680 coffee shops in brooklyn

count_queens = count(Queens,cuisine)
count_queens

ggplot(data = count_queens, aes(x = cuisine, y = n)) +
  geom_col()
#from this I can see there are 728 Chinese restaurants and 117 coffee shops in brooklyn

count_staten_island = count(Staten_island,cuisine)
count_staten_island

ggplot(data = count_staten_island, aes(x = cuisine, y = n)) +
  geom_col()
#from this I can see there are 88 Chinese restaurants and 19 coffee shops in brooklyn

##adding random shit lmao
##coffee
sim5 <- lm(average_rating ~ year, data=coffee_ratings)
coef(sim5)

grid <- data_grid(coffee_ratings, year)
grid <- add_predictions(grid, sim5)

ggplot(coffee_ratings, aes(year)) +
  geom_point(aes(y = average_rating)) +
  geom_line(aes(y=pred), data = grid, colour = "red", size  = 1)

coffee_ratings <- coffee_ratings %>% add_predictions(sim5)

sqrt(mean((coffee_ratings$average_rating - coffee_ratings$pred)^2))
##coffee

##italian
sim4 <- lm(average_rating ~ year, data=Italian_ratings)
coef(sim4)

grid <- data_grid(Italian_ratings, year)
grid <- add_predictions(grid, sim4)

ggplot(Italian_ratings, aes(year)) +
  geom_point(aes(y = average_rating)) +
  geom_line(aes(y=pred), data = grid, colour = "red", size  = 1)

Italian_ratings <- Italian_ratings %>% add_predictions(sim4)

sqrt(mean((Italian_ratings$average_rating - Italian_ratings$pred)^2))
##italian

##pizza
sim3 <- lm(average_rating ~ year, data=Pizza_ratings)
coef(sim3)

grid <- data_grid(Pizza_ratings, year)
grid <- add_predictions(grid, sim3)

ggplot(Pizza_ratings, aes(year)) +
  geom_point(aes(y = average_rating)) +
  geom_line(aes(y=pred), data = grid, colour = "red", size  = 1)

Pizza_ratings <- Pizza_ratings %>% add_predictions(sim2)

sqrt(mean((Pizza_ratings$average_rating - Pizza_ratings$pred)^2))
##pizza

##chinese
sim2 <- lm(average_rating ~ year, data=Chinese_ratings)
coef(sim2)

grid <- data_grid(Chinese_ratings, year)
grid <- add_predictions(grid, sim2)

ggplot(Chinese_ratings, aes(year)) +
  geom_point(aes(y = average_rating)) +
  geom_line(aes(y=pred), data = grid, colour = "red", size  = 1)

Chinese_ratings <- Chinese_ratings %>% add_predictions(sim2)

sqrt(mean((Chinese_ratings$average_rating - Chinese_ratings$pred)^2))
##chinese

#american
sim1 <- lm(average_rating ~ year, data=American_ratings)
coef(sim1)

grid <- data_grid(American_ratings, year)
grid <- add_predictions(grid, sim1)

ggplot(American_ratings, aes(year)) +
  geom_point(aes(y = average_rating)) +
  geom_line(aes(y=pred), data = grid, colour = "red", size  = 1)

American_ratings <- American_ratings %>% add_predictions(sim1)

rmse(American_ratings$average_rating, American_ratings$pred)

sqrt(mean((American_ratings$average_rating - American_ratings$pred)^2))
##american






