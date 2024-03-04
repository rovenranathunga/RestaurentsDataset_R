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


#shows facet 1 of average ratings of each cuisine per borough
facet_1 <- c %>% group_by(borough,cuisine) %>%
  summarise(rating=mean(rating, na.rm=TRUE))

ggplot(data=facet_1)+
  geom_point(mapping=aes(x=cuisine,y=rating))+
  facet_wrap(~borough, nrow=2) + 
  scale_x_discrete(guide = guide_axis(angle=80))
##facet 1

##facet 2
#groups by name in the c dataset
names<- group_by(c,name) 

#counts the number of each restaurant
names1 <- count(names,name)

#combines the original c dataset with number of restaurants we just counted
new_table <- inner_join(x= names, y= names1, by = "name")

#the actual facet part
ggplot(data = new_table)+
  geom_point(mapping=aes(x=n, y = rating)) +
  facet_grid(cuisine ~ borough)
##facet2

### TASK 3
#group by cuisine
cuisineType <- group_by(df, cuisine)
#count each cuisine and store in variable nRestaurant
nRestaurant <- count(cuisineType)
#get rid of NA values
cuisineType=na.omit(cuisineType)
#calculate average of ratings and change cuisineType to a dataframe
cuisineType<- as.data.frame(summarise(cuisineType, avgRating=mean(rating)))
#append the count of cuisine to cuisineType dataframe
cuisineType$count = nRestaurant$n
View(nRestaurant)
view(df)
#consolidate the two dataframes by cuisine column
dataset<-inner_join(x=df, y=cuisineType, by="cuisine")
#select for columns 3-6
dataset=dataset[,3:6]
view(cuisineType)

View(dataset)
#plot with count on y and avgRating on x
ggplot(dataset,aes(count, avgRating)) + geom_point(size=2)
#get rid of NA values
dataset=na.omit(dataset)
#k-means clustering
set.seed(123)
#do k-means calculations
datasetCluster <- kmeans(dataset[,3:4], center=3,nstart=20)

#store in table to compare predicted and original values

cluster_matched = table(datasetCluster$cluster,dataset$cuisine)
view(cluster_matched)
#set cluster to categorical values
dataset$cluster <- datasetCluster$cluster
dataset$cluster <- factor(dataset$cluster)
View(dataset)

#plot cluster method

ggplot(dataset, aes(count, avgRating)) + geom_point(aes(col=cluster), size=2)

kmeans_accuracy = (cluster_matched[1,2]+cluster_matched[2,3]+cluster_matched[3,1])/nrow(dataset)
kmeans_accuracy

result<-kmeans(dataset, 3)

