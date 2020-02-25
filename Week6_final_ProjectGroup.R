Final_project=read.csv("C:/Users/aarthi/Documents/ALY 6015/FINAL/BlackFriday.csv")
Final_project
library(purrr)
library(ggplot2)
library(dplyr)
library(plotly)
summary(Final_project)

#grouping by custmers based on gender
Blackfriday_gender = Final_project %>% 
  select(User_ID, Gender) %>%
  group_by(User_ID) %>%
  distinct()
summary(Blackfriday_gender$Gender)
#plot for genders of customers
plotfor_gender  = ggplot(data = Blackfriday_gender) +
  geom_bar(mapping = aes(x = Gender, y = ..count.., fill = Gender))+
  labs(title = 'Plot representing the Customers Gender') 
plotfor_gender
#grouping by customers based on age groups
Blackfriday_age = Final_project %>%
  select(User_ID, Age) %>%
  distinct() %>%
  count(Age)
Blackfriday_age

#plot for age of the customers
plotfor_age = ggplot(data = Blackfriday_age) + 
  geom_bar(color = 'white', stat = 'identity', mapping = aes(x = Age, y = n, fill = Age)) +
  labs(title = 'Plot representing the Customers Age') 
plotfor_age

#grouping by the customers based on their location
BlackFriday_Customerlocation =  Final_project %>%
  select(User_ID, City_Category) %>%
  distinct()


#plot for location of the customers
plotfor_customerlocation= ggplot(data = BlackFriday_Customerlocation) +
  geom_bar(color = 'white', mapping = aes(x = City_Category, y = ..count.., fill = City_Category)) +
  labs(title = 'Plot for analyzing Customers Location') 
#gives the output of the plot
plotfor_customerlocation

#grouping by customers based on their purchase cities

BlackFriday_purchasecity = Final_project %>%
  group_by(City_Category) %>%
  summarise(Purchases = sum(Purchase))
purchasesmade_city = BlackFriday_purchasecity %>%
  mutate(purchasesThousands = BlackFriday_purchasecity$Purchases / 1000)
#gives the output of purchases made group by city
purchasesmade_city

#plot for purchases for a city
plotfor_purchasecity= ggplot(data = purchasesmade_city, aes(x = City_Category, y = purchasesThousands, fill = City_Category)) +
  geom_bar(color = 'white', stat = 'identity') +
  labs(title = 'Total Purchase Amount made by customers belonging to a particular City', y = '($1000s)', x = 'City Category') 
#gives the plot for purchases made customers grouping them by particular city
plotfor_purchasecity



# Data Clustering

BlackFridayForClustering <- Final_project %>%
  select(Purchase)

## Determine The Number of Cluster{.tabset .tabset-fade .tabset-pills}

# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = BlackFridayForClustering, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

## Cluster Model{.tabset .tabset-fade .tabset-pills}
# Build a kmeans model
model_km3 <- kmeans(BlackFridayForClustering, centers = 3)

# Extract the cluster assignment vector from the kmeans model
clust_km3 <- model_km3$cluster

# Create a new dataframe appending the cluster assignment
BlackFriday_Clust <- mutate(Final_project, cluster = clust_km3)

# summarise the clustering
BlackFriday_Clust_Note <- BlackFriday_Clust %>%
  group_by(cluster) %>%
  summarise(min_purchase = min(Purchase),
            max_purchase = max(Purchase),
            avg_purchase = round(mean(Purchase),0))
# how many people in each cluster
BlackFriday_Clust %>%
  group_by(City_Category, cluster) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=City_Category, y = n)) +
  geom_col(aes(fill = as.factor(cluster))) +
  theme_linedraw() + 
  theme(legend.box.background	= element_rect(colour = "black"),
        legend.background = element_rect(fill = "gainsboro"),
        panel.background = element_rect(fill = "gainsboro", colour = "white", size = 0.5, linetype = "solid"), #theme panel settings
        plot.background = element_rect(fill = "gainsboro"), #theme panel settings
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), #theme panel settings
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"), #theme panel settings
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black'), #title settings
        plot.subtitle = element_text(face = "italic")) + #subtitle settings
  labs(x = 'City Category', y = 'Total Purchase (dollars)', title = "Black Friday", #name title and axis
       subtitle = "Total people in each cluster by city") + #name subtitle
  guides(fill=guide_legend(title = "Cluster")) + #remove color legend
  scale_y_continuous(labels = scales::comma) #prevent scientific number in x-axis

