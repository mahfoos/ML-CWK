library(tidyverse)

library(readxl)

library(factoextra)

library(NbClust)

library(caret)

library(tidymodels)

library(ggfortify)

library(cluster)




# Read in the original excel datafile


vehicles_original <- read_excel("D:/Accadamic Materials/IIT/02nd Year/Second Semester/Machine Learnig/CourseWork/vehicles.xlsx")  %>%

#plot(vehicles_original)  
janitor::clean_names() %>%




  
#https://www.rdocumentation.org/packages/janitor/versions/1.2.0/topics/clean_names
  
  
mutate(class = as_factor(class))

# Get a birds eye view of how the dataset looks like and detect the Outlier

summary(vehicles_original)

vehicles_original %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "van") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) + 
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'van'")


vehicles_original %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) + 
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'bus'")

vehicles_original %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) + 
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'saab'")

vehicles_original %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) + 
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'opel'")

# Remove the Outlier

vehicles_bus = vehicles_original %>%
  filter(class == "bus") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_van = vehicles_original %>%
  filter(class == "van") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_opel = vehicles_original %>%
  filter(class == "opel") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_saab = vehicles_original %>%
  filter(class == "saab") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

# Combined the all claas 

combined = bind_rows(list(vehicles_bus,vehicles_opel,vehicles_saab,vehicles_van)) %>%
  arrange(samples)



print(combined)

# Transformed Outliers Class
combined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers class: 'bus'")

combined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "van") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers class: 'van'")

combined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers for class: saab")



combined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers for class: opel")

#fviz_nbclust(x, FUNcluster, method = c("silhouette", "wss", "gap_stat"))


# Remove the sample name and the class name. Both of these will be remove so that only n

#Numerical data is left for the algorithm.

vehicles_data_points = combined %>%
  select(-samples, -class)

# Now that we have the "vehicles_data_points" dataset, scaling is performed

vehicles_scaled = vehicles_data_points %>%
  mutate(across(everything(), scale))


set.seed(123)


#Determining Optimal Number of Clusters

# Use Euclidean for distance
cluster_euclidean = NbClust(vehicles_scaled,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")

# Plot the best cluster 
factoextra::fviz_nbclust(cluster_euclidean) + theme_minimal() + 
  ggtitle("NbClust's optimal number of clusters")



# Use manhattan for distance
cluster_manhattan = NbClust(vehicles_scaled,distance="manhattan", min.nc=2,max.nc=15,method="kmeans",index="all")

# Plot the best cluster 
factoextra::fviz_nbclust(cluster_manhattan) + theme_minimal() + 
  ggtitle("NbClust's optimal number of clusters")


# The Silhouette Method
fviz_nbclust(vehicles_scaled, kmeans, method = "silhouette", k.max = 24) + theme_minimal() + ggtitle("The Silhouette Plot")

#compute k-means in R with the kmeans() function:
result<- kmeans(vehicles_scaled ,centers = 2, nstart = 30) # aplly k-means algorithm with no. of k = 2

# Cluster plot
fviz_cluster(result, data = vehicles_scaled)

table(vehicles_original$class,result$cluster)
