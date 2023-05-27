library("readxl")
library("NbClust")
library("factoextra")
library("cluster")

#data read
data <- read_excel("vehicles.xlsx")
data = data[,-1] #removing sample column
data = data[,-19] #removing class column
boxplot(data)

remove_outliers <- function(df) {
  # Loop through each column of the data frame
  for (col in colnames(df)) {
    
    # Compute the statistics for the column
    stats <- boxplot.stats(df[[col]])
    
    # Find the values outside the upper and lower fences
    outliers <- df[[col]][df[[col]] < stats$stats[1] | df[[col]] > stats$stats[5]]
    
    # Replace the outliers with NA
    df[[col]][df[[col]] %in% outliers] <- NA
  }
  
  # Return the data frame with outliers removed
  df <- na.omit(df)
  return(df)
}

clean_data <- remove_outliers(data)
boxplot(clean_data)

scaled_df <- scale(clean_data)
boxplot(scaled_df)

#automated tools---------------------------------------------------------------
NbClust(
  data = scaled_df,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 10,
  index = "all",
  alphaBeale = 0.1,
  method = "kmeans",
)

fviz_nbclust(scaled_df, kmeans, method = 'wss')

fviz_nbclust(scaled_df, kmeans, method = 'gap_stat')

fviz_nbclust(scaled_df, kmeans, method = 'silhouette')
-------------------------------------------------------------------------------
#performing kmenas for 3
kmeans3 <- kmeans(scaled_df, centers <- 3, nstart <- 10)
kmeans3

fviz_cluster(kmeans3, data = scaled_df, stand = FALSE, geom = "point", palette = "jco", ggtheme = theme_minimal())
silhouette_plot <- silhouette(kmeans3$cluster, dist(scaled_df))
fviz_silhouette(silhouette_plot)

#----------------------------------PCA-----------------------------------------
PCA_dataset = prcomp(scaled_df)
PCA_dataset
PCA_dataset_reduced = as.data.frame(-PCA_dataset$x[, 0:6])
head(PCA_dataset_reduced)

#automated tools---------------------------------------------------------------
NbClust(
  data = PCA_dataset_reduced,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 10,
  index = "all",
  alphaBeale = 0.1,
  method = "kmeans"
)

fviz_nbclust(PCA_dataset_reduced, kmeans, method = 'wss')

fviz_nbclust(PCA_dataset_reduced, kmeans, method = 'gap_stat')

fviz_nbclust(PCA_dataset_reduced, kmeans, method = 'silhouette')
-------------------------------------------------------------------------------
#performing kmenas for 2
kmeans2 <- kmeans(PCA_dataset_reduced, centers <- 2, nstart <- 10)
kmeans2

fviz_cluster(kmeans2, data = scaled_df, stand = FALSE, geom = "point", palette = "jco", ggtheme = theme_minimal())

silhouette_plot <- silhouette(kmeans2$cluster, dist(scaled_df))

fviz_silhouette(silhouette_plot)

# Calinski-Harabasz index
ch_index <- calinhara(scaled_df, kmeans2$cluster)
print(ch_index)