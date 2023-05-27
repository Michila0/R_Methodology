# Load the data
data("USArrests")
# Standardize the data
df  <-  scale(USArrests)     # the z-score normalisation
# Show the first 6 rows
head(df)
  
# Compute the matrix with the Euclidean distances
res.dist <- dist(df, method = "euclidean")
as.matrix(res.dist)[1:6, 1:6]
res.hc1 <- hclust(d = res.dist, method = "complete")   # here we use the complete linkage
# cex: label size
library(factoextra)
fviz_dend(res.hc1, cex = 0.5)




library(cluster)
sil <- silhouette(grp, res.dist)     # complete with 4 clusters
fviz_silhouette(sil)
