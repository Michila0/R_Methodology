library(datasets)
head(iris)

names(iris)

x = iris[,-5]         # here the x now contains only the first 4 columns, as the 5th has been omitted. 

y = iris$Species  #the $ sign is associated with the label of that particular column 
kc <- kmeans(x,3)
kc

wss = kc$tot.withinss     #pay attention the $ sign and its use)
bss = kc$betweenss
wss
bss

table(y,kc$cluster)  

plot(x[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=23, cex=3)

#IRIS Dataset (k-means code that includes also some tools for "finding" the proper number of clusters)
library(NbClust)
data(iris)
str(iris)   # see the outcome of this str command; very informative information for our dataset
head(iris)

#library(arules)
#library(arulesViz)
#library(datasets)

