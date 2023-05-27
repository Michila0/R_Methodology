#age Vector
age <- c(25, 35, 50)
#Salary Vector
salary <- c(200000, 1200000, 2000000)
#data frame created using age and salary
df <- data.frame("Age"= age, "Salary" = salary, stringsAsFactors = FALSE)
df

#Linear Normalization
normalize <- function(x){
  return(x - min(x)) / (max(x) - min(x)) 
}
dfNorm <- as.data.frame(lapply(df, normalize))
#one could also use sequencw such as df[1:2]
dfNorm <- as.data.frame(lapply(df[1:2], normalize))
dfNorm

#General Normalizatiion
#Note df[2]
dfNorm <- as.data.frame(lapply(df[2], normalize))
#or df["Salar"]
dfNorm <- as.data.frame(lapply(df["Salary"], normalize))

new_normalize <- function(x, new_max = 1, new_min = 0){
  a = (((x - min(x) *(new_max - new_min)) /(max(x)- min(x)))) + new_min
  return(a)
}
dfNorm1 <- as.data.frame(lapply(df[1:2], new_normalize))
dfNorm1


#Z-Score Standardization
dfNormZ <- as.data.frame(scale(df[1:2]))
dfNormZ

Z_score = function(x){
  return((x - mean(x)) / sd(x))
}
dfNorm4 <- as.data.frame(lapply(df, Z_score))
dfNorm4


#PCA
library(tidyverse) # data manipulation and visualization (first load tidyverse and then rlang!)
library(gridExtra) # plot arrangement
library(ggplot2) # for ggplot and qplot
library(rlang) # is needed in tidyverse package

data("USArrests")ss
head(USArrests, 10) # show the first 10 lines of the dataset

#Preparing the Data
# compute variance of each variable
apply(USArrests, 2, var)
# create new data frame with centered variables
scaled_df <- apply(USArrests, 2, scale)
head(scaled_df)

scaled_df1 <- scale(USArrests)
head(scaled_df1)

# Calculate eigenvalues & eigenvectors
arrests.cov <- cov(scaled_df)
arrests.eigen <- eigen(arrests.cov)
str(arrests.eigen) # remember that str and summary are very informative functions

# Extract the loadings
(phi <- arrests.eigen$vectors[,1:2]) # see the "," inside the brackets. All rows.

phi <- -phi # see the - sign
row.names(phi) <- c("Murder", "Assault", "UrbanPop", "Rape")
col.names(phi) <- c("PC1", "PC2")
phi

# Calculate Principal Components scores
PC1 <- as.matrix(scaled_df) %*% phi[,1] # %*% in R does matrix multiplication
PC2 <- as.matrix(scaled_df) %*% phi[,2]
# Create data frame with Principal Components scores
PC <- data.frame(State = row.names(USArrests), PC1, PC2)
head(PC)

# Plot Principal Components for each State
ggplot(PC, aes(PC1, PC2)) +
  modelr::geom_ref_line(h = 0) +
  modelr::geom_ref_line(v = 0) +
  geom_text(aes(label = State), size = 3) +
  xlab("First Principal Component") +
  ylab("Second Principal Component") +
  ggtitle("First Two Principal Components of USArrests Data")
