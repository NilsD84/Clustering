# Libraries ----
library(cluster)
library(RColorBrewer)
library(plot3D)
library(mclust)
library(dplyr)



# Data Setup ----
## Work Directory and libraries ----
setwd("C:/Users/nilsd/OneDrive/Documents/Statistical Learning in Marketing/Individual Assignments")

## Importing dataset ----
segmentation <- read.csv("segment.csv")

## Data overview ----
str(segmentation)
summary(segmentation)

segmentation_clean <- segmentation[!(segmentation$Taste == 0 | segmentation$Taste == 8 |
                                       segmentation$Fattiness == 0 | segmentation$Fattiness == 8 |
                                       segmentation$Salt == 0 | segmentation$Salt == 8 |
                                       segmentation$Spreadability == 0 | segmentation$Spreadability == 8 |
                                       segmentation$Appearance == 0 | segmentation$Appearance == 8 |
                                       segmentation$Recycling == 0 | segmentation$Recycling == 8 |
                                       segmentation$Bio.content == 0 | segmentation$Bio.content == 8), ]

summary(segmentation_clean)

cor_matrix <- cor(segmentation_clean[,c(6:12)])
print(cor_matrix)

## Neither active or passive variables ----

# Column 1: Student ID

# Column 13: ID

## Passive Variables ----
# Column 2: Gender: 1 = female, 2 = male, 3 = other

# Column 3: Age: Number of years, continuous variable

# Column 4: Education: 1 = high school profession-oriented degree, 2 = high school theory-oriented degree,
# 3 = higher education non-university degree, 4 = university degree, 5 = other

# Column 5: Area: 1 = metropolitan, 2 = urban, 3 = suburban, 4 = countryside

## Active Variables ----
# Column 6: Taste: How strong should the flavor of the sandwich spread be? 1-7, 1 = mild flavor, 7 = strong flavor

# Column 7: Fattiness: How much fat should the sandwich spread contain? 1-7, 1 = low fat level, 7 = high fat level

# Column 8: Salt: How much salt should the sandwich spread contain? 1-7, 1 = low salt level, 7 = high salt level 

# Column 9: Spreadability: How spreadable should the sandwich spread be? 1-7, 1 = dense structure, 7 = loose structure

# Column 10: Appearance: How attractive should the packaging be? 1-7, 1 = low attractiveness, 7 = high attractiveness

# Column 11:Recycling: How environmentally-friendly should the packaging be? 1-7, 
# 1 = low environmental-friendliness, 7 = high environmental-friendliness

# Column 12: Bio.content: How much of the ingredients of the sandwich spread should be organic/bio?
# 1-7, 1 = low organic/bio level, 7 = high organic/bio level 



sum(is.na(segmentation_clean))

# Data Standardization ----

segmentation2 <- segmentation_clean
segmentation2[,6:12] <- data.frame(scale(segmentation_clean[,6:12]))
summary(segmentation2)
segmentation_active <- as.data.frame(c(segmentation2$Taste, segmentation2$Fattiness, segmentation2$Recycling, segmentation2$Bio.content))

# Euclidian distance ----
d <- dist(segmentation2[,c(6:12)])     
as.matrix(d)[1:10, 1:10]

# Hierarchicial Clustering ----

segmentation2a.hc <- hclust(d, method = "single")
plot(segmentation2a.hc)
segmentationaggloa <- cbind(as.data.frame(segmentation2a.hc[1]), as.data.frame(segmentation2a.hc[2]))
segmentationaggloa[1:15,]
segmentationaggloa[475:491,]

segmentation2b.hc <- hclust(d, method = "complete")
plot(segmentation2b.hc)
segmentationagglob <- cbind(as.data.frame(segmentation2b.hc[1]), as.data.frame(segmentation2b.hc[2]))
segmentationagglob[1:15,]
segmentationagglob[475:491,]

segmentation2c.hc <- hclust(d, method = "average")
plot(segmentation2c.hc)
segmentationaggloc <- cbind(as.data.frame(segmentation2c.hc[1]), as.data.frame(segmentation2c.hc[2]))
segmentationaggloc[1:15,]
segmentationaggloc[475:491,]

segmentation2d.hc <- hclust(d, method = "centroid")
plot(segmentation2d.hc)
segmentationagglod <- cbind(as.data.frame(segmentation2d.hc[1]), as.data.frame(segmentation2d.hc[2]))
segmentationagglod[1:15,]
segmentationagglod[475:491,]

segmentation2e.hc <- hclust(d, method = "ward.D2")
plot(segmentation2e.hc)
segmentationaggloe <- cbind(as.data.frame(segmentation2e.hc[1]), as.data.frame(segmentation2e.hc[2]))
segmentationaggloe[1:15,]
segmentationaggloe[475:491,]

## Screeplot ----
segmentationscreea <- sort(segmentationaggloa[475:491, c(3)], decreasing = TRUE)
plot(segmentationscreea, type="l", col="red", lwd=5, xlab="clusters", ylab="Cluster distance", main="Single Linkage Scree Plot", xaxt="n")
axis(1, at=seq(1,20,by = 1))

segmentationscreeb <- sort(segmentationagglob[475:491, c(3)], decreasing = TRUE)
plot(segmentationscreeb, type="l", col="red", lwd=5, xlab="clusters", ylab="Cluster distance", main="Complete Linkage Scree Plot", xaxt="n")
axis(1, at=seq(1,20,by = 1))

segmentationscreec <- sort(segmentationaggloc[475:491, c(3)], decreasing = TRUE)
plot(segmentationscreec, type="l", col="red", lwd=5, xlab="clusters", ylab="Cluster distance", main="Average Linkage Scree Plot", xaxt="n")
axis(1, at=seq(1,20,by = 1))

segmentationscreed <- sort(segmentationagglod[475:491, c(3)], decreasing = TRUE)
plot(segmentationscreed, type="l", col="red", lwd=5, xlab="clusters", ylab="Cluster distance", main="Centroid Scree Plot", xaxt="n")
axis(1, at=seq(1,20,by = 1))

segmentationscreee <- sort(segmentationaggloe[475:491, c(3)], decreasing = TRUE)
plot(segmentationscreee, type="l", col="red", lwd=5, xlab="clusters", ylab="Cluster distance", main="Ward Scree Plot", xaxt="n")
axis(1, at=seq(1,20,by = 1))

# WARD Method continuation ----

# Dendograms for possible amount of clusters

# see hclust's proposal for 6 groups
plot(segmentation2e.hc)
rect.hclust(segmentation2e.hc, k=6, border="red")

# see hclust's proposal for 5 groups
plot(segmentation2e.hc)
rect.hclust(segmentation2e.hc, k=5, border="red")

# see hclust's proposal for 4 groups
plot(segmentation2e.hc)
rect.hclust(segmentation2e.hc, k=4, border="red")

# see hclust's proposal for 3 groups
plot(segmentation2e.hc)
rect.hclust(segmentation2e.hc, k=3, border="red")

# see hclust's proposal for 2 groups
plot(segmentation2e.hc)
rect.hclust(segmentation2e.hc, k=2, border="red")

# Screeplot suggests 2 cluster
# Dendrogram also suggests 2 clusters, however it could be argued up to 3 

segmentation2e.hc.segment2 <- cutree(segmentation2e.hc, k=2)
table(segmentation2e.hc.segment2)

segmentation2e.hc.segment3 <- cutree(segmentation2e.hc, k=3)
table(segmentation2e.hc.segment3)

#Silhouette Coefficient to confirm whether 2 clusters are ideal
silhouette_values <- silhouette(segmentation2e.hc.segment2, d)
summary(silhouette_values)
plot(silhouette_values)

for (k in 2:10) {
  cluster_labels <- cutree(segmentation2e.hc, k = k)
  silhouette_values <- silhouette(cluster_labels, d)
  avg_silhouette_width <- mean(silhouette_values[, 3])
  cat("Average silhouette width for", k, "clusters:", avg_silhouette_width, "\n")
}

# silhouette width is best for 2 clusters

seg.summ <- function(data , groups) {aggregate(data , list(groups), function(x) mean(as.numeric(x)))}
seg.summ(segmentation2[,c(6:12)], segmentation2e.hc.segment2)
segmentation2e.hc.mean <- seg.summ(segmentation2[,c(6:12)], segmentation2e.hc.segment2)

clusmember2 <- as.factor(segmentation2e.hc.segment2)
segmentation2eaovbase2 <- cbind(clusmember2,segmentation2[,c(6:12)])

clusmember3 <- as.factor(segmentation2e.hc.segment3)
segmentation2eaovbase3 <- cbind(clusmember3,segmentation2[,c(6:12)])

segmentationwithage <- cbind(clusmember2, segmentation2)

## ANOVA Tests ----

# ANOVA test for Taste

#2 cluster
segmentation2.aov_Taste2 <- aov(Taste ~ clusmember2, data = segmentation2eaovbase2)
summary(segmentation2.aov_Taste2)
TukeyHSD(segmentation2.aov_Taste2)

#3 cluster
segmentation2.aov_Taste3 <- aov(Taste ~ clusmember3, data = segmentation2eaovbase3)
summary(segmentation2.aov_Taste3)
TukeyHSD(segmentation2.aov_Taste3)

# ANOVA Test Fattiness

#2 cluster
segmentation2.aov_Fattiness2 <- aov(Fattiness ~ clusmember2, data = segmentation2eaovbase2)
summary(segmentation2.aov_Fattiness2)
TukeyHSD(segmentation2.aov_Fattiness2)

# 3 cluster
segmentation2.aov_Fattiness3 <- aov(Fattiness ~ clusmember3, data = segmentation2eaovbase3)
summary(segmentation2.aov_Fattiness3)
TukeyHSD(segmentation2.aov_Fattiness3)

# ANOVA Test for Salt

#2 cluster
segmentation2.aov_Salt2 <- aov(Salt ~ clusmember2, data = segmentation2eaovbase2)
summary(segmentation2.aov_Salt2)
TukeyHSD(segmentation2.aov_Salt2)

# 3 cluster
segmentation2.aov_Salt3 <- aov(Salt ~ clusmember3, data = segmentation2eaovbase3)
summary(segmentation2.aov_Salt3)
TukeyHSD(segmentation2.aov_Salt3)

# ANOVA Test for Spreadability

# 2 cluster
segmentation2.aov_Spreadability2 <- aov(Spreadability ~ clusmember2, data = segmentation2eaovbase2)
summary(segmentation2.aov_Spreadability2)
TukeyHSD(segmentation2.aov_Spreadability2)

# 3 cluster
segmentation2.aov_Spreadability3 <- aov(Spreadability ~ clusmember3, data = segmentation2eaovbase3)
summary(segmentation2.aov_Spreadability3)
TukeyHSD(segmentation2.aov_Spreadability3)

# ANOVA Test for Appearance

# 2 cluster
segmentation2.aov_Appearance2 <- aov(Appearance ~ clusmember2, data = segmentation2eaovbase2)
summary(segmentation2.aov_Appearance2)
TukeyHSD(segmentation2.aov_Appearance2)

# 3 cluster
segmentation2.aov_Appearance3 <- aov(Appearance ~ clusmember3, data = segmentation2eaovbase3)
summary(segmentation2.aov_Appearance3)
TukeyHSD(segmentation2.aov_Appearance3)

# ANOVA Test for Reycyling

#2 cluster
segmentation2.aov_Recycling2 <- aov(Recycling ~ clusmember2, data = segmentation2eaovbase2)
summary(segmentation2.aov_Recycling2)
TukeyHSD(segmentation2.aov_Recycling2)

#3 cluster
segmentation2.aov_Recycling3 <- aov(Recycling ~ clusmember3, data = segmentation2eaovbase3)
summary(segmentation2.aov_Recycling3)
TukeyHSD(segmentation2.aov_Recycling3)

# ANOVA Test for Bio.content

# 2 cluster
segmentation2.aov_Bio.content2 <- aov(Bio.content ~ clusmember2, data = segmentation2eaovbase2)
summary(segmentation2.aov_Bio.content2)
TukeyHSD(segmentation2.aov_Bio.content2)

# 3 cluster
segmentation2.aov_Bio.content3 <- aov(Bio.content ~ clusmember3, data = segmentation2eaovbase3)
summary(segmentation2.aov_Bio.content3)
TukeyHSD(segmentation2.aov_Bio.content3)

# ANOVA test for age
segmentation2.aov_age <- aov(Age ~ clusmember2, data = segmentationwithage)
summary(segmentation2.aov_age)
TukeyHSD(segmentation2.aov_age)

# Categorical variables significance tests

#Gender
segmentation2e.freq.gender <-table(clusmember2, segmentation2[,c(2)])
segmentation2e.freq.gender
segmentation2e.chisq.gender <- chisq.test(segmentation2e.freq.gender)
segmentation2e.chisq.gender

#Education
segmentation2e.freq.education <-table(clusmember2, segmentation2[,c(4)])
segmentation2e.freq.education 
segmentation2e.chisq.education <- chisq.test(segmentation2e.freq.education)
segmentation2e.chisq.education

#Area
segmentation2e.freq.area <-table(clusmember2, segmentation2[,c(5)])
segmentation2e.freq.area 
segmentation2e.chisq.area <- chisq.test(segmentation2e.freq.area)
segmentation2e.chisq.area

segmentation2e.hc.segment <- cutree(segmentation2e.hc, k=2) 
pca <- prcomp(segmentation2[,c(6:12)])
pca_scores <- predict(pca, segmentation2[,c(6:12)])
plot(pca_scores[, 1], pca_scores[, 2], col = segmentation2e.hc.segment, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "Cluster Plot with PCA Dimensions")

# demographic comparison

segmentation_clean$segment_ward <- segmentation2e.hc.segment2
demographic_summary <- segmentation_clean %>%
  group_by(segment_ward) %>%
  summarise(
    gender_proportion_female = mean(Gender == 1, na.rm = TRUE),
    gender_proportion_male = mean(Gender ==2, na.rum= TRUE),
    age_mean = mean(Age, na.rm = TRUE),
    education_mode = names(sort(table(Education), decreasing = TRUE))[1], # Mode for Education
    area_mode = names(sort(table(Area), decreasing = TRUE))[1] # Mode for Area
  )
demographic_summary

# K-means clustering ----

# determining optimal n of clusters

data <- segmentation2[, c(6:12)]  # Your data subset for clustering
max_k <- 10
wss <- numeric(max_k)
for (k in 1:max_k) {
  set.seed(123)  # Set seed for reproducibility
  kmeans_model <- kmeans(data, centers = k)
  wss[k] <- kmeans_model$tot.withinss
}
plot(1:max_k, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for Optimal k")
# The WSS decreases sharply from 2 to 3 clusters.
#After 3 clusters, the rate of decrease in WSS becomes more gradual, showing a diminishing return on adding more clusters.
#This suggests that 3 clusters is a good balance

set.seed(123)
segmentatione2.k2 <- kmeans(segmentation2[,c(6:12)], centers=2)
set.seed(123)
segmentatione2.k3 <- kmeans(segmentation2[,c(6:12)], centers=3)
set.seed(123)
segmentatione2.k4 <- kmeans(segmentation2[,c(6:12)], centers=4)
set.seed(123)
segmentatione2.k5 <- kmeans(segmentation2[,c(6:12)], centers=5)
set.seed(123)
segmentatione2.k6 <- kmeans(segmentation2[,c(6:12)], centers=6)

# silhouette scores
set.seed(123)
silhouette_scores <- c()
k_values <- 2:6
for (k in k_values) {
  kmeans_model <- kmeans(segmentation2[, 6:12], centers = k)
  sil <- silhouette(kmeans_model$cluster, dist(segmentation2[, 6:12]))
  avg_sil_width <- mean(sil[, 3])
  silhouette_scores <- c(silhouette_scores, avg_sil_width)
}
plot(k_values, silhouette_scores, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters (k)",
     ylab = "Average silhouette score",
     main = "Silhouette Method for Optimal k")
silhouette_scores

# BIC values
bic_kmeans <- function(kmeans_model, data) {
  # Extract the number of clusters and data points
  k <- length(kmeans_model$centers)  # number of clusters
  n <- nrow(data)                    # number of data points
  d <- ncol(data)      
  WSS <- kmeans_model$tot.withinss
  variance <- WSS / (n - k)
  log_likelihood <- -WSS / (2 * variance)
  BIC <- -2 * log_likelihood + log(n) * k * d
  return(BIC)
}

data <- segmentation2[, c(6:12)]
BIC_k2 <- bic_kmeans(segmentatione2.k2, data)
BIC_k3 <- bic_kmeans(segmentatione2.k3, data)
BIC_k4 <- bic_kmeans(segmentatione2.k4, data)
BIC_k5 <- bic_kmeans(segmentatione2.k5, data)

BIC_values <- data.frame(
  Clusters = 2:5,
  BIC = c(BIC_k2, BIC_k3, BIC_k4, BIC_k5)
)

print(BIC_values)

seg.summ(segmentation2[,c(6:12)], segmentatione2.k2$cluster)
seg.summ(segmentation2[,c(6:12)], segmentatione2.k3$cluster)
seg.summ(segmentation2[,c(6:12)], segmentatione2.k4$cluster)
seg.summ(segmentation2[,c(6:12)], segmentatione2.k5$cluster)
seg.summ(segmentation2[,c(6:12)], segmentatione2.k6$cluster)

table(segmentatione2.k5$cluster)

# testing significance of different cluster solutions

# 6 clusters
anova_results_k6 <- list()  
segmentation2$kcluster6 <- segmentatione2.k6$cluster
variables <- colnames(segmentation2[, c(6:12)]) 
for (var in variables) {
  formula <- as.formula(paste(var, "~ kcluster6"))
  anova_results_k6[[var]] <- summary(aov(formula, data = segmentation2))
}
anova_results_k6

# 5 clusters
anova_results_k5 <- list()  
segmentation2$kcluster5 <- segmentatione2.k5$cluster
variables <- colnames(segmentation2[, c(6:12)]) 
for (var in variables) {
  formula <- as.formula(paste(var, "~ kcluster5"))
  anova_results_k5[[var]] <- summary(aov(formula, data = segmentation2))
}
anova_results_k5

## all are signfiicant except taste
# 4 cluster
anova_results_k4 <- list()  
segmentation2$kcluster4 <- segmentatione2.k4$cluster
for (var in variables) {
  formula <- as.formula(paste(var, "~ kcluster4"))
  anova_results_k4[[var]] <- summary(aov(formula, data = segmentation2))
}
anova_results_k4 

## all are significant except Bio.content

# 3 cluster
anova_results_k3 <- list()
segmentation2$kcluster3 <- segmentatione2.k3$cluster
for (var in variables) {
  formula <- as.formula(paste(var, "~ kcluster3"))
  anova_results_k3[[var]] <- summary(aov(formula, data = segmentation2))
}
anova_results_k3


# 2 cluster
anova_results_k2 <- list()
segmentation2$kcluster2 <- segmentatione2.k2$cluster
for (var in variables) {
  formula <- as.formula(paste(var, "~ kcluster3"))
  anova_results_k2[[var]] <- summary(aov(formula, data = segmentation2))
}
anova_results_k2

## all variables are significant across 3 clusters
segment_count_k5 <- table(segmentatione2.k5$cluster)
segment_count_k5

segment_count_k3 <- table(segmentatione2.k3$cluster)
segment_count_k3

segment_count_k2 <- table(segmentatione2.k2$cluster)
segment_count_k2
# plotting the variables


# Taste
boxplot(segmentation2$Taste ~ segmentatione2.k2$cluster, ylab="Taste", xlab="Cluster")

# Fattiness
boxplot(segmentation2$Fattiness ~ segmentatione2.k2$cluster, ylab="Fattiness", xlab="Cluster")

# Salt
boxplot(segmentation2$Salt ~ segmentatione2.k2$cluster, ylab="Salt", xlab="Cluster")

# Spreadability
boxplot(segmentation2$Spreadability ~ segmentatione2.k2$cluster, ylab="Spreadability", xlab="Cluster")

# Appearance
boxplot(segmentation2$Appearance ~ segmentatione2.k2$cluster, ylab="Appearance", xlab="Cluster")

# Recycling
boxplot(segmentation2$Recycling ~ segmentatione2.k2$cluster, ylab="Recycling", xlab="Cluster")

# Bio.content
boxplot(segmentation2$Bio.content ~ segmentatione2.k2$cluster, ylab="Bio.content", xlab="Cluster")

#2 clusters
segmentatione2.k2.segment <- segmentatione2.k2$cluster
pca2k <- prcomp(segmentation2[,c(6:12)])
pca_scores2k <- predict(pca2k, segmentation2[,c(6:12)])
plot(pca_scores2k[, 1], pca_scores2k[, 2], col = segmentatione2.k2.segment, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "Cluster Plot with PCA Dimensions")

segmentation_clean$segmentk2 <- segmentatione2.k2.segment
demographic_summary_k2 <- segmentation_clean %>%
  group_by(segmentk2) %>%
  summarise(
    gender_proportion_female = mean(Gender == 1, na.rm = TRUE),
    gender_proportion_male = mean(Gender ==2, na.rum= TRUE),
    age_mean = mean(Age, na.rm = TRUE),
    education_mode = names(sort(table(Education), decreasing = TRUE))[1], # Mode for Education
    area_mode = names(sort(table(Area), decreasing = TRUE))[1] # Mode for Area
  )
demographic_summary_k2

## significance check of demographic variables

kclus2 <- as.factor(segmentatione2.k2.segment)


#Gender
segmentation2e2.freq.gender <-table(kclus2, segmentation2[,c(2)])
segmentation2e2.freq.gender
segmentation2e2.chisq.gender <- chisq.test(segmentation2e2.freq.gender)
segmentation2e2.chisq.gender

#Education
segmentation2e2.freq.education <-table(kclus2, segmentation2[,c(4)])
segmentation2e2.freq.education 
segmentation2e2.chisq.education <- chisq.test(segmentation2e2.freq.education)
segmentation2e2.chisq.education

#Area
segmentation2e2.freq.area <-table(kclus2, segmentation2[,c(5)])
segmentation2e2.freq.area 
segmentation2e2.chisq.area <- chisq.test(segmentation2e2.freq.area)
segmentation2e2.chisq.area

ksegwithage <- cbind(kclus2, segmentation2)

segmentation2e2.aov_age <- aov(Age ~ kclus2, data = ksegwithage)
summary(segmentation2.aov_age)
TukeyHSD(segmentation2.aov_age)


#K means with outcomes of hierarchical cluster as starting values ----
set1 <- segmentation2[segmentation2e.hc.mean == 1, c(6:12)]
kmeans_set1 <- kmeans(set1, centers = 2)
set2 <- segmentation2[segmentation2e.hc.mean == 2, c(6:12)]
kmeans_set2 <- kmeans(set2, centers = 2)
kmeanstart_4 <- rbind(kmeans_set1$centers, kmeans_set2$centers) # Now you have 5 starting centroids
segmentatione2.k5 <- kmeans(segmentation2[, c(6:12)], centers = kmeanstart_4)

seg.summ(segmentation2[,6:12], segmentatione2.k4$cluster)

wardstart_k4 <- segmentatione2.k5$cluster

table(wardstart_k4)

# significance
segmentation2$wardstartcluster4 <- segmentatione2.k4$cluster
anova_results <- list()
for (variable in names(segmentation2)[6:12]) {
  anova_results[[variable]] <- summary(aov(segmentation2[[variable]] ~ segmentation2$wardstartcluster4))
}
anova_results



pca4 <- prcomp(segmentation2[,c(6:12)])
pca4_scores <- predict(pca4, segmentation2[,6:12])
plot(pca4_scores[, 1], pca4_scores[, 2], col = wardstart_k4, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "Cluster Plot with PCA Dimensions")


#test for significance
clusmember2 <- as.factor(wardstart_k4)
segmentation2eaovbase2 <- cbind(clusmember2, segmentation2[,c(6:12)])

segmentation2.aov2_Taste <- aov(Taste ~ clusmember2, data = segmentation2eaovbase2)
summary(segmentation2.aov2_Taste)
TukeyHSD(segmentation2.aov2_Taste)

segmentation2.aov2_Fattiness <- aov(Fattiness ~ clusmember2, data = segmentation2eaovbase2)
summary(segmentation2.aov2_Fattiness)
TukeyHSD(segmentation2.aov2_Fattiness)

segmentation2.aov2_Salt <- aov(Salt ~ clusmember2, data = segmentation2eaovbase2)
summary(segmentation2.aov2_Salt)
TukeyHSD(segmentation2.aov2_Salt)

segmentation2.aov2_Spreadability <- aov(Spreadability ~ clusmember2, data = segmentation2eaovbase2)
summary(segmentation2.aov2_Spreadability)
TukeyHSD(segmentation2.aov2_Spreadability)

segmentation2.aov2_Appearance <- aov(Appearance ~ clusmember2, data = segmentation2eaovbase2)
summary(segmentation2.aov2_Appearance)
TukeyHSD(segmentation2.aov2_Appearance)

segmentation2.aov2_Recycling <- aov(Recycling ~ clusmember2, data = segmentation2eaovbase2)
summary(segmentation2.aov2_Recycling)
TukeyHSD(segmentation2.aov2_Recycling)

segmentation2.aov2_Bio.content <- aov(Bio.content ~ clusmember2, data = segmentation2eaovbase2)
summary(segmentation2.aov2_Bio.content)
TukeyHSD(segmentation2.aov2_Bio.content)

segmentation2.aov2_Age <- aov(Age ~ clusmember2, data = segmentation2)
summary(segmentation2.aov2_Age)
TukeyHSD(segmentation2.aov2_Age)

#Gender
segmentation2e2.freq.gender <-table(clusmember2, segmentation2[,c(2)])
segmentation2e2.freq.gender
segmentation2e2.chisq.gender <- chisq.test(segmentation2e2.freq.gender)
segmentation2e2.chisq.gender

#Education
segmentation2e2.freq.education <-table(clusmember2, segmentation2[,c(4)])
segmentation2e2.freq.education 
segmentation2e2.chisq.education <- chisq.test(segmentation2e2.freq.education)
segmentation2e2.chisq.education

#Area
segmentation2e2.freq.area <-table(clusmember2, segmentation2[,c(5)])
segmentation2e2.freq.area 
segmentation2e2.chisq.area <- chisq.test(segmentation2e2.freq.area)
segmentation2e2.chisq.area

segmentation_clean$segmentk4 <- wardstart_k4
demographic_summary_k4 <- segmentation_clean %>%
  group_by(segmentk4) %>%
  summarise(
    gender_proportion_female = mean(Gender == 1, na.rm = TRUE),
    gender_proportion_male = mean(Gender ==2, na.rum= TRUE),
    age_mean = mean(Age, na.rm = TRUE),
    education_mode = names(sort(table(Education), decreasing = TRUE))[1], # Mode for Education
    area_mode = names(sort(table(Area), decreasing = TRUE))[1] # Mode for Area
  )
demographic_summary_k4



#
mclust_result <- Mclust(segmentation2[, c(6:12)])  
summary(mclust_result)  
plot(mclust_result) 

# Mclust ----
mod5 <- densityMclust(segmentation2[,c(6:12)])
summary(mod5)
plot(mod5, what = "density", type = "persp", col=brewer.pal(10,"Spectral"))

# check all model solutions up to 9 clusters
mclustBIC(segmentation2[,c(6:12)])

# what if we estimate 6 clusters?
segmentation2.mc6 <- Mclust(segmentation2[,c(6:12)], G=6, modelName = "EEV")
summary(segmentation2.mc6)

# what if we estimate 3 clusters?
segmentation2.mc3 <- Mclust(segmentation2[,c(6:12)], G=3, modelName = "EEV")
summary(segmentation2.mc3)

# compare the models
BIC(segmentation2.mc3, segmentation2.mc6)

# examine the 6-cluster model
seg.summ(segmentation2[,c(6:12)], segmentation2.mc6$class)

# examine the 3-cluster model
seg.summ(segmentation2[,c(6:12)], segmentation2.mc3$class)



# plot the 3-cluster model
segmentation2.mc3.segment <- segmentation2.mc3$class
pca.mclust3 <- prcomp(segmentation2[,c(6:12)])
pca_scores.mclust3 <- predict(pca.mclust3, segmentation2[,c(6:12)])
plot(pca_scores.mclust3[, 1], pca_scores.mclust3[, 2], col = segmentation2.mc3.segment, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "Cluster Plot with PCA Dimensions")
