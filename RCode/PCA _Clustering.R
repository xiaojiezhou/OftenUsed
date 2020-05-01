#####  PCA and k-mean clustering: http://www.r-bloggers.com/pca-and-k-means-clustering-of-delta-aircraft/
#####  hierarchical agglomerative and model based:  http://www.statmethods.net/advstats/cluster.html

#--- load data 
  setwd("/Users/x644435/Documents/Churn/R")
  load("df_long_fill0.RData")

  library(dplyr)
  library(reshape2)
  library(sqldf)
  
  df.long.fill0$M_HH_SPEND[df.long.fill0$M_HH_SPEND<0]=0
  
  a1= df.long.fill0 %>%
    mutate(log10_M_HH_SPEND = log10(M_HH_SPEND+1), 
           Diff_log10_M_HH_SPEND = abs(log10_M_HH_SPEND -lag(log10_M_HH_SPEND)))  %>%
    group_by(Spend_Cat, HSHD_ID) %>%
    summarise(mean_Lspend=mean(log10_M_HH_SPEND), sd_Lspend=sd(log10_M_HH_SPEND),
              mean_Ldspend=mean(Diff_log10_M_HH_SPEND, na.rm=TRUE), sd_Ldspend=sd(Diff_log10_M_HH_SPEND, na.rm=TRUE)
    ) %>% ungroup

  
#--- Summary data
  str(a1)  
  summary(a1[,3:6])  
  plot(a1[,3:6])
  
#--- Scale data
  a2 = data.frame(scale(a1[,3:6]))
  summary(a2)
  var(a2, na.rm=TRUE)
  
#--- PCA (eigndecomposition) via princomp
  pc <- princomp(a2)
  
  plot(pc)      # 2 components with variances >=1
  summary(pc)   # Key: 2 comp explained ~90% of variance
  loadings(pc)  
  
#--- PCA via prcomp (SVD)
  pc <- prcomp(a2)
  plot(pc)
  plot(pc, type='l')
  summary(pc) # The 1st two component explained about 90% of variances and with variance larger or close to 1
  screeplot(pc, type="lines")
  
# First for principal components
  comp <- data.frame(pc$x[,1:3])
  # Plot
  plot(comp, pch=16, col=rgb(0,0,0,0.5))
  
  library(rgl)
  # Multi 3D plot
  plot3d(comp$PC1, comp$PC2, comp$PC3)
  
########### Clustering
#--- Determine number of clusters: looking for “elbow” in the scree plot => k=3 or 4.
  a3 = comp[,1:3]
  wss <- (nrow(a3)-1)*sum(apply(a3,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(a3, centers=i, nstart=25, iter.max=1000)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  
  # From scree plot elbow occurs at k = 3 or 4
  # Apply k-means with k=4
  k <- kmeans(a3, 3, nstart=25, iter.max=1000)
  library(RColorBrewer)
  library(scales)
  palette(alpha(brewer.pal(9,'Set1'), 0.5))
  plot(a3, col=k$clust, pch=16)
  
  # 3D plot
  plot3d(comp$PC1, comp$PC2, comp$PC3, col=k$clust)
  
  # Cluster sizes
  sort(table(k$clust))
  clust <- names(sort(table(k$clust)))
  # First cluster
  row.names(a2[k$clust==clust[1],])
  # Second Cluster
  row.names(a2[k$clust==clust[2],])
  # Third Cluster
  row.names(a2[k$clust==clust[3],])
  
  # Compare accommodation by cluster in boxplot
  boxplot(a1$sd_Ldspend ~ k$cluster,
          xlab='Cluster', main='Mean by Cluster')

  # get cluster means 
  aggregate(a2,by=list(k$cluster),FUN=mean)
  # append cluster assignment
  a2 <- data.frame(a2, fit$cluster)

    
#----- Ward Hierarchical Clustering ----#
  d <- dist(a3, method = "euclidean") # distance matrix
  fit.h <- hclust(d, method="ward.D2") 
  plot(fit.h) # display dendogram
  hgrps <- cutree(fit.h, k=4) # cut tree into 4 clusters
  # draw dendogram with red borders around the 4 clusters 
  rect.hclust(fit.h, k=4, border="red")
  
  # Ward Hierarchical Clustering with Bootstrapped p values
  # Be aware that pvclust clusters columns, not rows. Transpose data before using.
  library(pvclust)
  a4=t(a3)
  fit <- pvclust(a4, method.hclust="ward.D2",
                 method.dist="euclidean")
  plot(fit) # dendogram with p values
  # add rectangles around groups highly supported by the data
  pvrect(fit, alpha=.95)
  
#--- Model Based Clustering---#
  library(mclust)
  fit <- Mclust(a3)
  plot(fit) # plot results 
  summary(fit) # display the best model
  
  # Centroid Plot against 1st 2 discriminant functions
  library(fpc)
  plotcluster(a2, fit$cluster)
  
#--- comparing 2 cluster solutions
  library(fpc)
  cluster.stats(d, fit$cluster, hgrps)
  