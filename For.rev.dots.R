
# Plot for fragments from LRA
library(ggplot2)
library(cowplot)

path <-c("/home/cl3/Documents/mjc/jingwenr/lra/LRA/test/MergeClusterVersionCases/21-17-47/")
readname <- c("m54016_171122_124350_11862750_1721_26521_0_24800")
j = 0
jj = 0

# -------------- Plot for-matches.dots and rev-matches.dots------------------
clust <- read.delim(paste0(path, "for-matches.dots"), sep = "\t", header = FALSE)
colnames(clust) <- c("qStart", "tStart", "length", "clusternum", "strand")
cluster<- data.frame(matrix(ncol = 0, nrow = nrow(clust)))
cluster<- cbind(cluster, clust$qStart)
cluster <-cbind(cluster, clust$tStart)
 
cluster<- cbind(cluster, clust$qStart + clust[1,3])
cluster <-cbind(cluster, clust$tStart + clust[1,3])
cluster <- cbind(cluster, seq(0, 0, length.out = nrow(clust)))
colnames(cluster) <- c("qStart", "tStart", "qEnd", "tEnd", "strand")
point <-  data.frame(matrix(ncol = 2, nrow = 0))
point <- cluster[,1:2]
colnames(point) <- c("qEnd", "tEnd")
point <- rbind(point, cluster[,3:4])

t <- ggplot(point, aes(qEnd, tEnd))
t<- t + geom_segment(aes(x = qStart, y = tStart, xend = qEnd, yend = tEnd), data = cluster[, c(1:4)], color = "red", size = 1.5)
t+ xlab("read") + ylab("genome") + ggtitle("for-matches.dots")


# -----------------------------------------
clust1 <- read.delim(paste0(path, "rev-matches.dots"), sep = "\t", header = FALSE)
colnames(clust1) <- c("qStart", "tStart", "length", "clusternum", "strand")
cluster1<- data.frame(matrix(ncol = 0, nrow = nrow(clust1)))
cluster1<- cbind(cluster1, clust1$qStart)
cluster1 <-cbind(cluster1, clust1$tStart)

cluster1<- cbind(cluster1, clust1$qStart + clust1[1,3])
cluster1 <-cbind(cluster1, clust1$tStart + clust1[1,3])
cluster1 <- cbind(cluster1, rep(1, length.out = nrow(clust1)))
colnames(cluster1) <- c("qStart", "tStart", "qEnd", "tEnd", "strand")
point1 <-  data.frame(matrix(ncol = 2, nrow = 0))
point1 <- cluster1[,1:2]
colnames(point1) <- c("qEnd", "tEnd")
point1 <- rbind(point1, cluster1[,3:4])


point <- rbind(point, point1)
cluster <- rbind(cluster, cluster1)


t <- ggplot(point, aes(qEnd, tEnd))
t<- t + geom_segment(aes(x = qStart, y = tStart, xend = qEnd, yend = tEnd,  color = factor(cluster$strand)), data = cluster[, c(1:4)], size = 1.5)
t+ xlab("read") + ylab("genome")




#-------------------clusters-pre-merge.tab --------------------real seeds 
clust <- read.delim(paste0(path, "clusters-pre-merge.tab"), sep = "\t", header = FALSE)
colnames(clust) <- c("qStart", "tStart", "length", "clusternum", "strand")
cluster<- data.frame(matrix(ncol = 0, nrow = nrow(clust)))
cluster<- cbind(cluster, clust$qStart)
cluster <-cbind(cluster, clust$tStart)

cluster<- cbind(cluster, clust$qStart + clust[1,3])
cluster <-cbind(cluster, clust$tStart + clust[1,3])
cluster <- cbind(cluster, clust$clusternum, clust$strand)
colnames(cluster) <- c("qStart", "tStart", "qEnd", "tEnd", "clusternum", "strand")
point <-  data.frame(matrix(ncol = 2, nrow = 0))
point <- cluster[,1:2]
colnames(point) <- c("qEnd", "tEnd")
point <- rbind(point, cluster[,3:4])
colnames(point) <- c("x", "y")

t <- ggplot(point, aes(x, y))
t<- t + geom_segment(aes(x = qStart, y = tStart, xend = qEnd, yend = tEnd,  color = factor(cluster$clusternum)), data = cluster[, c(1:4)], size = 1.5)
t+ xlab("read") + ylab("genome") + ggtitle("clusters-pre-merge.tab")



#-------------------clusters-sdp.dots --------------------real seeds 
clust <- read.delim(paste0(path, "clusters-sdp.0.dots"), sep = "\t", header = FALSE)
colnames(clust) <- c("qStart", "tStart", "length", "clusternum", "strand")
cluster<- data.frame(matrix(ncol = 0, nrow = nrow(clust)))
cluster<- cbind(cluster, clust$qStart)
cluster <-cbind(cluster, clust$tStart)

cluster<- cbind(cluster, clust$qStart + clust[1,3])
cluster <-cbind(cluster, clust$tStart + clust[1,3])
cluster <- cbind(cluster, clust$strand)
colnames(cluster) <- c("qStart", "tStart", "qEnd", "tEnd", "strand")
point <-  data.frame(matrix(ncol = 2, nrow = 0))
point <- cluster[,1:2]
colnames(point) <- c("qEnd", "tEnd")
point <- rbind(point, cluster[,3:4])

t <- ggplot(point, aes(qEnd, tEnd))
t<- t + geom_segment(aes(x = qStart, y = tStart, xend = qEnd, yend = tEnd,  color = factor(cluster$strand)), data = cluster[, c(1:4)], size = 1.5)
t+ xlab("read") + ylab("genome") + ggtitle("clusters-sdp.dots")






#-------------------clusters-after-remove-overlapping.tab--------------------real seeds 
clust <- read.delim(paste0(path, "clusters-after-remove-overlapping.tab"), sep = "\t", header = FALSE)
colnames(clust) <- c("qStart", "tStart", "length", "clusternum", "strand")
cluster<- data.frame(matrix(ncol = 0, nrow = nrow(clust)))
cluster<- cbind(cluster, clust$qStart)
cluster <-cbind(cluster, clust$tStart)

cluster<- cbind(cluster, clust$qStart + clust[1,3])
cluster <-cbind(cluster, clust$tStart + clust[1,3])
cluster <- cbind(cluster, clust$strand)
colnames(cluster) <- c("qStart", "tStart", "qEnd", "tEnd", "strand")
point <-  data.frame(matrix(ncol = 2, nrow = 0))
point <- cluster[,1:2]
colnames(point) <- c("qEnd", "tEnd")
point <- rbind(point, cluster[,3:4])

t <- ggplot(point, aes(qEnd, tEnd))
t<- t + geom_segment(aes(x = qStart, y = tStart, xend = qEnd, yend = tEnd,  color = factor(cluster$strand)), data = cluster[, c(1:4)], size = 1.5)
t+ xlab("read") + ylab("genome") + ggtitle("clusters-after-remove-overlapping.tab")








# -------------------Plot clust.dots---------- requires 4 cols: qstart, tstart, length, cluster
Cluster <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(Cluster) <- c("qStart", "tStart", "qEnd", "tEnd", "cluster")
point <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(point) <- c("V1", "V2")
for (i in j:jj) {
  Cluster0 <- read.delim(paste0(path, readname, ".", as.character(i), ".clust.dots"),
                                sep = "\t", header = FALSE)
  
  point0 <- Cluster0[,c(1,2)]
  a <-Cluster0[,c(1,2,4)] 
  a[, c(1,2)] <- a[, c(1,2)]+ Cluster0[1, 3]
  point <- rbind(point, point0)
  point <-rbind(point, a[, c(1:2)])
  
  colnames(a) <- c("V3", "V4", "V5")
  fragments <- cbind(point0,a)
  colnames(fragments) <- c("qStart", "tStart", "qEnd", "tEnd", "cluster")
  Cluster <- rbind(refinedCluster, fragments)
}


p <- ggplot(point, aes(V1, V2))
p <- p +  geom_segment(aes(x = qStart, y = tStart, xend = qEnd, yend = tEnd,  color = factor(Cluster$cluster)), data = Cluster[, c(1:4)], size = 1.5) +
  xlab("read") + ylab("genome") + ggtitle(paste(readname, "clust.dots", sep = "\t"))



# -------------------plot for orig.dots--------- requires 4 cols: qstart, tstart, length, cluster
refinedCluster <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(refinedCluster) <- c("qStart", "tStart", "qEnd", "tEnd", "cluster")
point <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(point) <- c("V1", "V2")
for (i in j:jj) {
  refinedCluster0 <- read.delim(paste0(path, readname, ".", as.character(i), ".orig.dots"),
                                sep = "\t", header = FALSE)
  
  point0 <- refinedCluster0[,c(1,2)]
  a <- refinedCluster0[,c(1,2,4)] 
  a[, c(1,2)] <- a[, c(1,2)]+ refinedCluster0[1, 3]
  point <- rbind(point, point0)
  point <-rbind(point, a[, c(1:2)])
  
  colnames(a) <- c("V3", "V4", "V5")
  fragments <- cbind(point0,a)
  colnames(fragments) <- c("qStart", "tStart", "qEnd", "tEnd", "cluster")
  refinedCluster <- rbind(refinedCluster, fragments)
}


p <- ggplot(point, aes(V1, V2))
p <- p +  geom_segment(aes(x = qStart, y = tStart, xend = qEnd, yend = tEnd,  color = factor(refinedCluster$cluster)), data = refinedCluster[, c(1:4)], size = 1.5) +
  xlab("read") + ylab("genome") + ggtitle(paste(readname, "refinedClusters.dots", sep = "\t"))


#------1st sdp ------------- requires 4 cols: qstart, tstart, length, cluster
sdp <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(sdp) <- c("qStart", "tStart", "qEnd", "tEnd", "cluster")
point <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(point) <- c("V1", "V2")
for (i in j:jj) {
  sdp0 <- read.delim(paste0(path, readname, ".", as.character(i), ".first-sdp.dots"),
                     sep = "\t", header = FALSE)
  
  point0 <- sdp0[,c(1,2)]
  point <- rbind(point, point0)
  a <- sdp0[,c(1,2,3)] 
  a[, c(1,2)] <- a[, c(1,2)]+ sdp0[1, 3]
  point <- rbind(point, a[, c(1:2)])
  
  fragments <- cbind(sdp0[, c(1,2)], a)
  colnames(fragments) <- c("qStart", "tStart", "qEnd", "tEnd", "cluster")
  fragments$tStart =  fragments$tStart 
  fragments$tEnd =  fragments$tEnd
  
  sdp <- rbind(sdp, fragments)
}

#pdf("/home/cl3/Documents/mjc/jingwenr/lra/LRA/test/teatsdp2.pdf")
p<-p +  geom_segment(aes(x = qStart, y = tStart, xend = qEnd, yend = tEnd), data = sdp[, c(1:4)], size = 1.5 , color = "blue") +
  xlab("read") + ylab("genome")
#dev.off()
