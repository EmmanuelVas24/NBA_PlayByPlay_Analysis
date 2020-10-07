library(readr)
library(cluster)
library(factoextra)
library(ggplot2)

library(tidyverse)
library(hrbrthemes)
#library(patchwork)
library(GGally)
library(viridis)

library("kml")
library("readxl")


d1 <- read_csv("D:\\MY-DOC\\Downloads\\RBPottery.csv")

d1$zAl <- (d1$Al2O3-mean(d1$Al2O3))/sd(d1$Al2O3)
d1$zFe <- (d1$Fe2O3-mean(d1$Fe2O3))/sd(d1$Fe2O3)
d1$zMg <- (d1$MgO-mean(d1$MgO))/sd(d1$MgO)
d1$zCa <- (d1$CaO-mean(d1$CaO))/sd(d1$CaO)
d1$zNa <- (d1$Na2O-mean(d1$Na2O))/sd(d1$Na2O)
d1$zK  <- (d1$K2O-mean(d1$K2O))/sd(d1$K2O)
d1$zTi <- (d1$TiO2-mean(d1$TiO2))/sd(d1$TiO2)
d1$zMn <- (d1$MnO-mean(d1$MnO))/sd(d1$MnO)
d1$zBa <- (d1$BaO-mean(d1$BaO))/sd(d1$BaO)

d1$Kiln <- as.factor(d1$Kiln)

ggpairs(d1[,13:21],col=d1$Kiln,
        lower.panel=NULL,
        cex.labels=2,pch=19,cex=1.2)

ggpairs(d1[,13:17],col=d1$Kiln,
        lower.panel=NULL,
        cex.labels=2,pch=19,cex=1.2)

round(cor(d1[,13:17]),2)

ggpairs(d1[,14:17],col=d1$Kiln,
        lower.panel=NULL,
        cex.labels=2,pch=19,cex=1.2)

fviz_nbclust(d1[,14:17],kmeans,method="wss")

fviz_nbclust(d1[,13:17],kmeans,method="wss")

chindex <- data.frame(2:10)
names(chindex) <- c("CH")
for(i in 2:10){
  clust <- kmeans(d1[,14:17],i)
  chindex[i-1,] <- clust$betweenss/(i-1)/(sum(clust$withinss)/(length(clust$cluster)-i))
}
ggplot(data=chindex)+geom_line(mapping=aes(y=chindex$CH,x=2:(length(chindex$CH)+1)),size=2,color="blue")+
  geom_point(y=max(chindex$CH),x=which.max(chindex$CH)+1,color="red",size=6)+xlab("Number of Clusters")+ylab("CH Index")+
  ggtitle("C-H Index for k-means")+theme_bw()+scale_x_continuous(breaks=2:10)+theme(axis.text=element_text(size=14),
                                                                                    axis.title=element_text(size=16,face="bold"),plot.title = element_text(size=22))
k3 <- kmeans(d1[,14:17],3)
d1$cluster_k3 <- as.factor(k3$cluster)
pairs(d1[,14:17],col=d1$cluster_k3,
      lower.panel=NULL,
      cex.labels=2,pch=19,cex=1.2)


k3 <- kmeans(d1[,14:17],centers=matrix(c(-1.5,-1,-1,-1,0.9,-0.4,1.2,0.9,0,1.1,-0.8,0.1),ncol=4))
d1$cluster_k3 <- as.factor(k3$cluster)
pairs(d1[,14:17],col=d1$cluster_k3,
      lower.panel=NULL,
      cex.labels=2,pch=19,cex=1.2)


k4 <- kmeans(d1[,13:17],4)
d1$cluster_k4 <- as.factor(k4$cluster)
pairs(d1[,13:17],col=d1$cluster_k4,
      lower.panel=NULL,
      cex.labels=2,pch=19,cex=1.2)

k3$size

round(k3$centers,1)

cor(as.numeric(d1$Kiln),as.numeric(d1$cluster_k3))
cor(as.numeric(d1$Kiln),as.numeric(d1$cluster_k4))

d2 <- iris
pairs(iris[,1:4],col=d2$Species,
      lower.panel=NULL,
      cex.labels=2,pch=19,cex=1.2)
d2 %>% 
  ggparcoord(
    columns = 1:4, groupColumn = 5, order = "anyClass",
    showPoints = TRUE, 
    title = "Parallel Coordinate Plot for the Iris Data",
    alphaLines = 0.3
  ) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )

times <- read.table("C:/Users/Devin/Desktop/Hypoxia_treat/Treated/times3.csv")
treat <- read.csv("C:/Users/Devin/Desktop/Hypoxia_treat/Treated/tdf3.csv", header=FALSE)
names<-as.vector(treat$V1)
times<-times[,1]
treat<-subset(treat,select=-c(1))
ldt<-cld(traj=treat,idAll=names,time=times,varNames="rv")
kml(ldt,nbRedrawing=50, toPlot="criteria")