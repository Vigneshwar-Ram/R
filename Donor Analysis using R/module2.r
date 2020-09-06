library(plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)


gd=read.csv(file.choose(),header = T,sep=",")
head(gd)
names(gd)
pd=as.matrix(gd[,c("Donors","A.positive","B.positive","O.positive","A.negative","AB.negative","B.positive","O.positive","AB.positive","Latitude","Longitude")])

od=pd[,10:11]

od[1:10,]

wss<-numeric(15)

for(k in 1:15)  
  wss[k]<-sum(kmeans(od,centers = k,nstart = 25)$withinss)

plot(1:15,wss,type = "b",xlab="number of clusters",ylab = "wss")

km=kmeans(od,4,nstart = 25)
km

c(wss[3],sum(km$withinss))

df=as.data.frame(pd[,10:11])
df
df$cluster=factor(km$cluster)

centers=as.data.frame(km$centers)

plot1=ggplot(data=df,aes(x=Latitude,y=Longitude,color=cluster))+geom_point()+geom_point(data=centers,aes(x=Latitude,y=Longitude,color=as.factor(c(1,2,3,4))),size=10,alpha=.3)



plot1


print("The no of donation centers in cluster 1 is")
(km$size[1])

print("The no of donation centers in cluster 2 is")
(km$size[2])

print("The no of donation centers in cluster 3 is")
(km$size[3])

print("The no of donation centers in cluster 4 is")
(km$size[4])


write.csv(km$centers,file="F:/Resultant Donor Camp Centers.csv")
