a=read.csv(file.choose(),header = T,sep=",")
aa=a[,2:3]
plot(aa,xlab = "Latitude",ylab = "Longitude",main="Centralized Donor camp")
