data_csv=read.csv(file.choose(),header=T,sep=",")
head(data_csv)
names(data_csv)



Apositive <- function(){
  
 plot(data_csv$Donors,data_csv$A.positive, main = "A-positive percentage in all town",xlab="Donation center",ylab="No.of.litres")
 temp=data_csv$A.positive <30
 data.frame(data_csv$Donors,temp)
 smry0=summary(data_csv$A.positive)
 smry1=summary(temp)
 smry=c(smry0,smry1)
 return(smry)
  
}



Bpositive <- function(){
  
  plot(data_csv$Donors,data_csv$B.positive,xlab="Donation center",ylab="No.of.litres",main = "B-positive percentage in all town")
  temp=data_csv$B.positive <30
  data.frame(data_csv$Donors,temp)
  smry0=summary(data_csv$B.positive)
  smry1=summary(temp)
  smry=c(smry0,smry1)
  return(smry)
  
}



Opositive <- function(){
  
  plot(data_csv$Donors,data_csv$O.positive,xlab="Donation camp",ylab="No.of.litres",main = "O-positive percentage in all town")
  temp=data_csv$O.positive <30
  data.frame(data_csv$Donors,temp)
  smry0=summary(data_csv$O.positive)
  smry1=summary(temp)
  smry=c(smry0,smry1)
  return(smry)
  
}



Anegative <- function(){
  
  plot(data_csv$Donors,data_csv$A.negative,xlab="Donation camp",ylab="No.of.litres",main = "A-negative percentage in all town")
  temp=data_csv$A.negative <30
  data.frame(data_csv$Donors,temp)
  smry0=summary(data_csv$A.negative)
  smry1=summary(temp)
  smry=c(smry0,smry1)
  return(smry)
  
}



Bnegative <- function(){
  
  plot(data_csv$Donors,data_csv$B.negative,xlab="Donation camp",ylab="No.of.litres",main = "B-negative percentage in all town")
  temp=data_csv$B.negative <30
  data.frame(data_csv$Donors,temp)
  smry0=summary(data_csv$B.negative)
  smry1=summary(temp)
  smry=c(smry0,smry1)
  return(smry)
  
}



Onegative <- function(){
  
  plot(data_csv$Donors,data_csv$O.negative,xlab="Donation camp",ylab="No.of.litres",main = "O-negative percentage in all town")
  temp=data_csv$O.negative <30
  data.frame(data_csv$Donors,temp)
  smry0=summary(data_csv$O.negative)
  smry1=summary(temp)
  smry=c(smry0,smry1)
  return(smry)
  
}




ABpositive <- function(){
  
  plot(data_csv$Donors,data_csv$AB.positive,xlab="Donation camp",ylab="No.of.litres",main = "AB-positive percentage in all town")
  temp=data_csv$AB.positive <30
  data.frame(data_csv$Donors,temp)
  smry0=summary(data_csv$AB.positive)
  smry1=summary(temp)
  smry=c(smry0,smry1)
  return(smry)
  
}



ABnegative <- function(){
  
  plot(data_csv$Donors,data_csv$AB.negative,xlab="Donation camp",ylab="No.of.litres",main = "AB-negative percentage in all town")
  temp=data_csv$AB.negative <30
  data.frame(data_csv$Donors,temp)
  smry0=summary(data_csv$AB.negative)
  smry1=summary(temp)
  smry=c(smry0,smry1)
  return(smry)
  
  
}





print("Select operation for plot")
print("1.A-positive")
print("2.B-positive")
print("3.O-positive")
print("3.A-negative")
print("3.B-negative")
print("3.O-negative")
print("3.AB-positive")
print("3.AB-negative")


choice = as.integer(readline(prompt="Enter choice[1/2/3]: "))


result <- switch(choice,Apositive(),Bpositive(),Opositive(),Anegative(),Bnegative(),Onegative(),ABpositive(),ABnegative())


print(result)


a=mean(data_csv$A.positive)
b=mean(data_csv$B.positive)
o=mean(data_csv$O.positive)
an=mean(data_csv$A.negative)
bn=mean(data_csv$B.negative)
on=mean(data_csv$O.negative)
ab=mean(data_csv$AB.positive)
abn=mean(data_csv$AB.negative)
pie_chart=c(a,b,o,an,bn,on,ab,abn)
pie(pie_chart,main ="Total blood donations",col=blues9)


barplot(pie_chart, xlab = "Blood group", main="NO.of.litres" , col=blues9)

