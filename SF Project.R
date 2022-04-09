rawdata=read.csv("C:/Users/hp/Downloads/Data.txt",header = TRUE)
rawdata
View(rawdata)
colnames(rawdata)
summary(rawdata)
cor(rawdata)

attach(rawdata)
library(tidyverse)



den<-density(rawdata$Scores)
hist(rawdata$Scores,col="red", prob=TRUE,
xlab = "Scores",main="Histogram And Density")
lines(density(rawdata$Scores),lwd=3,col="chocolate3")
plot(den, frame= FALSE, col="blue",main="Density Plot")
den2<-density(rawdata$Hours)
plot(den2,frame=FALSE,col="red" ,main="Density plot")
install.packages("lmtest")
library(lmtest)
reg=lm(Scores~Hours, data=rawdata)
summary(reg)
ggplot(rawdata,aes(Hours,Scores))+
  geom_point()+
  stat_smooth(method = lm)
predicted_scores=2.4837+9.7758*Hours

view(predicted_scores)
attach(predicted_scores)



df1=data.frame(rawdata,predicted_scores)
view(df1)
attach(df1)
plot(x=predicted_scores,y=Scores,xlab="predicted_scores",ylab="Scores",
     main="Predicted vs Actual Values")
ggplot(df1,aes(Hours))+
  geom_point(aes(y=Scores,col="red"))+
  geom_point(aes(y=predicted_scores,col="blue"))
