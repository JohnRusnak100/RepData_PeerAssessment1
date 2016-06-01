library(dplyr)
library(ggplot2)
orig<- read.csv("activity.csv",stringsAsFactors = F)
orig$date<-as.Date(orig$date)
orig<-mutate(orig,dayofweek=weekdays(date))
o2<-group_by(orig,date) %>% summarize(Dailysum=sum(steps,na.rm=T))
hist(o2$Dailysum,col="red",xlab="Daily Total Steps Taken",
     main="Daily Total Steps Taken\nOriginal Data",ylab="Frequency")
summary(o2$Dailysum)
o3<-group_by(orig,interval) %>% summarize(Dailymean=mean(steps,na.rm=T))
plot(o3$interval,o3$Dailymean,xlab="Interval ",ylab="Average # Steps Taken",
     main="Average Number of Steps Taken\nAcross All Days",type ="l")
Dailymax<-max(o3$Dailymean)
o3[o3$Dailymean==Dailymax,]
colSums(is.na(orig))
o4<-filter(orig,is.na(steps))
o4<-xtabs(  ~ o4$interval + o4$date,data=o4)
colSums(o4)
o5<-merge(orig,o3)
nainx<-is.na(o5$steps)
o5[nainx,2] <-o5[nainx,5]
rev<-o5
r2<-group_by(rev,date) %>% summarize(Dailysum=sum(steps,na.rm=T))
hist(r2$Dailysum,col="red",xlab="Daily Total Steps Taken",
     main="Daily Total Steps Taken\nAdjusted Data",ylab="Frequency")
summary(r2$Dailysum)
dow=as.factor(1*(rev$dayofweek=="Saturday")| 1* (rev$dayofweek=="Sunday"))
levels(dow)=c("Weekday","Weekend")    
r3<-mutate(rev,dow)
r3<-group_by(r3,dow,interval) %>% summarize(Dailymean=mean(steps,na.rm=T))
g<-ggplot(r3,aes(interval,Dailymean,color=dow))
                 g + geom_line() + 
                 facet_wrap( ~ dow,nrow=2,ncol=1) + 
                 labs(y="Average # of Total Steps Taken") + 
                 labs(x="Interval") + 
                 labs(title="Avg Number of Steps Taken \nWeekend vs Weekday") +
                 theme(axis.text.y=element_text(size=10,face="bold")) +
                 theme(axis.text.x=element_text(size=9,face="bold"))  +
                 theme(legend.position="none")
                 
                 
                 
                 