dFull <- read.csv('full_data.csv')

dFull$subject <- factor(dFull$subject)

library(ggplot2)

RE<-dFull$RE
subject<-dFull$subject

t1<-(dFull$thr1)           
t2<-(dFull$thr2)
t3<-(dFull$thr3)
t4<-(dFull$thr4)
t5<-(dFull$thr5)
t6<-(dFull$thr6)

dTransform<-data.frame(t4, t5, t6)

colnames(dTransform)<-c('S_negative','S_positive', 'S')
pA <- ggplot(data = dTransform, aes(x=S_positive,y=S)) + geom_point(size=1)
print(pA)
pA <- ggplot(data = dTransform, aes(x=S_negative,y=S)) + geom_point(size=1)
print(pA)

write.csv(dTransform, file = 'exercise3.csv')

