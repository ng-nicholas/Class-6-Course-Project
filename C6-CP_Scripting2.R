data(ToothGrowth)
summary(ToothGrowth)

ToothGrowth$dose<-as.factor(ToothGrowth$dose)
library(ggplot2)
g<-ggplot(ToothGrowth,aes(x=dose,y=len))+
    geom_point()+
    facet_grid(~supp)
g

OJdat<-ToothGrowth[ToothGrowth$supp=="OJ",]
VCdat<-ToothGrowth[ToothGrowth$supp=="VC",]
t.test(OJdat$len[OJdat$dose==0.5],
       OJdat$len[OJdat$dose==2],
       paired=FALSE,
       var.equal=FALSE)$conf