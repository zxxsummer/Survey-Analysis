rm(list=ls())
#ctrl+L clean history on the console window

library(readstata13)
library(dplyr)
library(ggplot2)
library(gridExtra)

#read the dataset from stata
data<-read.dta13("survey_analysis.dta")

#add Q21name variable to specify the country name instead of simple number
Q21name<-read.csv("Q21name.csv",header=FALSE)
for (i in 1:nrow(data)){
  data$Q21name[i]<-as.character(Q21name[data$Q21[i],2])
}

#add Q24_i_NY variables with "NO"/"YES" response instead of 2/1 response in Q24_i
for ( j in 1:nrow(data)){
  if (data$Q24_1[j]==1){
    data$Q24_1_NY[j]="YES"
  }else{
    data$Q24_1_NY[j]="NO"
  }
}
for ( j in 1:nrow(data)){
  if (data$Q24_2[j]==1){
    data$Q24_2_NY[j]="YES"
  }else{
    data$Q24_2_NY[j]="NO"
  }
}
for ( j in 1:nrow(data)){
  if (data$Q24_3[j]==1){
    data$Q24_3_NY[j]="YES"
  }else{
    data$Q24_3_NY[j]="NO"
  }
}
for ( j in 1:nrow(data)){
  if (data$Q24_4[j]==1){
    data$Q24_4_NY[j]="YES"
  }else{
    data$Q24_4_NY[j]="NO"
  }
}
for ( j in 1:nrow(data)){
  if (data$Q24_5[j]==1){
    data$Q24_5_NY[j]="YES"
  }else{
    data$Q24_5_NY[j]="NO"
  }
}

#Separate the original dataset into two. One is only for selected companies in EAP, another for other regions.
d3_EAP<-data %>% filter(sel_reg=="East Asia & Pacific")
d3_notEAP<-data %>% filter(sel_reg!="East Asia & Pacific")

#add EAP variable, if the selected affiliate is in EAP, EAP is "YES", otherwise is "NO"
data$EAP<-rep("NO",nrow(data))
for (i in 1:nrow(data)){
  if (data$sel_reg[i]=="East Asia & Pacific"){
    data$EAP[i]="YES"
  }
}

#Test whether is significantly different among 5 motivations
t.test(d3_notEAP$Q24_1,d3_EAP$Q24_1,paired=FALSE)#P=0.3843
t.test(d3_notEAP$Q24_2,d3_EAP$Q24_2,paired=FALSE)#P=1.533e-06 Significant!
t.test(d3_notEAP$Q24_3,d3_EAP$Q24_3,paired=FALSE)#P=0.0004063 Significant!
t.test(d3_notEAP$Q24_4,d3_EAP$Q24_4,paired=FALSE)#P=0.4576
t.test(d3_notEAP$Q24_5,d3_EAP$Q24_5,paired=FALSE)#P=0.8181

#Generate graphs to present the difference of 5 motivations between two groups: EAP VS NOT EAP
Q24_1plot<-ggplot(data,aes(x=EAP,fill=Q24_1_NY))+geom_bar(position="fill")+coord_flip()
p1<-Q24_1plot+ylab("Proportion") +xlab("EAP")+ggtitle("Access New Markets or New Customers")+theme_grey(base_size = 8)+
  scale_fill_discrete(name="Motivation")

Q24_2plot<-ggplot(data,aes(x=EAP,fill=Q24_2_NY))+geom_bar(position="fill")+coord_flip()
p2<-Q24_2plot+ylab("Proportion") + xlab("EAP")+ggtitle("Lower Production Costs*")+theme_grey(base_size = 8)+
  scale_fill_discrete(name="Motivation")

Q24_3plot<-ggplot(data,aes(x=EAP,fill=Q24_3_NY))+geom_bar(position="fill")+coord_flip()
p3<-Q24_3plot+ylab("Proportion") +xlab("EAP")+ggtitle("Coordinate Company's Value Chain*")+theme_grey(base_size = 8)+
  scale_fill_discrete(name="Motivation")

Q24_4plot<-ggplot(data,aes(x=EAP,fill=Q24_4_NY))+geom_bar(position="fill")+coord_flip()
p4<-Q24_4plot+ylab("Proportion")+xlab("EAP")+ggtitle("Access Natural Resources and Raw Materials")+theme_grey(base_size = 8)+
  scale_fill_discrete(name="Motivation")

Q24_5plot<-ggplot(data,aes(x=EAP,fill=Q24_5_NY))+geom_bar(position="fill")+coord_flip()
p5<-Q24_5plot+ylab("Proportion") +xlab("EAP")+ggtitle("Acquire another firm")+theme_grey(base_size = 8)+
  scale_fill_discrete(name="Motivation")

EAP_Mot1<-grid.arrange(p1,p4,p5,nrow=3,ncol=1)    
EAP_Mot2<-grid.arrange(p2,p3,nrow=2,ncol=1)   

