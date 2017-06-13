rm(list=ls())
#ctrl+L clean history on the console window

library(readstata13)
library(dplyr)
library(ggplot2)

#read the dataset from stata
data<-read.dta13("survey_analysis.dta")

#add Q1name and Q21name variable to specify the country name instead of simple number
Q1name<-read.csv("Q1name.csv",header=FALSE)
for (i in 1:nrow(data)){
  data$Q1name[i]<-as.character(Q1name[data$Q1[i],2])
}

for (i in 1:nrow(data)){
  data$hq_name[i]<-as.character(Q1name[data$hq[i],2])
}

data$hq_name
Q21name<-read.csv("Q21name.csv",header=FALSE)
for (i in 1:nrow(data)){
  data$Q21name[i]<-as.character(Q21name[data$Q21[i],2])
}

list_Q21_mena<-c("Algeria","Djibouti","Egypt, Arab Republic","Iran, Islamic Republic","Iraq","Jordan","Lebanon","Libya","Morocco","Syrian Arab Republic","Tunisia","West Bank and Gaza (Palestine)","Yemen, Republic")
#Prepare data set of MENA
d2_MENA<-data %>% filter(sel_reg=="Middle East & North Africa") %>% select(hq_name,Q1,Q1name,Q21,Q21name)

#Calculate % of MENA companies in Q21
round(nrow(d2_MENA)/nrow(data)*100,2)

#Generate graph of MENA selected companies 
MENA1<-ggplot(d2_MENA,aes(x=Q21name,fill=hq_name))+geom_bar()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+coord_flip()
MENA1+ylab("Count") + xlab("Selected Country") + 
  ggtitle("2.65% of Surveyed Companies Select Affiliates in MENA")+theme_grey(base_size = 15)+scale_fill_discrete(name="Headquarter Country")

#Prepare data set of EAP
d2_EAP<-data %>% filter(sel_reg=="East Asia & Pacific") %>% select(hq_name,Q1,Q1name,Q21,Q21name)
#Calculate % of MENA companies in Q21
round(nrow(d2_EAP)/nrow(data)*100,2)

#Generate graph of MENA selected companies, EAP1 with color, EAP2 without
EAP1<-ggplot(d2_EAP,aes(x=Q21name,fill=hq_name))+geom_bar()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+coord_flip()
EAP1+ylab("Count") + xlab("Selected Country") + 
  ggtitle("20.8% of Surveyed Companies Select Affiliates in EAP")+theme_grey(base_size = 15)+scale_fill_discrete(name="Headquarter Country")+theme(legend.position="bottom")

EAP2<-ggplot(d2_EAP,aes(x=Q21name))+geom_bar()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+coord_flip()
EAP2+ylab("Count") + xlab("Selected Country") + 
  ggtitle("20.8% of Surveyed Companies Select Affiliates in EAP")+theme_grey(base_size = 15)+scale_fill_discrete(name="Headquarter Country")+theme(legend.position="bottom")

#Count and show which countries select affiliates in EAP
count<-c(0,0,0,0,0,0,0,0,0,0)
for (i in 1:nrow(d2_EAP)){
  if(d2_EAP$Q21name[i]=="Vietnam"){
    count[1]=count[1]+1
    print(d2_EAP$Q1name[i])}
}
for (i in 1:nrow(d2_EAP)){
  if(d2_EAP$Q21name[i]=="Thailand"){
    count[2]=count[2]+1
    print(d2_EAP$Q1name[i])}
}
count[2]
for (i in 1:nrow(d2_EAP)){
  if(d2_EAP$Q21name[i]=="Samoa"){
    count[3]=count[3]+1
    print(d2_EAP$Q1name[i])}
}
count[3]
for (i in 1:nrow(d2_EAP)){
  if(d2_EAP$Q21name[i]=="Philippines"){
    count[4]=count[4]+1
    print(d2_EAP$Q1name[i])}
}
count[4]
for (i in 1:nrow(d2_EAP)){
  if(d2_EAP$Q21name[i]=="Mongolia"){
    count[5]=count[5]+1
    print(d2_EAP$Q1name[i])}
}
count[5]
for (i in 1:nrow(d2_EAP)){
  if(d2_EAP$Q21name[i]=="Malaysia"){
    count[6]=count[6]+1
    print(d2_EAP$Q1name[i])}
}
count[6]
for (i in 1:nrow(d2_EAP)){
  if(d2_EAP$Q21name[i]=="Korea, Democratic People's Republic"){
    count[7]=count[7]+1
    print(d2_EAP$Q1name[i])}
}
count[7]
for (i in 1:nrow(d2_EAP)){
  if(d2_EAP$Q21name[i]=="Indonesia"){
    count[8]=count[8]+1
    print(d2_EAP$Q1name[i])}
}
count[8]
for (i in 1:nrow(d2_EAP)){
  if(d2_EAP$Q21name[i]=="China"){
    count[9]=count[9]+1
    print(d2_EAP$Q1name[i])}
}
count[9]
for (i in 1:nrow(d2_EAP)){
  if(d2_EAP$Q21name[i]=="Cambodia"){
    count[10]=count[10]+1
    print(d2_EAP$Q1name[i])}
}
count[10]