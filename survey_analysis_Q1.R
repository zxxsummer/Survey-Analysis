rm(list=ls())
#ctrl+L clean history on the console window

library(readstata13)
library(dplyr)
library(ggplot2)

#read the dataset from stata
data<-read.dta13("survey_analysis.dta")

#add Q1name variable to specify the country name instead of simple number
Q1name<-read.csv("Q1name.csv",header=FALSE)
for (i in 1:nrow(data)){
  data$Q1name[i]<-as.character(Q1name[data$Q1[i],2])
}
for (i in 1:nrow(data)){
  data$hq_name[i]<-as.character(Q1name[data$hq[i],2])
}

#Prepare data set of MENA
d1_MENA<-data %>% filter(AFF_MENA!=0) %>% select(hq_name,Q1name,AFF_MENA,AFF_DZA,AFF_DJI,AFF_EGY,AFF_IRN,AFF_IRQ,AFF_JOR,AFF_LBN,AFF_LBY,AFF_MAR,AFF_SYR,AFF_TUN,AFF_WBG,AFF_YEM)
list_aff_mena<-c("Algeria","Djibouti","Egypt, Arab Rep.","Iran, Islamic Rep.","Iraq","Jordan","Lebanon","Libya","Morocco","Syrian Arab Republic","Tunisia","West Bank and Gaza","Yemen, Rep.")

data$AFF_MENA
data$hq_name

aff_mena<-data.frame()
##???????????????????????????????????????affiliate?????????
# for (i in 1:nrow(d1_MENA)){
#  aff_mena[i,1]<-d1_MENA[i,1]
#     for (j in 3:ncol(d1_MENA)) {
#     if(d1_MENA[i,j]==1){
#       aff_mena[i,2]=list_aff_mena[j-3]
#     }
#   }
# }
colnames(aff_mena)<-c("Headquarter_Country","Affiliate_Country")
aff_mena

#Calculate % of companies have affiliates in MENA
round(nrow(aff_mena)/nrow(data)*100,2)

#Generate the graph of countries
MENA1<-ggplot(aff_mena,aes(x=Affiliate_Country,fill=Headquarter_Country))+geom_bar()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+coord_flip()
MENA1+ylab("Count") + xlab("Affiliate Country") + 
  ggtitle("3.76% of Surveyed Companies Have Affiliates in MENA")+theme_grey(base_size = 15)+scale_fill_discrete(name="Headquarter Country")

#Prepare data set of EAP
d1_EAP<-data %>% filter(AFF_EAP==1) %>% select(Q1,Q1name,AFF_ASM,AFF_KHM,AFF_CHN,AFF_FJI,AFF_IDN,AFF_KIR,AFF_PRK,AFF_LAO,AFF_MYS,AFF_MHL,AFF_FSM,AFF_MNG,AFF_MMR,AFF_PLW,AFF_PNG,AFF_PHL,AFF_WSM,AFF_SLB,AFF_THA,AFF_TMP,AFF_TON,AFF_TUV,AFF_VUT,AFF_VNM)
list_aff_eap<-c("American Samoa","Cambodia","China","Fiji","Indonesia","Kiribati","Korea, Dem. People's Rep.","Lao PDR","Malaysia","Marshall Islands","Micronesia, Fed. Sts.","Mongolia","Myanmar","Palau","Papua New Guinea","Philippines","Samoa","Solomon Islands","Thailand","Timor-Leste","Tonga","Tuvalu","Vanuatu","Vietnam")

aff_eap<-data.frame()
for (i in 1:nrow(d1_EAP)){
  aff_eap[i,1]<-d1_EAP[i,2]
  for (j in 3:ncol(d1_EAP)) {
    if(d1_EAP[i,j]==1){
      aff_eap[i,2]=list_aff_eap[j-2]
    }
  }
}
colnames(aff_eap)<-c("Headquarter_Country","Affiliate_Country")
aff_eap

#Calculate % of companies have affiliates in EAP
round(nrow(aff_eap)/nrow(data)*100,2)

data$AFF_MENA
#Generate the graph of countries, EAP1 with color
EAP1<-ggplot(aff_eap,aes(x=Affiliate_Country,fill=Headquarter_Country))+geom_bar()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+coord_flip()
EAP1+ylab("Count") + xlab("Affiliate Country") + 
  ggtitle("28.1% of Surveyed Companies Have Affiliates in EAP")+theme_grey(base_size = 15)+scale_fill_discrete(name="Headquarter Country")+
  theme(legend.position="bottom")

EAP2<-ggplot(aff_eap,aes(x=Affiliate_Country))+geom_bar()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+coord_flip()
EAP2+ylab("Count") + xlab("Affiliate Country") +
  ggtitle("28.1% of Surveyed Companies Have Affiliates in EAP")+theme_grey(base_size = 15)+scale_fill_discrete(name="Headquarter Country")

#Count and show which countries select affiliates in EAP
count<-c(0,0,0,0,0,0,0,0,0)
for (i in 1:nrow(aff_eap)){
  if(aff_eap[i,2]=="Vietnam"){
    count[1]=count[1]+1
    print(aff_eap[i,1])}
}
for (i in 1:nrow(aff_eap)){
  if(aff_eap[i,2]=="Thailand"){
    count[2]=count[2]+1
    print(aff_eap[i,1])}
}
for (i in 1:nrow(aff_eap)){
  if(aff_eap[i,2]=="Samoa"){
    count[3]=count[3]+1
    print(aff_eap[i,1])}
}
for (i in 1:nrow(aff_eap)){
  if(aff_eap[i,2]=="Philippines"){
    count[4]=count[4]+1
    print(aff_eap[i,1])}
}
for (i in 1:nrow(aff_eap)){
  if(aff_eap[i,2]=="Myanmar"){
    count[5]=count[5]+1
    print(aff_eap[i,1])}
}
for (i in 1:nrow(aff_eap)){
  if(aff_eap[i,2]=="Malaysia"){
    count[6]=count[6]+1
    print(aff_eap[i,1])}
}
for (i in 1:nrow(aff_eap)){
  if(aff_eap[i,2]=="Korea, Dem. People's Rep."){
    count[7]=count[7]+1
    print(aff_eap[i,1])}
}
for (i in 1:nrow(aff_eap)){
  if(aff_eap[i,2]=="Indonesia"){
    count[8]=count[8]+1
    print(aff_eap[i,1])}
}
for (i in 1:nrow(aff_eap)){
  if(aff_eap[i,2]=="China")
    count[9]=count[9]+1
  print(aff_eap[i,1])
}
count