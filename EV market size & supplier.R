data<-read.csv("VehicleYear-2021.csv") 
head(data)
str(data)
#change col names
colnames(data)
colnames(data)[17]<-c("Power1") #"power" name crash with built-in function 
colnames(data)[22]<-c("OCountry")
str(data)
unique(data$Power1)
unique(data$OCountry)

#filter the data
library(dplyr)
data<-filter(data,Power1==c("PETROL","DIESEL","ELECTRIC","PETROL HYBRID",
                                "PLUGIN PETROL HYBRID"))
#extract a new data 
new<-data[,c(17,22)]

library(ggplot2)
#Power Bar Chart
a<-ggplot(data.frame(new),aes(x=Power1))+geom_bar(fill="red",alpha=0.5)+
   theme(axis.text.x = element_text(face = "bold", size = 8.5, angle = 20))
a+xlab("Motive_Power")+ylab("Frequency")+ggtitle("2021 NZ vehicle")
  theme(axis.title.x=element_text(colour="Black",size=10),
        axis.title.y=element_text(colour="Black",size=15), 
        axis.text.x =element_text(size=1),  
        axis.text.y=element_text(size=1))

#proportion bar chart
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)   
new %>% 
  count(Power1) %>% 
  mutate(perc = n / nrow(new)) -> new1
b<-ggplot(new1,aes(x=Power1,y=perc))+geom_bar(stat="identity",fill="red",alpha=0.5)+
  theme(axis.text.x = element_text(face = "bold", size =9, angle = 20))+
  geom_text(aes(label = scales::percent(perc),
            y=perc),position = position_dodge(width = .9),vjust = -0.4,size=4)+
  scale_y_continuous(labels = scales::percent)
b+xlab("Motive_Power")+ylab("Proportion")+ggtitle("2021 NZ vehicle")
  theme(axis.title.x=element_text(colour="Black",size=20),
      axis.title.y=element_text(colour="Black",size=20), 
      axis.text.x =element_text(size=10),  
      axis.text.y=element_text(size=10))

#Original Country,filter the data, only show EVs
new2<-filter(new, Power1==c("ELECTRIC","PETROL HYBRID","PLUGIN PETROL HYBRID"))
#EVs Original Country overall
c<-ggplot(data=new2,aes(x=OCountry))+geom_bar(fill="darkgreen",alpha=0.6)+
  theme(axis.text.x = element_text(face = "bold", size =7, angle = 20))
c+xlab("Original Country")+ylab("Frequency")+ggtitle("2021 EVs Original Country")
  theme(axis.title.x=element_text(colour="Black",size=10),
      axis.title.y=element_text(colour="Black",size=15), 
      axis.text.x =element_text(size=1),  
      axis.text.y=element_text(size=1))
  
#above shows up 14 countries,too many, some count really small, filter count>10  
library(tidyverse)
new2 %>% 
  group_by(OCountry) %>%
  count %>%
  filter(n > 10) ->new3
ggplot(new3,aes(x = OCountry, y = n)) + 
  geom_bar(stat ="identity",fill="darkgreen",alpha=0.6)+
  theme(axis.text.x = element_text(face = "bold", size =8.5, angle = 20))+
  xlab("Original Country")+ylab("Frequency")+ggtitle("2021 EVs Original Country")


#2016 NZ Vehicle
data_0<-read.csv("VehicleYear-2016.csv") 
head(data_0)
str(data_0)
#change col names
colnames(data_0)
colnames(data_0)[17]<-c("Power1")  
colnames(data_0)[22]<-c("OCountry")
str(data_0)
unique(data_0$Power1)
 
#filter the data
library(dplyr)
data_0<-filter(data_0,Power1==c("PETROL","DIESEL","ELECTRIC","PETROL HYBRID",
                            "PLUGIN PETROL HYBRID"))
#extract a new data 
new_0<-data_0[,c(17,22)]

#proportion bar chart
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)   
new_0 %>% 
  count(Power1) %>% 
  mutate(perc = n / nrow(new)) -> new_1
b<-ggplot(new_1,aes(x=Power1,y=perc))+geom_bar(stat="identity",fill="red",alpha=0.5)+
  theme(axis.text.x = element_text(face = "bold", size =9, angle = 20))+
  geom_text(aes(label = scales::percent(perc),
                y=perc),position = position_dodge(width = .9),vjust = -0.4,size=4)+
  scale_y_continuous(labels = scales::percent)
b+xlab("Motive_Power")+ylab("Proportion")+ggtitle("2016 NZ vehicle")
theme(axis.title.x=element_text(colour="Black",size=20),
      axis.title.y=element_text(colour="Black",size=20), 
      axis.text.x =element_text(size=10),  
      axis.text.y=element_text(size=10))


