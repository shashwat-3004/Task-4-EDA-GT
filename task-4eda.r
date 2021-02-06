#########################
library(tidyverse) # Attach the packages
library(gridExtra)
library(plotly)
#########################
data<-read_csv("globalterrorismdb_0718dist.csv") # Read the data
#########################
head(data)                     # Basic data precprocessing and summary of the data
tail(data)

colnames(data)

req_col<-c("iyear","country_txt","region_txt","provstate","city","success","suicide","attacktype1_txt","targtype1_txt","natlty1_txt","gname","weaptype1_txt","nkill","nwound")
data<-data[,req_col]


str(data)

data<-data%>%na.exclude()
summary(data)

colnames(data)<-c("Year","Country","Region","State","City","Success","Suicide","Attacktype","Targettype","Nationality","Group","Weapontype","Kills","Wounded")
#########################

# Exploratory Data Analysis

##########################
pl<-data%>%group_by(Year)%>%summarise(Count=n())%>%ggplot(aes(x=Year,y=Count))+geom_point(colour="blue",size=2)+
  geom_line(colour="red",lwd=1)
ggplotly(pl)
#########################
top_15_country<-data%>%group_by(Country)%>%summarise(Count=n())%>%slice_max(Count,n=15)

least_15_country<-data%>%group_by(Country)%>%summarise(Count=n())%>%slice_min(Count,n=15)

top_15_country%>%mutate(Country=reorder(Country,Count))%>%ggplot()+geom_col(aes(x=Country,y=Count),fill="#eb4934")+
  coord_flip()

least_15_country%>%mutate(Country=reorder(Country,Count))%>%ggplot()+geom_col(aes(x=Country,y=Count),fill="#4feb34")+
  coord_flip()

#########################

data%>%ggplot()+geom_bar(aes(x=Region),fill="red")+coord_flip()

#########################
data$Success<-factor(data$Success)

data%>%ggplot()+ geom_bar(aes(x=Year,fill=Success),position = "dodge")
table(data$Success)

data$Suicide<-factor(data$Suicide)

data%>%ggplot()+geom_bar(aes(x=Year,fill=Suicide),position = "dodge",binwidth = 1)
#########################
ggplot(data,aes(x=Year,y=Kills))+geom_smooth()

#########################


library(scales)

data%>%group_by(Attacktype)%>%summarise(Kills=sum(Kills))%>%mutate(Attacktype=reorder(Attacktype,Kills))%>%ggplot()+
  geom_col(aes(x=Attacktype,y=Kills),fill="red")+scale_y_continuous(labels = comma)+coord_flip()
#########################

data%>%group_by(Targettype)%>%summarise(Kills=sum(Kills))%>%mutate(Targettype=reorder(Targettype,Kills))%>%ggplot()+
  geom_col(aes(x=Targettype,y=Kills),fill="red")+coord_flip()

#########################
data%>%group_by(Nationality)%>%summarise(Kills=sum(Kills))%>%mutate(Nationality=reorder(Nationality,Kills))%>%
   slice_max(Kills,n=10)%>%ggplot()+geom_col(aes(x=Nationality,y=Kills),fill="#eb4934")+coord_flip()
#########################
grp_data<-data%>%group_by(Group)%>%summarise(Count=n(),nkills=sum(Kills))

p1<-grp_data%>%mutate(Group=reorder(Group,Count))%>%slice_max(Count,n=10)%>%ggplot()+
  geom_col(aes(x=Group,y=Count),fill="#eb4934")+coord_flip()

p2<-grp_data%>%mutate(Group=reorder(Group,nkills))%>%slice_max(nkills,n=10)%>%ggplot()+
  geom_col(aes(x=Group,y=nkills),fill="#eb4934")+coord_flip()

grid.arrange(p1,p2,nrow=2)
#########################

data%>%group_by(Weapontype)%>%summarise(nkill=sum(Kills))%>%mutate(Weapontype=reorder(Weapontype,nkill))%>%
  slice_max(nkill,n=5)%>%ggplot()+geom_col(aes(x=Weapontype,y=nkill),fill="#eb4934")+coord_flip()

#########################

data%>%ggplot()+geom_bar(aes(Attacktype,fill=Targettype),position = "dodge")

#########################

data_india<-data%>%filter(Country=="India")

#########################

plot<-data_india%>%group_by(Year)%>%summarise(Count=n())%>%ggplot()+geom_point(aes(x=Year,y=Count),colour="blue")+
  geom_line(aes(x=Year,y=Count),colour="red")

ggplotly(plot)
#########################
plt1<-data_india%>%group_by(City)%>%summarise(Count=n())%>%slice_max(Count,n=4)%>%ggplot(aes(x=City,y=Count))+
  geom_col(fill="darkred")+coord_flip()

plt2<-data_india%>%group_by(State)%>%summarise(Count=n())%>%slice_max(Count,n=4)%>%ggplot(aes(x=State,y=Count))+
  geom_col(fill="darkred")+coord_flip()

grid.arrange(plt1,plt2,nrow=2)

#########################


