library(dplyr)
library(ggplot2)
theme_set(theme_classic())

## Initialize Dataset and pre-processing
cities = c("Los Angeles", "Phoenix", "San Diego", "San Francisco", "Seattle", "Los Angeles", "Phoenix", "San Diego", "San Francisco", "Seattle")
ontime = c(497, 221, 212, 503, 1841, 694, 4840, 383, 320, 201)
delayed = c(62, 12, 20, 102, 305, 117, 415, 65, 129, 61)
flight = c("Alaska Airlines", "Alaska Airlines", "Alaska Airlines", "Alaska Airlines", "Alaska Airlines", "America West", "America West", "America West", "America West", "America West")

data = data.frame(cities, ontime, delayed, flight)
data_AA = filter(data, cities=="Los Angeles")
data_AW = filter(data, flight=="America West")

#Find a cleaner way to do this
data = rbind(data, data.frame(cities='Total', ontime=sum(data_AA$ontime), delayed = sum(data_AA$delayed), flight="Alaska Airlines"))
data = rbind(data, data.frame(cities='Total', ontime=sum(data_AW$ontime), delayed = sum(data_AW$delayed), flight="America West"))

data['percentage_delayed'] = round( ((data$delayed / (data$delayed + data$ontime) ) * 100), digits = 1)

data_LA = filter(data, cities=="Los Angeles")
data_PHX = filter(data, cities=="Phoenix")
data_SD = filter(data, cities=="San Diego")
data_SF = filter(data, cities=="San Francisco")
data_SEA = filter(data, cities=="Seattle")
data_TOT = filter(data, cities == "Total")

## Bar Charts for both Airlines
ggplot(data=data, aes(x=cities, y=percentage_delayed, fill=flight)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  labs(title="Bar Chart", 
       subtitle="Percentage of Delayed Flight Per City for America West and Alaska Airlines", 
       caption="Source: Moore, D.S. (2003) dataset, Statistics for the number of delayed and ontime flights") +
  geom_text(aes(label = paste0(percentage_delayed, "%")), vjust = -0.5, position = position_dodge(0.9))

## Pie Chart for Cities
## TODO: Fix Pie chart labels
par(mfrow=c(2,2))
a = ggplot(data=data_LA)+
  geom_bar(aes(x="", y=percentage_delayed, fill=flight), stat="identity", width = 1)+
  coord_polar("y", start=0, direction = -1)+
  labs(title="Pie Chart", 
       subtitle="Percentage of Delayed Flight Per City for Los Angeles", 
       caption="Source: Moore, D.S. (2003) dataset") + 
  theme_void() 

b = ggplot(data=data_PHX)+
  geom_bar(aes(x="", y=percentage_delayed, fill=flight), stat="identity", width = 1)+
  coord_polar("y", start=0, direction = -1)+
  labs(title="Pie Chart", 
       subtitle="Percentage of Delayed Flight Per City for Phoenix", 
       caption="Source: Moore, D.S. (2003) dataset") + 
  theme_void() 


c = ggplot(data=data_SD)+
  geom_bar(aes(x="", y=percentage_delayed, fill=flight), stat="identity", width = 1)+
  coord_polar("y", start=0, direction = -1)+
  labs(title="Pie Chart", 
       subtitle="Percentage of Delayed Flight Per City for San Diego", 
       caption="Source: Moore, D.S. (2003) dataset") + 
  theme_void() 


d = ggplot(data=data_SF)+
  geom_bar(aes(x="", y=percentage_delayed, fill=flight), stat="identity", width = 1)+
  coord_polar("y", start=0, direction = -1)+
  labs(title="Pie Chart", 
       subtitle="Percentage of Delayed Flight Per City for San Francisco", 
       caption="Source: Moore, D.S. (2003) dataset") + 
  theme_void() 


e = ggplot(data=data_SEA)+
  geom_bar(aes(x="", y=percentage_delayed, fill=flight), stat="identity", width = 1)+
  coord_polar("y", start=0, direction = -1)+
  labs(title="Pie Chart", 
       subtitle="Percentage of Delayed Flight Per City for Seattle", 
       caption="Source: Moore, D.S. (2003) dataset") + 
  theme_void() 

f = ggplot(data=data_TOT)+
  geom_bar(aes(x="", y=percentage_delayed, fill=flight), stat="identity", width = 1)+
  coord_polar("y", start=0, direction = -1)+
  labs(title="Pie Chart", 
       subtitle="Percentage of Delayed Flight Per City for All Cities", 
       caption="Source: Moore, D.S. (2003) dataset") + 
  theme_void() 

ggarrange(a, b, c, d, e, f, labels = c('a', 'b', 'c', 'd', 'e', 'f'), ncol = 3, nrow = 3)