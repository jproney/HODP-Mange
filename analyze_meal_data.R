library(dplyr)
library(jsonlite)
library(plyr)
library(ggplot2)
library(reshape)
library(parsedate)
library(lubridate)
library(scales)
setwd("~/Documents/Harvard/hodp/dhall_scrape/analysis/")
mealData = read_json("meal_data.json")

days = c('S','M','T','W','R','F','S')


mealToDf = function(meal) {
  cbind(ldply(meal$data, function(meal){ cbind(data.frame(meal$data), time = data.frame(meal$time), status = data.frame(meal$status))}), mealType = meal$meal, date = meal$date)
}

fullData = ldply(mealData, mealToDf)
byHouse = reshape::melt(fullData, id=c("meal.time","meal.status","mealType","date"))
colnames(byHouse)[5:6] <- c("house","numOrders")
byHouse = byHouse %>% mutate(hour = hour(parse_date(meal.time)) - 4,min = minute(parse_date(meal.time)), day = day(parse_date(meal.time)),mon = month(parse_date(meal.time)), wday = wday(parse_date(meal.time))) %>% select(-c(meal.time,date))
byHouse = byHouse %>% filter((mealType == "Dinner" & (hour*60 + min <= 19*60) & wday != 5) | (mealType == "Lunch" & (hour >= 12) & (hour < 14))) %>% filter(mon == 10 | (mon == 1 & day < 25)) %>% filter(house != "quincy" & house != "winthrop") #Strip times when grill isn't open

lunch = byHouse %>% filter(mealType == "Lunch") %>% group_by(hour,mealType, house, min) %>% dplyr::summarise(demand = mean(numOrders)) 
dinner =byHouse %>% filter(mealType == "Dinner", wday != "R") %>% group_by(hour,mealType, house, min) %>% dplyr::summarise(demand = mean(numOrders)) 

lunch_days = byHouse %>% filter(mealType == "Lunch") %>% group_by(hour,mealType, house, min, wday) %>% dplyr::summarise(demand = mean(numOrders)) 
dinner_days = byHouse %>% filter(mealType == "Dinner") %>% group_by(hour,mealType, house, min, wday) %>% dplyr::summarise(demand = mean(numOrders)) 

ggplot(data = lunch, aes(x = (hour - 12)*60 + min, y = demand, fill = house)) + scale_x_continuous(breaks=c(0,30,60,90,120),labels=c("12:00","12:30","1:00","1:30","2:00")) + geom_area(position='stack') + ylab("Average Number of Pending Orders") + xlab("Time")+ ggtitle("Average Demand: Lunch") + geom_vline(xintercept = 75, size=1) + geom_text(aes(x=76, label="\nEnd of 12:00 Classes", y=17), angle=90)
ggplot(data = lunch_days, aes(x = (hour - 12)*60 + min, y = demand, fill = house)) + scale_x_continuous(breaks=c(30,60,90,120),labels=c("12:30","1:00","1:30","2:00")) + geom_area(position='stack') + ylab("Average Number of Pending Orders") + xlab("Time")+ ggtitle("Average Demand: Lunch") + facet_grid(~ wday , labeller = function(l){lapply(l,function(i){days[i]})})

lunch_normed = lunch %>% group_by(house, mealType) %>% dplyr::mutate(demand = (demand - mean(demand))/sd(demand))
ggplot(data = lunch_normed, aes(x = (hour - 12)*60 + min, y = demand, color = house))+ geom_line() + ylab("Average Number of Pending Orders") + xlab("Minutes since 12:00 PM") + ggtitle("Aggregate Demand: Lunch")

ggplot(data = dinner, aes(x = (hour - 17)*60 + min, y = demand, fill = house)) + scale_x_continuous(breaks=c(0,30,60,90,120),labels=c("5:00","5:30","6:00","6:30","7:00")) + geom_area(position='stack') + ylab("Average Number of Pending Orders") + xlab("Time")+ ggtitle("Average Demand: Dinner")
ggplot(data = dinner_days, aes(x = (hour - 17)*60 + min, y = demand, fill = house))+ scale_x_continuous(breaks=c(0,30,60,90,120),labels=c("5:00","5:30","6:00","6:30","7:00")) + geom_area(position='stack') + ylab("Average Number of Pending Orders") + xlab("Time")+ ggtitle("Average Demand: Dinner") + facet_grid(~ wday , labeller = function(l){lapply(l,function(i){days[i]})})

dinner_normed = dinner %>% group_by(house, mealType) %>% dplyr::mutate(demand = (demand - mean(demand))/sd(demand))
ggplot(data = dinner_normed, aes(x = (hour - 12)*60 + min, y = demand, color = house))+ geom_line() + ylab("Total number of pending orders") + xlab("Minutes since 5:00 PM") + ggtitle("Aggregate Demand: Dinner")

#broken down by day
lunchd = byHouse %>% group_by(mealType, wday, house) %>% dplyr::summarise(demand = mean(numOrders)) 
ggplot(data=lunchd, aes(x = mealType, y= demand,fill = house)) + geom_bar(stat="identity") + facet_grid(~ wday , labeller = function(l){lapply(l,function(i){days[i]})}) + xlab("Meal")


houses = byHouse %>% group_by(house) %>% dplyr::summarise(demand = sum(numOrders)) 
ggplot(data=houses, aes(x = reorder(house, demand), y= demand)) + geom_bar(aes(fill = house),stat="identity",position='dodge') + xlab("House") + ylab("Total observed orders")
houses = byHouse %>% group_by(house, mealType) %>% dplyr::summarise(demand = sum(numOrders)) 
ggplot(data=houses, aes(x = reorder(house, demand), y= demand)) + geom_bar(aes(fill = mealType),stat="identity",position='dodge') + xlab("House") + ylab("Total observed orders")

house_pops = c(dunster=360,eliot=430,adams=450,kirkland=360,quincy=450,mather=400,lowell=415, winthrop=400,currier=350,cabot=360,annenberg=1650,leverett=500,pfoho=360)

houses = byHouse %>% group_by(house) %>% dplyr::summarise(demand = sum(numOrders)) 
ggplot(data=houses, aes(x = reorder(house, demand/house_pops[house]), y= demand/house_pops[house])) + geom_bar(aes(fill = house),stat="identity",position='dodge') + xlab("House") + ylab("Total observed orders / house population")
houses = byHouse %>% group_by(house, mealType) %>% dplyr::summarise(demand = sum(numOrders)) 
ggplot(data=houses, aes(x = reorder(house, demand/house_pops[house]), y= demand/house_pops[house])) + geom_bar(aes(fill = mealType),stat="identity",position='dodge') + xlab("House") + ylab("Total observed orders / House population")

houses = byHouse %>% group_by(house) %>% dplyr::summarise(demand = sum(numOrders)) %>% filter(house != "annenberg")
ggplot(data=houses, aes(x = as.numeric(house_pops[house]), y= demand)) + geom_point(aes(color=house)) + geom_smooth(method = "lm", se = FALSE)

houses = byHouse %>% filter(mealType == "Lunch", house != "annenberg") %>% group_by(house) %>% dplyr::summarise(demand = sum(numOrders)) 
ggplot(data=houses, aes(x = as.numeric(house_pops[house]), y= demand)) + geom_point(aes(color=house), size=4) + geom_smooth(method = "lm", se = FALSE) + xlab("House Population") + ylab("Average Lunch Demand")

houses = byHouse %>% filter(mealType == "Dinner", house != "annenberg") %>% group_by(house) %>% dplyr::summarise(demand = sum(numOrders)) 
ggplot(data=houses, aes(x = as.numeric(house_pops[house]), y= demand)) + geom_point(aes(color=house), size=4) + geom_smooth(method = "lm", se = FALSE) + xlab("House Population") + ylab("Average Dinner Demand")

