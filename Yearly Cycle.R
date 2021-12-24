# Cyclical pattern over the year

#Presets: 
# Load Data
setwd("~/LMU/Fall 2021/BSAN 6050 - Customer Relationship Management Analytics/Group Project")
df <- read.csv("group.csv")
variable.names(df)
library(dplyr)
library(ggplot2)
# converting dates properly
a <- as.Date(df$Order.Date, format = "%d/%m/%Y")
b <- as.Date(df$Order.Date, format = "%d-%m-%Y")
a[is.na(a)] <- b[!is.na(b)]
df$Order.Date <- a
#creating a month column
df <- df %>% mutate(Order.Month = substr(Order.Date, 6,7))
#creating a year column
df <- df %>% mutate(Order.Year = substr(Order.Date, 1,4))

#summing sales by month and year
df2011 <- df %>% filter(Order.Year=='2011') %>% 
  group_by(Order.Month) %>% 
  summarise(sales.month = sum(Sales))
df2011$Order.Year = as.character('2011')
df2012 <- df %>% filter(Order.Year=='2012') %>% 
  group_by(Order.Month) %>% 
  summarise(sales.month = sum(Sales))
df2012$Order.Year = as.character('2012')
df2013 <- df %>% filter(Order.Year=='2013') %>% 
  group_by(Order.Month) %>% 
  summarise(sales.month = sum(Sales))
df2013$Order.Year = as.character('2013')
df2014 <- df %>% filter(Order.Year=='2014') %>% 
  group_by(Order.Month) %>% 
  summarise(sales.month = sum(Sales))
df2014$Order.Year = as.character('2014')
df.allyear <- rbind(df2011, df2012, df2013, df2014)

#graph
ggplot(df.allyear, aes(Order.Month, sales.month, group=Order.Year)) + 
  geom_line(aes(color=Order.Year)) +
  geom_point(aes(color=Order.Year)) + 
  labs(x = 'Month',
       y = 'Total Sales by Month',
       title = 'Total Sales by Month for Each Year') +
  theme_minimal()
