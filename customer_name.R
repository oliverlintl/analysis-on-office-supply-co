setwd("~/LMU/Fall 2021/BSAN 6050 - Customer Relationship Management Analytics/Group Project")
df <- read.csv("group.csv")
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(geepack)
library(pseudo)

a <- as.Date(df$Order.Date, format = "%d/%m/%Y")
b <- as.Date(df$Order.Date, format = "%d-%m-%Y")
a[is.na(a)] <- b[!is.na(b)]
df$Order.Date <- a

df4 <- df[order(df$Customer.Name, df$Order.Date),]
df4$Order.Gap <- c(NA,
                   as.numeric(df4$Order.Date[2:nrow(df4)] - 
                                df4$Order.Date[1:(nrow(df4) - 1)], units='days'))
df4$Order.Gap.Adj[1] <- 0
df4$Order.Gap.Adj[2:nrow(df4)] <-
  ifelse(df4$Order.Gap[2:nrow(df4)] < 0, 0, df4$Order.Gap[2:nrow(df4)])
#define new order
df4$New.Order <- NA
df4$New.Order[1] <- TRUE
df4$New.Order[2:nrow(df4)] <- 
  ifelse(df4$Customer.Name[2:nrow(df4)] != 
           df4$Customer.Name[1:(nrow(df4) - 1)], TRUE, 
         df4$Order.Gap[2:nrow(df4)] != 0)
summary(df4$New.Order)

df4.2 <- df4 %>% group_by(Customer.Name) %>% summarise(Sum.Order.Gap.Adj = sum(Order.Gap.Adj))
df4.3 <- df4 %>% group_by(Customer.Name) %>% count(New.Order)
df4.4 <- df4.3 %>% filter(New.Order == 'TRUE')
df4.2$Count.True <- df4.4$n
df4.2$Order.Freq <- round(df4.2$Sum.Order.Gap.Adj/df4.2$Count.True,0)
customer.df = df4.2

summary(customer.df$Count.True)
summary(customer.df$Order.Freq)

customer.df <- customer.df %>%  mutate(Customer.Class = cut(Order.Freq, breaks=c(26,38,43,48,90), 
                                                            labels=c("A", "B", "C", "D")))
summary(customer.df$Customer.Class)
df5.1 <- df %>% group_by(Customer.Name) %>% summarise(Sum.Sale = sum(Sales))
df5.2 <- df %>% group_by(Customer.Name) %>% summarise(Sum.Profit = sum(Profit))
customer.df$Sum.Sale <- round(df5.1$Sum.Sale, 2)
customer.df$Sum.Profit <- round(df5.2$Sum.Profit, 2)
customer.df$Profit.Margin <- customer.df$Sum.Profit/customer.df$Sum.Sale
customer.df$Profitability[1:nrow(customer.df)] <- 
  ifelse(customer.df$Profit.Margin > 0, 'Profitable', 'Unprofitable')

df7 <- df %>% group_by(Customer.Name) %>% summarise(Last.Order.Date = max(Order.Date))
customer.df$Last.Order.Date <- df7$Last.Order.Date
customer.df$Days.Since.Last.Order <- c(as.numeric(as.Date("2014-12-31") - as.Date(customer.df$Last.Order.Date[1:nrow(customer.df)]), units='days'))


df9 <- df %>% group_by(Customer.Name) %>% summarise(Average.Discount = mean(Discount))
summary(df9$Average.Discount)
df9 <- df9 %>%  mutate(Discount.Level = cut(Average.Discount, breaks=c(0.04,0.13752,0.3), 
                                                            labels=c("Low", "High")))
summary(df9$Discount.Level)
customer.df$Average.Discount <- df9$Average.Discount
customer.df$Discount.Level <- df9$Discount.Level
df$Discount.Amount <- df$Discount*df$Sales/(1-df$Discount)
df10 <- df %>% group_by(Customer.Name) %>% summarise(Sum.Discount = sum(Discount.Amount))
df11 <- df %>% group_by(Customer.Name) %>% summarise(Average.Discount.Amount = mean(Discount.Amount))
customer.df$Discount.Amount <- df10$Sum.Discount
customer.df$Average.Discount.Amount <- df11$Average.Discount.Amount

df8 <- df[, c('Customer.Name', 'Segment')]
df8 <- df8 %>% distinct(Customer.Name, .keep_all = T)
customer.df <- customer.df %>% left_join(df8, by='Customer.Name')



write.csv(customer.df, "~/LMU/Fall 2021/BSAN 6050 - Customer Relationship Management Analytics/Group Project/customer_name_freq.csv", row.names = FALSE)

