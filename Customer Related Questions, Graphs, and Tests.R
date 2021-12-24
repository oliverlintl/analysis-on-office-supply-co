# Customer Deepdive

#Presets: 
# Load Data
setwd("~/LMU/Fall 2021/BSAN 6050 - Customer Relationship Management Analytics/Group Project")
df <- read.csv("group.csv")
customer.df <- read.csv('customer_name_freq.csv')
variable.names(df)
library(dplyr)
library(ggplot2)

###### Histogram of Total Order By Customer
summary(customer.df$Count.True)
ggplot(customer.df, aes(x=Count.True)) +
  geom_histogram(aes(y=..density..), color='black', fill='darkseagreen') +
  geom_density(alpha=0.4, fill='lightblue')  +
  labs(x = 'Total Number of Orders',
       y = 'Relative Frequency',
       title = 'Histogram of Total Order by Customer') + 
  theme_minimal()
ggplot(customer.df, aes(x=Average.Sale)) +
  geom_histogram(aes(y=..density..), color='black', fill='darkseagreen') + 
  geom_density(alpha=0.4, fill='blue')  +
  labs(x = 'Average Sale',
       y = 'Relative Frequency',
       title = 'Histogram of Average Sale by Customer') + 
  theme_minimal()

##### Graphing Profitability by Customer Class
table5 <- table(customer.df$Customer.Class, customer.df$Profitability)
table5
profitability <- c(rep('Profitable', 4), rep('Unprofitable', 4))
class <- rep(c('A', 'B', 'C', 'D'), 2)
dat2 <- data.frame(profitability, class)
dat2
dat2$counts <- c(table5[,1], table5[,2])
dat2
ggplot(dat2, aes(profitability, counts)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'aquamarine2') + 
  labs(x = 'Profitability', 
       y = 'Counts',
       title = 'Profitability by Customer Class') + 
  geom_text(aes(label = counts), vjust=1) +
  facet_wrap(~class) +
  theme_minimal()
###### ANOVA Test for customer class and profit margin 
library(multcomp)
model <- aov(Profit.Margin ~ -1 + Customer.Class, data = customer.df)
summary(model)
glht(model)
plot(glht(model))

##### Avg # of order by segment 
order.seg <- customer.df %>% group_by(Segment) %>% 
  summarise(Avg.Order = round(mean(Count.True)))
ggplot(order.seg, aes(Segment, Avg.Order)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'gold',
           alpha = 0.6) + 
  labs(x = 'Segment', 
       y = 'Average Number of Orders',
       title = 'Average Number of Orders by Segment') + 
  theme_pubclean()


##### Avg Sales by segment 
avg.sales.seg <- customer.df %>% group_by(Segment) %>% 
  summarise(Avg.Sales = round(mean(Sum.Sale), 2))
ggplot(avg.sales.seg, aes(Segment, Avg.Sales)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'turquoise3',
           alpha=0.6) + 
  labs(x = 'Segment', 
       y = 'Average Sales',
       title = 'Average Sales by Segment') + 
  theme_pubclean()

##### Graphing sum profit and average profit by segment
sum.prof.seg <- customer.df %>% group_by(Segment) %>% 
  summarise(Sum.Profit = sum(Sum.Profit))
avg.prof.seg <- customer.df %>% group_by(Segment) %>% 
  summarise(Avg.Prof = round(mean(Sum.Profit),2))
ggplot(sum.prof.seg, aes(Segment, Sum.Profit)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'green') + 
  labs(x = 'Segment', 
       y = 'Sum of Profit',
       title = 'Sum of Profit by Segment') + 
  theme_minimal()

ggplot(avg.prof.seg, aes(Segment, Avg.Prof)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'lightslateblue',
           alpha = 0.6) + 
  labs(x = 'Segment', 
       y = 'Average Profit',
       title = 'Average Profit by Segment') + 
  theme_pubclean()
##### Average discount by segment
avg.dis.seg <- customer.df %>% group_by(Segment) %>% 
  summarise(Avg.Dis = round(mean(Sum.Discount),2))
ggplot(avg.dis.seg, aes(Segment, Avg.Dis)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'darkseagreen') + 
  labs(x = 'Segment', 
       y = 'Average Discount Amount',
       title = 'Average Discount by Segment') + 
  theme_minimal()
##### Sum Discount by segment
sum.dis.seg <- customer.df %>% group_by(Segment) %>% 
  summarise(Sum.Dis = round(sum(Sum.Discount),2))
ggplot(sum.dis.seg, aes(Segment, Sum.Dis)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'darkseagreen') + 
  labs(x = 'Segment', 
       y = 'Total Discount Amount',
       title = 'Total Discount by Segment') + 
  theme_minimal()


##### Graphing Discount Level by Customer Class
table6 <- table(customer.df$Customer.Class, customer.df$Discount.Level)
table6
discount.level <- c(rep('High', 4), rep('Low', 4))
class <- rep(c('A', 'B', 'C', 'D'), 2)
dat3 <- data.frame(discount.level, class)
dat3
dat3$counts2 <- c(table6[,1], table6[,2])
dat3
ggplot(dat3, aes(discount.level, counts2)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'gold1',
           alpha=0.6) + 
  labs(x = 'Discount Level', 
       y = 'Counts',
       title = 'Discount Level by Customer Class') + 
  facet_wrap(~class) +
  theme_minimal()

##### Average discount of different class
average.discount.df <- customer.df %>% group_by(Customer.Class) %>% 
  summarise(Average.Discount.Amount = round(mean(Average.Discount.Amount),2))
ggplot(average.discount.df, aes(Customer.Class, Average.Discount.Amount)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'gold1',
           alpha=0.6) + 
  labs(x = 'Customer Class', 
       y = 'Average Discount Amount',
       title = 'Average Discount Amount by Customer Class') + 
  theme_pubclean()
##### Sum discount of different class
sum.discount.df <- customer.df %>% group_by(Customer.Class) %>% 
  summarise(Total.Discount.Amount = round(sum(Sum.Discount),2))
ggplot(sum.discount.df, aes(Customer.Class, Total.Discount.Amount)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'darkseagreen') + 
  labs(x = 'Customer Class', 
       y = 'Total Discount Amount',
       title = 'Total Discount Amount by Customer Class') + 
  theme_minimal()

##### Avg Sales by class 
avg.sales.class <- customer.df %>% group_by(Customer.Class) %>% 
  summarise(Avg.Sales = round(mean(Sum.Sale), 2))
ggplot(avg.sales.class, aes(Customer.Class, Avg.Sales)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'forestgreen') + 
  labs(x = 'Class', 
       y = 'Average Sales',
       title = 'Average Sales by Customer Class') + 
  theme_minimal()

##### Sum Sales of different customer class
sum.sales.df <- customer.df %>% group_by(Customer.Class) %>% 
  summarise(Sum.Sales = round(sum(Sum.Sale),2))
average.sales.df <- customer.df %>% group_by(Customer.Class) %>% 
  summarise(Average.Sales = round(mean(Sum.Sale),2))
ggplot(sum.sales.df, aes(Customer.Class, Sum.Sales)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'darkseagreen') + 
  labs(x = 'Customer Class', 
       y = 'Total Sales',
       title = 'Total Sales by Customer Class') + 
  theme_minimal()
##### Average Sales of different customer class
ggplot(average.sales.df, aes(Customer.Class, Average.Sales)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'turquoise3',
           alpha = 0.6) + 
  labs(x = 'Customer Class', 
       y = 'Average Sales',
       title = 'Average Sales by Customer Class') + 
  theme_pubclean()

##### Sum Profit of different customer class
sum.profit.df <- customer.df %>% group_by(Customer.Class) %>% 
  summarise(Sum.Profit = round(sum(Sum.Profit),2))
average.profit.df <- customer.df %>% group_by(Customer.Class) %>% 
  summarise(Average.Profit = round(mean(Sum.Profit),2))
ggplot(sum.profit.df, aes(Customer.Class, Sum.Profit)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'royalblue1') + 
  labs(x = 'Customer Class', 
       y = 'Sum of Profit',
       title = 'Sum of Profit by Customer Class') + 
  theme_minimal()
##### Average Profit of different customer class
ggplot(average.profit.df, aes(Customer.Class, Average.Profit)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'lightslateblue',
           alpha = 0.6) + 
  labs(x = 'Customer Class', 
       y = 'Average Profit',
       title = 'Average Profit by Customer Class') + 
  theme_pubclean()

##### Number of customer by segment
cus.seg <- customer.df %>% group_by(Segment) %>% 
  summarise(cus.count = length(Customer.Name))
ggplot(cus.seg, aes(Segment, cus.count)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = 'darkseagreen') + 
  labs(x = 'Segment', 
       y = 'Number of Customers',
       title = 'Number of Customer by Segment') + 
  theme_minimal()

