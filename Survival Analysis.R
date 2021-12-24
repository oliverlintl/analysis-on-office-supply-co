setwd("~/LMU/Fall 2021/BSAN 6050 - Customer Relationship Management Analytics/Group Project")
library(ggplot2)
library(survival)
library(survminer)
library(dplyr)
library(geepack)
library(pseudo)

df <- read.csv('Group.csv')
summary(df$Order.Date)
a <- as.Date(df$Order.Date, format = "%d/%m/%Y")
b <- as.Date(df$Order.Date, format = "%d-%m-%Y")
a[is.na(a)] <- b[!is.na(b)]
df$Order.Date <- a
customer.df <- read.csv("customer_name_freq.csv")
customer.df$Followtime <- c(as.numeric(as.Date("2014-12-31") - as.Date(customer.df$First.Order.Date[1:nrow(customer.df)]), units='days'))

plot(customer.df$Days.Since.Last.Order)
ggplot(customer.df, aes(Days.Since.Last.Order))+
  geom_density()

customer.df$Churn[1:nrow(customer.df)] <- 
  ifelse(customer.df$Days.Since.Last.Order <= 100, 1, 0)
# K-M test  
customer.df$Survival <- Surv(customer.df$Followtime/30, customer.df$Churn)

fit1 <- survfit(Survival~1, data = customer.df)
ggsurvplot(fit1, data=customer.df)
fit2 <- survfit(Survival~Customer.Class, data = customer.df)
ggsurvplot(fit2, data=customer.df, 
           pval = T)
fit3 <- survfit(Survival~Profitability, data = customer.df)
ggsurvplot(fit3, data=customer.df, 
           pval = T)
fit4 <- survfit(Survival~Segment, data = customer.df)
ggsurvplot(fit4, data=customer.df, 
           pval = T)
fit5 <- survfit(Survival~Discount.Level, data = customer.df)
ggsurvplot(fit5, data=customer.df, 
           pval = T)

# log-rank test
survdiff(Survival~Customer.Class, data = customer.df)
# compare coupon use --> different survival time
print(fit2, 
      print.rmean=getOption("survfit.print.rmean"),
      rmean = 48)

#cox Regression
results <- coxph(Survival ~ Customer.Class + Sum.Profit + Segment + Count.True + Order.Freq, data = customer.df)
results
cox.zph(results) # cox.zph test for factors that has no impact on survival (pval small factor is removed)
results <- coxph(Survival ~ Sum.Profit + Segment, data = customer.df)
cox.zph(results)
results

#pseudo
customer.df$Pseudos <- pseudomean(customer.df$Followtime/30, customer.df$Churn, 48)
# new step:
# add id for each customer
n <- length(customer.df$Followtime/30)
customer.df <- data.frame(customer.df, id=1:n)
fit6 <- geese(Pseudos~Count.True,
              data = customer.df,
              id = id,
              jack = TRUE,
              family = gaussian,
              corstr = "independence",
              scale.fix = FALSE)
summary(fit6)

  fit7 <- geese(Pseudos~Order.Freq,
              data = customer.df,
              id = id,
              jack = TRUE,
              family = gaussian,
              corstr = "independence",
              scale.fix = FALSE)
summary(fit7)

fit8 <- geese(Pseudos~Average.Discount,
              data = customer.df,
              id = id,
              jack = TRUE,
              family = gaussian,
              corstr = "independence",
              scale.fix = FALSE)
summary(fit8)

fit9 <- geese(Pseudos~Average.Discount,
              data = customer.df,
              id = id,
              jack = TRUE,
              family = gaussian,
              corstr = "independence",
              scale.fix = FALSE)
summary(fit9)

#Dividing Customer Class and analyse separately 
classA.df <- read.csv('classA.csv')
classB.df <- read.csv('classB.csv')
classC.df <- read.csv('classC.csv')
classD.df <- read.csv('classD.csv')

### A
classA.df$Followtime <- c(as.numeric(as.Date("2014-12-31") - as.Date(classA.df$First.Order.Date[1:nrow(classA.df)]), units='days'))
ggplot(classA.df, aes(Days.Since.Last.Order))+
  geom_density()

classA.df$Churn[1:nrow(classA.df)] <- 
  ifelse(classA.df$Days.Since.Last.Order <= 100, 1, 0)

classA.df$Survival <- Surv(classA.df$Followtime/30, classA.df$Churn)

A.discount.fit <- survfit(Survival~Discount.Level, data = classA.df)
ggsurvplot(A.discount.fit, data=classA.df, 
           pval = T)
A.seg.fit <- survfit(Survival~Segment, data = classA.df)
ggsurvplot(A.seg.fit, data=classA.df, 
           pval = T)
#Pseudo
classA.df$Pseudos <- pseudomean(classA.df$Followtime/30, classA.df$Churn, 48)
n <- length(classA.df$Followtime/30)
classA.df <- data.frame(classA.df, id=1:n)
A.order.fit <- geese(Pseudos~Count.True,
              data = classA.df,
              id = id,
              jack = TRUE,
              family = gaussian,
              corstr = "independence",
              scale.fix = FALSE)
summary(A.order.fit) # not significant
A.freq.fit <- geese(Pseudos~Order.Freq,
                     data = classA.df,
                     id = id,
                     jack = TRUE,
                     family = gaussian,
                     corstr = "independence",
                     scale.fix = FALSE)
summary(A.freq.fit) #sig
A.dis.fit <- geese(Pseudos~Discount.Level,
                   data = classA.df,
                   id = id,
                   jack = TRUE,
                   family = gaussian,
                   corstr = "independence",
                   scale.fix = FALSE)
summary(A.dis.fit) # not sig

### B
classB.df$Followtime <- c(as.numeric(as.Date("2014-12-31") - as.Date(classB.df$First.Order.Date[1:nrow(classB.df)]), units='days'))
ggplot(classB.df, aes(Days.Since.Last.Order))+
  geom_density()

classB.df$Churn[1:nrow(classB.df)] <- 
  ifelse(classB.df$Days.Since.Last.Order <= 100, 1, 0)

classB.df$Survival <- Surv(classB.df$Followtime/30, classB.df$Churn)

B.discount.fit <- survfit(Survival~Discount.Level, data = classB.df)
ggsurvplot(B.discount.fit, data=classB.df, 
           pval = T)
B.seg.fit <- survfit(Survival~Segment, data = classB.df)
ggsurvplot(B.seg.fit, data=classB.df, 
           pval = T)

classB.df$Pseudos <- pseudomean(classB.df$Followtime/30, classB.df$Churn, 48)
n <- length(classB.df$Followtime/30)
classB.df <- data.frame(classB.df, id=1:n)
B.order.fit <- geese(Pseudos~Count.True,
                     data = classB.df,
                     id = id,
                     jack = TRUE,
                     family = gaussian,
                     corstr = "independence",
                     scale.fix = FALSE)
summary(B.order.fit) # not significant
B.freq.fit <- geese(Pseudos~Order.Freq,
                    data = classB.df,
                    id = id,
                    jack = TRUE,
                    family = gaussian,
                    corstr = "independence",
                    scale.fix = FALSE)
summary(B.freq.fit) #sig
B.dis.fit <- geese(Pseudos~Discount.Level,
                   data = classB.df,
                   id = id,
                   jack = TRUE,
                   family = gaussian,
                   corstr = "independence",
                   scale.fix = FALSE)
summary(B.dis.fit) # not sig

### C
classC.df$Followtime <- c(as.numeric(as.Date("2014-12-31") - as.Date(classC.df$First.Order.Date[1:nrow(classC.df)]), units='days'))
ggplot(classC.df, aes(Days.Since.Last.Order))+
  geom_density()

classC.df$Churn[1:nrow(classC.df)] <- 
  ifelse(classC.df$Days.Since.Last.Order <= 100, 1, 0)

classC.df$Survival <- Surv(classC.df$Followtime/30, classC.df$Churn)

C.discount.fit <- survfit(Survival~Discount.Level, data = classC.df)
ggsurvplot(C.discount.fit, data=classC.df, 
           pval = T)
C.seg.fit <- survfit(Survival~Segment, data = classC.df)
ggsurvplot(C.seg.fit, data=classC.df, 
           pval = T)

classC.df$Pseudos <- pseudomean(classC.df$Followtime/30, classC.df$Churn, 48)
n <- length(classC.df$Followtime/30)
classC.df <- data.frame(classC.df, id=1:n)
C.order.fit <- geese(Pseudos~Count.True,
                     data = classC.df,
                     id = id,
                     jack = TRUE,
                     family = gaussian,
                     corstr = "independence",
                     scale.fix = FALSE)
summary(C.order.fit) # not significant
C.freq.fit <- geese(Pseudos~Order.Freq,
                    data = classC.df,
                    id = id,
                    jack = TRUE,
                    family = gaussian,
                    corstr = "independence",
                    scale.fix = FALSE)
summary(C.freq.fit) #sig
C.dis.fit <- geese(Pseudos~Discount.Level,
                   data = classC.df,
                   id = id,
                   jack = TRUE,
                   family = gaussian,
                   corstr = "independence",
                   scale.fix = FALSE)
summary(C.dis.fit) # not sig

### D
classD.df$Followtime <- c(as.numeric(as.Date("2014-12-31") - as.Date(classD.df$First.Order.Date[1:nrow(classD.df)]), units='days'))
ggplot(classD.df, aes(Days.Since.Last.Order))+
  geom_density()

classD.df$Churn[1:nrow(classD.df)] <- 
  ifelse(classD.df$Days.Since.Last.Order <= 100, 1, 0)

classD.df$Survival <- Surv(classD.df$Followtime/30, classD.df$Churn)

D.discount.fit <- survfit(Survival~Discount.Level, data = classD.df)
ggsurvplot(D.discount.fit, data=classD.df, 
           pval = T)
D.seg.fit <- survfit(Survival~Segment, data = classD.df)
ggsurvplot(D.seg.fit, data=classD.df, 
           pval = T)

classD.df$Pseudos <- pseudomean(classD.df$Followtime/30, classD.df$Churn, 48)
n <- length(classD.df$Followtime/30)
classD.df <- data.frame(classD.df, id=1:n)
D.order.fit <- geese(Pseudos~Count.True,
                     data = classD.df,
                     id = id,
                     jack = TRUE,
                     family = gaussian,
                     corstr = "independence",
                     scale.fix = FALSE)
summary(D.order.fit) #significant
D.freq.fit <- geese(Pseudos~Order.Freq,
                    data = classD.df,
                    id = id,
                    jack = TRUE,
                    family = gaussian,
                    corstr = "independence",
                    scale.fix = FALSE)
summary(D.freq.fit) # NOT sig
D.dis.fit <- geese(Pseudos~Discount,
                   data = classD.df,
                   id = id,
                   jack = TRUE,
                   family = gaussian,
                   corstr = "independence",
                   scale.fix = FALSE)
summary(D.dis.fit) # not sig