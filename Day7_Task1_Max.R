#links:
# https://www.kaggle.com/c/titanic/data 
# https://www.kaggle.com/poojagulati/exploring-survival-on-the-titanic 

library(pacman)
pacman::p_load(tidyverse, reshape2, readxl, jsonlite, corrplot,XLConnect,magrittr) 

getwd()
setwd("C:/Users/Max/Documents/R/titanic")

data = read.csv('titanic3.csv')

t = data

#count unique values
count_t = sapply(t, function(x) length(unique(x)))

#check duplicate
dup_t = anyDuplicated(t, incomparables = FALSE)
summary(dup_t)

#removing missing can take away required info like survived 1 and 0
d.miss <- melt(apply(data[, -2], 2, function(x) sum(is.na(x) | x=="")))
cbind(row.names(d.miss)[d.miss$value>0], d.miss[d.miss$value>0,])

missing = data %>%
  filter(!complete.cases(.))

non_missing = data %>%
  filter(complete.cases(.))

#survived rate
survivedBar = ggplot(data, aes(x=survived)) + geom_bar()
survivedLabels = labs(x= "survived", y= "Number of Passengers", title = "Survival Rate", subtitle = "On the Titanic")
survivedBar+survivedLabels

#survived barplot
t %>%
  group_by(survived) %>%
  summarise(count_level = n(), 
            percentage = n()/nrow(t)) %>%
  ggplot(aes(x=as.factor(survived), y=count_level,fill=as.factor(survived))) +
  geom_bar(stat='identity') +
  geom_text(aes(label=round(percentage,2)),vjust=2)

#gender of passenger on survival
t %>%
  ggplot() +
  geom_jitter(aes(sex, age, colour = factor(survived))) +
  facet_grid(.~survived)

#embarked of passenger on survival, have 2 missing values
t %>%
  ggplot() +
  geom_jitter(aes(embarked, age, colour = factor(survived))) +
  facet_grid(.~survived)

t %>%
  ggplot() +
  geom_jitter(aes(sex, embarked, colour = factor(survived))) +
  facet_grid(.~survived)

#missing data for 2 female had fare of 80, find median of 80, so embarked at C
subset(t, t$embarked=="")
ggplot(t,aes(x=embarked,y=fare,fill=factor(pclass)))+geom_boxplot()+geom_hline(yintercept = 80)
t$embarked[c(169,285)] = "C"
ggplot(t,aes(x=embarked,y=fare,fill=factor(pclass)))+geom_boxplot()

#cabin of passenger on survival, assuming first letter are cabin class
t$pclass = as.factor(t$pclass)
t$cl = substring(t$cabin,1,1)
unique(t$cl)
summary(t[!substring(t$cabin,1,1) == "",]$embarked)
summary(t[!substring(t$cabin,1,1) == "",]$pclass)
ggplot(t, aes(x=cl, y=fare, fill=factor(survived))) + geom_boxplot() #survived mostly from b class
ggplot(t, aes(x=cl, y=fare, fill=factor(survived))) + geom_bar(stat='identity')
#has high amount of missing value, likely passenger choose or switch cabin while on board, unable to use cabin to model against survival 
#unless impute?
  