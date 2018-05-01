install.packages('pacman')
library(pacman)
pacman::p_load(tidyverse, reshape2, readxl, jsonlite, corrplot,XLConnect,magrittr) 

getwd()
setwd("C:/Users/Max/Documents/R/titanic")

data("Titanic")

#read.csv help set delimiter but read.table does not, read.table need to speicify on your own
data = read.csv('titanic3.csv')
data2 = data

str(data)
str(data2)
names(data)
names(data2)

file = 'titanic3.txt' #this is file
data_tsv = read.csv(file, sep = '\t') #need to read to run file

file_2 = 'titanic3.xlsx'
workbook = loadWorkbook(file_2)
data_x1 = readWorksheet(workbook, sheet = 1, header = TRUE) 

aVector = c(1:10)
aMtrix  = matrix(aVector, nrow = 2) 
aMtrix
aMtrix  = matrix(aVector, nrow = 2, ncol = 2)

#apply is for loop, 1 - row wise, 2 - column wise for function_to_apply, 
#args required for that function like mean
apply(aMtrix, 1, mean)
sapply(aMtrix, mean) #return results as row
lapply(aMtrix, mean) #return results as col

co2 <- CO2
View(co2)
apply(as.matrix(data.frame(lapply(co2$conc,as.numeric))),2,max,na.rm=FALSE)
apply(co2[4:5],2,mean) #col 4 and 5 in dataset

evenOdd = function(x)
  {   
  if((x %% 2) == 0)
    {        
    return('Even number')      
    }      
  else 
    {        
    return('Odd Number')      
    }
  } 
aList = seq(1, 4) 
class(alist)
typeof(alist)

#lapply  
#A list is a generic vector 
#containing other objects
lapply(aList, evenOdd)

test = list(beaver1)
lapply(test,"[", 3)

#tapplt applt a function over ragged array - row of different size, tapply does group by
tapply(beaver1[,'temp'], beaver1[,'activ'], mean)

#which() subset()
alist = seq(1, 10)
alist = c(alist, NA)
alist%%2 #???
alist%%2 == 0
which(alist%%2 == 0)
subset(alist, alist%%2 == 0)

f1 = read.csv("f1.csv")
f1 = read.csv("f1.csv", check.names = F) #to remove 'X' in column name as R read column as char else default 'X'. remove 'X' with check.names
head(f1, 5)

#tidyr tidyverse - gather() and spread() - group common values in column - tide data
#gather() is like transpose
gather(f1, 'year', 'count', 2:ncol(f1)) #first row getting replicated, r has no 0 index, start at index 1
gather(f1, 'year', 'count', 2:ncol(f1), convert = T) #convert char to int

f1_long = f1 %>%
  gather(f1, 'year', 'count', 2:ncol(f1), convert = T)

options(dplyr.width = Inf) 

#facet_weight - ggplot seperation  
f1_long %>%   
  group_by(Variables) %>%   
  filter(str_detect(Variables, 'Revenue'))%>%   
  ggplot(aes(x = year, y = count)) + geom_line() + facet_wrap(~ Variables)

#convert int to categorical factor sFactors
cols_to_change = c('pclass','survived')
sapply(data[cols_to_change],class)
data[cols_to_change] = lapply(data[cols_to_change], factor)
sapply(data[cols_to_change], class)

data%>%
  group_by(embarked)%>%
  summarise_all(mean)

#more than 40% missing, lead to bias results so dont use
prop_na = data %>%
  + map(~ mean(is.na(.)))
prop_na[prop_na > 0]
#body has 90% missing

data %>%
  count(pclass, sex, age)

tab = table(data$survived, data$pclass)
addmargins(tab) #to know sum, just add margin

prop.table(table(data$sex, data$survived))

#aggregate func
func = function(x) {     
  sum(x)/length(x)   
  } 
aggregate(as.numeric(survived) ~ pclass + sex, data=data2, FUN = func, na.action = na.omit)

func = function(x) {     
  sum(x)/length(x)   
  } 
aggregate(weight ~ Diet + Time, dataC=data3, FUN = func, na.action = na.omit)

data %>%   top_n(n=-2, wt=age)

t = data2
ggplot(t, aes(x=survived)) + geom_bar()

t %>%   
  group_by(survived) %>%   
  summarise(count_level = n(), percentage = n()/nrow(t))%>%   
  ggplot(aes(x = as.factor(survived), y = count_level,fill=as.factor(survived) )) + geom_bar(stat='identity') + geom_text(aes(label=round(percentage,2)),vjust = 2)

# t$AgeBin[t$age < && t$age> ] = 'Children'

ageHistogram = ggplot(t, aes(x=age)) + geom_histogram() + facet_wrap(~sex)

t%>%   
  ggplot() + geom_jitter(aes(embarked, age, colour = factor(survived)))

#density plot - seperation between dependent variable better else do log transformation
#melt is like gather

#count unique values
sapply(t, function(x) length(unique(x)))

t.count()

