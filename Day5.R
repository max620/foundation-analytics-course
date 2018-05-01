library(tidyverse) # for ggplot, purr etc.
library(caret)
# short for Classification and regression training (confusionmatrix, sample.split)
library(ROCR)
library(reshape2) # for meit()
library(car) # for vif values
library(corrplot) # for correlation plots
library(caTools) # for coLAUC

setwd( "C:/Users/Max/Documents/R")

day5 <- read.csv("subset_loan_default.csv")

library(magrittr)

missing = day5 %>%
  filter(!complete.cases(.))

nrow(missing)

nrow(missing)/nrow(data)

