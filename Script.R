rm(list=ls())#removes all variables stored previously
library(Hmisc) #import


Dataset <- read.csv("C:/Users/pitso/OneDrive/Desktop/Project_R/COVID19_line_list_data.csv")
describe(Dataset) #Hmisc command

#Cleaned up death column
Dataset$death_dummy <- as.integer(Dataset$death !=0) 
# Death rate
sum(Dataset$death_dummy) / nrow(Dataset)

#age
#Claim: people who die are older
dead = subset(Dataset, death_dummy ==1)
alive = subset(Dataset, death_dummy ==0)

#"na.rm = TRUE"(This will ignore where the age entry is unknown)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

#Checking if this is statistically significant
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)

#normally, if p-value < 0.05, we reject null hypothesis
#here, p-value ~ 0, so we reject the null hypothesis and 
#conclude that this is statistically significant


#gender
#Claim: Gender has no effect
men = subset(Dataset, gender == "male") #8.5%
women = subset(Dataset, gender == "female") #3.7%
mean(men$death_dummy, na.rm = TRUE)
mean(women$death_dummy, na.rm = TRUE)

#Checking if this is statistically significant
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)
#99% confidence: men have from 0.8% to 8.8% higher chance of dying
#p-value =0.002 < 0.05, so this is statistically significant
