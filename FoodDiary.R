setwd("~/Desktop/Research/Food_Diary")
library(dplyr)
library(purrr)
library(tidyverse)

# import data
food.diary.uva.data <- read.csv("TGGoodDiaryUVA.csv")
food.diary.mturk.w.data <- read.csv("TGFoodDiarymTurkW.csv")
food.diary.mturk.t.data <- read.csv("TGFoodDiarymTurkT.csv")
food.diary.mturk.f.data <- read.csv("TGFoodDiarymTurkFri.csv")

# save rows with no worker ID (and test data)
noID <- food.diary.mturk.w.data[c(215, 216, 217, 218),]
w.test <- food.diary.mturk.w.data[3:5,]
t.test <- food.diary.mturk.t.data[3:5,]

# combine into one dataframe
a <- rbind(noID, w.test)
all.noID <- rbind.fill(a, t.test)

# remove saved rows from data frame (they'll be added back after merging)
food.diary.mturk.w.data <- food.diary.mturk.w.data[-c(3, 4, 5, 215, 216, 217, 218),]
food.diary.mturk.t.data <- food.diary.mturk.t.data[-c(3, 4, 5),]

# merge mturk datasets
# from https://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list
all.mturk <- list(food.diary.mturk.w.data, food.diary.mturk.t.data, food.diary.mturk.f.data) %>% reduce(full_join, by = "Q47")

# new variable "day" for the number of days completed
all.noID$day <- 1                               # for participants w/o an ID, I set the variable equal to 1

all.mturk$day <- NA
for (i in 3:214){                               # for each row in Wednesday dataset
  value <- food.diary.mturk.w.data$Q47[i]       # store id as value
  count <- 1                                    # initial count is 1 because they participated on Wednesday    
  if (value %in% food.diary.mturk.t.data$Q47)   # check if ID is in Thursday dataset
  {count <- count + 1}                          # if so, add 1 to the count
  if (value %in% food.diary.mturk.f.data$Q47)   # check if ID is in Friday dataset
  {count <- count + 1}                          # if so, add 1 to the count
  
  all.mturk$day[i] <- count                     # store final count for row
}

# add back rows with no ID
all.mturk <- rbind.fill(all.mturk, all.noID)

# new variable for mTurk or UVA participant
all.mturk$participant <- "MTurk"
food.diary.uva.data$participant <- "UVA"

# merge uva and mturk data
all.data <- rbind.fill(all.mturk, food.diary.uva.data)

# export .csv
write.csv(all.data, file="merged.csv")
