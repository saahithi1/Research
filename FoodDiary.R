setwd("~/Desktop/Research/Food_Diary")
library(dplyr)
library(purrr)
library(tidyverse)

# import data
food.diary.uva.data <- read.csv("TGGoodDiaryUVA.csv", stringsAsFactors = FALSE)
food.diary.mturk.w.data <- read.csv("TGFoodDiarymTurkW.csv", stringsAsFactors = FALSE)
food.diary.mturk.t.data <- read.csv("TGFoodDiarymTurkT.csv", stringsAsFactors = FALSE)
food.diary.mturk.f.data <- read.csv("TGFoodDiarymTurkFri.csv", stringsAsFactors = FALSE)

# merge mturk datasets
# from https://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list
all.mturk <- list(food.diary.mturk.w.data, food.diary.mturk.t.data, food.diary.mturk.f.data) %>% reduce(full_join, by = "Q47")

# new variable "day" for the number of days completed
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

# new variable for mTurk or UVA participant
all.mturk$participant <- "MTurk"
food.diary.uva.data$participant <- "UVA"

# merge uva and mturk data
all.data <- bind_rows(all.mturk, food.diary.uva.data)

# export .csv
# << you can change the name of the variable by changing "merged">>
write.csv(all.data, file="merged.csv")
