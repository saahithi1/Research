setwd("~/Desktop/Research/Food_Diary")
library(dplyr)
library(purrr)
library(tidyverse)

# import data (removing first 2 rows for easy merging)
w.mturk.data <- read.csv("TGFoodDiarymTurkW.csv", header = TRUE, stringsAsFactors = FALSE)[-c(1,2),]
t.mturk.data <- read.csv("TGFoodDiarymTurkT.csv", header = TRUE, stringsAsFactors = FALSE)[-c(1,2),]
f.mturk.data <- read.csv("TGFoodDiarymTurkFri.csv", header = TRUE, stringsAsFactors = FALSE)[-c(1,2),]
uva.data <- read.csv("TGGoodDiaryUVA.csv", header = TRUE, stringsAsFactors = FALSE)[-c(1,2),]


###########################################################################
########################## merging all responses ##########################
###########################################################################

# new variable for which dataset participant came from
uva.data$original_dataset <- "TGGoodDiaryUVA"
w.mturk.data$original_dataset <- "TGFoodDiarymTurkW"
t.mturk.data$original_dataset <- "TGFoodDiarymTurkT"
f.mturk.data$original_dataset <- "TGFoodDiarymTurkFri"

# merge mturk data
mturk.merge <- bind_rows(list(w.mturk.data, t.mturk.data, f.mturk.data))

# new variable to differentiate between UVA vs MTurk participant
uva.data$participant_type <- "UVA"
mturk.merge$participant_type <- "MTurk"

# calculate number of days completed for mturk participants * 
mturk.table <- table(mturk.merge$Q47)
mturk.merge$number_of_days <- NA
for (i in 1:368){
  value <- mturk.merge$Q47[i]
  count <- as.numeric(mturk.table[names(mturk.table)==value])
  mturk.merge$number_of_days[i] <- count
}

# calculate number of days completed for UVA participants
uva.data$UVA_ID <- tolower(uva.data$UVA_ID)   # change values to all lowercase
uva.data$UVA_ID[49] <- "sm4ztg"               # one person added @virginia.edu so I manually edited it

uva.table <- table(uva.data$UVA_ID)
uva.data$number_of_days <- NA
for (i in 1:122){
  value <- uva.data$UVA_ID[i]
  count <- as.numeric(uva.table[names(uva.table)==value])
  uva.data$number_of_days[i] <- count
}

# merge mturk and uva data
merge.1 <- bind_rows(list(mturk.merge, uva.data))

# changing NA to blanks
merge.1[is.na(merge.1)] <- ""

# recoding variables
for (i in 1:490){
  for(j in 1:55){
    value = j
    if(value == "Much less than normal" || "Strongly disagree"){value = 1}
    if(value == "Moderately less than normal" || "Moderately disagree"){value = 2}
    if(value == "Slightly less than normal" || "Neither disagree or agree"){value = 3}
    if(value == "About the same" || "Moderately agree"){value = 4}
    if(value == "Slightly more than normal" || "Strongly agree"){value = 5}
    if(value == "Moderately more than normal"){value = 6}
    if(value == "Much more than normal"){value = 7}
    
    if(value == "Yes, I am planning on exercising"){value = 1}
    if(value == "No, I am not planning on exercise"){value = 0}
  }
}

# export .csv
write.csv(merge.1, file="merge1.csv")

############################################################################
#################### merging by matching participant ID ####################
############################################################################

# record rows with no worker ID (and test data)
w.noID = w.mturk.data[c(213, 214, 215, 216),]
w.test = w.mturk.data[1:3,]
t.test = t.mturk.data[1:3,]

# combine into one dataframe
all.w = bind_rows(w.noID, w.test)
all.noID = bind_rows(all.w, t.test)

# remove rows w/o ID from data frame (to be added back after merging)
w.mturk.data = w.mturk.data[-c(1, 2, 3, 213, 214, 215, 216),]
t.mturk.data = t.mturk.data[-c(1, 2, 3),]

# merge MTurk datasets
# referenced: https://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list
mturk.merge2 = list(w.mturk.data, t.mturk.data, f.mturk.data) %>% reduce(full_join, by = "Q47")

# new variable to differentiate between MTurk vs UVA participant
mturk.merge2$participant_type = "MTurk"
uva.data$participant_type = "UVA"

# new variable "day" for the number of days completed 
all.noID$number_of_days = 1

mturk.merge2$number_of_days = NA
for (i in 1:217){                             # for each row in combined MTurk dataset
  value = mturk.merge2$Q47[i]                 # store id as value
  count = 0                                   # initial count is 0
  if (value %in% w.mturk.data$Q47 == TRUE)    # check if ID is in Thursday dataset
  {count = count + 1}                         # if so, add 1 to the count
  if (value %in% t.mturk.data$Q47 == TRUE)    # check if ID is in Thursday dataset
  {count = count + 1}                         # if so, add 1 to the count
  if (value %in% f.mturk.data$Q47 == TRUE)    # check if ID is in Friday dataset
  {count = count + 1}                         # if so, add 1 to the count
  
  mturk.merge2$number_of_days[i] = count      # store final count for row
}

# calculate number of days completed for UVA participants (same as earlier)
uva.data$UVA_ID = tolower(uva.data$UVA_ID)                   
uva.data$UVA_ID[49] = "sm4ztg"                 

uva.table = table(uva.data$UVA_ID)
uva.data$number_of_days = NA
for (i in 1:122){                                 
  value = uva.data$UVA_ID[i]                       
  count = as.numeric(uva.table[names(uva.table)==value])  
  uva.data$number_of_days[i] = count                        
}

# add back rows with no ID
mturk.merge2 = bind_rows(mturk.merge2, all.noID)

# merge uva and MTurk data
merge2 = bind_rows(mturk.merge2, uva.data)

# export .csv
write.csv(merge2, file="Food_Diary_merge2.csv")

