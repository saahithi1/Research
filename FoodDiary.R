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
merge1[merge1 == "Much less than normal"] <- 1              # change more/less normal variables
merge1[merge1 == "Moderately less than normal"] <- 2
merge1[merge1 == "Slightly less than normal"] <- 3
merge1[merge1 == "About the same"] <- 4
merge1[merge1 == "Slightly more than normal"] <- 5
merge1[merge1 == "Moderately more than normal"] <- 6
merge1[merge1 == "Much more than normal"] <- 7

merge1[merge1 == "Strongly disagree"] <- 1                  # change agree/disagree variables
merge1[merge1 == "Moderately disagree"] <- 2
merge1[merge1 == "Neither disagree nor agree"] <- 3
merge1[merge1 == "Moderately agree"] <- 4
merge1[merge1 == "Strongly agree"] <- 5

merge1[merge1 == "No, I am not planning on exercise"] <- 0  # change yes/no variables
merge1[merge1 == "No"] <- 0
merge1[merge1 == "No, I do not plan to exercise"] <- 0

merge1[merge1 == "Yes, I am planning on exercising"] <- 1
merge1[merge1 == "Yes"] <- 1
merge1[merge1 == "Yes, I plan to exercise"] <- 1
merge1[merge1 == "Yes, I would like to take part in this study, and confirm that I AM A US RESIDENT, and I  and am 18 or older."] <- 1

merge1[merge1 == "I have already exercised"] <- 2
merge1[merge1 == "I have already exercised today"] <- 2

# character to numeric
cols = c(18,20:28, 37:46)
merge1[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))#

# export .csv
write.csv(merge.1, file="merge1.csv")
