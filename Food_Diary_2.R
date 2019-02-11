setwd("~/Desktop/Research/Food_Diary") 
library(dplyr)
library(data.table)
library(tidyr)

# import data (removing first 2 rows for easy merging)
w.mturk.data = read.csv("TGFoodDiarymTurkW.csv", header = TRUE, 
                        stringsAsFactors = FALSE, strip.white = TRUE)[-c(1,2),]
t.mturk.data = read.csv("TGFoodDiarymTurkT.csv", header = TRUE, 
                        stringsAsFactors = FALSE, strip.white = TRUE)[-c(1,2),]
f.mturk.data = read.csv("TGFoodDiarymTurkFri.csv", header = TRUE, 
                        stringsAsFactors = FALSE, strip.white = TRUE)[-c(1,2),]
uva.data = read.csv("TGGoodDiaryUVA.csv", header = TRUE, 
                        stringsAsFactors = FALSE, strip.white = TRUE)[-c(1,2),]

###########################################################################
############################## preprocessing ##############################
###########################################################################

# clean data
uva.data$UVA_ID = tolower(uva.data$UVA_ID)                    # change values to all lowercase
uva.data$UVA_ID[49] = "sm4ztg"                                # one person added @virginia.edu so I manually edited it

# separate uva.data by days
no.id.uva.data <- uva.data[which(uva.data$UVA_ID == ""),]
w.uva.data <- uva.data[which(uva.data$UVA_ID != "" & uva.data$date == "Wednesday, November 21"),]
t.uva.data <- uva.data[which(uva.data$UVA_ID != "" & uva.data$date == "Thursday, November 22"),]
f.uva.data <- uva.data[which(uva.data$UVA_ID != "" & uva.data$date == "Friday, November 23"),]

# check for duplicate entries
w.mturk.data[which(duplicated(w.mturk.data[,"Q47"])),"Q47"]
# duplicates: "test" "AEEYCRN6WP4JV" "A9TENHCM24SAZ" "A7HJXH4A7V78Q" ""

t.mturk.data[which(duplicated(t.mturk.data[,"Q47"])),"Q47"]
# duplicates: "test"

f.mturk.data[which(duplicated(f.mturk.data[,"Q47"])),"Q47"]
# duplicates: "A1H55AUY7JFDHH"

w.uva.data[which(duplicated(w.uva.data[,"UVA_ID"])),"UVA_ID"]
# duplicates: "mw2ke"

t.uva.data[which(duplicated(t.uva.data[,"UVA_ID"])),"UVA_ID"]
# duplicates: "mw2ke" "hh4db"

f.uva.data[which(duplicated(f.uva.data[,"UVA_ID"])),"UVA_ID"]
# duplicates: "rbs8gs" "ael2ve"

# edit column names to keep track of which dataset values came from
colnames(w.mturk.data) = paste("W", colnames(w.mturk.data), sep = "_")
colnames(w.mturk.data)[19] = "Q47"

colnames(t.mturk.data) = paste("T", colnames(t.mturk.data), sep = "_")
colnames(t.mturk.data)[19] = "Q47"

colnames(f.mturk.data) = paste("F", colnames(f.mturk.data), sep = "_")
colnames(f.mturk.data)[19] = "Q47"

colnames(w.uva.data) = paste("W_UVA", colnames(w.uva.data), sep = "_")
colnames(w.uva.data)[47] = "UVA_ID"

colnames(t.uva.data) = paste("W_UVA", colnames(t.uva.data), sep = "_")
colnames(t.uva.data)[47] = "UVA_ID"

colnames(f.uva.data) = paste("W_UVA", colnames(f.uva.data), sep = "_")
colnames(f.uva.data)[47] = "UVA_ID"

###########################################################################
########################## merging all responses ##########################
###########################################################################

# merge MTurk data
mturk.merge = bind_rows(list(w.mturk.data, t.mturk.data, f.mturk.data)) # 368 obs. of 85 var.

# merge MTurk and uva data
merge1 = bind_rows(list(mturk.merge, uva.data)) # 490 obs. of 134 var.

# changing NA to blanks
merge1[is.na(merge1)] = ""

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

# merge common ID
# source https://stackoverflow.com/questions/19592706/setting-na-to-blank

mturk.id.subset <- merge1[which(merge1$Q47 != "" & merge1$Q47 != "test"),]
mturk.id.subset <- sapply(mturk.id.subset, as.character)
mturk.id.subset[is.na(mturk.id.subset)] <- ""
mturk.id.subset <- as.data.frame(mturk.id.subset) # 358 obs. of 134 var.

uva.id.subset <- merge1[which(merge1$UVA_ID != ""),]
uva.id.subset <- sapply(uva.id.subset, as.character)
uva.id.subset[is.na(uva.id.subset)] <- ""
uva.id.subset <- as.data.frame(uva.id.subset) # 111 obs. of 134 var.

no.id.subset <- merge1[which(merge1$Q47 == "" & merge1$UVA_ID == "" | merge1$Q47 == "test"),]
no.id.subset <- sapply(no.id.subset, as.character)
no.id.subset[is.na(no.id.subset)] <- ""
no.id.subset <- as.data.frame(no.id.subset) # 21 obs. of 134 var.
no.id.subset$participant_type = NA
no.id.subset$number_of_days = NA

# subset mturk rows and merge by Q47/UVA_ID
# source https://stackoverflow.com/questions/41068734/r-collapse-multiple-rows-into-1-row-same-columns
mturk.merge1 <- data.table(mturk.id.subset)
mturk.merge1 <- mturk.merge1[, lapply(.SD, paste0, collapse=""), by=Q47] # 213 obs. of 134 var.

uva.merge1 <- data.table(uva.id.subset)
uva.merge1 <- uva.merge1[, lapply(.SD, paste0, collapse=""), by=UVA_ID] # 38 obs. of 134 var.

# new variable to differentiate between MTurk vs UVA participant
mturk.merge1$participant_type = "MTurk"
uva.merge1$participant_type = "UVA"

# calculate number of days completed for MTurk participants 
mturk.merge1$number_of_days = NA
for (i in 1:213){                             # for each row in combined MTurk dataset
  value = mturk.merge1$Q47[i]                 # store id as value
  count = 0                                   # initial count is 0
  if (value %in% w.mturk.data$Q47 == TRUE)    # check if ID is in Thursday dataset
  {count = count + 1}                         # if so, add 1 to the count
  if (value %in% t.mturk.data$Q47 == TRUE)    # check if ID is in Thursday dataset
  {count = count + 1}                         # if so, add 1 to the count
  if (value %in% f.mturk.data$Q47 == TRUE)    # check if ID is in Friday dataset
  {count = count + 1}                         # if so, add 1 to the count
  
  mturk.merge1$number_of_days[i] = count      # store final count for row
}

# calculate number of days completed for UVA participants
uva.table = table(uva.data$UVA_ID)
uva.merge1$number_of_days = NA
for (i in 1:38){                                             # for each row in uva.data dataset
  value = uva.merge1$UVA_ID[i]                                  # value = UVA ID
  count = as.numeric(uva.table[names(uva.table)==value])      # count = frequency of UVA ID in dataset
  uva.merge1$number_of_days[i] = count                          # record count
}

# merge mturk and uva subsets
merge1 <- rbind(mturk.merge1, uva.merge1, no.id.subset) # 272 obs. of 136 var.
merge1 <- as.data.frame(merge1)

# character to numeric
merge1[,c(18,20:28, 37:46)] <- sapply(merge1[,c(18,20:28, 37:46)],as.numeric)

# export .csv
write.csv(merge1, file="Food_Diary_merge2.csv")
