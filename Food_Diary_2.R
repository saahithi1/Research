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
########################## merging all responses ##########################
###########################################################################

# edit column names to keep track of which dataset values came from
colnames(w.mturk.data) = paste("W", colnames(w.mturk.data), sep = "_")
colnames(w.mturk.data)[19] = "Q47"

colnames(t.mturk.data) = paste("T", colnames(t.mturk.data), sep = "_")
colnames(t.mturk.data)[19] = "Q47"

colnames(f.mturk.data) = paste("F", colnames(f.mturk.data), sep = "_")
colnames(f.mturk.data)[19] = "Q47"

colnames(uva.data) = paste("UVA", colnames(uva.data), sep = "_")
colnames(uva.data)[47] = "UVA_ID"

# merge MTurk data
mturk.merge = bind_rows(list(w.mturk.data, t.mturk.data, f.mturk.data))

# new variable to differentiate between MTurk vs UVA participant
mturk.merge$participant_type = "MTurk"
uva.data$participant_type = "UVA"

# calculate number of days completed for MTurk participants 
mturk.table = table(mturk.merge$Q47)
mturk.merge$number_of_days = NA
for (i in 1:368){                                             # for each row in mturk.merge dataset
  value = mturk.merge$Q47[i]                                  # value = worker ID
  count = as.numeric(mturk.table[names(mturk.table)==value])  # count = frequency of worker ID in combined dataset
  mturk.merge$number_of_days[i] = count                       # record count
}

# calculate number of days completed for UVA participants
uva.data$UVA_ID = tolower(uva.data$UVA_ID)                    # change values to all lowercase
uva.data$UVA_ID[49] = "sm4ztg"                                # one person added @virginia.edu so I manually edited it

uva.table = table(uva.data$UVA_ID)
uva.data$number_of_days = NA
for (i in 1:122){                                             # for each row in uva.data dataset
  value = uva.data$UVA_ID[i]                                  # value = UVA ID
  count = as.numeric(uva.table[names(uva.table)==value])      # count = frequency of UVA ID in dataset
  uva.data$number_of_days[i] = count                          # record count
}

# merge MTurk and uva data
merge1 = bind_rows(list(mturk.merge, uva.data))

# changing NA to blanks
#merge1[is.na(merge1)] = ""

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
# source https://stackoverflow.com/questions/50293671/r-merge-rows-in-group-while-replacing-nas

mturk.id.subset <- merge1[which(merge1$Q47 != "" & merge1$Q47 != "test"),]
mturk.id.subset <- sapply(mturk.id.subset, as.character)
mturk.id.subset[is.na(mturk.id.subset)] <- " "
mturk.id.subset <- as.data.frame(mturk.id.subset)

no.id.subset <- merge1[which(merge1$Q47 == "" & merge1$UVA_ID == "" | merge1$Q47 == "test"),]
no.id.subset <- sapply(no.id.subset, as.character)
no.id.subset[is.na(no.id.subset)] <- " "
no.id.subset <- as.data.frame(no.id.subset)

uva.id.subset <- merge1[which(merge1$UVA_ID != ""),]
uva.id.subset <- sapply(uva.id.subset, as.character)
uva.id.subset[is.na(uva.id.subset)] <- " "
uva.id.subset <- as.data.frame(uva.id.subset)

mturk.id.subset %>%
  group_by(Q47) %>%
  summarise_all(funs(trimws(paste(., collapse = '')))) -> mturk.merge1

uva.id.subset %>%
  group_by(UVA_ID) %>%
  summarise_all(funs(trimws(paste(., collapse = '')))) -> uva.merge1

merge1 <- rbind(mturk.merge1, uva.merge1, no.id.subset)

# character to numeric
merge1[,c(18,20:28, 37:46)] <- sapply(merge1[,c(18,20:28, 37:46)],as.numeric)

# export .csv
write.csv(merge1, file="Food_Diary_merge2.csv")
