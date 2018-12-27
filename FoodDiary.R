setwd("~/Desktop/Research/Food_Diary")

# import data
food.diary.uva.data <- read.csv("TGGoodDiaryUVA.csv")
food.diary.mturk.w.data <- read.csv("TGFoodDiarymTurkW.csv")
food.diary.mturk.t.data <- read.csv("TGFoodDiarymTurkT.csv")
food.diary.mturk.f.data <- read.csv("TGFoodDiarymTurkFri.csv")

# new variable for which dataset participant came from
food.diary.uva.data$dataset <- "TGGoodDiaryUVA"
food.diary.mturk.w.data <- "TGFoodDiarymTurkW"
food.diary.mturk.t.data <- "TGFoodDiarymTurkT"
food.diary.mturk.f.data <- "TGFoodDiarymTurkFri"

# save rows with no worker ID/test
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
all.mturk <- join_all(list(food.diary.mturk.w.data, food.diary.mturk.t.data, food.diary.mturk.f.data),
                  by="Q47", type = "full")

# new variable for mTurk or UVA participant
all.mturk$participant <- "MTurk"
food.diary.uva.data$participant <- "UVA"

# new variable for day
all.mturk$day <- NA
for (i in 1:230){
  count <- 0
  if (is.null(all.mturk$WEat[i]) == FALSE)
  {count <- count + 1}
  if (is.null(all.mturk$TEat[i]) == FALSE)
  {count <- count + 1})
  if (is.null(all.mturk$FEat[i]) == FALSE)
  {count <- count + 1}
  
  all.mturk$day[i] <- count
}

# export .csv
write.csv(all.mturk, file="merged.csv")


###########
# Testing # 
###########

is.null(all.mturk$WEat[2])
