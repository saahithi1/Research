setwd("~/Desktop/Research/Food_Diary")

# import data
food.diary.uva.data <- read.csv("TGGoodDiaryUVA.csv")
food.diary.mturk.w.data <- read.csv("TGFoodDiarymTurkW.csv")
food.diary.mturk.t.data <- read.csv("TGFoodDiarymTurkT.csv")
food.diary.mturk.f.data <- read.csv("TGFoodDiarymTurkFri.csv")

# save rows with no worker ID/test
noID <- food.diary.mturk.w.data[c(215, 216, 217, 218),]
w.test <- food.diary.mturk.w.data[3:5,]
t.test <- food.diary.mturk.t.data[3:5,]

# remove saved rows from data frame (they'll be added back after merging)
food.diary.mturk.w.data <- food.diary.mturk.w.data[-c(3, 4, 5, 215, 216, 217, 218),]
food.diary.mturk.t.data <- food.diary.mturk.t.data[-c(3, 4, 5),]

# merge mturk datasets
w.t <- merge(food.diary.mturk.w.data, food.diary.mturk.t.data, by="Q47", all = TRUE)
w.t.f <- merge(w.t, food.diary.mturk.f.data, by="Q47", all = TRUE)

# export .csv
write.csv(w.t.f, file="merged.csv")
