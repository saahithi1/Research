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
all.mturk <- join_all(list(food.diary.mturk.w.data, food.diary.mturk.t.data, food.diary.mturk.f.data),
                  by="Q47", type = "full")
all.mturk <- rbind()

# export .csv
write.csv(all.mturk, file="merged.csv")


###########
# Testing # 
###########
