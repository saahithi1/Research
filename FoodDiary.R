library(haven)
getwd()
setwd("~/Desktop/Research/Food_Diary")

food.diary.uva.data <- read_sav("TGFoodDiaryUVA.sav")
food.diary.mturk.w.data <- read_sav("ThanksgivingFoodDiarymTurkWed.sav")
food.diary.mturk.t.data <- read_sav("ThanksgivingFoodDiarymTurkThurs.sav")
food.diary.mturk.f.data <- read_sav("ThanksgivingFoodDiarymTurkFri.sav")

