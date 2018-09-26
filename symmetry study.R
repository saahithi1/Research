# Setting working directory
setwd("~/Desktop/Research/Symmetry Study")

# Loading packages
library(ggplot2)
library(dplyr)
library(jmv)
library(foreign)
library("tidyr")  


# Reading in raw data 
sym<-read.csv("Symmetry_Study___Practice_Trial_Defaults copy.csv", header=FALSE, skip=2)
sym.names<-read.csv("Symmetry_Study___Practice_Trial_Defaults copy.csv")
names(sym)<-names(sym.names)
rm(sym.names)

# Screening people who didn't finish
sym <- filter(sym, V10 == 1)

######################################################################################
# Cleaning
######################################################################################

# Defining condition
sym$cond <- sym$DO.BR.FL_12
levels(sym$cond) <- c("Subtraction Practice", "Addition Practice")

# Defining strategy question
sym$strategy <- factor(sym$strategy)
levels(sym$strategy) <- c("I added squares until they were symmetrical",
                          "I subtracted squares until they were symmetrical")
sym$strategyNum <- as.numeric(sym$strategy)  # And creating a numeric version for correlations later

# Fixing age variable (there was a Qualtrics coding error; "52" got recorded as "5")
sym$age[sym$age==5] <- 52

# Defining gender (Note, there was a third response option for Other/prefer not to say, but no one selecetd it)
sym$gender <- factor(sym$gender,
                     levels = 1:3,
                     labels = c("Male", "Female", "Other/Prefer not to say"))

# Defining ethnicity 
sym$ethnicity <- factor(sym$ethnicity,
                        levels = 1:7,
                        labels = c("Hispanic or Latino",
                                   "American Indian or Alaska Native",
                                   "Asian",
                                   "Black or African American",
                                   "White, Caucasian, or European",
                                   "Native Hawaiian or other Pacific Islander",
                                   "Other, prefer not to answer, or unknown"))

# Defining education
sym$education <- factor(sym$education,
                        levels = 1:7,
                        labels = c("Less than high school",
                                   "High school diploma or equivalent",
                                   "Some college",
                                   "Associate degree (e.g., AA)",
                                   "Bachelor's degree (e.g., BA, BS)",
                                   "Master's degree (e.g., MA, MS)",
                                   "Doctoral or professional degree (e.g, PhD, MD, JD)"))

# Defining income
sym$income <- factor(sym$income,
                     levels = 1:5,
                     labels = c("Less than $25,000",
                                "$25,001 to $50,000",
                                "$50,001 to $100,000",
                                "$100,001 to $250,000",
                                "More than $250,000"))

######################################################################################

sym.new <- sym # Making a copy of the data in a new variable
sym.new[sym.new==""] <- NA # Making all blank cells NA
sym.new$color <- NA # Creating a new column "color" to differentiate between green and white
sym.new <- sym.new[, c(1:14, 875, 15:874)] # Moving "color" next to "consent"

# Loop to shift green_practice rows
for(i in 1:101){
  # Identify if rows are white or green practice
  value <- sym.new$white_practice_11[i]
  
  if(is.na(value) == TRUE)
  {sym.new$color[i] <- "green"}
  else
  {sym.new$color[i] <- "white"}
  
  # If green_practice, move data
  if(sym.new$color[i] == "green")
  {sym.new[i,16:119] <- sym.new[i,120:223]}
}

# Delete unecessary green_practice columns
sym.new <- sym.new[, -c(120:223)]
colnames(sym.new)

# Rename white_practice columns to practice
sub("white_", "", colnames(sym.new))

# Export file
write.csv(sym.new, file="sym.csv")
