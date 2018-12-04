# Setting working directory
setwd("~/Desktop/Research/Symmetry Study")

# Loading packages
library(dplyr)
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

# Making a copy of the data in a new variable
sym.new <- sym 

# Making all blank cells NA
sym.new[sym.new==""] <- NA 

# Creating a new column "color" to differentiate between green and white
sym.new$color <- NA 

# Moving "color" next to "consent"
sym.new <- sym.new[, c(1:14, 875, 15:874)] 

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
colnames(sym.new) <- sub("white_", "", colnames(sym.new))
colnames(sym.new)

# Recoding  variables to "0" and "1" (Unselected = 0, Selected = 1)
# Here's a function to do it a little faster
recoding <- function(vrbl){
  vrbl <- recode(vrbl,`1` = 0, `2` = 1)
  vrbl
}

# Applying to all relevant columns:
sym.new <- mutate_at(sym.new, vars(pattern_1_11:pattern_1_110,
                                   pattern_2_11:pattern_2_110,
                                   pattern_3_11:pattern_3_110,
                                   pattern_4_11:pattern_4_110,
                                   pattern_5_11:pattern_5_110,
                                   pattern_6_11:pattern_6_110,
                                   practice_11:practice_110), funs(recoding))

# Export file
write.csv(sym.new, file="sym.csv")


######################################################################################
# Verifying Symmetry 
######################################################################################

# Check for symmetry

grid <- list(16, 26, 36, 46, 56, 66, 76, 86, 96, 106) # index of 1st column indices in 10x10 grid
symmetry.vector <- NULL                               # vector to store final symmetric/not symmetric values

for(r in 1:101){                                      # for loop to iterate through each participant row
  check <- NULL                                       # empty vector to store symmetric/not symmetric values for each grid row
  
  for (i in grid){                                    # for loop to iterate through grid rows
    index_left <- i                                   # variable to identify the index of the left side of grid row
    index_right <- index_left + 9                     # variable to identify the index of the right side of grid row
    name <- paste('check row',i,sep=' ')
    count <- 1                                        # initial count to keep track of iterations
    while (count <= 5){                               # while loop to repeat left and right comparisons 5 times
      left_side <- sym.new[r, index_left]             # get value of left side using index
      right_side <- sym.new[r, index_right]           # get value of right side using index
      
      if(identical(left_side, right_side) == TRUE)    # check if left is equal to right - if so, add 0 to vector
      {check <- append(check, 0)}
      
      else                                            # if left is not equal to right - add 1 to vector
      {check <- append(check, 1)}
      
      index_left <- index_left + 1                    # update left and right indices to check next left/right pair
      index_right <- index_right - 1
      count = count + 1                               # update count to keep track of next iteration
    }
  }
  
  if("not symmetric" %in% check == TRUE)              # if any grid row is not symmetric - that means the whole picture is not symmetric
  {symmetry.vector[r] <- "not symmetric"}
  
  else
  {symmetry.vector[r] <- "symmetric"}
}

# Count number of rows not symmetric
symmetry.vector
which(symmetry.vector == "not symmetric")
length(which(symmetry.vector == "not symmetric"))

######################################################################################
# Testing
######################################################################################

check <- NULL
for (i in grid){
  index_left <- i
  index_right <- index_left + 9
  name <- paste('row',i,sep=' ')
  count <- 1
  while (count <= 5){
    left_side <- sym.new[74, index_left]
    right_side <- sym.new[74, index_right]
    
    if(identical(left_side, right_side) == TRUE)   
    {check <- append(check, 0)}
    
    else                                            
    {check <- append(check, 1)}
    
    index_left <- index_left + 1                   
    index_right <- index_right - 1
    count = count + 1   
  }
}
check
m <- matrix(check, nrow=10, byrow=T); m
