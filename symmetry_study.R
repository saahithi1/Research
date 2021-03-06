# Setting working directory
setwd("~/Desktop/Research/Symmetry Study")

# Loading packages
library(dplyr)
library(foreign) 
library(ggplot2)
library(reshape2)

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
# Verifying Left-Right Symmetry 
######################################################################################

# Identify column indices to use in loop
lr.practice <- list(16, 26, 36, 46, 56, 66, 76, 86, 96, 106)
lr.pattern.1 <- list(132, 142, 152, 162, 172, 182, 192, 202, 212, 222)
lr.pattern.2 <- list(236, 246, 256, 266, 276, 286, 296, 306, 316, 326)
lr.pattern.3 <- list(340, 350, 360, 370, 380, 390, 400, 410, 420, 430)
lr.pattern.4 <- list(444, 454, 464, 474, 484, 494, 504, 514, 524, 534)
lr.pattern.5 <- list(548, 558, 568, 578, 588, 598, 608, 618, 628, 638)
lr.pattern.6 <- list(652, 662, 672, 682, 692, 702, 712, 722, 732, 742)

grid <- lr.pattern.6                                  # list of 1st column indices in 10x10 grid; replace with patterns to check other grids
symmetry.vector <- NULL                               # vector to store final symmetric/not symmetric values

for(r in 1:101){                                      # for loop to iterate through each participant row
  check <- NULL                                       # empty vector to store symmetric/not symmetric values for each grid row
  
  for (i in grid){                                    # for loop to iterate through grid rows
    index_left <- i                                   # variable to identify the index of the left side of grid row
    index_right <- index_left + 9                     # variable to identify the index of the right side of grid row
    count <- 1                                        # initial count to keep track of iterations
    while (count <= 5){                               # while loop to repeat left/right comparisons 5 times
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
  
  if(1 %in% check == TRUE)                            # if any grid row is not symmetric - that means the whole picture is not symmetric
  {symmetry.vector[r] <- "not symmetric"}             # update symmetry.vector to have one value for each row - "not symmetric" or "symmetric"
  
  else
  {symmetry.vector[r] <- "symmetric"}
}

# Count number of rows not symmetric
symmetry.vector
which(symmetry.vector == "not symmetric")
length(which(symmetry.vector == "not symmetric"))

# Number of Non Symmetric Rows 

# Practice - 78
# 1 14 18 23 27 28 31 35 36 38 51 56 57 58 62 63 66 70 76 78 82 84 85 are symmetric

# Pattern 1 - 100
# 89 is symmetric

# Pattern 2 - 101

# Pattern 3 - 101

# Pattern 4 - 100
# 19 is symmetric

# Pattern 5 - 100
# 64 is symmetric 

# Pattern 6 - 4
# 15 70 74 89 are not symmetric


######################################################################################
# Testing 
######################################################################################

# verify symmetry
grid <- lr.pattern.3
check <- NULL
for (i in grid){
  index_left <- i
  index_right <- index_left + 9
  name <- paste('row',i,sep=' ')
  count <- 1
  while (count <= 5){
    left_side <- sym.new[15, index_left]
    right_side <- sym.new[15, index_right]
    
    if(identical(left_side, right_side) == TRUE)   
    {check <- append(check, 0)}
    
    else                                            
    {check <- append(check, 1)}
    
    index_left <- index_left + 1                   
    index_right <- index_right - 1
    count = count + 1   
  }
}

# symmetry matrix
m <- matrix(check, nrow=10, byrow=T); m

######################################################################################
# Verifying Up-Down Symmetry 
######################################################################################

# Check participant rows 
ud.practice <- list(16, 17, 18, 19, 20, 21, 22, 23, 24, 25)
ud.pattern.1 <- list(132, 133, 134, 135, 136, 137, 138, 139, 140, 141)
ud.pattern.2 <- list(236, 237, 238, 239, 240, 241, 242, 243, 244, 245)
ud.pattern.3 <- list(340, 341, 342, 343, 344, 345, 346, 347, 348, 349)
ud.pattern.4 <- list(444, 445, 446, 447, 448, 449, 450, 451, 452, 453)
ud.pattern.5 <- list(548, 549, 550, 551, 552, 553, 554, 555, 556, 557)
ud.pattern.6 <- list(652, 653, 654, 655, 656, 657, 658, 659, 660, 661)

grid <- ud.pattern.1                                    
symmetry.vector <- NULL                               

for(r in 1:101){                                      
  check <- NULL                                       
  
  for (i in grid){                                    
    index_top <- i                                  
    index_bottom <- index_top + 90                     
    count <- 1                                        
    while (count <= 5){                              
      top_side <- sym.new[r, index_top]             
      bottom_side <- sym.new[r, index_bottom]
      
      if(identical(top_side, bottom_side) == TRUE)    
      {check <- append(check, 0)}
      
      else                                           
      {check <- append(check, 1)}
      
      index_top <- index_top + 10                  
      index_bottom <- index_bottom - 10
      count = count + 1                           
    }
  }
  
  if(1 %in% check == TRUE)                            
  {symmetry.vector[r] <- "not symmetric"}             
  
  else
  {symmetry.vector[r] <- "symmetric"}
}

# Count number of columns not symmetric
symmetry.vector
which(symmetry.vector == "not symmetric")
length(which(symmetry.vector == "not symmetric"))

# Non Symmetric Columns 

# Practice - 81
# 1 14 18 23 28 31 35 36 38 51 56 58 62 63 66 70 78 82 84 85 are symmetric

# Pattern 1 - 100
# 89 is symmetric

# Pattern 2 - 101

# Pattern 3 - 101

# Pattern 4 - 101

# Pattern 5 - 101

# Pattern 6 - 101


######################################################################################
# Recreating Grids
######################################################################################

# from https://stackoverflow.com/questions/12081843/r-matrix-plot-with-colour-threshold-and-grid

grid.practice <- 16:115 
grid.pattern.1 <- 132:231
grid.pattern.2 <- 236:335
grid.pattern.3 <- 340:439
grid.pattern.4 <- 444:543
grid.pattern.5 <- 548:647
grid.pattern.6 <- 652:751


grid <- grid.pattern.1
for(r in 1:101){
  x <- as.numeric(sym.new[r,grid])
  x <- matrix(x,nrow=10)

  x1 <- melt(x)
  names(x1) <- c("x","y","color")

  x1$color <- factor(x1$color==1)
  levels(x1$color) <- c("0","1")

  title <- paste("Participant", r, sep = ' ')
  plot <- qplot(x, y, fill=color, data=x1,geom='tile') + labs(title=title, x="", y="") + 
          scale_fill_manual(values=c("#EEF0EB", "#2EA666")) +
          theme(axis.text.x = element_blank(),axis.text.y = element_blank()) +
          scale_y_reverse() 
  
  ggsave(plot,filename=paste("Participant",r,".jpg",sep=""))
}

######################################################################################
# Testing 
######################################################################################
grid <- grid.pattern.5
x <- as.numeric(sym.new[61,grid])
x <- matrix(x,nrow=10);x

x1 <- melt(x); x1
names(x1)=c("x","y","color");x1
x1$color <- factor(x1$color==1)
levels(x1$color) <- c("0","1")

title <- paste("Participant", 1, sep = ' ')
qplot(x, y, fill=color, data=x1,geom='tile') + labs(title=title, x="", y="") + 
  scale_fill_manual(values=c("#EEF0EB", "#2EA666")) +
  theme(axis.text.x = element_blank(),axis.text.y = element_blank()) +
  scale_y_reverse() 

