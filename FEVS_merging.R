# reference: https://www.r-bloggers.com/merge-all-files-in-a-directory-using-r-into-a-single-dataframe/
# install.packages("openxlsx")
library(openxlsx)

# Set working directory and get the list of files
setwd("/Users/saahithi/Desktop/test")
file_list <- list.files()

# Loop to merge the files into one dataframe
for (file in file_list){
  
  # If the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.xlsx(file)
    dataset<-rbind(dataset, temp_dataset)
  }
  
  # If the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.xlsx(file)
  }
} 

# Export dataframe into an excel file
write.xlsx(dataset, "all_combined.xlsx")
