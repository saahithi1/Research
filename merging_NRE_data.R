setwd("/Users/saahithi/Desktop/Research/merging")

# Reading in data
coding <- data.frame(read.csv("AnneChrisMerged.csv", header = TRUE))
NRE <- data.frame(read.csv("Latest_NRE_data_feb_2018_w_group.csv", header = TRUE))

# Looking at column names
names(coding)
names(NRE)

# Renaming column B in coding to match column Z in NRE
coding$DescribePerjuryorFalseAccusation <- coding$CDescribePerjuryorFalseAccusation

# Merge by column
total <- merge(NRE, coding, by="DescribePerjuryorFalseAccusation")

# Download .csv
write.csv(total, file="merged.csv")
