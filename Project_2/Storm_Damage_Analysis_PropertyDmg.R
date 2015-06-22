##Set working directory and download the file
setwd("C:/Users/james/SkyDrive/Data Science/Coursera_Reproducible_Research/Project_2")
file <- download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
                      ,destfile="./Data/storm.csv.bz2")
##Read the file
storm_df <- read.csv(bzfile("./Data/storm.csv.bz2"))

##Need to create a loop to create 2 different vectors which include information
##for each unique event type (EVTYPE).
#Before starting, make sure to change the values in EVTYPE variable to lowercase
##as there are a lot of repeat values that should be unique, but are not due to case.
storm_df$EVTYPE <- tolower(storm_df$EVTYPE)
type <- vector()  #First vector representing the type of storm
property_damage <- vector()  #Second vector representing the property damage
for (i in 1:length(unique(storm_df$EVTYPE))){
  temp_df <- storm_df[storm_df$EVTYPE==unique(storm_df$EVTYPE)[i],]
  type <- append(type, unique(storm_df$EVTYPE)[i])
  property_damage <- append(property_damage, sum(temp_df$PROPDMG))
}

##Create the data frame with the vectors just made
##The datafame should have the unique event and total property damage for that event
prop_dmg_df <- data.frame(cbind(type, property_damage))
colnames(prop_dmg_df)[2] <- "property_damage"
prop_dmg_df[,2] <- as.numeric(as.character(prop_dmg_df[,2]))

##Figure 3 Code showing which events caused the most property damage
##Sort decreasing order for property damage
prop_df <- prop_dmg_df[order(prop_dmg_df$property_damage, decreasing=TRUE),]
#Subset this dataframe to get the first five rows to include in plot, then plot
prop_dfs <- prop_df[1:5,]
barplot(prop_dfs$property_damage, names.arg=prop_dfs$type, border=TRUE, xlab="Event", ylab="Amount of Property Damage",
        main="Property Damage for Most Destructive Events", col=c("blue","red","green","orange","purple"),
        cex.names=.6)

