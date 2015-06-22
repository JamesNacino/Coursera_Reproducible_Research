##Set working directory and download the file
setwd("C:/Users/james/SkyDrive/Data Science/Coursera_Reproducible_Research/Project_2")
file <- download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
                      ,destfile="./Data/storm.csv.bz2")
##Read the file
storm_df <- read.csv(bzfile("./Data/storm.csv.bz2"))

##Need to create a loop to create 3 different vectors which include information
##for each unique event type (EVTYPE).
#Before starting, make sure to change the values in EVTYPE variable to lowercase
##as there are a lot of repeat values that should be unique, but are not due to case.
storm_df$EVTYPE <- tolower(storm_df$EVTYPE)
type <- vector()  #First vector representing the type of storm
fatalities <- vector()  #Second vector representing the number of fatalites
injuries <- vector()  #Third vector representing the number of injuries
for (i in 1:length(unique(storm_df$EVTYPE))){
  temp_df <- storm_df[storm_df$EVTYPE==unique(storm_df$EVTYPE)[i],]
  type <- append(type, unique(storm_df$EVTYPE)[i])
  fatalities <- append(fatalities, sum(temp_df$FATALITIES))
  injuries <- append(injuries, sum(temp_df$INJURIES))
}

##Create the data frame with the vectors just made
##Then make a new column with the sum of the values from fatalities and injuries
pop_dmg_df <- data.frame(cbind(type, fatalities, injuries))
colnames(pop_dmg_df)[2] <- "fatalities"
colnames(pop_dmg_df)[3] <- "injuries"
pop_dmg_df[,2] <- as.numeric(as.character(pop_dmg_df[,2]))
pop_dmg_df[,3] <- as.numeric(as.character(pop_dmg_df[,3]))
pop_dmg_df <- cbind(pop_dmg_df, "sum_fatal_injury"=pop_dmg_df$fatalities+pop_dmg_df$injuries)


##Figure 1 code showing which events caused the most fatalities
##Sort decreasing order for fatalities
fatal_df <- pop_dmg_df[order(pop_dmg_df$fatalities, decreasing=TRUE),]
#Subset this dataframe to get the first five rows to include in plot, then plot
fatal_dfs <- fatal_df[1:5,]
plot(fatal_dfs$fatalities, fatal_dfs$injuries, main="Events Which Caused the Most Fatalities",
     xlab="Fatalities", ylab="Injuries")
text(fatal_dfs$fatalities, fatal_dfs$injuries, labels=fatal_dfs$type, cex=.8, pos=3)
points(fatal_dfs[1,2], fatal_dfs[1,3], col="blue", pch=19)
points(fatal_dfs[2,2], fatal_dfs[2,3], col="red", pch=19)
points(fatal_dfs[3,2], fatal_dfs[3,3], col="orange", pch=19)
points(fatal_dfs[4,2], fatal_dfs[4,3], col="green", pch=19)
points(fatal_dfs[5,2], fatal_dfs[5,3], col="purple", pch=19)
legend("topleft", legend=fatal_dfs$type, col=c("blue", "red","orange", "green", "purple"),
                           cex=.65, pch=19)
#Save as '.png' file
dev.copy(png, file="figure_1.png")
dev.off() 

##Figure 2 Code showing which events caused the most injuries
##Sort decreasing order for injuries
injury_df <- pop_dmg_df[order(pop_dmg_df$injuries, decreasing=TRUE),]
#Subset this dataframe to get the first five rows to include in plot, then plot
injury_dfs <- injury_df[1:5,]
plot(injury_dfs$injuries, injury_dfs$fatalities, main="Events Which Caused the Most Injuries",
     xlab="Injuries", ylab="Fatalities")
text(injury_dfs$injuries, injury_dfs$fatalities, labels=injury_dfs$type, cex=.8, pos=3)
points(injury_dfs[1,3], fatal_dfs[1,2], col="blue", pch=19)
points(injury_dfs[2,3], injury_dfs[2,2], col="red", pch=19)
points(injury_dfs[3,3], injury_dfs[3,2], col="orange", pch=19)
points(injury_dfs[4,3], injury_dfs[4,2], col="green", pch=19)
points(injury_dfs[5,3], injury_dfs[5,2], col="purple", pch=19)
legend("topleft", legend=injury_dfs$type, col=c("blue", "red","orange", "green", "purple"),
       cex=.65, pch=19)
#Save as '.png' file
dev.copy(png, file="figure_2.png")
dev.off() 


