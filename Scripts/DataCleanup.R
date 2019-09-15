# The "DataCleanup.R" script removes manually identified duplicate and false positive records from collected data

#read in citing article data from "CitingArticleFinalData.csv"
citingPath <- paste(getwd(),"/ProcessedData/CitingArticleFinalData.csv", sep="" )
citingData <- read.csv(citingPath, row.names= NULL, fill = TRUE, header = TRUE, stringsAsFactors = FALSE)

#read in article data from "WOS_SciConf_20180531_CollectedData_YeEdited20181113.csv"
dataPath <- paste(getwd(),"/ProcessedData/WOS_SciConf_20180531_CollectedData_YeEdited20181113.csv", sep="" )
articleData <- read.csv(dataPath, row.names= NULL, fill = TRUE, header = TRUE, stringsAsFactors = FALSE)

#read in exclusion data from "Exclusions.csv"
exclusionsPath <- paste(getwd(),"/ProcessedData/Exclusions.csv", sep="" )
exclusions <- read.csv(exclusionsPath, row.names= NULL, fill = TRUE, header = TRUE, stringsAsFactors = FALSE)

#get AIDs of Articles to exclude
AIDExc <- unique(exclusions[exclusions$Action.Taken == 'Exclude', "AID"])

#extract AID, CID of articles to DEDUPLICATE
dedupExc <- data.frame(exclusions[exclusions$Action.Taken == 'Dedup', c("AID", "CID")])

#Split the list of AIDs and CIDs by '/'
dedupExc$AID <- strsplit(dedupExc$AID, '/')
dedupExc$CID <- strsplit(dedupExc$CID, '/')

#get AIDs to exclude by leaving one copy of the Article information
AIDExc <- append(AIDExc, unlist(lapply(dedupExc$AID, FUN = function(x){(x[-1])})))

#reassign the removed AIDs for the citing articles to the unremoved AID
for(i in 1:nrow(dedupExc)){
  citingData[citingData$CID %in% unlist(dedupExc[i,2]), "AID" ]= unlist(dedupExc[i,1])[1]  
}

#remove rows from article_data with the AIDs to exclude
articleData <- subset(articleData, !(AID %in% AIDExc))

#write cleaned data to files
write.csv(citingData, "ProcessedData/CitingArticle_Cleaned.csv", row.names = FALSE)
write.csv(articleData, "ProcessedData/WOS_SciConf_Cleaned.csv", row.names = FALSE)
