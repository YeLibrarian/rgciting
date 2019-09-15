#The "CitationInput.R" script reads in tab-delimited text files containing citations for the cited articles and generate a table to relate AIDs and CIDS and store citing article data 

#Read collected CitedAs, Locatable, and AID data for Citing Articles from spreadsheet
dataPath <- paste(getwd(),"/ProcessedData/CitingArticleCollectedData.csv", sep="" )
citingArticleData <- read.csv(dataPath, row.names= NULL, fill = TRUE, header = FALSE, stringsAsFactors = FALSE)

#function to generate CIDs
CreateCID <- function(data, id = 'C') {
  CID <- seq(nrow(data)) #generate sequence with length of data
  CID <- paste(id, formatC(CID, width=3, flag="0"), sep="") #append number to 'C'
  newColNames <- c("AID", "CitedAs", "Locatable")
  data <- cbind(data$V3, data$V1, data$V2)
  colnames(data) <- newColNames 
  data <- cbind(CID, data)
}

#function to import data, correct the mistake in column header (extra row.names for the first column). 
DataIn <- function(fileName) {
  tempData <- NULL
  tempData <- read.delim(fileName, sep="\t", header = TRUE, row.names = NULL, fill = TRUE, na.strings = c("", " "), stringsAsFactors = FALSE, fileEncoding = "UCS-2LE")
  colnames(tempData) <- c(colnames(tempData)[-1], "temp")
  tempData <- tempData[ , 1:(ncol(tempData)-1)]
  return(tempData)
}

#input citation files 
ReadFiles <- function(data){
  finalData <- NULL
  for(i in seq(nrow(data))) {
    file_name <- paste(getwd(),"/Data/CitingArticles/", data[i, 1], ".txt", sep="" )
    
    #identify citing articles with missing citation information text files
    if(is.na(data[i,2]) && is.na(data[i,3]) && is.na(data[i,4])){
      temp_data <- NA
      print("Missing file")
      print(file_name)
    } else {
      temp_data <- DataIn(file_name) 
    }
    
    finalData <- rbind(finalData, temp_data)
  }
  
  finalData <- cbind(data, finalData)
  print("Files read")
  return(finalData)
} 

citingArticleData <- CreateCID(citingArticleData)
citingArticleData <- ReadFiles(citingArticleData)

#remove columns with all NA values from WOS_SciConf as the main dataset for further processing
citingArticleData <- citingArticleData[ , ! apply(citingArticleData, 2, function(x) all(is.na(x)))]

#write table to file
write.csv(citingArticleData, "ProcessedData/CitingArticleFinalData.csv", row.names = FALSE)

