#The "PrelimStats.R" script calculates quantitative statistics to get a first overview of the data

##@knitr part1
require(plyr)

#determine data paths and read data for citing articles and articles
citingPath <- paste(getwd(),"/ProcessedData/CitingArticle_Cleaned.csv", sep="" )
citingData <- read.csv(citingPath, row.names= NULL, fill = TRUE, header = TRUE, stringsAsFactors = FALSE)
dataPath <- paste(getwd(),"/ProcessedData/WOS_SciConf_Cleaned.csv", sep="" )
articleData <- read.csv(dataPath, row.names= NULL, fill = TRUE, header = TRUE, stringsAsFactors = FALSE)

#Additional Stats
#Copyright count
copyrightCount <- count(articleData, 'Copyrighted')

#fulltext count
fulltextCount <- count(articleData, 'FulltextAvailable')

#how are articles cited as: Title, URL, etc
citedAs <- count(citingData, 'CitedAs')

#types of articles
articleType <- count(articleData, 'ItemClass')

## @knitr a1
# a1: how many articles are locatable
locatable <- count(citingData, 'Locatable')
locatableArticles <- articleData[(articleData$AID %in% citingData[ (citingData$Locatable == 'Yes') & (!is.na(citingData$Locatable)),"AID"]), ]


#b1: how many articles were cited as title but have true source
tempTitles <- unlist(unique(citingData[(citingData['CitedAs'] == 'Title'), "AID"])) #removed duplicate AIDs
titleSource <- articleData[(articleData[,"AID"] %in% tempTitles) & (!is.na(articleData[,"SourcePubs"])) & (articleData[,"SourcePubs"] != "No"),] #remove values that are NA or No

#b2: how many articles cited as a title but have details available in RG
titleDetails <- articleData[(articleData[,"AID"] %in% tempTitles) & (articleData[,"CitationDetail_RG"] == "Yes") & (!is.na(articleData[,"CitationDetail_RG"])),] #remove values that are NA or No

#b3: Sources that have citing articles citing the original source AND a research gate source
temp <- citingData[duplicated(citingData["AID"]),]$AID #get the AIDs that are duplicated
dups <- citingData[(citingData$AID %in% temp), ] #extract duplicated rows
originalTitle <- ddply(dups,~AID,summarise,TF=(grep("Title", as.character(unique(CitedAs))) & grep("Original",as.character(unique(CitedAs)))))

#c1: articles with no publication source/DOI RG
doirg <- articleData[ (articleData['DOI_RG'] == 'Yes' & !is.na(articleData['DOI_RG'])) & (articleData['SourcePubs'] == 'No'), ]

# d1: copyrighted articles w/ fulltext available on research gate
fullCopyright <- articleData[ (articleData['Copyrighted'] == 'Yes' & !is.na(articleData['Copyrighted'])) & (articleData['FulltextAvailable'] == 'Yes' & !is.na(articleData['FulltextAvailable'])) , ]
copyrightInfo <- fullCopyright[,c("Copyrighted", "FulltextAvailable", "AID", "AU", "TI")]
copyrightInfo <- rbind(nrow(fullCopyright), copyrightInfo)

#d2: num copyrighted articles with also fulltext in specified locations
copyrightRepo <- articleData[ (articleData['Copyrighted'] == 'Yes') & (!is.na(articleData['Copyrighted'])) & (!is.na(articleData["SourceRepo"])) & (articleData['SourceRepo'] != 'No'),]
copyrightPreprint <- articleData[ (articleData['Copyrighted'] == 'Yes') & (!is.na(articleData['Copyrighted'])) & (!is.na(articleData["SourcePreprint"])) & (articleData['SourcePreprint'] != 'No'),]
copyrightWebsite <- articleData[ (articleData['Copyrighted'] == 'Yes') & (!is.na(articleData['Copyrighted'])) & (!is.na(articleData["SourceWebsite"])) & (articleData['SourceWebsite'] != 'No'),]

#d3: copyrighted articles with no full text available on RG
copyrightNoFulltext <- articleData[ (articleData['Copyrighted'] == 'Yes') & (!is.na(articleData['Copyrighted'])) & (!is.na(articleData["FulltextAvailable"])) & (articleData['FulltextAvailable'] == 'No'),]

#d4: Articles w/ OA or Cretive Commons License
oaNum <- articleData[ !is.na(articleData['Licensed']) & articleData['Licensed']!="No", ]

#d4: Articles w/ HCISI option
hcisi <- articleData[ !is.na(articleData['HCISI']) & articleData['HCISI']!="No", ]

#Create table to summarize results
##@knitr tablef

names <- c("A1", "B1", "B2", "B3", "C1", "D1","D2R", "D2P", "D2W", "D3", "D4O", "D4H")
table <- NULL
table <- rbind(table,
               c("How many of the articles were locatable",nrow(locatableArticles), list(locatableArticles$AID)),
               c("Articles cited as a title but have a True source", nrow(titleSource), list(titleSource$AID)),
               c("Articles cited as a title but have details available in researchgate", nrow(titleDetails), list(titleDetails$AID)),
               c("Citing articles citing the original source AND a research gate source", nrow(originalTitle), list(originalTitle$AID)),
               c("Articles that have RG Doi/no other source", nrow(doirg), list(doirg$AID)),
               c("Articles that are copyrighted and have full text available on RG", nrow(fullCopyright), list(fullCopyright$AID)),
               c("Copyrighted articles on repository", nrow(copyrightRepo), list(copyrightRepo$AID)),
               c("Copyrighted articles on preprint", nrow(copyrightPreprint), list(copyrightPreprint$AID)),
               c("Copyrighted atricles on website", nrow(copyrightWebsite), list(copyrightWebsite$AID)),
               c("Articles that are copyrighted and have NO full text available", nrow(copyrightNoFulltext), list(copyrightNoFulltext$AID)),
               c("Open Access articles", nrow(oaNum), list(oaNum$AID)),
               c("HCISI articles", nrow(hcisi), list(hcisi$AID)))
table <- data.frame(table)  
row.names(table) <- names
colnames(table) <- c("Question", "Number Matches", "AIDs of Matches")


