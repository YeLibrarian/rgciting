#install.packages('plyr')


##@knitr part1
library(plyr)

#determine data paths/read data for citing articles and articles
citing_path <- paste(getwd(),"/ProcessedData/CitingArticle_Cleaned.csv", sep="" )
citing_data <- read.csv(citing_path, row.names= NULL, fill = TRUE, header = TRUE, stringsAsFactors = FALSE)
data_path <- paste(getwd(),"/ProcessedData/WOS_SciConf_Cleaned.csv", sep="" )
article_data <- read.csv(data_path, row.names= NULL, fill = TRUE, header = TRUE, stringsAsFactors = FALSE)

#exclude NA NA
citing_data <- citing_data[!((is.na(citing_data$CitedAs)) & (is.na(citing_data$Locatable))),] 

#Additional Stats
#Copyright count
copyright_count <- count(article_data, 'Copyrighted')
copyright_yes <- copyright_count[((copyright_count$Copyrighted == 'Yes') & (!is.na(copyright_count$Copyrighted))),]
#fulltext count
fulltext_count <- count(article_data, 'FulltextAvailable')

#how are articles cited as: Title, URL, etc
cited_as <- count(citing_data, 'CitedAs')
cited_as$CitedAs<- strsplit(cited_as$CitedAs, c(';'))
l <- sapply(cited_as$CitedAs, length)
test <- data.frame(unlist(cited_as$CitedAs), rep(cited_as$freq, l), stringsAsFactors = FALSE)
colnames(test) <- c("Cited_As", "freq")
cited_as <-ddply(test, "Cited_As", numcolwise(sum))
cited_as <- cited_as[order(cited_as$freq, decreasing = FALSE),]
cited_as$Cited_As <-lapply(cited_as$Cited_As,  function(x) x = ifelse(is.na(x), "N/A", x))
#types of articles
article_type <- count(article_data, 'ItemClass')
article_type$ItemClass <-lapply(article_type$ItemClass,  function(x) x = ifelse(is.na(x), "N/A", x))
article_type <- article_type[order(article_type$freq, decreasing = FALSE),]
article_type <- article_type[!article_type$ItemClass == 'N/A', ]

pubs <- function (data){
  data <-strsplit(data, c(';'))
  data <- trimws(unlist(data))
  data <- data.frame(data, stringsAsFactors = FALSE, row.names = NULL)
  data <- count(data)
  data <- data[order(data$freq, decreasing = TRUE),]
  return <- data
}

source_web <- pubs(article_data$SourceWebsite)
source_pubs <- pubs(article_data$SourcePubs)
source_preprint <- pubs(article_data$SourcePreprint)
source_repo <- pubs(article_data$SourceRepo)


source_web$data <-lapply(source_web$data,  function(x) x = ifelse(is.na(x), "N/A", x))
source_web$data <-lapply(source_web$data,  function(x) x = ifelse(x == "No", "No other websites", x))
source_web <- source_web[!source_web$data == 'N/A', ]



## @knitr a1
# a1: how many articles are locatable
locatable <- count(citing_data, 'Locatable')
locatable_articles <- article_data[(article_data$AID %in% citing_data[ (citing_data$Locatable == 'Yes') & (!is.na(citing_data$Locatable)),"AID"]), ]


#b1: how many articles were cited as title but have true source
temp_titles <- unlist(unique(citing_data[(citing_data['CitedAs'] == 'Title'), "AID"])) #removed duplicate AIDs
title_source <- article_data[(article_data[,"AID"] %in% temp_titles) & (!is.na(article_data[,"SourcePubs"])) & (article_data[,"SourcePubs"] != "No"),] #remove values that are NA or No

#b2: how many articles cited as a title but have details available in RG
title_details <- article_data[(article_data[,"AID"] %in% temp_titles) & (article_data[,"CitationDetail_RG"] == "Yes") & (!is.na(article_data[,"CitationDetail_RG"])),] #remove values that are NA or No

#b3: Sources that have citing articles citing the original source AND a research gate source
temp <- citing_data[duplicated(citing_data["AID"]),]$AID #get the AIDs that are duplicated
dups <- citing_data[(citing_data$AID %in% temp), ] #extract duplicated rows
original_title <- ddply(dups,~AID,summarise,TF=(grep("Title|URL;Title|DOI;Title|ArticleTitle", as.character(unique(CitedAs))) & grep("Original",as.character(unique(CitedAs)))))

no_rg <- ddply(dups,~AID,summarise,(unique(CitedAs)=="Original"))
no_rg <- no_rg[(no_rg$TF==TRUE),]
#c1: articles with no publication source/DOI RG
doirg <- article_data[ (article_data['DOI_RG'] == 'Yes' & !is.na(article_data['DOI_RG'])) & (article_data['SourcePubs'] == 'No'), ]

# d1: copyrighted articles w/ fulltext available on research gate
full_copyright <- article_data[ (article_data['Copyrighted'] == 'Yes' & !is.na(article_data['Copyrighted'])) & (article_data['FulltextAvailable'] == 'Yes' & !is.na(article_data['FulltextAvailable'])) , ]
copyright_info <- full_copyright[,c("Copyrighted", "FulltextAvailable", "AID", "AU", "TI")]
copyright_info <- rbind(nrow(full_copyright), copyright_info)

#d2: num copyrighted articles with also fulltext in specified locations
copyright_repo <- article_data[ (article_data['Copyrighted'] == 'Yes') & (!is.na(article_data['Copyrighted'])) & (!is.na(article_data["SourceRepo"])) & (article_data['SourceRepo'] != 'No'),]
copyright_preprint <- article_data[ (article_data['Copyrighted'] == 'Yes') & (!is.na(article_data['Copyrighted'])) & (!is.na(article_data["SourcePreprint"])) & (article_data['SourcePreprint'] != 'No'),]
copyright_website <- article_data[ (article_data['Copyrighted'] == 'Yes') & (!is.na(article_data['Copyrighted'])) & (!is.na(article_data["SourceWebsite"])) & (article_data['SourceWebsite'] != 'No'),]
copyright_yes <- article_data[ (article_data['Copyrighted'] == 'Yes') & (!is.na(article_data['Copyrighted'])),]
#d3: copyrighted articles with no full text available on RG
copyright_no_fulltext <- article_data[ (article_data['Copyrighted'] == 'Yes') & (!is.na(article_data['Copyrighted'])) & (!is.na(article_data["FulltextAvailable"])) & (article_data['FulltextAvailable'] == 'No'),]

#d4: Articles w/ OA or Cretive Commons License
oa_num <- article_data[ !is.na(article_data['Licensed']) & article_data['Licensed']!="No", ]

#d4: Articles w/ HCISI option
hcisi <- article_data[ !is.na(article_data['HCISI']) & article_data['HCISI']!="No", ]

#Create table to summarize results
##@knitr tablef

table <- NULL
table <- rbind(table,
               c("How many of the articles were locatable",nrow(locatable_articles)),
               c("Articles cited as a title but have a True source", nrow(title_source)),
               c("Articles cited as a title but have details available in researchgate", nrow(title_details)),
               c("Citing articles citing the original source AND a research gate source", nrow(original_title)),
               c("Articles that have RG Doi/no other source", nrow(doirg)),
               c("Articles that are copyrighted and have full text available on RG", nrow(full_copyright)),
               c("Copyrighted articles on repository", nrow(copyright_repo)),
               c("Copyrighted articles on preprint", nrow(copyright_preprint)),
               c("Copyrighted atricles on website", nrow(copyright_website)),
               c("Articles that are copyrighted and have NO full text available", nrow(copyright_no_fulltext)),
               c("Open Access articles", nrow(oa_num)),
               c("HCISI articles", nrow(hcisi)),
              c("Copyright #", nrow(copyright_yes)))

table <- data.frame(table) 
colnames(table) <- c("Question", "Number Matches")

write.csv(source_web, "DocFilesAndrea/web.csv", row.names = FALSE)



#Data visualization
#Plot article types
par("mar" = c(3,10.5,3,3))
slices <- article_type$freq
lbs <- article_type$ItemClass
barplot(slices, names.arg=lbs, main="What Types of Articles Were Cited?", col=rainbow(length(lbs)), horiz = TRUE, axes = TRUE, cex.names=.8, las=1, xlim = c(0,200))

#Plot RG in Citation
par("mar" = c(5,8,3,3))
slices <- cited_as$freq
lbs <- cited_as$Cited_As
barplot(slices, names.arg=lbs, main="How does ResearchGate Appear in Citations?", col=rainbow(length(lbs)), axes = TRUE, horiz = TRUE, cex.names =1, las = 1, xlab = "# of Instances in Citations")

slices <- table$`Number Matches`
lbs <- table$Question
barplot(unlist(slices), names.arg=as.matrix(lbs), main="How was ResearchGate Cited?", col=rainbow(length(lbs)), axes = FALSE, cex.names =1, ylab = "# of Instances in Citations", xlab = "How RG Appears in the Citation", horiz = TRUE, las = 2)

copyrightplot <- c(nrow(copyright_yes),
                   nrow(oa_num),
                   nrow(hcisi))
copyrightlbs <- c ("Copyrighted Articles",
                   "Open Access Articles",
                   "How-Can-I-Share-It Articles")
barplot(unlist(copyrightplot), names.arg=as.matrix(copyrightlbs), main="Copyright", col=rainbow(length(lbs)), axes = TRUE, xlim = c(0,100), cex.names =.8, horiz = TRUE, las = 1)

#Top 10 Websites
par("mar" = c(3,8,3,3))
slices <- source_web$freq[11:2]
lbs <- source_web$data[11:2]
barplot(unlist(slices), names.arg=as.matrix(lbs), main="Top 10 other websites original fulltext was found on", col=rainbow(length(lbs)), axes = TRUE, cex.names =1, cex.main = 1.5 , horiz = TRUE, las = 1, xlim = c(0,50))


