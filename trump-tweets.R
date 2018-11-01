library(shiny)
library(tidyr)
library(dplyr)
library(scales)
library(DT)
library(tm)
library(corpus)
library(SnowballC)
library(SentimentAnalysis)
library(sentimentr)

tweets2 <- read.csv("MyData.csv")
tweets2$Date <- as.Date(tweets2$Date, "%Y-%m-%d")
tweets2$rando <- as.numeric(tweets2$rando)
tweets2$text <- as.character(tweets2$text)
tweets2$pos_neg <- as.character(tweets2$pos_neg)
tweets2 <- tweets2 %>% group_by(Date) %>% mutate(Total = seq_along(Date)) %>% as.data.frame() #create running total that will reset bassed on date b/c of the group_by(Date)
tweets2$Total <- as.numeric(tweets2$Total)
tweets2$sentiment <- as.numeric(tweets2$sentiment)
tweets2$text <- gsub("http\\S+\\s*", "", tweets2$text)
tweets2$text <- gsub("amp", "&", tweets2$text)
for (i in 1:length(tweets2$text)) {
  if (!is.character(tweets2$text[i])) { tweets2$sentiment[i] <- NA }
  else  { tweets2$sentiment[i] <- sentiment(tweets2$text[i])$sentiment }
}
tweets2$sentiment <- as.numeric(tweets2$sentiment)
for (i in 1:length(tweets2$text)) {
  if (tweets2$sentiment[i] > 0) {tweets2$pos_neg[i] <- "POSITIVE"}
  else if (tweets2$sentiment[i] == 0) {tweets2$pos_neg[i] <- "NEUTRAL"}
  else {tweets2$pos_neg[i] <- "NEGATIVE"}
 }
write.csv(tweets2, file = "MyData.csv", row.names = FALSE)
tweets2$sentiment <- na.omit(sentiment(tweets2$text[1])$sentiment)

## Buzz word sentiment graph ##
sentGraph <- tweets2[c(2,9,10)]
guns <- grep("gun", tweets2$text) ## Find words (buzzwords) in strings, returns vector with rows that contain pattern
immigrants <- grep("immigrant", tweets2$text)
women <- grep("women", tweets2$text)
latinos <- grep("latino", tweets2$text)
nk <- grep("North Korea", tweets2$text)
liberal <- grep("liberal", tweets2$text)
democrat <- grep("democrat", tweets2$text)
republican <- grep("republican", tweets2$text)
sentGraph["guns"] <- NA ## create empty columns
sentGraph["women"] <- NA
sentGraph["immigrants"] <- NA
sentGraph["latinos"] <- NA
sentGraph["liberal"] <- NA
sentGraph["democrat"] <- NA
sentGraph["republicans"] <- NA
sentGraph[guns,]$guns <- tweets2[guns,]$sentiment ## fills columns with sentiment from specfic rows from vector above
sentGraph[immigrants,]$immigrants <- tweets2[immigrants,]$sentiment
sentGraph[latinos,]$latinos <- tweets2[latinos,]$sentiment
sentGraph[liberal,]$liberal <- tweets2[liberal,]$sentiment
sentGraph[democrat,]$democrat <- tweets2[democrat,]$sentiment
sentGraph[republican,]$republicans <- tweets2[republican,]$sentiment
sentGraph[women,]$women <- tweets2[women,]$sentiment
womendata <- subset(sentGraph, !is.na(sentGraph$women)) ## subset data so I can graph it without N/A values
gunsData <- subset(sentGraph, !is.na(sentGraph$guns))
immigrantData <- subset(sentGraph, !is.na(sentGraph$immigrants))
latinoData <- subset(sentGraph, !is.na(sentGraph$latinos))
liberalData <- subset(sentGraph, !is.na(sentGraph$liberal))
democratData <- subset(sentGraph, !is.na(sentGraph$democrat))
repubData <- subset(sentGraph, !is.na(sentGraph$republicans))

## Corpus and Text Mining ##
docs1 <- Corpus(DirSource("tm1"))
docs1 <- tm_map(docs1, content_transformer(function(x) gsub("http\\S+\\s*", "", x))) ## Removes all urls from corpus ##
docs1 <- tm_map(docs1, content_transformer(function(x) gsub("amp", "&", x))) ## fixes some reading errors of text format ##
docs1 <- tm_map(docs1, content_transformer(function(x) gsub("-", " ", x)))
docs1 <- tm_map(docs1, content_transformer(tolower))
docs1 <- tm_map(docs1, removePunctuation)
docs1 <- tm_map(docs1, removeNumbers)
docs1 <- tm_map(docs1, removeWords, stopwords("SMART"))
docs1 <- tm_map(docs1, stripWhitespace)
dtm1 <- DocumentTermMatrix(docs1, control = list(wordLengths = c(3, 20)))## corpus -> matrix, also stips the corpus of words with length 3 char or lower and 20 char or higher
freq1 <- colSums(as.matrix(dtm1)) # org terms by freq
freq2 <- as.data.frame(freq1)
freq2$word <- row.names(freq2)
