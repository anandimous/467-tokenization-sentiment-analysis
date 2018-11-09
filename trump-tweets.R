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

## FEC Data ##
fec <- read.csv("Trump.FEC.Data.Large.csv")
fec$amount <- as.numeric(fec$amount)
fec$Date <-as.Date(fec$Date, "%m/%d/%Y")
fec <- fec %>% group_by(committee_name) %>% mutate(total_amount = cumsum(amount)) %>% as.data.frame() # create running total all the way down
fec$total_amount <- as.numeric(fec$total_amount)
fec[1:8,10:12] <- format(as.character(fec[1:8,10:12]))

#ap <- Approval.Raiting
#ap$Date <- as.Date(ap$Date, "%m/%d/%Y")

## Random Variables ##
j <- as.Date("1970-01-01")
pal = brewer.pal(8,"Dark2")

ui <- fluidPage(
   dateRangeInput(inputId = "date1", 
                 strong("Date Range"),
                 start = "2017-03-01", end = "2018-03-01",
                 min = "2009-05-01", max ="2018-03-01"),  
  fluidRow(
    splitLayout(cellWidths = c("50%", "50%"),
    plotOutput("TrumpGraph",
             click = "plotClick")))
)

## Things to Add:
##   1. Create a CORPUS of Trumps tweets (e.g. Wordcount, sentiment analysis, possibly co-occurence), load from vector
##       a. lowercase, strip: white space, special characters, stemming(?), numbers, punctuation
##   2. Create another text box that will output a WORD COUNT of words in that tweet of words used over the whole corpus
##       a. renderTable(?)
##   3. Create SENTIMENT ANALYSIS graph:
##       a. Tweet: overwhemeling pos/neg
##       b. Of whole corpus, possible line graph of positive/negative over TPD
##   4. WordCloud (of corpus/tweet?)
##   5. Connect words (in corpus/tweets) to donations (HOW???)
##   6. Change in sentiment toward certian things
##       a. add search bar and plot results (e.g. - guns)   

server <- function(input, output) {
  
  output$TrumpGraph <- renderPlot({
    ggplot(tweets2, aes(x = Date, y = count, fill = Total)) +        
      geom_bar(stat = "identity", position = "stack") +
      #geom_line(data = ap, aes(x = Date, y = Trump, fill = Trump)) +
      #scale_y_continuous(name = "Tweets Per Day", sec.axis = (~. *100 / 160)) +
      scale_x_date(limits = c(input$date1[1], input$date1[2])) +  ###### X-Axis for reactive date range (REACTIVITY WON'T WORK WITHOUT THIS) #######
      scale_fill_gradient(low="yellow", high="red") + 
      theme(panel.background = element_rect(fill = "grey"), axis.text.x = element_text(face = "bold"))
  })

  ## Tweet text click output ##
  output$click_info <- renderText({
      t <- input$plotClick$x
      xx <- input$plotClick$x
      t <- as.character(t)
      yy <- strsplit(t, "\\.")
      z <- yy[[1]][[2]]
      z <- substr(z, 1, 1)
      z <- as.numeric(z)
      x <- as.numeric(t)
      if (z <= 5) {x <- x - 1}
      x <- floor(as.numeric(x))
      y <- input$plotClick$y
      y <- ceiling(as.numeric(y))
      d <- which((tweets2$rando == x) & (tweets2$Total == y), arr.ind = TRUE)
      d <- as.numeric(d)
      i <- tweets2$text[d]
      HTML("Tweet:\n",i) 
    })