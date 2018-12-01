library(shiny);library(tidyr);library(dplyr);library(scales);library(DT);library(tm);
library(corpus);library(SnowballC);library(SentimentAnalysis);library(sentimentr);library(ggplot2);

## Tweets (All comented out code is code I already ran once and don't have to do again in this section) ##
tweets2 <- read.csv("MyData.csv")
tweets2$Date <- as.Date(tweets2$Date, "%Y-%m-%d")
tweets2$rando <- as.numeric(tweets2$rando)
tweets2$text <- as.character(tweets2$text)
tweets2$pos_neg <- as.character(tweets2$pos_neg)
# tweets2 <- tweets2 %>% group_by(Date) %>% mutate(Total = seq_along(Date)) %>% as.data.frame() #create running total that will reset bassed on date b/c of the group_by(Date)
# tweets2$Total <- as.numeric(tweets2$Total)
# tweets2$sentiment <- as.numeric(tweets2$sentiment)
# tweets2$text <- gsub("http\\S+\\s*", "", tweets2$text)
# tweets2$text <- gsub("amp", "&", tweets2$text)
# for (i in 1:length(tweets2$text))
#  {
#  if (!is.character(tweets2$text[i])) { tweets2$sentiment[i] <- NA }
#  else  { tweets2$sentiment[i] <- sentiment(tweets2$text[i])$sentiment }
# }
# tweets2$sentiment <- as.numeric(tweets2$sentiment)
# for (i in 1:length(tweets2$text))
# {
#  if (tweets2$sentiment[i] > 0) {tweets2$pos_neg[i] <- "POSITIVE"}
#  else if (tweets2$sentiment[i] == 0) {tweets2$pos_neg[i] <- "NEUTRAL"}
#  else {tweets2$pos_neg[i] <- "NEGATIVE"}
# }
# write.csv(tweets2, file = "MyData.csv", row.names = FALSE)
# tweets2$sentiment <- na.omit(sentiment(tweets2$text[1])$sentiment)

### This code used to make the sentiment reactive but is no longer necessary with the newly written csv ###
# sentiment <- analyzeSentiment(tweets2$text[d])  
# if (sentiment$SentimentQDAP < 0)
# { w <- "NEGATIVE"}
# else if (sentiment$SentimentQDAP == 0)
# { w <- "NEUTRAL"}
# sentiment <- convertToBinaryResponse(sentiment)

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

## TO DO: Create a sidebar panel for FEC Output
ui <- fluidPage(
  dateRangeInput(inputId = "date1", 
                 strong("Date Range"),
                 start = "2017-03-01", end = "2018-03-01",
                 min = "2009-05-01", max ="2018-03-01"),  
  fluidRow(
    splitLayout(cellWidths = c("50%", "50%"),
                plotOutput("TrumpGraph",
                           click = "plotClick"),
                plotOutput("sentiments"))),
  fluidRow(
    column(width = 6,
           verbatimTextOutput("click_info")),
    column(width = 6,
           verbatimTextOutput("click_info2"))),
  fluidRow(
    column(width = 12,
           dataTableOutput("click_info1"))
  ),
  fluidRow(
    plotOutput("spec_sents")
  )
)

server <- function(input, output) {
  
  output$TrumpGraph <- renderPlot({
    ggplot(tweets2, aes(x = Date, y = count, fill = Total)) +        
      geom_bar(stat = "identity", position = "stack") +
      scale_x_date(limits = c(input$date1[1], input$date1[2])) +  ###### X-Axis for reactive date range (REACTIVITY WON'T WORK WITHOUT THIS) #######
    scale_fill_gradient(low="yellow", high="red") + 
      theme(panel.background = element_rect(fill = "grey"), axis.text.x = element_text(face = "bold"))
  })
  
  output$sentiments <- renderPlot({
    ggplot(tweets2, aes(x = Date, y = sentiment, fill = pos_neg, group = pos_neg)) +        
      geom_line(aes(color = pos_neg), size = 0.6) +
      scale_x_date(limits = c(input$date1[1], input$date1[2])) +  ###### X-Axis for reactive date range (REACTIVITY WON'T WORK WITHOUT THIS) #######
    theme(panel.background = element_rect(fill = "grey"), axis.text.x = element_text(face = "bold"))
  })
  
  output$spec_sents <- renderPlot({
    ggplot(sentGraph, aes(x = Date)) +
      geom_line(data = womendata, aes(x = Date, y = women, color = "women")) +
      geom_line(data = gunsData, aes(x = Date, y = guns, color = "guns")) +
      geom_line(data = immigrantData, aes(x = Date, y = immigrants, color = "immigrants")) +
      geom_line(data = latinoData, aes(x = Date, y = latinos, color = "latinos")) +
      geom_line(data = liberalData, aes(x = Date, y = liberal, color = "liberal")) +
      geom_line(data = democratData, aes(x = Date, y = democrat, color = "democrat")) +
      geom_line(data = repubData, aes(x = Date, y = republicans, color = "republican")) +
      geom_point(data = womendata, aes(x = Date, y = women, color = "women")) +
      geom_point(data = gunsData, aes(x = Date, y = guns, color = "guns")) +
      geom_point(data = immigrantData, aes(x = Date, y = immigrants, color = "immigrants")) +
      geom_point(data = latinoData, aes(x = Date, y = latinos, color = "latinos")) +
      geom_point(data = liberalData, aes(x = Date, y = liberal, color = "liberal")) +
      geom_point(data = democratData, aes(x = Date, y = democrat, color = "democrat")) +
      geom_point(data = repubData, aes(x = Date, y = republicans, color = "republican")) +
      geom_hline(yintercept = 0, linetype ="dashed") + ylab("Sentiment") +
      scale_y_continuous(limits = c(-1.25, 1.25)) + ## ADD a + here if you want to go on
      ggplot2::annotate("text", x = min(sentGraph$Date), y = 0, vjust= -0.4, label = "Positive") + ## THERE IS A PROBLEM HERE THAT ANOTHER PACKAGE IS MASKING GGPLOT'S ANNOTATE #
      ggplot2::annotate("text", x = min(sentGraph$Date), y = 0, vjust= 1.2, label = "Negative")
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
  
  output$click_info2 <- renderText({
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
    w <- tweets2$pos_neg[d]
    HTML("The sentiment of this tweet is:", w)
  })
  
  output$click_info1 <- renderDataTable({
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
    q <- Corpus(VectorSource(tweets2$text[d]))
    q <- tm_map(q, content_transformer(function(x) gsub("http\\S+\\s*", "", x))) ## Removes all urls from corpus ##
    q <- tm_map(q, content_transformer(function(x) gsub("amp", "&", x))) ## fixes some reading errors of text format ##
    q <- tm_map(q, content_transformer(function(x) gsub("-", " ", x))) ## fixes some reading errors of text format ##
    q <- tm_map(q, content_transformer(tolower))
    q <- tm_map(q, removePunctuation)
    q <- tm_map(q, removeNumbers)
    q <- DocumentTermMatrix(q, control = list(wordLengths = c(4, 20)))
    qq <- as.matrix(q)
    qqq <- as.data.frame(qq)
    datatable(qqq)
  })
  
}

shinyApp(ui = ui, server = server)
