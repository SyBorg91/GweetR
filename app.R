#############################################################
#' GweetR - A Sentiment analysis app
#' Author : Satyabrat Borgohain
############################################################

#Libraries & dependencies
library(shiny)
library(shinyjs)
library(shinydashboard)
library(gtrendsR)
library(twitteR)
library(stringr)
library(tidytext)
library(tidyr)
library(dplyr)
library(DT)
library(leaflet)
library(markdown)
library(geojsonio)
library(visNetwork)
library(d3wordcloud)
library(d3heatmap)
library(RColorBrewer)
library(streamgraph)

#Variable definitions :
geocodes <- list(
  Brisbane = c("-27.4697707,153.0251235,200km"),
  Adelaide = c("-34.9284989,138.6007456,200km"),
  Wollongong = c("-34.4278121,150.8930607,200km"),
  Perth = c("-31.9505269,115.8604572,200km"),
  Sydney = c("-33.8688197,151.2092955,200km"),
  Melbourne = c("-37.8136276,144.9630576,200km"),
  Newcastle = c("-32.9282712,151.7816802,200km"),
  Canberra = c("-35.2809368,149.1300092,200km")
)
au.cities <- c(
  "Brisbane",
  "Adelaide",
  "Wollongong",
  "Perth",
  "Sydney",
  "Melbourne",
  "Newcastle",
  "Canberra"
)
lon.vec <- c(
  153.025124,
  138.600746,
  150.893061,
  115.860457,
  151.209296,
  144.963058,
  151.781680,
  149.130009
)
lat.vec <- c(
  -27.469771,-34.928499,-34.427812,-31.950527,-33.868820,-37.813628,-32.928271,-35.280937
)
city.df <-
  data.frame(
    location = unlist(au.cities),
    lon = unlist(lon.vec),
    lat = unlist(lat.vec)
  )

#Function definitions :
#To fetch the current google trends
get.trends <- function() {
  html_content <-
    readLines("https://trends.google.com/trends/hottrends/atom/feed?pn=p8")
  str1 <- "<title>"
  str2 <- "</title>"
  raw.trends <-
    str_extract_all(html_content, paste(str1, "(.*)", str2, sep = ""))
  tidy.trends <- c()
  for (trend in raw.trends) {
    trend <- gsub(str1, "", trend)
    trend <- gsub(str2, "", trend)
    tidy.trends <- c(tidy.trends , trend)
  }
  tidy.trends <- tidy.trends[-1]
  top.trends <- tidy.trends
  return(top.trends)
}
#Twitter authentication for app
get.auth <- function() {
  #Reading credentials from confidential authentication file
  creds <- read.csv(file = "authentication/auth.csv")
  setup_twitter_oauth(
    creds$consumer_key,
    creds$consumer_secret,
    creds$access_token,
    creds$access_secret
  )
}
#To fetch tweets of a selected trend
get.tweets <- function(trend, max_tweets) {
  tweet.df <-
    data.frame(
      "id" = character(),
      "keyword" = character(),
      "tweet" = character(),
      "created" = character(),
      "retweet" = character(),
      "location" = character(),
      stringsAsFactors = FALSE
    )
  for (city in au.cities) {
    tweets <- searchTwitter(
      searchString = trend,
      n = as.numeric(max_tweets),
      lang = "en",
      geocode = geocodes[city]
    )
    if (length(tweets) != 0) {
      for (i in 1:length(tweets)) {
        tweet.id <- tweets[[i]]$id
        keyword <- trend
        tweet <- tweets[[i]]$text
        created.date <- as.character(tweets[[i]]$created)
        is.retweet <- tweets[[i]]$isRetweet
        location <- city
        tweet.df <-
          rbind(
            tweet.df,
            data.frame(
              tweet.id,
              keyword,
              tweet,
              created.date,
              is.retweet,
              location,
              stringsAsFactors = FALSE
            )
          )
      }
    }
  }
  if (nrow(tweet.df) > 0) {
    tweet.df <- filter(tweet.df, tweet.df$is.retweet == FALSE)
    tweet.df <- subset(tweet.df, select = -c(is.retweet))
  }
  return(tweet.df)
}
#To preprocess & analyse sentiments of the tweets
get.sentiments <- function(tweet.df) {
  #Cleaning tweets
  tweet.df$tweet <- gsub("&amp", "", tweet.df$tweet)
  tweet.df$tweet <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweet.df$tweet)
  tweet.df$tweet <- gsub("@\\w+", "", tweet.df$tweet)
  tweet.df$tweet <- gsub("[[:punct:]]", "", tweet.df$tweet)
  tweet.df$tweet <- gsub("[[:digit:]]", "", tweet.df$tweet)
  tweet.df$tweet <- gsub("http\\w+", "", tweet.df$tweet)
  tweet.df$tweet <- gsub("[ \t]{2,}", "", tweet.df$tweet)
  tweet.df$tweet <- gsub("[ \t]{2,}", "", tweet.df$tweet)
  tweet.df$tweet <- gsub("^\\s+|\\s+$", "", tweet.df$tweet)
  tweet.df$tweet <- gsub("\n", "", tweet.df$tweet)
  tweet.df$tweet <- iconv(tweet.df$tweet, "latin1", "ASCII", sub = "")
  tweet.df$tweet <- str_replace_all(tweet.df$tweet, "[^[:graph:]]", " ") 
  # Making all letters lowercase
  tweet.df$tweet <- tolower(tweet.df$tweet)
  #for POSIXlt to POSIXct
  tweet.df$created.date <- as.POSIXct(tweet.df$created.date,
                                      format = "%Y-%m-%d %H:%M:%S")
  tweet.df$date <- as.Date(tweet.df$created.date)
  tweet.df$time <- format(tweet.df$created.date, "%H:%M")
  #Simplified time
  tweet.df$simple.time <-
    paste(substr(tweet.df$time, 1, 2), "00", sep = ":")
  #Adding lon-lat information to the dataframe
  tweet.df <- inner_join(x = tweet.df, y = city.df, by = 'location')
  # Tokenization of the tweets
  tokenized.tweet.df <- unnest_tokens(tweet.df, word, tweet)
  #sentiment analysis (using three lexicons : bing, nrc, afinn)
  sentiments.df <- inner_join(x = tokenized.tweet.df,
                    y = get_sentiments(lexicon = "bing"),
                    by = 'word')
  sentiments.df <- inner_join(x = sentiments.df,
                     y = get_sentiments(lexicon = "nrc"),
                     by = 'word')
  sentiments.df <- inner_join(x = sentiments.df,
                     y = get_sentiments(lexicon = "afinn"),
                     by = 'word')
  names(sentiments.df)[names(sentiments.df) == 'sentiment.x'] <- 'bing'
  names(sentiments.df)[names(sentiments.df) == 'sentiment.y'] <- 'nrc'
  names(sentiments.df)[names(sentiments.df) == 'score'] <- 'afinn'
  return(sentiments.df)
}
#For maps
get.map <- function(sentiments.df) {
  region <- geojsonio::geojson_read("aus.geojson", what = "sp")
  
  MAPBOX_ACCESS_TOKEN <-
    'pk.eyJ1Ijoic3lib3JnIiwiYSI6ImNqZnhidmliZDA0N3AycW9iOXd1NmNrc24ifQ.mYid4aPTyQw47yWMXfSKyg'
  
  
  # Create a map with the locations
  if (is.data.frame(sentiments.df) && nrow(sentiments.df) == 0) {
    
    sentiment.map  <- leaflet(region) %>%
      setView(lat = -27.833,
              lng =  133.583,
              zoom = 4) %>%
      addProviderTiles("MapBox",
                       options = providerTileOptions(
                         id = "mapbox.light",
                         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')
                       )) %>%
      addMarkers(
        lng = lon.vec,
        lat = lat.vec,
        label = au.cities,
        labelOptions = labelOptions(
          textsize = "15px",
          opacity = 0.7,
          direction = "top"
        )
      ) %>%
      addPolygons(
        weight = 1,
        opacity = 1,
        color = "grey",
        dashArray = "2"
      )
    
    
  }
  else{
    sent.trend <- unique(subset(sentiments.df, select = tweet.id:bing))
    pos.score <- sent.trend %>%
      filter(bing == "positive") %>%
      group_by(location) %>%
      mutate(score = 1) %>%
      summarise(score = sum(score))
    
    neg.score <- sent.trend %>%
      filter(bing == "negative") %>%
      group_by(location) %>%
      mutate(score = -1) %>%
      summarise(score = sum(score))
    
    tot.score <- full_join(pos.score, neg.score) %>%
      group_by(location) %>%
      summarise(score = sum(score))
    icons <-
      iconList(
        sentiment1 = makeIcon(
          "1.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        sentiment2 = makeIcon(
          "2.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        sentiment3 = makeIcon(
          "3.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        sentiment4 = makeIcon(
          "4.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        sentiment5 = makeIcon(
          "5.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        sentiment6 = makeIcon(
          "6.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        sentiment7 = makeIcon(
          "7.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        sentiment8 = makeIcon(
          "8.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        sentiment9 = makeIcon(
          "9.png",
          iconWidth = 32,
          iconHeight = 32
        )
      )
    # Logic for assigning emojis to sentiments
    sentiment <- rep("", nrow(tot.score))
    sentiment[tot.score$score >= 100] <- "sentiment1"
    sentiment[tot.score$score < 100 & tot.score$score >= 50] <- "sentiment2"
    sentiment[tot.score$score < 50 & tot.score$score >= 20] <- "sentiment3"
    sentiment[tot.score$score < 20 & tot.score$score > 0] <- "sentiment4"
    sentiment[tot.score$score == 0] <-"sentiment5"                          
    sentiment[tot.score$score < 0 & tot.score$score >= -20] <- "sentiment6"
    sentiment[tot.score$score < -20 & tot.score$score >= -50] <- "sentiment7"
    sentiment[tot.score$score < -50 & tot.score$score >= -100] <- "sentiment8"
    sentiment[tot.score$score < -100] <-"sentiment9"
    
    tot.score$sentiment <- sentiment
    tot.score$score <- as.character(tot.score$score)
    score.df <- merge(tot.score, city.df)
    
    sentiment.map  <- leaflet(region) %>%
      setView(lat = -27.833,
              lng =  133.583,
              zoom = 4) %>%
      addProviderTiles("MapBox",
                       options = providerTileOptions(
                         id = "mapbox.light",
                         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')
                       )) %>%
      addMarkers(
        lng = score.df$lon,
        lat = score.df$lat,
        label = score.df$location,
        icon = icons[score.df$sentiment],
        popup = score.df$score,
        labelOptions = labelOptions(
          textsize = "15px",
          opacity = 0.7,
          direction = "top"
        )
      ) %>%
      addPolygons(
        weight = 1,
        opacity = 1,
        color = "grey",
        dashArray = "2"
      )
  }
  return(sentiment.map)
}
#For creating streamgraph
create.streamgraph <- function(sentiments.df) {
  stream.df <- unique(subset(sentiments.df, select = tweet.id:word))
  
  stream.df %>%
    group_by(date, word) %>%
    tally() %>%
    ungroup() %>%
    streamgraph(key = 'word',
                value = 'n',
                date = "date") %>%
    sg_legend(show = TRUE, label = "word : ") %>%
    sg_axis_x(1, "date", "%d-%m")
}
#For creating wordcloud
create.wordcloud <- function(sentiments.df, mode) {
  runjs("$('#wordcloud svg g').empty()")
  words <- unique(subset(sentiments.df, select = tweet.id:word))
  
  if (mode == '1') {
    df <- words %>%
      group_by(word) %>%
      count()
    wordcloud <-
      d3wordcloud(
        df$word,
        df$n,
        tooltip = TRUE,
        size.scale = "log",
        font = "Anton",
        padding = 7
      )
  }
  else if (mode == '2') {
    df <-
      subset(unique(subset(sentiments.df, select = tweet.id:bing)), bing == "positive") %>%
      group_by(word) %>%
      count()
  
      wordcloud <-
        d3wordcloud(
          df$word,
          df$n,
          tooltip = TRUE,
          size.scale = "log",
          font = "Anton",
          padding = 7,
          colors = '#3fbf81'
        )
  }
  else{
    df <-
      subset(unique(subset(sentiments.df, select = tweet.id:bing)), bing == "negative") %>%
      group_by(word) %>%
      count()

    wordcloud <-
      d3wordcloud(
        df$word,
        df$n,
        tooltip = TRUE,
        size.scale = "log",
        font = "Anton",
        padding = 7,
        colors = '#9c1276'
      )
  }
  return(wordcloud)
}
#For creating network
create.network <- function(sentiments.df) {
  emotions <-
    c('anticipation',
      'trust',
      'anger',
      'joy',
      'fear',
      'sadness',
      'surprise',
      'disgust')
  emo.id <- seq(1, 8)
  sent.emo <-
    subset(sentiments.df, sentiments.df$nrc %in% emotions)
  
  d <- sent.emo %>%
    group_by(word, nrc) %>%
    count(word) %>%
    ungroup() %>%
    filter(n > 2) %>%
    mutate(word = reorder(word, n)) %>%
    arrange(nrc, n)
  
  nodes.df <-
    data.frame(node = emotions, stringsAsFactors = FALSE)
  
  words <- d %>%
    distinct(word) %>%
    rename(node = word)
  
  nodes.df <- rbind(nodes.df, words)
  nodes.df  <-
    mutate(nodes.df , id = as.integer(rownames(nodes.df)))
  
  temp <- nodes.df %>%
    rename(word = node) %>%
    right_join(d, by = 'word')
  
  edges <-  temp %>%
    rename(node = nrc) %>%
    left_join(nodes.df, by = 'node') %>%
    rename(to = id.x) %>%
    rename(from = id.y) %>%
    select('from', 'to')
  
  nodes.df <- nodes.df %>%
    mutate(title = node) %>%
    mutate(label = node) %>%
    mutate(shape = ifelse(id > 8, NA, 'box')) %>%
    mutate(color = ifelse(id > 8, '#3498db', NA))
  
  nodes.df <- nodes.df %>%
    rename(group = node) %>%
    mutate(group = ifelse(id > 8, NA, group))
  
  nodes <- nodes.df
  
  visNetwork(nodes, edges, height = "100%", width = "100%") %>%
    visOptions(highlightNearest = list(
      enabled = T,
      degree = 1,
      hover = T
    )) %>%
    visInteraction(keyboard = TRUE)
}
#For creating the heatmap
create.heatmap <- function(sentiments.df) {
  
  sent.afinn <-
    unique(subset(sentiments.df, select = c(tweet.id:word, afinn)))
  
  sent.df <- data.frame(
    time = sent.afinn$simple.time,
    date = sent.afinn$date,
    afinn = sent.afinn$afinn
  )
  
  sent.df <- sent.df %>%
    group_by(date, time) %>%
    summarise(afinn = sum(afinn)) %>%
    spread(time, afinn)
  
  sent.df[is.na(sent.df)] <- 0
  sent.df <- sent.df %>%
    arrange(date)
  
  indices <- sent.df$date
  if (length(indices)>1){
    sent.df <- sent.df[,-(1)]
    
    rownames(sent.df) <- indices
    
    d3heatmap(
      sent.df,
      dendrogram = 'none',
      xaxis_font_size = "12px",
      yaxis_font_size = "12px",
      Rowv = FALSE,
      Colv = FALSE,
      scale = "row",
      colors = "RdYlBu"
    )
  }
  else{
    return('NA')
  }
  

}
#For onload loading
onload <- function() {
  showElement(id = "ajax",
              anim = TRUE,
              animType = "fade")
  Sys.sleep(3)
  hide(id = "ajax",
       anim = TRUE,
       animType = "fade")
}
#For analysis loading
loading <- function(status) {
  if (status == TRUE) {
    hide("main")
    showElement(id = "ajax",
                anim = TRUE,
                animType = "fade")
    showElement(id = "msg",
                anim = TRUE,
                animType = "fade")
  }
  else{
    hide(id = "ajax")
    hide(id = "msg")
    showElement("main", anim = TRUE, animType = "fade")
    
  }
}
#For auto scroll down
scrollDown <- function() {
  runjs("$('html,body').animate({scrollTop: document.body.scrollHeight},1200)")
}
#For displaying error
showError <- function() {
  hide("msg")
  hide("ajax", anim = TRUE, animType = "fade")
  showElement(id = "dataError",
              anim = TRUE,
              animType = "fade")
  Sys.sleep(4)
  hide("dataError")
}
showHeatMap <- function(show){
  if (show){
    showElement('#heatmap')
  }
  else{
    hide('#heatmap')
  }
}
#Application css
appCSS <- '
/* css for app */
.leaflet-container {
background-color:rgba(255,0,0,0.0);
}
.leaflet-control-attribution{
display : none;
}
#streamPlot{
height:0px;
}
.main-header .logo {
font-family: "sans-serif";
font-weight: bold;
font-size: 30px;
}
.skin-black .main-header .logo {
background-image: -webkit-linear-gradient(left, #70e1f5,#ffd194); /* For Chrome and Safari */
background-image:    -moz-linear-gradient(left, #70e1f5,#ffd194); /* For old Fx (3.6 to 15) */
background-image:     -ms-linear-gradient(left, #70e1f5,#ffd194); /* For pre-releases of IE 10*/
background-image:      -o-linear-gradient(left, #70e1f5,#ffd194); /* For old Opera (11.1 to 12.0) */
background-image:         linear-gradient(to right,#70e1f5,#ffd194);
color:transparent;
-webkit-background-clip: text;
background-clip: text;
}
#map {
height: 480px !important;
}
#trendcloud text{
cursor: pointer;
}
#trendcloud text:hover {
fill : orange  !important;
font-size : 40px !important;
z-index : 1 !important;
-webkit-transition: fill 0.5s;
transition: fill 0.5s;
}
#wordcloud text:hover {
fill : orange  !important;
z-index : 1 !important;
-webkit-transition: fill 0.5s;
transition: fill 0.5s;
}
#trend li a {
white-space: nowrap;
width: 230px;
overflow: hidden;
text-overflow: ellipsis;
}
#maindivnetworkplot{
height : 580px !important;
}
.box {
box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
}
#analyse{
background-color:yellowgreen;color:white;
}
#analyse:hover{
box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
}
.content-wrapper,
.right-side {
background: linear-gradient(to right,#70e1f5,#ffd194);
}
.bg-aqua, .bg-purple, .bg-yellow {
cursor: default;
}
#ajax {
position: absolute;
left: 60%;
top: 50%;
z-index: 1;
margin: -75px 0 0 -75px;
border: 16px solid #f3f3f3;
border-radius: 50%;
border-top: 16px solid #3498db;
width: 100px;
height: 100px;
-webkit-animation: spin 2s linear infinite;
animation: spin 2s linear infinite;
}
@-webkit-keyframes spin {
0% { -webkit-transform: rotate(0deg); }
100% { -webkit-transform: rotate(360deg); }
}
@keyframes spin {
0% { transform: rotate(0deg); }
100% { transform: rotate(360deg); }
}
.error{
text-align: center;
color : orange;
}
#msg{
text-align: center;
-webkit-animation: color-change 2s infinite;
-moz-animation: color-change 2s infinite;
-o-animation: color-change 2s infinite;
-ms-animation: color-change 2s infinite;
animation: color-change 2s infinite;
}
@-webkit-keyframes color-change {
0% { color: #3498db; }
50% { color: white; }
100% { color: #3498db; }
}
@-moz-keyframes color-change {
0% { color: #3498db; }
50% { color: white; }
100% { color: #3498db; }
}
@-ms-keyframes color-change {
0% { color: #3498db; }
50% { color: white; }
100% { color: #3498db; }
}
@-o-keyframes color-change {
0% { color: #3498db; }
50% { color: white; }
100% { color: #3498db; }
}
@keyframes color-change {
0% { color: #3498db; }
50% { color: white; }
100% { color: #3498db; }
}
'

##########UI component of app##########
ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(
    title = 'GweetR',
    dropdownMenu(
      headerText = 'Developed by',
      icon = icon("question"),
      badgeStatus = NULL,
      messageItem(
        from = "Satyabrat Borgohain",
        icon =  icon("linkedin"),
        message = "Click to view on LinkedIn",
        href = "https://www.linkedin.com/in/satyabrat-borgohain-a802a5107/"
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      sidebarMenuOutput("trend"),
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      menuItem("Tweets", tabName = "tweets_tab", icon = icon("twitter")),
      sidebarMenuOutput("network_tab"),
      sidebarMenuOutput("word_cloud_tab"),
      sidebarMenuOutput("heat_map_tab")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(appCSS)
    ),
    tabItems(
      #1st tab - Dashboard
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("tweetBox"),
          valueBoxOutput("trendBox"),
          valueBoxOutput("citiesBox")
        ),
        #For loading and messages
        fluidRow(hidden(div(id = "ajax"),
                        div(
                          id = "msg",
                          h4("Loading tweets.. Analysing sentiments..")
                        ))),
        fluidRow(hidden(div(
          id = 'dataError', class = "error",
          h3(
            "Sorry, not enough relevant tweets for analysis. Try another trend."
          )
        ))),
        div(
          #Main div of the page
          id = "main",
          fluidRow(
            column
            (width = 9,
              d3wordcloudOutput("trendcloud")
            ),
            column(width = 3,
                   absolutePanel(
                     draggable = FALSE,
                     wellPanel(
                       HTML(markdownToHTML(
                         fragment.only = TRUE,
                         text = c(
                           "<b>GweetR</b> allows you to analyse the sentiments towards the <b>top Google trends</b> across Australia,
                    as expressed on <b>Twitter</b>.
                    <br></br>
                    <b>Click on one of the trends</b> to begin. You can also choose to view only the top 5
                    trends & change the no of tweets to fetch :"
                         )
                       )),
                       checkboxInput("checkbox", label = "Show only top 5", value = FALSE),
                       sliderInput(
                         "max_tweets",
                         "Max no. of tweets (per city):",
                         min = 100,
                         max = 500,
                         value = 100,
                         step = 50,
                         sep = ","
                       )
                     ),
                     style = "opacity: 0.8; position : relative;"
                   )
              )
            ),
          br(),
          #Map
          fluidRow(column(width = 4,
                          uiOutput("map_box")),
                   column
                   (width = 8,
                     leafletOutput("map"))),
          br(),
          #Streamgraph
          fluidRow(column
                   (width = 9,
                     uiOutput('stream')
                     ),
                   column(width = 3,
                          uiOutput('stream_box')))
          )
    ),
    #2nd tab - Tweets
    tabItem(tabName = "tweets_tab",
            fluidRow(column(
              width = 12,
              box(
                title = "Tweets",
                width = NULL,
                DT::dataTableOutput("tweets"),
                textOutput("nodata"),
                status = "info",
                solidHeader = TRUE
              )
            ))),
    #3rd tab - Network
    tabItem(tabName = "networktab",
            fluidRow(
              column(
                width = 4, absolutePanel(
                  draggable = FALSE,
                  wellPanel(
                    HTML(markdownToHTML(
                      fragment.only = TRUE,
                      text = c(
                        "<h4><b>Emotions behind words</b></h4>
This network visualises the connections between words and the emotion(s) associated with it.
                        You can <b>navigate the network, zoom, pan and also click on the words (nodes)</b>
                        to find its associations with one of the emotion and vice versa. The core emotions are represented as 
                        <b>boxes</b> and the words as <b>circles</b>."
                      )
                    ))
                  ),
                  style = "opacity: 0.8; position : relative"
                )
                
              ),
              column(width = 8,
                     uiOutput('network'))
            )),
    #4th tab - Word cloud
    tabItem(tabName = "wordcloudtab",
            fluidRow(column(
              width = 6,
              absolutePanel(
                draggable = FALSE,
                wellPanel(
                  HTML(markdownToHTML(
                    fragment.only = TRUE,
                    text = c(
                      "<h4><b>Most used words</b></h4>
It shows the most commonly used words in the tweets for the selected trend.
    You can <b>select an option</b> from the drop down menu to view positive or negative words.
                      Additionally, the occurence is displayed <b>on hovering</b> over the words."
                    )
                  )),
                  selectInput(
                    "sentiment_selected",
                    label = "",
                    choices = list(
                      "All Words" = 1,
                      "Positive Words" = 2,
                      "Negative Words" = 3
                    ),
                    selected = 1
                  )
                ),
                style = "opacity: 0.8; position : relative"
              )
            )),
            fluidRow(d3wordcloudOutput("wordcloud"))),
    #5th tab - Heat map
    tabItem(tabName = "heatmaptab",
            fluidRow(column(
              width = 5,
              absolutePanel(
                draggable = FALSE,
                wellPanel(
                  HTML(markdownToHTML(
                    fragment.only = TRUE,
                    text = c(
                      "<h4><b>Everyday moods</b></h4>
This heatmap visualises the change of the sentiments across a given day. Here the warmer colors tend to be towards the negative emotional spectrum whereas the cooler
    colors represent a relative positivity. You can <b>drag to select and zoom</b> a particular slice of time, to examine the
                      overall sentiment expressed at that moment."
                    )
                    ))
                    ),
                style = "opacity: 0.8; position : relative"
                  )
            )),
            fluidRow(column (
              width = 12, 
              d3heatmapOutput("heatmap")
            )))
    )
  )
)

##########Server component of the app##########
server <- function(input, output) {
  onload()
  no.tweets <- 0
  no.trends <- 0
  sentiments.df <- data.frame()
  all.trends <- get.trends()
  no.trends <- length(all.trends)
  #Authentication
  get.auth()
  
  #For creating the trending wordcloud
  output$trendcloud <- renderD3wordcloud({
    #To clear canvas before redrawing
    runjs("$('#trendcloud svg g').empty()")
    
    if (input$checkbox) {
      top.trends <- all.trends[1:5]
      size <- rev(seq(1, 5))
    }
    else{
      top.trends <- all.trends
      size <- rev(seq(1, 20))
    }
    text <- top.trends
    
    df <- data.frame('word' = text, 'freq' = size)
    
    d3wordcloud(
      df$word,
      df$freq,
      spiral = "rectangular",
      rotate.min = 0,
      rotate.max = 0,
      font = 'Anton',
      padding = 7,
      rangesizefont = c(19, 40)
    )
  })
  output$map_box <- renderUI({
    uiOutput('map_info')
  })
  output$tweetBox <- renderValueBox({
    valueBox(no.tweets ,
             "No. of Tweets",
             icon = icon("twitter"),
             href = "#")
  })
  output$trendBox <- renderValueBox({
    valueBox(
      no.trends,
      "Google Trends",
      icon = icon("google"),
      color = "purple",
      href = "#"
    )
  })
  output$citiesBox <- renderValueBox({
    valueBox(
      "8",
      "Australian Cities",
      icon = icon("map-marker"),
      color = "yellow",
      href = "#"
    )
  })
  output$nodata <- renderText({
    "No data to display. Make sure to select one of the trends."
  })
  output$map <- renderLeaflet({
    get.map(sentiments.df)
  })
  # Main action trigger - onlick of a word
  observeEvent(input$d3wordcloud_click, {
    if (input$d3wordcloud_click %in% all.trends) {
      loading(TRUE)
      #get the tweets for the trend
      trend.tweets <- get.tweets(input$d3wordcloud_click, input$max_tweets)
      
      if (nrow(trend.tweets) > 0) {
        trend.tweets <- subset(trend.tweets, select = -c(keyword))
        
        sentiments.df <- get.sentiments(trend.tweets)
        
        unique.words <- sentiments.df %>%
          distinct(word) %>%
          nrow()
        
        #Main validation
        if (nrow(sentiments.df) > 0 & unique.words > 4) {
          clicked.trend <- input$d3wordcloud_click
          output$nodata <- renderText({
            ""
          })
          output$map_info <- renderUI({
            absolutePanel(
              draggable = FALSE,
              wellPanel(
                HTML(markdownToHTML(
                  fragment.only = TRUE,
                  text = c(
                    "<h4><b>Sentiments Map</b></h4>
The level of sentiments are represented by the expression of the emojis as seen on the map. You can <b>click</b> on it to
              know the magnitude of the overall sentiment for a particular city. Higher the number,
                more positive is the sentiment expressed and vice versa."
                  )
                  ))
                  ),
              style = "opacity: 0.8; position : relative;"
                )
          })
          output$trend <- renderMenu({
            sidebarMenu(menuItem(
              badgeLabel = "",
              text = paste("Trend :", clicked.trend),
              icon = icon("google")
            ))
          })
          output$map <- renderLeaflet({
            get.map(sentiments.df)
          })
          #Stream graph generation
          output$stream <- renderUI({
            streamgraphOutput("streamPlot")
          })
          output$stream_box <- renderUI({
            absolutePanel(
              draggable = FALSE,
              wellPanel(
                HTML(markdownToHTML(
                  fragment.only = TRUE,
                  text = c(
                    "<h4><b>Stream of Words</b></h4>
It shows the words of interest and its variation in frequency over a period of time.
            You can <b>hover over the graph</b> to find out the words and their frequencies (displayed on the upper left corner). Alternatively, you can
            also <b>select any word</b> from the drop down below."
                  )
                ))
              ),
              style = "opacity: 0.8; position : relative;"
            )
          })
          output$streamPlot <- renderStreamgraph({
            create.streamgraph(sentiments.df)
          })
          #Heat map generation
          output$heatmap <- renderD3heatmap({
            heat.map <- create.heatmap(sentiments.df)
            if (is.character(heat.map)){
              showHeatMap(FALSE)
            }
            else{
              showHeatMap(TRUE)
              heat.map
            }
          })
          output$wordcloud <-  renderD3wordcloud({
            create.wordcloud(sentiments.df, input$sentiment_selected)
          })
          output$network <- renderUI({
            visNetworkOutput("networkplot")
          })
          output$networkplot <- renderVisNetwork({
            create.network(sentiments.df)
          })
          output$tweets <- DT::renderDataTable({
            trend.tweets %>%
              rename(`tweet id` = tweet.id) %>%
              rename(date = created.date)
          },
          options = list(
            pageLength = 10,
            lengthChange = FALSE,
            autoWidth = TRUE,
            scrollX = TRUE,
            columnDefs = list(list(
              width = '10%', targets = "_all"
            ))
          ))
          output$network_tab <- renderMenu({
            sidebarMenu(menuItem(
              "Emotion-Word Network",
              tabName = "networktab",
              icon = icon("star")
            ))
          })
          output$word_cloud_tab <- renderMenu({
            sidebarMenu(menuItem(
              "Wordcloud",
              tabName = "wordcloudtab",
              icon = icon("cloud")
            ))
          })
          output$heat_map_tab <- renderMenu({
            sidebarMenu(menuItem(
              "Heatmap",
              tabName = "heatmaptab",
              icon = icon("th")
            ))
          })
          no.tweets <- nrow(trend.tweets)
          output$tweetBox <- renderValueBox({
            valueBox(no.tweets , "No. of tweets", icon = icon("twitter"))
          })
          scrollDown()
          }
        else{
          #Not enough relevant data
          showError()
        }
        }
      else{
        #Not enough relevant data
        showError()
      }
      loading(FALSE)
  }
  })
  output$map_info <- renderUI({
    absolutePanel(
      draggable = FALSE,
      wellPanel(
        HTML(markdownToHTML(
          fragment.only = TRUE,
          text = c(
            "<h4><b>Sentiments Map</b></h4>
The map shows the 8 cities for which the sentiment analysis will be done. The tweets are retrived geographically 
            and their sentiments after the analysis will be mapped onto the blue markers."
          )
        ))
      ),
      style = "opacity: 0.8; position : relative;"
    )
  })
  }

##########Running the app##########
shinyApp(ui, server)