library(ggplot2)
#library(ROAuth)
library(twitteR)
library(streamR)
library(textcat)
#library(stringi)

# http://www.r-bloggers.com/intro-to-text-analysis-with-r/
# https://sites.google.com/site/miningtwitter/questions/sentiment/sentiment

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

my_oauth <- OAuthFactory$new(consumerKey = Sys.getenv("consumerKey"), 
                             consumerSecret = Sys.getenv("consumerSecret"), 
                             requestURL = requestURL, 
                             accessURL = accessURL, 
                             authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))


i <- 1
while(TRUE) {
  file <- paste0("10-api/tweets/tweets", i, ".json")
  track <- "AUTNED" # Austria vs. Netherlands (soccer)
  follow <- NULL
  loc <- NULL
  lang <- NULL
  minutes <- 1
  time <- 60*minutes
  tweets <- NULL
  filterStream(file.name = file, 
               track = track,
               follow = follow, 
               locations = loc, 
               language = lang,
               timeout = time, 
               tweets = tweets, 
               oauth = my_oauth,
               verbose = TRUE)
  i <- i + 1
  #tweets.df <- parseTweets(file)  
}

# Spielstart: 20:35 tweets43-tweets44
# 9. 0:1 Vincent Janssen 
# 20. Gelbe Karte Vincent Janssen
# 45. Pause - wie lange?
# 66. 0:2 Georginio Wijnaldum 

# Spielernamen

# tweet.files <- list.files("10-api/tweets/", full.names = TRUE)

n <- 157
tweets.df <- NULL
for (i in 1:n) {
  file <- paste0("10-api/tweets/tweets", i, ".json")
  new.tweets <- NULL
  
  tryCatch({ 
    new.tweets <- parseTweets(file)
    new.tweets$moment <- i
  }, error = function(e) {
    print(paste(i, e))
  })
  
  tweets.df <- rbind(tweets.df, new.tweets)
}

#### languages ####

tweets.df$detected_lang <- ""
for (i in 1:nrow(tweets.df)) {
  tryCatch({
    tweets.df[i, ]$detected_lang <- textcat(tweets.df[i, ]$text)
  }, error = function(e) {
    tweets.df[i, ]$detected_lang <- "und"
  })  
}

# http://stackoverflow.com/questions/15748190/emoticons-in-twitter-sentiment-analysis-in-r
tweets.df$prep_text <- tolower(sapply(tweets.df$text,function(row) iconv(row, "latin1", "ASCII", sub="")))

lang_table <- table(tweets.df$lang, tweets.df$detected_lang)
heatmap(lang_table)

#### plots ####

# http://stackoverflow.com/questions/31789232/visualize-count-in-a-line-graph-with-ggplot2

start_tweet <- 43
goal_1 <- 9
goal_2 <- 66
hftime_break <- 17

events <- data.frame("tweet.nr"=c(start_tweet, start_tweet + goal_1, start_tweet + hftime_break + goal_2, start_tweet + hftime_break + 90 + 4), "event.type"=c("start", "goal", "goal", "end"))

# http://stackoverflow.com/questions/12545322/add-vline-to-existing-plot-and-have-it-appear-in-ggplot2-legend

ggplot(data=tweets.df, aes(x=moment)) + 
  geom_bar(aes(fill=..count..)) + 
  geom_vline(data=events, aes(xintercept = tweet.nr, colour = event.type), show.legend=TRUE)

