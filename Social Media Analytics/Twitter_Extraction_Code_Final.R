setwd("C:/Users/mma/Downloads/MBD/Semester 2/SOCIAL MEDIA ANALYTICS/Group Project")

# API Keys
source("C:/Users/mma/Downloads/MBD/Semester 2/SOCIAL MEDIA ANALYTICS/APIKeys.R")

# Load packages
if(!require("httr")) install.packages("httr"); library("httr")
if(!require("jsonlite")) install.packages("jsonlite"); library("jsonlite")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
if(!require("tidytext")) install.packages("tidytext"); library("tidytext")
if (!require("textdata")) install.packages("textdata") ; library("textdata")
if (!require("ggplot2")) install.packages("ggplot2") ; library("ggplot2")
if (!require("Matrix")) install.packages("Matrix") ; library("Matrix")
if (!require("irlba")) install.packages("irlba", quiet=TRUE) ; require("irlba")
if (!require("randomForest")) install.packages("randomForest", quiet=TRUE) ; require("randomForest")
if (!require("ROCR")) install.packages("ROCR", quiet=TRUE) ; require("ROCR")
if (!require("hunspell")) install.packages("hunspell", quiet=TRUE) ; require("hunspell")
if (!require("purrr")) install.packages("purrr", quiet=TRUE) ; require("purrr")
if (!require("SnowballC")) install.packages("SnowballC", quiet=TRUE) ; require("SnowballC")
if (!require("wordcloud")) install.packages("wordcloud"); library("wordcloud")
if (!require("tm")) install.packages("tm"); library("tm")
if (!require("topicmodels")) install.packages("topicmodels", quiet=TRUE) ; require("topicmodels")
if (!require("stringr")) install.packages("stringr", quiet=TRUE) ; require("stringr")

#########################
# Get Microsoft User ID #
#########################

# Get Microsoft User ID on Twitter first
url_for_id <- modify_url(
  url = "https://api.twitter.com",
  path = c("2", "users","by","username","microsoft")
)
resUser <- GET(url = url_for_id,add_headers(authorization = paste0("Bearer ",BearerToken)))
msid<- fromJSON(httr::content(resUser, "text"))
msid<-msid$data$id

write.csv(x=msid,file="msid")


tt_urls <- modify_url(
  url = "https://api.twitter.com",
  path = c("2", "users",msid,"tweets"),
  query = list(
    max_results = 100,
    tweet.fields="id,lang,text,created_at,author_id,conversation_id,in_reply_to_user_id"#,entities,public_metrics,referenced_tweets taken out first
  ))

##################
# Tweet timeline #
##################
number_of_tweets <- 3400
tt_AllTweets <- list()
tt_meta <- list()
for(i in 1:(number_of_tweets/100)){
  print(i)
  if(i == 1){
    
    url_complete <- modify_url(
      url = "https://api.twitter.com",
      path = c("2", "users",msid,"tweets"),
      query = list(
        max_results = 100,
        expansions = "author_id",
        tweet.fields="id,lang,text,created_at,author_id,conversation_id,in_reply_to_user_id,public_metrics,referenced_tweets",
        user.fields = "description,id,location,name,verified"
      ))
    
    tt_resTweets <- GET(url = url_complete,add_headers(authorization = paste0("Bearer ",BearerToken)))
    tt_AllTweets[[i]] <- fromJSON(httr::content(tt_resTweets, "text"),flatten=T)$data
    tt_meta[[i]] <- fromJSON(httr::content(tt_resTweets, "text"))$meta
    
  } else {
    if(sum(grepl("next_token",names(tt_meta[[i-1]])))){
      url_complete <- modify_url(
        url = "https://api.twitter.com",
        path = c("2", "users",msid,"tweets"),
        query = list(
          max_results = 100,
          expansions = "author_id",
          tweet.fields="id,lang,text,created_at,author_id,conversation_id,in_reply_to_user_id,public_metrics,referenced_tweets",
          user.fields = "description,id,location,name,verified",
          pagination_token = tt_meta[[i-1]]$next_token
        ))
      
      tt_resTweets <- GET(url = url_complete,add_headers(authorization = paste0("Bearer ",BearerToken)))
      tt_AllTweets[[i]] <- fromJSON(httr::content(tt_resTweets, "text"),flatten=T)$data
      tt_meta[[i]] <- fromJSON(httr::content(tt_resTweets, "text"))$meta
    }
  }
  
}

# Now we will bind all the data elements together using the rbindlist function from data.table
tt_c <- rbindlist(tt_AllTweets,use.names=T)

save(tt_c, file = "tt_Microsoft1.RData")

####################
# Mention timeline #
####################

number_of_tweets <- 3400
mt_AllTweets <- list()
mt_meta <- list()
for(i in 1:(number_of_tweets/100)){
  print(i)
  if(i == 1){
    
    url_complete <- modify_url(
      url = "https://api.twitter.com",
      path = c("2", "users",msid,"mentions"),
      query = list(
        max_results = 100,
        expansions = "in_reply_to_user_id",
        tweet.fields="id,lang,text,created_at,author_id,conversation_id,in_reply_to_user_id,public_metrics,referenced_tweets",
        user.fields = "description,id,location,name,verified"
      ))
    
    mt_resTweets <- GET(url = url_complete,add_headers(authorization = paste0("Bearer ",BearerToken)))
    mt_AllTweets[[i]] <- fromJSON(httr::content(mt_resTweets, "text"),flatten=T)$data
    mt_meta[[i]] <- fromJSON(httr::content(mt_resTweets, "text"))$meta
    
  } else {
    if(sum(grepl("next_token",names(mt_meta[[i-1]])))){
      url_complete <- modify_url(
        url = "https://api.twitter.com",
        path = c("2", "users",msid,"mentions"),
        query = list(
          max_results = 100,
          expansions = "in_reply_to_user_id",
          tweet.fields="id,lang,text,created_at,author_id,conversation_id,in_reply_to_user_id,public_metrics,referenced_tweets",
          user.fields = "description,id,location,name,verified",
          pagination_token = mt_meta[[i-1]]$next_token
        ))
      
      mt_resTweets <- GET(url = url_complete,add_headers(authorization = paste0("Bearer ",BearerToken)))
      mt_AllTweets[[i]] <- fromJSON(httr::content(mt_resTweets, "text"),flatten=T)$data
      mt_meta[[i]] <- fromJSON(httr::content(mt_resTweets, "text"))$meta
    }
  }
  
}

# Now we will bind all the data elements together using the rbindlist function from data.table
mt_c <- rbindlist(mt_AllTweets,use.names=T)
save(mt_c, file = "mt_Microsoft1.RData")

# The following code allows to get the information about the users on the mentioned tweets 
mt_e <- fromJSON(httr::content(mt_resTweets, "text"),flatten=T)$includes$users
save(mt_e, file = "mt_Users.RData")

###########
# Hashtag #
###########

number_of_tweets <- 3400
ht_AllTweets <- list()
ht_meta <- list()
for(i in 1:(number_of_tweets/100)){
  print(i)
  if(i == 1){
    
    url_complete <- modify_url(
      url = "https://api.twitter.com",
      path = c("2", "tweets","search","recent"),
      query = list(
        query = "#microsoft lang:en -is:retweet -is:reply",
        max_results = 100,
        tweet.fields="id,lang,text,created_at,author_id,conversation_id,public_metrics",
        expansions="author_id",
        user.fields = "description,id,location,name,verified"
      ))
    
    ht_resTweets <- GET(url = url_complete,add_headers(authorization = paste0("Bearer ",BearerToken)))
    ht_AllTweets[[i]] <- fromJSON(httr::content(ht_resTweets, "text"),flatten=T)$data
    ht_meta[[i]] <- fromJSON(httr::content(ht_resTweets, "text"))$meta
    
  } else {
    if(sum(grepl("next_token",names(ht_meta[[i-1]])))){
      url_complete <- modify_url(
        url = "https://api.twitter.com",
        path = c("2", "tweets","search","recent"),
        query = list(
          query = "#microsoft lang:en -is:retweet -is:reply",
          max_results = 100,
          tweet.fields="id,lang,text,created_at,author_id,conversation_id,public_metrics",
          expansions="author_id",
          user.fields = "description,id,location,name,verified",
          next_token = ht_meta[[i-1]]$next_token
        ))
      
      ht_resTweets <- GET(url = url_complete,add_headers(authorization = paste0("Bearer ",BearerToken)))
      ht_AllTweets[[i]] <- fromJSON(httr::content(ht_resTweets, "text"),flatten=T)$data
      ht_meta[[i]] <- fromJSON(httr::content(ht_resTweets, "text"))$meta
    }
  }
  
}

# Now we will bind all the data elements together using the rbindlist function from data.table
ht_c <- rbindlist(ht_AllTweets,use.names=T)
save(ht_c, file = "ht_Microsoft1.RData")

# The following code allows to get the information about the users on the mentioned tweets 
ht_e <- fromJSON(httr::content(ht_resTweets, "text"),flatten=T)$includes$users
save(ht_e, file = "ht_Users.RData")

#######################
# Microsoft Followers #
#######################

url_followers <- modify_url(
  url = "https://api.twitter.com",
  path = c("2", "users",msid,"followers"),
  query= list(
    max_results=100,
    user.fields = "description,id,location,name,verified"
  ))
resUsers <- GET(url = url_followers,add_headers(authorization = paste0("Bearer ",BearerToken)))
followers <- fromJSON(httr::content(resUsers, "text"))$data
save(followers, file = "followers.RData")

#######################
#Extraction with rtweet
#######################

twitter_token <- create_token(
  app = "chagallet",
  consumer_key = Key,
  consumer_secret = Secret,
  access_token = Access_Tok,
  access_secret = Access_Sec,
  set_renv=FALSE)


ms <- get_timeline("Microsoft", n = 3200, token=twitter_token)
save(ms, file = "ms.RData")

#####################
#Microsoft's Lists #
####################

url_lists <- modify_url(
  url = "https://api.twitter.com",
  path = c("2", "users",msid,"owned_lists"),
  query= list(
    expansions = "owner_id",
    list.fields = "follower_count,member_count,description,owner_id",
    user.fields = "description,id,location,name,verified"
  ))
resListsMT <- GET(url = url_lists,add_headers(authorization = paste0("Bearer ",BearerToken)))
MT_Lists <- fromJSON(httr::content(resListsMT, "text"))$data
save(MT_Lists, file = "Microsoft_Lists.RData")


