#################
# Load packages #
#################

libs <- c('httr','jsonlite','config','rvest','bea.R','devtools','gtrendsR','tidyquant','ggplot2',
          'DataCombine','topicmodels','tidyr','magrittr','stringr','lubridate','dplyr',
          'data.table','textstem','randomForest', 'httr','jsonlite','config','rvest',
          'bea.R','devtools','gtrendsR','tidyquant', 'plotly','ggplot2','DT', 'wordcloud',
          'wordcloud2','tidytext','dplyr','MLmetrics','mltools','neuralnet',
          'plotROC','ROCR','precrec','AUC','pROC','plotROC','GeomRoc','textdata','caret','Matrix',
          'irlba','hunspell','purrr','SnowballC','tm','tidyverse','udpipe')

for (i in libs){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

setwd("C:/Users/cgallet/OneDrive - IESEG/MBD/Social Media Analytics/Group Project")

# API Keys
source("C:/Users/cgallet/OneDrive - IESEG/MBD/Social Media Analytics/APIKeys.R")

msid <- read.csv("msid.csv")
load("ht_Microsoft1.RData")
load("mt_Users.RData")
load("ht_Users.RData")
load("mt_Microsoft1.RData")
load("tt_Microsoft1.RData")
load("followers.RData")
load("ms.RData")
load("Microsoft_Lists.RData")

##############################
###### Summary ###############
#############################

#Reference: https://towardsdatascience.com/a-guide-to-mining-and-analysing-tweets-with-r-2f56818fdd16

#Remove the rewteets and conversations to get the original tweet from Microsoft


originaltweet <- ms[ms$is_retweet==FALSE & is.na(ms$reply_to_status_id), ] 


######Most liked tweet
most_liked <- originaltweet[order(-originaltweet$favorite_count),]

most_liked[1,5]

######Most retweeted tweet
most_retweeted <- originaltweet[order(-originaltweet$retweet_count),]

most_retweeted[1,5]


#Isolating each type of tweet to see count of each done by microsoft


#Get retweets
retweets <- ms[ms$is_retweet==TRUE,]


#Get conversation(replies)
Replies <- ms[!is.na(ms$reply_to_status_id),]


#Combine counts of all the three types of tweets into a dataframe

tweet_types <- data.frame(Type = c("Original", "Retweets", "Replies"), Count = c(nrow(originaltweet), nrow(retweets), nrow(Replies)))



#####Visualize tweet types
#https://plotly.com/r/getting-started/#rendering-charts

plot_ly(tweet_types,values=~Count,labels=~factor(Type),marker=list(colors=c("blue","yellow","purple")),type="pie") %>% layout(title="Types of Tweets by Microsoft") 


######Plot frequency of tweets using ts_plot from rwteet
#Reference: https://www.rdocumentation.org/packages/rtweet/versions/0.3.7/topics/ts_plot


ts_plot(ms, "month") + theme_minimal() 


######Visualize where Microsoft tweets from

source_tweets <- ms %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())

plot_ly(source_tweets,values=~count,labels=~factor(source),marker=list(colors=c("light blue","purple")),type="pie") %>% layout(title="Source of Tweets by Microsoft")



######Visualize the most used words in Microsoft's tweets
#Reference: Social Media Analytics Session 2 by Matthijs Meire
#Reference: https://towardsdatascience.com/a-guide-to-mining-and-analysing-tweets-with-r-2f56818fdd16
#Refernce: https://plotly.com/r/horizontal-bar-charts/


originaltweet_cleaned <- mutate(originaltweet, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

originaltweet_tokenized <- originaltweet_cleaned %>% unnest_tokens(output = "word", 
                                                                   input = text,  
                                                                   token = "words", 
                                                                   drop=FALSE,to_lower=TRUE) 

originaltweet_tokenized <- filter(originaltweet_tokenized, substr(word, 1, 1) != '#', 
                                  substr(word, 1, 1) != '@')


originaltweet_tokenized <- originaltweet_tokenized %>% anti_join(get_stopwords())  



wordfreq <- data.frame(originaltweet_tokenized %>% group_by(word) %>% 
                         summarize(freq = n()) %>%
                         arrange(-freq)) %>%
  top_n(10)


plot_ly(wordfreq, x = ~freq, y = ~word, type = 'bar', orientation = 'h') %>% layout(title="Count of Most frequent Words in Microsoft's Tweets", xaxis = list(title = ""),
                                                                                    yaxis = list(title =""))





#Check the account name whose tweets are mostly retweeted by Microsoft
#Assumption: retweet_screen_name is the name of the account who wrote the original tweet


wordcloud(retweets$retweet_screen_name, min.freq=3, scale=c(3, 1), random.order=FALSE)




##########################################
################Topic Modelling using POS Tag###########
#######################################################


#Reference: Social Media Analytics Session 4 by Matthijs Meire 
#Reference: https://cran.r-project.org/web/packages/udpipe/vignettes/udpipe-usecase-topicmodelling.html


#Obtain all tweets except for replies

alltweets <- ms[is.na(ms$reply_to_status_id),]

alltweets <- mutate(alltweets, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))



ud_model <- udpipe_download_model(language = "english")


ud_model <- udpipe_load_model(ud_model$file_model)


x <- udpipe_annotate(ud_model, x = alltweets$text, doc_id = alltweets$status_id)

x <- as.data.frame(x)

x$topic_level_id <- unique_identifier(x, fields = c("doc_id", "paragraph_id", "sentence_id"))

dtf <- subset(x, upos %in% c("NOUN", "ADJ"))
dtf <- document_term_frequencies(dtf, document = "topic_level_id", term = "lemma")


## Create a document/term/matrix for building a topic model
dtm <- document_term_matrix(x = dtf)

## Remove words which do not occur that much
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 5)

## Or keep of these nouns the top 50 based on mean term-frequency-inverse document frequency
dtm_clean <- dtm_remove_tfidf(dtm_clean, top = 50)


m <- LDA(dtm_clean, k = 8, method = "Gibbs", control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6))


tweet_topics <- tidy(m, matrix = "beta")



top_tweet_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_tweet_terms

top_tweet_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()



tweet_documents <- tidy(m, matrix = "gamma")
# CHoose, per tweet, the most important topic (the one with the highest weight)
tweet_doc_topic <- tweet_documents %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) 

tweet_doc_topic %>%
  group_by(topic) %>% 
  summarise(nbr_documents = n())

##########################################
################Basic Followers information###########
#######################################################

# This function was taken from https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
  
}

follMode <- getmode(followers$verified) #The verified mode of followers of Microsoft shows that there are mainly non verified people. 

Verifs <- sum(followers$verified == TRUE)
NonVerifs <- sum(followers$verified == FALSE)


#http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html 
ggplot(followers, aes(x = '', fill = factor(verified))) + geom_bar(width=1)

###################################
# WordCloud on Microsoft Followers#
###################################

# 1. Doing Some Pre-processing steps to clean the data

#  Remove punctuation and numbers with regular expressions
MSFT_users <- mutate(followers, message = gsub(x = followers$description, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

#  Tokenization (+ going to lowercase)
MSFT_tokenized <- MSFT_users %>% unnest_tokens(output = "word",
                                               input = description, 
                                               token = "words", 
                                               drop=FALSE,to_lower=TRUE) 

MSFT_tokenized <- filter(MSFT_tokenized, substr(word, 1, 1) != '#', substr(word, 1, 1) != '@')
MSFT_tokenized <- MSFT_tokenized %>% anti_join(get_stopwords())
MSFT_tokenized <- MSFT_tokenized %>% count(id, word)

MSFT_Matrix <- MSFT_tokenized %>% cast_dtm(id,word,n,weighting = tm::weightTfIdf)
MSFT_DTM <- removeSparseTerms(MSFT_Matrix,0.7)

MSFT_Frequency <- MSFT_tokenized %>% group_by(word) %>% 
  summarize(freq = n()) %>%
  arrange(-freq)                  
head(MSFT_Frequency)

wordcloud(MSFT_Frequency$word, MSFT_Frequency$freq,
          min.freq = 2, 
          max.words=50,
          scale=c(3,1))

##################################
# Sentiment Analysis on followers#
##################################

########################################
###  Dictionary-based lookup           #
########################################

# 2. Find the sentiment score

MSFT_usersSentiment <- inner_join(MSFT_tokenized,get_sentiments("bing"))

###
# 3. Analysis

# 3.1 get the most positive/negative words

if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; library("ggplot2")

MSFT_summarySentiment <- MSFT_usersSentiment %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

MSFT_summarySentiment %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# 3.2 get a summary per description
MSFT_statusSentiment <- MSFT_usersSentiment %>%
  count(id, sentiment) %>%                # count the positives and negatives per id (status)
  spread(sentiment, n, fill = 0) %>%      # we want overall sentiment, and here we have text (positive and negative), making it difficult to direclty calculate sentiment. 
  mutate(sentiment = positive - negative)

ggplot(MSFT_statusSentiment,
       aes(x=sentiment), bins) + geom_histogram(bins=4, color = "white")

############################################
# Topic Modeling on followers using POS Tag#
############################################

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

MSFT_topic <- udpipe_annotate(ud_model, x = MSFT_tokenized$word, doc_id = MSFT_tokenized$id)
MSFT_topic <- as.data.frame(x)

MSFT_topic$topic_level_id <- unique_identifier(MSFT_topic, fields = c("doc_id", "paragraph_id", "sentence_id"))

MSFT_dtf <- subset(MSFT_topic, upos %in% c("NOUN", "ADJ"))
MSFT_dtf <- document_term_frequencies(MSFT_dtf, document = "topic_level_id", term = "lemma")
head(MSFT_dtf)

MSFT_topic_dtm <- document_term_matrix(x = MSFT_dtf)

#Cannot remove terms with low frequency, as they all seem to have very low frequency

MSFT_model <- LDA(MSFT_topic_dtm, k = 8, method = "Gibbs", control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6))

MSFT_users_topics <- tidy(MSFT_model, matrix = "beta")

MSFT_top_topics <- MSFT_users_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
MSFT_top_topics

MSFT_top_topics %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#####################################################################################
# Twitter Map gotten from YouTube Video https://www.youtube.com/watch?v=5QqZSc63Tpc #
#####################################################################################

source("https://biostat.jhsph.edu/~jleek/code/twitterMap.R")

setup_twitter_oauth(
  consumer_key = Key,
  consumer_secret = Secret,
  access_token = Access_Tok,
  access_secret = Access_Sec)

twitterMap("Microsoft")

#############################
# WordCloud on Mention Users#
#############################

# 1. Doing Some Pre-processing steps to clean the data

#  Remove punctuation and numbers with regular expressions
mt_users <- mutate(mt_e, message = gsub(x = mt_e$description, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

#  Tokenization (+ going to lowercase)
mt_tokenized <- mt_users %>% unnest_tokens(output = "word",
                                           input = description, 
                                           token = "words", 
                                           drop=FALSE,to_lower=TRUE) 

mt_tokenized <- filter(mt_tokenized, substr(word, 1, 1) != '#', substr(word, 1, 1) != '@')
mt_tokenized <- mt_tokenized %>% anti_join(get_stopwords())
mt_tokenized <- mt_tokenized %>% count(id, word)

mt_Matrix <- mt_tokenized %>% cast_dtm(id,word,n,weighting = tm::weightTfIdf)
mt_DTM <- removeSparseTerms(mt_Matrix,0.7)

mt_Frequency <- mt_tokenized %>% group_by(word) %>% 
  summarize(freq = n()) %>%
  arrange(-freq)                  
head(mt_Frequency)

wordcloud(mt_Frequency$word, mt_Frequency$freq,
          max.words=50,
          scale=c(3,1))

######################################
# Sentiment Analysis on Mention Users#
######################################

########################################
###  Dictionary-based lookup           #
########################################

# 2. Find the sentiment score

mt_usersSentiment <- inner_join(mt_tokenized,get_sentiments("bing"))

###
# 3. Analysis

# 3.1 get the most positive/negative words

if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; library("ggplot2")

mt_summarySentiment <- mt_usersSentiment %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

mt_summarySentiment %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# 3.2 get a summary per description
mt_statusSentiment <- mt_usersSentiment %>%
  count(id, sentiment) %>%                # count the positives and negatives per id (status)
  spread(sentiment, n, fill = 0) %>%      # we want overall sentiment, and here we have text (positive and negative), making it difficult to direclty calculate sentiment. 
  mutate(sentiment = positive - negative)

ggplot(mt_statusSentiment,
       aes(x=sentiment), bins) + geom_histogram(bins=4, color = "white")

################################################
# Topic Modeling on Mention Users using POS Tag#
################################################

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

mt_topic <- udpipe_annotate(ud_model, x = mt_tokenized$word, doc_id = mt_tokenized$id)
mt_topic <- as.data.frame(x)

mt_topic$topic_level_id <- unique_identifier(mt_topic, fields = c("doc_id", "paragraph_id", "sentence_id"))

mt_dtf <- subset(mt_topic, upos %in% c("NOUN", "ADJ"))
mt_dtf <- document_term_frequencies(mt_dtf, document = "topic_level_id", term = "lemma")
head(mt_dtf)

mt_topic_dtm <- document_term_matrix(x = mt_dtf)

#Cannot remove terms with low frequency, as they all seem to have very low frequency

mt_model <- LDA(mt_topic_dtm, k = 8, method = "Gibbs", control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6))

mt_users_topics <- tidy(mt_model, matrix = "beta")

mt_top_topics <- mt_users_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
mt_top_topics

mt_top_topics %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#############################
# WordCloud on Hashtag Users#
#############################

# 1. Doing Some Pre-processing steps to clean the data

#  Remove punctuation and numbers with regular expressions
ht_users <- mutate(ht_e, message = gsub(x = ht_e$description, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

#  Tokenization (+ going to lowercase)
ht_tokenized <- ht_users %>% unnest_tokens(output = "word",
                                           input = description, 
                                           token = "words", 
                                           drop=FALSE,to_lower=TRUE) 

ht_tokenized <- filter(ht_tokenized, substr(word, 1, 1) != '#', substr(word, 1, 1) != '@')
ht_tokenized <- ht_tokenized %>% anti_join(get_stopwords())
ht_tokenized <- ht_tokenized %>% count(id, word)

ht_Matrix <- ht_tokenized %>% cast_dtm(id,word,n,weighting = tm::weightTfIdf)
ht_DTM <- removeSparseTerms(ht_Matrix,0.7)

ht_Frequency <- ht_tokenized %>% group_by(word) %>% 
  summarize(freq = n()) %>%
  arrange(-freq)                  
head(ht_Frequency)

wordcloud(ht_Frequency$word, ht_Frequency$freq,
          max.words=50,
          scale=c(3,1))

######################################
# Sentiment Analysis on Hashtag Users#
######################################

########################################
###  Dictionary-based lookup           #
########################################

# note that there are packages that implement this on the dtm
# however, it is very simple and more flexible to do it yourself

###
# 1. we will start from the oxfam text, with some small pre-processing:
# we take the limited sample for this
#load("C:/Users/cgallet/OneDrive - IESEG/MBD/Social Media Analytics/Group Project/mt_Users.RData")

#  Remove punctuation and numbers with regular expressions
ht_users <- mutate(ht_e, message = gsub(x = ht_e$description, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

#  Tokenization (+ going to lowercase)
ht_tokenized <- mt_users %>% unnest_tokens(output = "word", # how should the new column be named?
                                           input = description, # where can we find the text? 
                                           token = "words", # which tokenization scheme should we follow?
                                           drop=FALSE,to_lower=TRUE) # drop=FALSE specifies that we want to keep our text; to_lower puts everyting to lowercase

###
# 2. Find the sentiment score

ht_usersSentiment <- inner_join(ht_tokenized,get_sentiments("bing"))

###
# 3. Analysis

# 3.1 get the most positive/negative words

ht_summarySentiment <- ht_usersSentiment %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

ht_summarySentiment %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# 3.2 get a summary per description
ht_statusSentiment <- ht_usersSentiment %>%
  count(id, sentiment) %>%                # count the positives and negatives per id (status)
  spread(sentiment, n, fill = 0) %>%      # we want overall sentiment, and here we have text (positive and negative), making it difficult to direclty calculate sentiment. 
  mutate(sentiment = positive - negative)

ggplot(ht_statusSentiment,
       aes(x=sentiment), bins) + geom_histogram(bins=4, color = "white")

################################################
# Topic Modeling on Hashtag Users using POS Tag#
################################################

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

ht_topic <- udpipe_annotate(ud_model, x = ht_users$description, doc_id = ht_users$username)
ht_topic <- as.data.frame(x)

ht_topic$topic_level_id <- unique_identifier(ht_topic, fields = c("doc_id", "paragraph_id", "sentence_id"))

ht_dtf <- subset(ht_topic, upos %in% c("NOUN", "ADJ"))
ht_dtf <- document_term_frequencies(ht_dtf, document = "topic_level_id", term = "lemma")
head(ht_dtf)

ht_topic_dtm <- document_term_matrix(x = ht_dtf)

ht_model <- LDA(ht_topic_dtm, k = 8, method = "Gibbs", control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6))

ht_users_topics <- tidy(ht_model, matrix = "beta")

ht_top_topics <- ht_users_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ht_top_topics

ht_top_topics %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#####################################
# Text Processing - Tweet Timelines #
#####################################

# Remove punctuation and numbers with regular expressions
tt_c_edit <- mutate(tt_c, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

# Tokenization (+ lowercase)
tt_c_Tokenized <- tt_c_edit %>% unnest_tokens(output = "word",
                                                input = text, 
                                                token = "words",
                                                drop=FALSE,to_lower=TRUE)

# Remove some other elements such as # and @ signs if they might occur
tt_c_Tokenized <- filter(tt_c_Tokenized, substr(word, 1, 1) != '#', 
                           substr(word, 1, 1) != '@')

# Spelling correction
correct_spelling <- function(input) {
  output <- case_when(
    # any manual corrections
    input == 'license' ~ 'licence',
    # check and (if required) correct spelling
    !hunspell_check(input, dictionary('en_GB')) ~
      hunspell_suggest(input, dictionary('en_GB')) %>%
      # get first suggestion, or NA if suggestions list is empty
      map(1, .default = NA) %>%
      unlist(),
    TRUE ~ input # if word is correct
  )
  # if input incorrectly spelled but no suggestions, return input word
  ifelse(is.na(output), input, output)
}

# apply the function above to each word in the dataset.
# create a new variable that contains the 'corrected' word
tt_c_Tokenized <- tt_c_Tokenized %>%  mutate(suggestion = correct_spelling(word))

# remove stopwords
tt_c_Tokenized <- tt_c_Tokenized %>% anti_join(get_stopwords())

# Stemming
tt_c_Tokenized <- tt_c_Tokenized %>% mutate(word = wordStem(word)) 

# Remove stop words
tt_c_Tokenized <- tt_c_Tokenized %>%
  anti_join(stop_words) %>%       # note that we use all stopword dictionaries here
  count(id,word , sort=TRUE) %>%
  cast_dtm(document = id, term = word,
           value = n, weighting = tm::weightTf)

# Create the document-term matrix
tt_c_Tokenized <- tt_c_Tokenized %>% count(id,word)
head(tt_c_Tokenized)

# then, we could perform weighting (e.g., tfidf) using the bind_tf_idf(word,id,n) function
# however, we will integrate this directly when making the document term matrix:
tt_cDTM <- tt_c_Tokenized %>% cast_dtm(id,word,n,weighting = tm::weightTfIdf)

# This is a very sparse matrix
# we can reduce sparseness by removing the most sparse terms:
tt_cDTMDense <- removeSparseTerms(tt_cDTM,0.7)
View(as.matrix(tt_cDTMDense))

# 1. we can look at associations/correlations between words (this is with the dtm):
findAssocs(tt_cDTM, terms = "microsoft", corlimit = 0.1)

# 2. investigate the most frequent terms

tt_cFreq <- tt_c_Tokenized %>% group_by(word) %>% # for this, we need to have the sum over all documents
  summarize(freq = n()) %>%
  arrange(-freq)                  # arrange = order; from most frequent term to lowest frequent
head(tt_cFreq)


###############################
# Wordcloud - Tweet Timelines #
###############################

# 1. Word cloud based on the original text

# use the termFreq of the tm package
# This also uses a tokenizer inside
tt_ctf <- termFreq(tt_c$text)
wordcloud(names(tt_ctf),tt_ctf,
          max.words=40,
          scale=c(3,1))

# 2. Word cloud based on the tibble and all text pre-processing

#create word cloud
wordcloud(tt_cFreq$word, tt_cFreq$freq,
          max.words=40,
          scale=c(3,1))

############################
# Bigram - Tweet Timelines #
############################

# Implementing bigrams

tt_c_editBigramCount <- tt_c_edit %>% unnest_tokens(output = "bigram",
                                                    input = text,
                                                    token = "ngrams",n=2, drop=FALSE) %>%
  count(id,bigram)
tt_c_editBigramDTM <- tt_c_editBigramCount  %>% cast_dtm(id,bigram,n)

# make a wordcloud

tt_c_editBiCount <- tt_c_editBigramCount %>% group_by(bigram) %>% summarize(freq = n())
wordcloud(tt_c_editBiCount$bigram,tt_c_editBiCount$freq,max.words = 40)

# remove the interesting words for negations
sw <- get_stopwords() %>% filter(!word %in% c("no","not","nor"))

tt_cbigrams_nosw <- tt_c_edit %>% unnest_tokens(output = "bigram",
                                                input = text,
                                                token = "ngrams",n=2, drop=FALSE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% # Split bigram into 2
  filter(!word1 %in% sw$word) %>% # Check for each of the words whether it is a stopword
  filter(!word2 %in% sw$word) %>% # if they are, delete
  unite(bigram, word1, word2, sep = " ")  # unite the words again

# remove stopwords first, may result in putting two words together that are not originally together

#######################################
# Sentiment Analysis - Tweet Timeline #
#######################################

##################################
# Dictionary-based lookup method #
##################################

# Find the sentiment score (bing dictionary)
tt_c_Sentiment <- inner_join(tt_c_Tokenized,get_sentiments("bing"))

# Get the most positive/negative words
tt_c_summarySentiment <- tt_c_Sentiment %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

tt_c_summarySentiment %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Exclude the product related words,e.g. cloud and excel
tt_c_summarySentiment_edited <- tt_c_Sentiment %>%
  filter(!word %in% c('cloud','excel')) %>%
  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

tt_c_summarySentiment_edited %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Get a summary per post
tt_statusSentiment <- tt_c_Sentiment %>%
  count(id, sentiment) %>%                # count the positives and negatives per id (status)
  spread(sentiment, n, fill = 0) %>%      # we want overall sentiment, and here we have text (positive and negative), making it difficult to direclty calculate sentiment. 
  mutate(sentiment = positive - negative)

tt_statusSentiment <- inner_join(tt_c_Tokenized,get_sentiments("afinn")) %>%
  group_by(id) %>%
  summarize(Sentiment = sum(value)) 

mean(tt_statusSentiment$Sentiment)

tt_statusSentiment <- tt_c_edit %>% left_join(tt_statusSentiment,by="id") %>% 
  mutate(Sentiment = ifelse(is.na(Sentiment),0,Sentiment))

tt_time  <- tt_statusSentiment$created_at

tt_sentiment <- tt_statusSentiment[order(tt_time),"Sentiment"]

lim <- max(abs(tt_sentiment))

plot(1:length(tt_sentiment$Sentiment), 
     tt_sentiment$Sentiment, 
     xaxt="n",
     type="l",
     ylab="Valence",
     xlab="Time (days:hour:minute)",
     main="Sentiment", 
     ylim=c(-lim,lim))
abline(h = 0, col = "red", lty = 3)

axis(1,at=1:length(tt_sentiment$Sentiment), 
     labels=paste0(substr(tt_time[order(tt_time)],1,10),"\n",substr(tt_time[order(tt_time)],12,16)))

##################
# Topic Modeling # # Not Clear Topics defined
##################

tt_c_1 <- tt_c

tt_c_1$text <-gsub('http.*\\s*', '', tt_c_1$text)

tt_c_TokenizedTM <- tt_c_1 %>% unnest_tokens(output = "word",
                                             input = text,
                                             token = "words",
                                             drop=FALSE,to_lower=TRUE)

tweets_processed$text <-gsub('http.*\\s*', '', tt_c_TokenizedTM$text)

tt_c_TokenizedTM <- tt_c_TokenizedTM %>%
  anti_join(stop_words) %>%       # note that we use all stopword dictionaries here
  count(id,word , sort=TRUE) %>%
  cast_dtm(document = id, term = word,
           value = n, weighting = tm::weightTf)


tt_c_lda <- LDA(tt_c_TokenizedTM, k = 3,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6))
tt_c_lda

tt_c_topics <- tidy(tt_c_lda, matrix = "beta")

# you can use the following code to get the top terms per topic
top_tt_c_terms <- tt_c_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_tt_c_terms

top_tt_c_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


# 2. get the topics per document: which topics determine the documents?
# in this model, this is captured in @gamma of the model
# These values give the estimated proportion of words in that tweet from topic 1, topic 2, .... 
## so adding up should be equal to 1 (100%) for the same document
# The higher the value, the better, because we have better distinction between topics

tt_c_documents <- tidy(tt_c_lda, matrix = "gamma")
# CHoose, per tweet, the most important topic (the one with the highest weight)
tt_c_doc_topic <- tt_c_documents %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) 

tt_c_doc_topic %>%
  group_by(topic) %>% 
  summarise(nbr_documents = n())

###########################################
#  Top 10 most liked and retweeted tweets # 
###########################################

# Source https://machinelearningsol.com/twitter-analytics-using-r/

tt_c_retweet <- tt_c %>% arrange(-public_metrics.retweet_count)
top_tt_c_retweet=tt_c_retweet[1:10,]
ggplot(top_tt_c_retweet, aes(x=reorder(text, public_metrics.retweet_count), y=public_metrics.retweet_count))+
  geom_bar(stat="identity", fill =  "#377F97")+
  ggtitle("Top 10 Most Retweeted Tweets")+
  labs(x="Organic Tweets", y=("Retweet Count"))+
  theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
        plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 80))

tt_c_reply <- tt_c %>% arrange(-public_metrics.reply_count)
top_tt_c_reply=tt_c_reply[1:10,]
ggplot(top_tt_c_reply, aes(x=reorder(text, public_metrics.reply_count), y=public_metrics.reply_count))+
  geom_bar(stat="identity", fill =  "#377F97")+
  ggtitle("Top 10 Most Reply Tweets")+
  labs(x="Organic Tweets", y=("Reply Count"))+
  theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
        plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 80))

tt_c_like <- tt_c %>% arrange(-public_metrics.like_count)
top_tt_c_like=tt_c_like[1:10,]
ggplot(top_tt_c_like, aes(x=reorder(text, public_metrics.like_count), y=public_metrics.like_count))+
  geom_bar(stat="identity", fill =  "#377F97")+
  ggtitle("Top 10 Most Like Tweets")+
  labs(x="Organic Tweets", y=("Like Count"))+
  theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
        plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 80))

tt_c_quote <- tt_c %>% arrange(-public_metrics.quote_count)
top_tt_c_quote=tt_c_quote[1:10,]
ggplot(top_tt_c_quote, aes(x=reorder(text, public_metrics.quote_count), y=public_metrics.quote_count))+
  geom_bar(stat="identity", fill =  "#377F97")+
  ggtitle("Top 10 Most Quote Tweets")+
  labs(x="Organic Tweets", y=("Quote Count"))+
  theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
        plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 80))

###############################
#  Tweets Frequency Per Month # 
###############################

tt_Time_Analysis = lapply(tt_c$created_at, FUN=function(x) substr(x[[1]], 1, 7))
tt_tmp=data.frame(table(unlist(tt_Time_Analysis)))
# Plot
ggplot(data=tt_tmp, aes(x=Var1, y=Freq, group=1)) +
  geom_line(size=1, color="black")+
  geom_point(size=3, color="black")+
  ggtitle("Number of tweets by Month")+
  labs(x="TimeStamp", y="Frequency - Tweets/Retweets/Replies")+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(size=10, angle=35))

############################
#  Top 10 Popular Hashtags # 
############################

tags <- function(x) toupper(grep("^#", strsplit(x, " +")[[1]], value = TRUE))
l=nrow(tt_c)

# Create a list of the tag sets for each tweet
taglist <- vector(mode = "list", l)

for (i in 1:l) taglist[[i]] <- tags(tt_c$text[i])
tmp=table(unlist(taglist))
tmp=sort(tmp, decreasing = T) %>% as.data.frame()
tmp$Var1=gsub("#","",tmp$Var1)
tmp$Var1=gsub(",","",tmp$Var1)
tmp <- tmp[!tmp$Var1 == "MICROSOFT", ]
tmp=tmp[1:10,]
ggplot(tmp, aes(x=reorder(Var1, Freq), y=Freq))+
  geom_bar(stat="identity", fill = "#377F97")+
  ggtitle("Popular Hashtags")+labs(x="Hashtags", y="Frequency")+
  theme(axis.text.x = element_text(angle = 70,face="bold", 
                                   color="black", size=12, hjust = 1),
        plot.title = element_text(hjust = 0.5))+coord_flip()

#######################################
# Text Processing - Mention Timelines #
#######################################


# Remove punctuation and numbers with regular expressions
mt_c_edit <- mutate(mt_c, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

# Tokenization (+ lowercase)
mt_c_Tokenized <- mt_c_edit %>% unnest_tokens(output = "word",
                                                input = text, 
                                                token = "words",
                                                drop=FALSE,to_lower=TRUE)

# Remove some other elements such as # and @ signs if they might occur
mt_c_Tokenized <- filter(mt_c_Tokenized, substr(word, 1, 1) != '#', 
                           substr(word, 1, 1) != '@')

# Spelling correction
correct_spelling <- function(input) {
  output <- case_when(
    # any manual corrections
    input == 'license' ~ 'licence',
    # check and (if required) correct spelling
    !hunspell_check(input, dictionary('en_GB')) ~
      hunspell_suggest(input, dictionary('en_GB')) %>%
      # get first suggestion, or NA if suggestions list is empty
      map(1, .default = NA) %>%
      unlist(),
    TRUE ~ input # if word is correct
  )
  # if input incorrectly spelled but no suggestions, return input word
  ifelse(is.na(output), input, output)
}

# apply the function above to each word in the dataset.
# create a new variable that contains the 'corrected' word
mt_c_Tokenized <- mt_c_Tokenized %>%  mutate(suggestion = correct_spelling(word))

# remove stopwords
mt_c_Tokenized <- mt_c_Tokenized %>% anti_join(get_stopwords())

# Stemming
mt_c_Tokenized <- mt_c_Tokenized %>% mutate(word = lemmatize_words(word))

# Remove stop words
mt_c_Tokenized <- mt_c_Tokenized %>%
  anti_join(stop_words) %>%       # note that we use all stopword dictionaries here
  count(id,word , sort=TRUE) %>%
  cast_dtm(document = id, term = word,
           value = n, weighting = tm::weightTf)

# Create the document-term matrix
mt_c_Tokenized <- mt_c_Tokenized %>% count(id,word)
head(mt_c_Tokenized)

# then, we could perform weighting (e.g., tfidf) using the bind_tf_idf(word,id,n) function
# however, we will integrate this directly when making the document term matrix:
mt_cDTM <- mt_c_Tokenized %>% cast_dtm(id,word,n,weighting = tm::weightTfIdf)

# This is a very sparse matrix
# we can reduce sparseness by removing the most sparse terms:
mt_cDTMDense <- removeSparseTerms(mt_cDTM,0.7)
View(as.matrix(mt_cDTMDense))

# 1. we can look at associations/correlations between words (this is with the dtm):
findAssocs(mt_cDTM, terms = "microsoft", corlimit = 0.1)

# 2. investigate the most frequent terms

mt_cFreq <- mt_c_Tokenized %>% group_by(word) %>% # for this, we need to have the sum over all documents
  summarize(freq = n()) %>%
  arrange(-freq)                  # arrange = order; from most frequent term to lowest frequent
head(mt_cFreq)


#################################
# Wordcloud - Mention Timelines #
#################################

# 1. Word cloud based on the original text

# use the termFreq of the tm package
# This also uses a tokenizer inside
mt_ctf <- termFreq(mt_c$text)
wordcloud(names(mt_ctf),mt_ctf,
          max.words=40,
          scale=c(2,1))

# 2. Word cloud based on the tibble and all text pre-processing

#create word cloud
wordcloud(mt_cFreq$word, mt_cFreq$freq,
          max.words=40,
          scale=c(2,1))

##############################
# Bigram - Mention Timelines #
##############################

# Implementing bigrams

mt_c_editBigramCount <- mt_c_edit %>% unnest_tokens(output = "bigram",
                                                    input = text,
                                                    token = "ngrams",n=2, drop=FALSE) %>%
  count(id,bigram)
mt_c_editBigramDTM <- mt_c_editBigramCount  %>% cast_dtm(id,bigram,n)

# make a wordcloud

mt_c_editBiCount <- mt_c_editBigramCount %>% group_by(bigram) %>% summarize(freq = n())
wordcloud(mt_c_editBiCount$bigram,mt_c_editBiCount$freq,max.words = 40)

# remove the interesting words for negations
sw <- get_stopwords() %>% filter(!word %in% c("no","not","nor"))

mt_cbigrams_nosw <- mt_c_edit %>% unnest_tokens(output = "bigram",
                                                input = text,
                                                token = "ngrams",n=2, drop=FALSE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% # Split bigram into 2
  filter(!word1 %in% sw$word) %>% # Check for each of the words whether it is a stopword
  filter(!word2 %in% sw$word) %>% # if they are, delete
  unite(bigram, word1, word2, sep = " ")  # unite the words again

# remove stopwords first, may result in putting two words together that are not originally together

##########################################
# Sentiment Analysis - Mention Timelines #
##########################################

##################################
# Dictionary-based lookup method #
##################################

# Find the sentiment score (bing dictionary)
mt_c_Sentiment <- inner_join(mt_c_Tokenized,get_sentiments("bing"))

# Get the most positive/negative words
mt_c_summarySentiment <- mt_c_Sentiment %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

mt_c_summarySentiment %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Exclude the product related words,e.g. cloud and excel
mt_c_summarySentiment_edited <- mt_c_Sentiment %>%
  filter(!word %in% c('cloud','excel')) %>%
  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

mt_c_summarySentiment_edited %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Get a summary per post
mt_statusSentiment <- mt_c_Sentiment %>%
  count(id, sentiment) %>%                # count the positives and negatives per id (status)
  spread(sentiment, n, fill = 0) %>%      # we want overall sentiment, and here we have text (positive and negative), making it difficult to direclty calculate sentiment. 
  mutate(sentiment = positive - negative)

mt_statusSentiment <- inner_join(mt_c_Tokenized,get_sentiments("afinn")) %>%
  group_by(id) %>%
  summarize(Sentiment = sum(value)) 

mean(mt_statusSentiment$Sentiment)

mt_statusSentiment <- mt_c_edit %>% left_join(mt_statusSentiment,by="id") %>% 
  mutate(Sentiment = ifelse(is.na(Sentiment),0,Sentiment))

mt_time  <- mt_statusSentiment$created_at

mt_sentiment <- mt_statusSentiment[order(mt_time),"Sentiment"]

lim <- max(abs(mt_sentiment))

plot(1:length(mt_sentiment$Sentiment), 
     mt_sentiment$Sentiment, 
     xaxt="n",
     type="l",
     ylab="Valence",
     xlab="Time (days:hour:minute)",
     main="Sentiment", 
     ylim=c(-lim,lim))
abline(h = 0, col = "red", lty = 3)

axis(1,at=1:length(mt_sentiment$Sentiment), 
     labels=paste0(substr(mt_time[order(mt_time)],1,10),"\n",substr(mt_time[order(mt_time)],12,16)))

##################
# Topic Modeling # # Not Clear Topics defined
##################

mt_c_1 <- mt_c

mt_c_1$text <-gsub('http.*\\s*', '', mt_c_1$text)

mt_c_TokenizedTM <- mt_c_1 %>% unnest_tokens(output = "word",
                                             input = text,
                                             token = "words",
                                             drop=FALSE,to_lower=TRUE)

mt_c_TokenizedTM <- mt_c_TokenizedTM %>%
  anti_join(stop_words) %>%       # note that we use all stopword dictionaries here
  count(id,word , sort=TRUE) %>%
  cast_dtm(document = id, term = word,
           value = n, weighting = tm::weightTf)


mt_c_lda <- LDA(mt_c_TokenizedTM, k = 3,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6))
mt_c_lda

mt_c_topics <- tidy(mt_c_lda, matrix = "beta")

# you can use the following code to get the top terms per topic
top_mt_c_terms <- mt_c_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_mt_c_terms

top_mt_c_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


# 2. get the topics per document: which topics determine the documents?
# in this model, this is captured in @gamma of the model
# These values give the estimated proportion of words in that tweet from topic 1, topic 2, .... 
## so adding up should be equal to 1 (100%) for the same document
# The higher the value, the better, because we have better distinction between topics

mt_c_documents <- tidy(mt_c_lda, matrix = "gamma")
# CHoose, per tweet, the most important topic (the one with the highest weight)
mt_c_doc_topic <- mt_c_documents %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) 

mt_c_doc_topic %>%
  group_by(topic) %>% 
  summarise(nbr_documents = n())

###########################################
#  Top 10 most liked and retweeted tweets # 
###########################################

# Source https://machinelearningsol.com/twitter-analytics-using-r/

mt_c_retweet <- mt_c %>% arrange(-public_metrics.retweet_count)
top_mt_c_retweet=mt_c_retweet[1:10,]
ggplot(top_mt_c_retweet, aes(x=reorder(text, public_metrics.retweet_count), y=public_metrics.retweet_count))+
  geom_bar(stat="identity", fill =  "#377F97")+
  ggtitle("Top 10 Most Retweeted Tweets")+
  labs(x="Organic Tweets", y=("Retweet Count"))+
  theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
        plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 80))

mt_c_reply <- mt_c %>% arrange(-public_metrics.reply_count)
top_mt_c_reply=mt_c_reply[1:10,]
ggplot(top_mt_c_reply, aes(x=reorder(text, public_metrics.reply_count), y=public_metrics.reply_count))+
  geom_bar(stat="identity", fill =  "#377F97")+
  ggtitle("Top 10 Most Reply Tweets")+
  labs(x="Organic Tweets", y=("Reply Count"))+
  theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
        plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 80))

mt_c_like <- mt_c %>% arrange(-public_metrics.like_count)
top_mt_c_like=mt_c_like[1:10,]
ggplot(top_mt_c_like, aes(x=reorder(text, public_metrics.like_count), y=public_metrics.like_count))+
  geom_bar(stat="identity", fill =  "#377F97")+
  ggtitle("Top 10 Most Like Tweets")+
  labs(x="Organic Tweets", y=("Like Count"))+
  theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
        plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 80))

mt_c_quote <- mt_c %>% arrange(-public_metrics.quote_count)
top_mt_c_quote=mt_c_quote[1:10,]
ggplot(top_mt_c_quote, aes(x=reorder(text, public_metrics.quote_count), y=public_metrics.quote_count))+
  geom_bar(stat="identity", fill =  "#377F97")+
  ggtitle("Top 10 Most Quote Tweets")+
  labs(x="Organic Tweets", y=("Quote Count"))+
  theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
        plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 80))

###################################
#  Tweets Frequency Per Date/Time # 
###################################

mt_Time_Analysis = lapply(mt_c$created_at, FUN=function(x) substr(x[[1]], 1, 13))
mt_tmp=data.frame(table(unlist(mt_Time_Analysis)))
# Plot
ggplot(data=mt_tmp, aes(x=Var1, y=Freq, group=1)) +
  geom_line(size=1, color="black")+
  geom_point(size=3, color="black")+
  ggtitle("Number of tweets by Date/Time")+
  labs(x="TimeStamp", y="Frequency - Tweets/Retweets/Replies")+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(size=10, angle=35))

#####################################
#  Top 10 Popular Mention Timelines # 
#####################################

tags <- function(x) toupper(grep("^#", strsplit(x, " +")[[1]], value = TRUE))
l=nrow(mt_c)

# Create a list of the tag sets for each tweet
taglist <- vector(mode = "list", l)

for (i in 1:l) taglist[[i]] <- tags(mt_c$text[i])
tmp=table(unlist(taglist))
tmp=sort(tmp, decreasing = T) %>% as.data.frame()
tmp$Var1=gsub("#","",tmp$Var1)
tmp$Var1=gsub(",","",tmp$Var1)
tmp <- tmp[!tmp$Var1 == "MICROSOFT", ]
tmp=tmp[1:10,]
ggplot(tmp, aes(x=reorder(Var1, Freq), y=Freq))+
  geom_bar(stat="identity", fill = "#377F97")+
  ggtitle("Popular Hashtags")+labs(x="Hashtags", y="Frequency")+
  theme(axis.text.x = element_text(angle = 70,face="bold", 
                                   color="black", size=12, hjust = 1),
        plot.title = element_text(hjust = 0.5))+coord_flip()

#############################
# Text Processing - Hashtag #
#############################

# Remove punctuation and numbers with regular expressions
ht_c_edit <- mutate(ht_c, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

# Tokenization (+ lowercase)
ht_c_Tokenized <- ht_c_edit %>% unnest_tokens(output = "word",
                                                input = text, 
                                                token = "words",
                                                drop=FALSE,to_lower=TRUE)

# Remove some other elements such as # and @ signs if they might occur
ht_c_Tokenized <- filter(ht_c_Tokenized, substr(word, 1, 1) != '#', 
                           substr(word, 1, 1) != '@')

# Spelling correction
correct_spelling <- function(input) {
  output <- case_when(
    # any manual corrections
    input == 'license' ~ 'licence',
    # check and (if required) correct spelling
    !hunspell_check(input, dictionary('en_GB')) ~
      hunspell_suggest(input, dictionary('en_GB')) %>%
      # get first suggestion, or NA if suggestions list is empty
      map(1, .default = NA) %>%
      unlist(),
    TRUE ~ input # if word is correct
  )
  # if input incorrectly spelled but no suggestions, return input word
  ifelse(is.na(output), input, output)
}

# apply the function above to each word in the dataset.
# create a new variable that contains the 'corrected' word
ht_c_Tokenized <- ht_c_Tokenized %>%  mutate(suggestion = correct_spelling(word))

# remove stopwords
ht_c_Tokenized <- ht_c_Tokenized %>% anti_join(get_stopwords())

# Stemming
ht_c_Tokenized <- ht_c_Tokenized %>% mutate(word = wordStem(word)) 

# Create the document-term matrix
ht_c_Tokenized <- ht_c_Tokenized %>% count(id,word)
head(ht_c_Tokenized)

# then, we could perform weighting (e.g., tfidf) using the bind_tf_idf(word,id,n) function
# however, we will integrate this directly when making the document term matrix:
ht_cDTM <- ht_c_Tokenized %>% cast_dtm(id,word,n,weighting = tm::weightTfIdf)

# This is a very sparse matrix
# we can reduce sparseness by removing the most sparse terms:
ht_cDTMDense <- removeSparseTerms(ht_cDTM,0.7)
View(as.matrix(ht_cDTMDense))

# 1. we can look at associations/correlations between words (this is with the dtm):
findAssocs(ht_cDTM, terms = "microsoft", corlimit = 0.1)

# 2. investigate the most frequent terms

ht_cFreq <- ht_c_Tokenized %>% group_by(word) %>% # for this, we need to have the sum over all documents
  summarize(freq = n()) %>%
  arrange(-freq)                  # arrange = order; from most frequent term to lowest frequent
head(ht_cFreq)


#######################
# Wordcloud - Hashtag #
#######################

# 1. Word cloud based on the original text

# use the termFreq of the tm package
# This also uses a tokenizer inside
ht_ctf <- termFreq(ht_c$text)
wordcloud(names(ht_ctf),ht_ctf,
          max.words=40,
          scale=c(3,1))

# 2. Word cloud based on the tibble and all text pre-processing

#create word cloud
wordcloud(ht_cFreq$word, ht_cFreq$freq,
          max.words=40,
          scale=c(3,1))

####################
# Bigram - Hashtag #
####################

# Implementing bigrams

ht_c_editBigramCount <- ht_c_edit %>% unnest_tokens(output = "bigram",
                                                    input = text,
                                                    token = "ngrams",n=2, drop=FALSE) %>%
  count(id,bigram)
ht_c_editBigramDTM <- ht_c_editBigramCount  %>% cast_dtm(id,bigram,n)

# make a wordcloud

ht_c_editBiCount <- ht_c_editBigramCount %>% group_by(bigram) %>% summarize(freq = n())
wordcloud(ht_c_editBiCount$bigram,ht_c_editBiCount$freq,max.words = 40)

# remove the interesting words for negations
sw <- get_stopwords() %>% filter(!word %in% c("no","not","nor"))

ht_cbigrams_nosw <- ht_c_edit %>% unnest_tokens(output = "bigram",
                                                input = text,
                                                token = "ngrams",n=2, drop=FALSE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% # Split bigram into 2
  filter(!word1 %in% sw$word) %>% # Check for each of the words whether it is a stopword
  filter(!word2 %in% sw$word) %>% # if they are, delete
  unite(bigram, word1, word2, sep = " ")  # unite the words again

# remove stopwords first, may result in putting two words together that are not originally together

################################
# Sentiment Analysis - Hashtag #
################################

##################################
# Dictionary-based lookup method #
##################################

# Find the sentiment score (bing dictionary)
ht_c_Sentiment <- inner_join(ht_c_Tokenized,get_sentiments("bing"))

# Get the most positive/negative words
ht_c_summarySentiment <- ht_c_Sentiment %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

ht_c_summarySentiment %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Exclude the product related words,e.g. cloud and excel
ht_c_summarySentiment_edited <- ht_c_Sentiment %>%
  filter(!word %in% c('cloud','excel')) %>%
  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

ht_c_summarySentiment_edited %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Get a summary per post
ht_statusSentiment <- ht_c_Sentiment %>%
  count(id, sentiment) %>%                # count the positives and negatives per id (status)
  spread(sentiment, n, fill = 0) %>%      # we want overall sentiment, and here we have text (positive and negative), making it difficult to direclty calculate sentiment. 
  mutate(sentiment = positive - negative)

ht_statusSentiment <- inner_join(ht_c_Tokenized,get_sentiments("afinn")) %>%
  group_by(id) %>%
  summarize(Sentiment = sum(value)) 

mean(ht_statusSentiment$Sentiment)

ht_statusSentiment <- ht_c_edit %>% left_join(ht_statusSentiment,by="id") %>% 
  mutate(Sentiment = ifelse(is.na(Sentiment),0,Sentiment))

ht_time  <- ht_statusSentiment$created_at

ht_sentiment <- ht_statusSentiment[order(ht_time),"Sentiment"]

lim <- max(abs(ht_sentiment))

plot(1:length(ht_sentiment$Sentiment), 
     ht_sentiment$Sentiment, 
     xaxt="n",
     type="l",
     ylab="Valence",
     xlab="Time (days:hour:minute)",
     main="Sentiment", 
     ylim=c(-lim,lim))
abline(h = 0, col = "red", lty = 3)

axis(1,at=1:length(ht_sentiment$Sentiment), 
     labels=paste0(substr(ht_time[order(ht_time)],1,10),"\n",substr(ht_time[order(ht_time)],12,16)))

##################
# Topic Modeling # # Not Clear Topics defined
##################

ht_c_1 <- ht_c

ht_c_1$text <-gsub('http.*\\s*', '', ht_c_1$text)

ht_c_TokenizedTM <- ht_c_1 %>% unnest_tokens(output = "word",
                                             input = text,
                                             token = "words",
                                             drop=FALSE,to_lower=TRUE)

ht_c_TokenizedTM <- ht_c_TokenizedTM %>%
  anti_join(stop_words) %>%       # note that we use all stopword dictionaries here
  count(id,word , sort=TRUE) %>%
  cast_dtm(document = id, term = word,
           value = n, weighting = tm::weightTf)


ht_c_lda <- LDA(ht_c_TokenizedTM, k = 3,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6))
ht_c_lda

ht_c_topics <- tidy(ht_c_lda, matrix = "beta")

# you can use the following code to get the top terms per topic
top_ht_c_terms <- ht_c_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_ht_c_terms

top_ht_c_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


# 2. get the topics per document: which topics determine the documents?
# in this model, this is captured in @gamma of the model
# These values give the estimated proportion of words in that tweet from topic 1, topic 2, .... 
## so adding up should be equal to 1 (100%) for the same document
# The higher the value, the better, because we have better distinction between topics

ht_c_documents <- tidy(ht_c_lda, matrix = "gamma")
# CHoose, per tweet, the most important topic (the one with the highest weight)
ht_c_doc_topic <- ht_c_documents %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) 

ht_c_doc_topic %>%
  group_by(topic) %>% 
  summarise(nbr_documents = n())

###########################################
#  Top 10 most liked and retweeted tweets # 
###########################################

# Source https://machinelearningsol.com/twitter-analytics-using-r/

ht_c_retweet <- ht_c %>% arrange(-public_metrics.retweet_count)
top_ht_c_retweet=ht_c_retweet[1:10,]
ggplot(top_ht_c_retweet, aes(x=reorder(text, public_metrics.retweet_count), y=public_metrics.retweet_count))+
  geom_bar(stat="identity", fill =  "#377F97")+
  ggtitle("Top 10 Most Retweeted Tweets")+
  labs(x="Organic Tweets", y=("Retweet Count"))+
  theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
        plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 80))

ht_c_reply <- ht_c %>% arrange(-public_metrics.reply_count)
top_ht_c_reply=ht_c_reply[1:10,]
ggplot(top_ht_c_reply, aes(x=reorder(text, public_metrics.reply_count), y=public_metrics.reply_count))+
  geom_bar(stat="identity", fill =  "#377F97")+
  ggtitle("Top 10 Most Reply Tweets")+
  labs(x="Organic Tweets", y=("Reply Count"))+
  theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
        plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 80))

ht_c_like <- ht_c %>% arrange(-public_metrics.like_count)
top_ht_c_like=ht_c_like[1:10,]
ggplot(top_ht_c_like, aes(x=reorder(text, public_metrics.like_count), y=public_metrics.like_count))+
  geom_bar(stat="identity", fill =  "#377F97")+
  ggtitle("Top 10 Most Like Tweets")+
  labs(x="Organic Tweets", y=("Like Count"))+
  theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
        plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 80))

ht_c_quote <- ht_c %>% arrange(-public_metrics.quote_count)
top_ht_c_quote=ht_c_quote[1:10,]
ggplot(top_ht_c_quote, aes(x=reorder(text, public_metrics.quote_count), y=public_metrics.quote_count))+
  geom_bar(stat="identity", fill =  "#377F97")+
  ggtitle("Top 10 Most Quote Tweets")+
  labs(x="Organic Tweets", y=("Quote Count"))+
  theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
        plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 80))

###################################
#  Tweets Frequency Per Date/Time # 
###################################

ht_Time_Analysis = lapply(ht_c$created_at, FUN=function(x) substr(x[[1]], 1, 13))
ht_tmp=data.frame(table(unlist(ht_Time_Analysis)))
# Plot
ggplot(data=ht_tmp, aes(x=Var1, y=Freq, group=1)) +
  geom_line(size=1, color="black")+
  geom_point(size=3, color="black")+
  ggtitle("Number of tweets by Date/Time")+
  labs(x="TimeStamp", y="Frequency - Tweets/Retweets/Replies")+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(size=10, angle=35))

#############################
#  Top 10 Popular Hash Tags # 
#############################

tags <- function(x) toupper(grep("^#", strsplit(x, " +")[[1]], value = TRUE))
l=nrow(ht_c)

# Create a list of the tag sets for each tweet
taglist <- vector(mode = "list", l)

for (i in 1:l) taglist[[i]] <- tags(ht_c$text[i])
tmp=table(unlist(taglist))
tmp=sort(tmp, decreasing = T) %>% as.data.frame()
tmp$Var1=gsub("#","",tmp$Var1)
tmp$Var1=gsub(",","",tmp$Var1)
tmp <- tmp[!tmp$Var1 == "MICROSOFT", ]
tmp=tmp[1:10,]
ggplot(tmp, aes(x=reorder(Var1, Freq), y=Freq))+
  geom_bar(stat="identity", fill = "#377F97")+
  ggtitle("Popular Hashtags")+labs(x="Hashtags", y="Frequency")+
  theme(axis.text.x = element_text(angle = 70,face="bold", 
                                   color="black", size=12, hjust = 1),
        plot.title = element_text(hjust = 0.5))+coord_flip()

#####################################
# Machine learning method - Hashtag #
#####################################

options(scipen = 999)
ht_sentiment <- read.csv(file = "C:/Users/mma/Downloads/MBD/Semester 2/SOCIAL MEDIA ANALYTICS/Group Project/Ref2.csv")

#load the downloaded data (Testing 500 tweets)
load("C:/Users/mma/Downloads/MBD/Semester 2/SOCIAL MEDIA ANALYTICS/Group Project/Data Real/ht_Microsoft1.RData")
ht_c_real <- ht_c

#load the downloaded data (Testing 500 tweets)
load("C:/Users/mma/Downloads/MBD/Semester 2/SOCIAL MEDIA ANALYTICS/Group Project/Data used/ht_Microsoft.RData")

ht_c <- ht_c %>%
  mutate(nid = rownames(ht_c))

ht_sentiment <- ht_sentiment %>%
  mutate(nid = rownames(ht_sentiment))

ht_sentiment <-  ht_sentiment %>% select(nid, sentiment_group)

#ht_sentiment <- 
SentimentReal <- as.data.frame(inner_join(ht_c, ht_sentiment))

names(SentimentReal)
# Make our dependent variable dichotomous
SentimentReal[,"sentiment_group"] <- as.factor(SentimentReal[,"sentiment_group"] )
y <- as.factor(SentimentReal[,"sentiment_group"] )
levels(y)

# Define proportion to be in training set 
p <- 0.8

# Define observations to be in training set (we use proportional sampling)
class1_train <- sample(which(y==as.integer(levels(y)[1])), floor(p*table(y)[1]),replace=FALSE)
class2_train <- sample(which(y==as.integer(levels(y)[2])), floor(p*table(y)[2]),replace=FALSE)
c(class1_train,class2_train)
training_locations <- c(class1_train,class2_train) 
training_ids <- SentimentReal[training_locations,"id"]
training_ids

#names(SentimentReal)

# Create a term frequency table for the training set
dtm <- SentimentReal %>% 
  unnest_tokens(output = "word",
                input = text,
                token = "words",
                drop=FALSE,to_lower=TRUE) %>%  
  anti_join(get_stopwords()) %>%
  count(id,word , sort=TRUE)%>%
  cast_dtm(document = id, term = word,
           value = n, weighting = tm::weightTf)

# subset the dtm for the training obs
dtm_t <- dtm[dtm$dimnames$Docs %in% training_ids,]

dtm.to.sm <- function(dtm) {sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,dims=c(dtm$nrow, dtm$ncol))}

# apply this function, take the column sums and find out which columns have a sum of zero
termsToDelete <- which(colSums(dtm.to.sm(dtm_t))==0)


# subset the dtm based on these columns 
dtm <- dtm[,-termsToDelete]


# split the final dtm into a dtm for the training set and one for the test set 

dtm_tr <-  dtm[dtm$dimnames$Docs %in% training_ids,]
dtm_test <-  dtm[!dtm$dimnames$Docs %in% training_ids,]

train=dtm.to.sm(dtm_tr)

k = 20
trainSVD <- irlba(t(train), nu=k, nv=k)

train <- as.data.frame(trainSVD$v)
# add our rownames again, as a columns, in order to be able to merge
train <- data.frame(cbind(id = rownames(dtm_tr),train),stringsAsFactors=FALSE)

test=dtm.to.sm(dtm_test)

test <- as.data.frame(as.matrix(test %*% trainSVD$u %*%  solve(diag(trainSVD$d))))

# add our rownames again, as columns, in order to be able to merge
test <- cbind(id = rownames(dtm_test),test,stringsAsFactors=FALSE)

# merge with any other data that we might need
names(SentimentReal[training_locations,c("id","sentiment_group")]) <- c("nid", "sentiment_group")

names(train) 

train <- inner_join(SentimentReal[training_locations,c("id","sentiment_group")],train )
test <- inner_join(SentimentReal[-training_locations,c("id","sentiment_group")],test)

#train <- merge(SentimentReal[training_locations,c("id","sentiment_group")],train,by="id")
#test <- merge(SentimentReal[-training_locations,c("id","sentiment_group")],test,by="id")

RF_model_train <- randomForest(x=train[,3:ncol(train)],y=train[,2],importance=TRUE,ntree=700)
RF_predict <- predict(RF_model_train,test[,3:ncol(test)],type = "prob")[,2]


test$sentiment_group <- ifelse(test$sentiment_group==1, 1, 0)
RF_predict <- ifelse(RF_predict <.5, 0, 1)

#predML<- prediction(RF_predict,test$sentiment_group)

# ROC curve
auc <- pROC::auc(RF_predict, test$sentiment_group)

auc
df_res <- as.data.frame(cbind(RF_predict, test$sentiment_group))
names(df_res) <- c("y_pred", "y_test")

# Calculate auc
if (!require("ROCR")) install.packages("ROCR", quiet=TRUE) ; require("ROCR")
if (!require("plotROC")) install.packages("plotROC", quiet=TRUE) ; require("plotROC")


#plot AUC
rocplot <- ggplot(df_res, aes(m = y_pred, d = y_test))+ plotROC::geom_roc(n.cuts=20,labels=FALSE)
rocplot + plotROC::style_roc(theme = theme_bw)


##plot ROC & Precision
precrec_obj <- precrec::evalmod(scores = df_res$y_pred, labels = df_res$y_test, mode="prcroc")
autoplot(precrec_obj)



##plot all metrics
precrec_obj <- precrec::evalmod(scores = df_res$y_pred, labels = df_res$y_test, mode="basic")
autoplot(precrec_obj)


#confusion matrix
MLmetrics::LiftAUC(df_res$y_pred, df_res$y_test)
MLmetrics::RMSE(df_res$y_pred, df_res$y_test)



###
confusion_matrix <- as.data.frame(table(df_res$y_pred, df_res$y_test))
confusion_matrix



##plot cf
ggplot(data = confusion_matrix,
       mapping = aes(x = Var1 ,
                     y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red")