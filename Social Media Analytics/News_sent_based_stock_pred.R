#load libraries


libs <- c('httr','jsonlite','config','rvest','bea.R','devtools','gtrendsR','tidyquant','ggplot2',
          'DataCombine','topicmodels','tidyr','magrittr','stringr','lubridate','dplyr',
          'data.table','textstem','randomForest', 'httr','jsonlite','config','rvest',
          'bea.R','devtools','gtrendsR','tidyquant', 'plotly','ggplot2','DT', 'wordcloud', 
          'wordcloud2','topicmodels','tidytext','dplyr','MLmetrics','mltools','neuralnet',
          'plotROC','ROCR','precrec','AUC','pROC','plotROC','GeomRoc','caret')



for (i in libs){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

setwd("C:/Users/irana/OneDrive - IESEG/Documents/Courses/Semester_2/SMA/Group_Project")
source("./data/tokens.R")

####################Fetch news from NY Times API####################

# set parameters
term <- "microsoft" # Need to use + to string together separate words
begin_date <- "20200101"
end_date <- "20220130"

#create API url
baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",nyt_api, sep="")

#load in JSON
initialQuery <- fromJSON(baseurl)

#create a variable to loop through pages
maxPages <- round((initialQuery$response$meta$hits[1] / 20)-1) 

#loop to extract data by looping through pages
pages <- list()
for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  message("Retrieving page ", i)
  pages[[i+1]] <- nytSearch 
  Sys.sleep(5) 
}

#combine all pages into a single dataframe
allNYTSearch <- rbind_pages(pages)

#save data
#save(allNYTSearch, file ="allNYTSearch.RData")

#load data
load("./data/allNYTSearch.RData")

#check data date range
min(allNYTSearch$response.docs.pub_date)
max(allNYTSearch$response.docs.pub_date)


# Visualize coverage by section
allNYTSearch %>% 
  group_by(response.docs.section_name) %>%
  summarize(count=n()) %>%
  mutate(percent = (count / sum(count))*100) %>%
  filter(percent >2) %>%
  ggplot() +
  geom_bar(aes(y=percent, x=response.docs.section_name, fill=response.docs.section_name), 
           stat = "identity") +
  coord_flip()

###check by date - not useful (maybe subset by dates and plot?)
allNYTSearch %>%
  mutate(pubDay=gsub("T.*","",response.docs.pub_date)) %>%
  group_by(pubDay) %>%
  summarise(count=n()) %>%
  #filter(count >= 2) %>%
  ggplot() +
  geom_bar(aes(x=reorder(pubDay, count), y=count), stat="identity") + coord_flip()

#datatable(allNYTSearch$response.docs.lead_paragraph)

#########################Topic Modeling Start###################
#set seed to replicate results
set.seed(123)

#tokenize news articles
news_tokenized <- allNYTSearch %>% unnest_tokens(output = "word",
                                               input = response.docs.lead_paragraph,
                                               token = "words",
                                               drop=FALSE,to_lower=TRUE)
##remove stop words
news_tokenized <- news_tokenized %>%
  anti_join(stop_words) %>%       
  count(response.docs._id,word , sort=TRUE) 


#cast as dtm matrix
news_tokenized_dtm <- news_tokenized %>%
  cast_dtm(document = response.docs._id, term = word,
           value = n, weighting = tm::weightTf)


#run LDA
news_lda <- LDA(news_tokenized_dtm, k = 6,
                method="gibbs",
                control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6) )

#clean up the data object into a readable table
news_topics <- tidy(news_lda, matrix = "beta")

# get the top terms per topic
top_news_terms <- news_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


# name the topics based on news terms
top_news_terms$topic[top_news_terms$topic == 1] = "Stock Market"
top_news_terms$topic[top_news_terms$topic == 2] = "Covid WFH"
top_news_terms$topic[top_news_terms$topic == 3] = "Gaming/XBox"
top_news_terms$topic[top_news_terms$topic == 4] = "Govt. Policies"
top_news_terms$topic[top_news_terms$topic == 5] = "Tech Tuesday"
top_news_terms$topic[top_news_terms$topic == 6] = "Bill Gates - Covid efforts"

#save data
#save(top_news_terms, file="top_news_terms.RData")

#plot topics
top_news_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

##topics per doc
news_documents <- tidy(news_lda, matrix = "gamma")

# Choose, per news, the most important topic (the one with the highest weight)
news_doc_topic <- news_documents %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) 

news_doc_topic %>%
  group_by(topic) %>% 
  summarise(nbr_documents = n())

#remove unwanted columns
ny_articles <- allNYTSearch %>%
  select(response.docs.pub_date, response.docs.section_name, response.docs.headline.main,
         response.docs.lead_paragraph)

##########################topic modeling end########################

##########################get stock data start########################

#Load stock data

#set start and end dates
date_from = substr(today()-dyears(3),1,10)
date_to = substr(today(),1,10)

#set ticker value
symbol = "MSFT"

#get stocks
stock <- tq_get(symbol,
                from = date_from,
                to = date_to,
                get = "stock.prices")

#order by date
stock <- stock[order(stock$date),]

#calculate change % by date
stock$change <- c(-diff(stock$close)/stock$close[-1] *  100, NA)

#calculate daily lag
stock$lag <- stock$change-(lag(stock$change))/lag(stock$change)

#look at the data
datatable(head(stock,5))

##########combine stock data with with news based on date################
ny_articles <- ny_articles %>%   rename(date = response.docs.pub_date)

ny_articles$date = lubridate::as_date(ny_articles$date)
stock$date = lubridate::as_date(stock$date)

stock_news <- inner_join(ny_articles, stock)

####################################data load end#############################

#see the stock trends
plot(stock_news$date, stock_news$open)

#check date ranges of combined data
min(stock_news$date)
max(stock_news$date)

datatable(head(stock_news))

str(stock_news)

names(stock_news)

################## sentiments analysis start###################

#tokenize data
stock_news_tokenized <- stock_news %>%
  #select(c("title","description", "content", "publishedAt")) %>%
  unnest_tokens(word, response.docs.lead_paragraph) %>%
  filter(!word %in% append(stop_words$word, values = "chars"), str_detect(word, "^[a-z']+$"))


#lemmatize words
stock_news_tokenized <- stock_news_tokenized %>% mutate(word = lemmatize_words(word)) 


#get word frequencies
words_only <- stock_news_tokenized %>%
  count(word, sort =TRUE)

#save data for ease of access
#save(words_only, file = "words_only.RData")

set.seed(123)
#plot wordcloud
wordcloud(words = words_only$word, 
          freq = words_only$n, 
          scale=c(5,.5),
          max.words=50, 
          colors=brewer.pal(8, "Dark2"))


wordcloud2(data=words_only,
           size=.2, color='random-light',
           backgroundColor="black", 
           shape = 'triangle')

#####News Sentiment over Time
afinn <- get_sentiments("afinn")
#names(stock_news_tokenized)


##join with sentiment dictionart
sentiment_summary <- stock_news_tokenized %>%
  left_join(afinn) %>%
  filter(!is.na(value)) %>%
  group_by(date, response.docs.section_name, response.docs.headline.main,
           symbol, open, high, low, close, volume, adjusted, change, lag, word) %>%
  summarise(score = mean(value)) %>%
  mutate(sentiment = ifelse(score>0, "positive","negative")) 

#look at the data
datatable(sentiment_summary)


#plot sentiments by time
#save(sentiment_summary, file = "sentiment_summary.RData")

ggplot(sentiment_summary, aes(date, score)) +
  geom_bar(stat = "identity", aes(fill=sentiment)) + 
  ggtitle(paste0(symbol, ": News Sentiment Over Time"))

########################sentiment analysis end####################################


########################sentiment based stock prediction start####################################
base_table <- sentiment_summary
##declare target variable

base_table['positive_poc'] <- ifelse(base_table$change > 0.5 , 1, 0)

#prep base table for modeling
base_table <- base_table %>% 
  mutate(value = 1)  %>% 
  mutate(score = scale(score))  %>% 
  spread(sentiment, value,  fill = 0) %>% 
  select(-c(response.docs.section_name, word))

base_table <- na.omit(base_table)

base_table <- base_table %>% 
  ungroup %>%
  select(date,open, high, low, close, volume, adjusted, change, lag, score, positive_poc) %>%
  distinct

#save(base_table, file = "basetable.RData")
load("./data/basetable.RData")
datatable(base_table)

smp_size <- floor(0.75 * nrow(base_table))

## set the seed to make results reproducible
set.seed(123)
##we splot train and test by date due to the nature of this experiment
x_train <- base_table %>% filter(base_table$date <='2021-09-01')
x_test <- base_table %>% filter(base_table$date >'2021-09-01')

##create target variable objects
y_train <- x_train$positive_poc
y_test <- x_test$positive_poc

#remove target from train and test objects
x_train$positive_poc <- NULL
x_test$positive_poc <- NULL

dim(x_test)
length(y_test)
dim(x_train)
length(y_train)

#set scipen to avoid scientific notations
options(scipen = 999)
set.seed(123)

#check for optimal no of trees
#replicate(5000, mean(randomForest(x=x_train, y=y_train)$ntree))

#train model
rf <- randomForest(x=x_train, y=y_train, ntree = (500))

#check mse
mean(rf$mse)


y_pred <- predict(rf, newdata = x_test)
y_pred_proba <- y_pred
y_pred <- ifelse(y_pred<0, 0, 1)

#predict(rf, newdata = x_test)
df_res <- as.data.frame(list(
  y_test=y_test,
  y_pred=y_pred
))


#save(df_res, file = "model_perf.RData")
#load("model_perf.RData")
###########check model preformance############
auc <- pROC::auc(df_res$y_pred, df_res$y_test)
auc


#plot AUC
rocplot <- ggplot(df_res, aes(m = y_pred, d = y_test))+ plotROC::geom_roc(n.cuts=20,labels=FALSE)
rocplot + plotROC::style_roc(theme = theme_bw)

##plot ROC & Precision
precrec_obj <- precrec::evalmod(scores = df_res$y_pred, labels = df_res$y_test, mode="prcroc")
autoplot(precrec_obj)

##plot all metrics
precrec_obj <- precrec::evalmod(scores = df_res$y_pred, labels = df_res$y_test, mode="basic")
autoplot(precrec_obj)

#plot AUC
pred =ROCR::prediction(y_pred, y_test)
perf = ROCR::performance(pred, "tpr", "fpr")
plot(perf, lwd=2,  main="AUC - Random Forest", colorize=TRUE)
abline(a=0, b=1)

#confusion matrix
MLmetrics::LiftAUC(df_res$y_pred, df_res$y_test)
MLmetrics::RMSE(df_res$y_pred, df_res$y_test)

###
confusion_matrix <- as.data.frame(table(df_res$y_pred, df_res$y_test))
colnames(confusion_matrix) <- c('predicted', 'actual', 'Freq')

confusion_matrix



##plot cf
ggplot(data = confusion_matrix,
       mapping = aes(x = predicted ,
                     y = actual)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red")

##check if stock rinse and fall patern matchec with predictions and actual
plot_data <- x_test

plot_data$pred <- df_res$y_pred

g <- ggplot(plot_data, aes(date))  
g <- g + geom_line(aes(y=y_test), colour="red")
g <- g + geom_line(aes(y=y_pred), colour="green")
g
#head(df_res)

#####################################model performance end########################

#####################################more model experiments########################

#####################################try neaural networks########################

x_train <- base_table %>% filter(base_table$date <='2021-12-01')
  x_test <- base_table %>% filter(base_table$date >'2021-12-01')


y_train <- x_train$positive_poc
y_test <- x_test$positive_poc

x_train$positive_poc <- NULL
x_test$positive_poc <- NULL

#?neuralnet
nn=neuralnet(y_train~open+high+low+close+volume+adjusted+change+lag+score,
             data=x_train, hidden=15,act.fct = "tanh",
             linear.output = TRUE)

plot(nn)

Predict <- predict(nn, x_test, type='raw')
#Predict=compute(nn,x_test)
#Predict$net.result
#prob <- Predict$net.result
pred <- ifelse(prob>.5, 3, 5)
#pred
#prob
#pred



pred = prediction(pred, y_test)

perf = performance(pred, "tpr", "fpr")
plot(perf, lwd=2, col="blue", main="ROC - Title")



pred_nn <- as.data.frame(list(
  y_test_nn=y_test,
  y_pred_nn=pred
))


AUC(prob, y_test)

ConfusionMatrix(pred_nn$y_pred_nn, pred_nn$y_test_nn)


confusion_matrix <- as.data.frame(table(pred_nn$y_pred_nn, pred_nn$y_test_nn))

ggplot(data = confusion_matrix,
       mapping = aes(x = Var1 ,
                     y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red")