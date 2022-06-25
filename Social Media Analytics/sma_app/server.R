library(shiny)


load("./data/allNYTSearch.RData")
msid <- read.csv("./data/msid.csv")
load("./data/ht_Microsoft1.RData")
load("./data/mt_Users.RData")
load("./data/ht_Users.RData")
load("./data/mt_Microsoft1.RData")
load("./data/tt_Microsoft1.RData")
load("./data/followers.RData")
load("./data/ms.RData")
load("./data/Microsoft_Lists.RData")
load("./data/basetable.RData")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {


################ TAB: Base Table###############  
    output$Basetable <- DT::renderDataTable({
        Basetable <- base_table
        DT::datatable(Basetable)
    })


    output$news <- renderPlot({
        
        ###########################
        # Visualize coverage by section
        allNYTSearch %>% 
            group_by(response.docs.section_name) %>%
            summarize(count=n()) %>%
            mutate(percent = (count / sum(count))*100) %>%
            ggplot() +
            geom_bar(aes(y=percent, x=response.docs.section_name, fill=response.docs.section_name), 
                     stat = "identity") +
            coord_flip()
        })
    
    ################demographics###########
    output$news_wordcloud <- renderPlot({
        load("./data/words_only.RData")
        set.seed(123)
        
        wordcloud(words = words_only$word, 
                  freq = words_only$n, 
                  scale=c(5,.5),
                  max.words=50, 
                  colors=brewer.pal(8, "Dark2"))

        
    })
    
   
    
    output$sent_news <- renderPlot({
        load("./data/sentiment_summary.RData")
        ggplot(sentiment_summary, aes(date, score)) +
            geom_bar(stat = "identity", aes(fill=sentiment)) + 
            ggtitle(paste0("MSFT", ": News Sentiment Over Time"))
    
    })
    

    output$news_topics <- renderPlot({
        load("./data/top_news_terms.RData")

        top_news_terms %>%
            mutate(term = reorder_within(term, beta, topic)) %>%
            ggplot(aes(term, beta, fill = factor(topic))) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~ topic, scales = "free") +
            coord_flip() +
            scale_x_reordered()
        
    })
    
    
    output$model_perf <- renderPlot({
        load("./data/model_perf.RData")
        
        #plot AUC
        pred =ROCR::prediction(df_res$y_pred, df_res$y_test)
        perf = ROCR::performance(pred, "tpr", "fpr")
        plot(perf, lwd=2,  main="AUC - Random Forest", colorize=TRUE)
        abline(a=0, b=1)
        
    })    
    
    output$tweet_type <- renderPlotly({
        originaltweet <- ms[ms$is_retweet==FALSE & is.na(ms$reply_to_status_id), ] 
        most_liked <- originaltweet[order(-originaltweet$favorite_count),]
        ######Most retweeted tweet
        most_retweeted <- originaltweet[order(-originaltweet$retweet_count),]
        #Get retweets
        retweets <- ms[ms$is_retweet==TRUE,]
        Replies <- ms[!is.na(ms$reply_to_status_id),]
        tweet_types <- data.frame(Type = c("Original", "Retweets", "Replies"), 
                                  Count = c(nrow(originaltweet), nrow(retweets), nrow(Replies)))

        #####Visualize tweet types
        plot_ly(tweet_types,values=~Count,labels=~factor(Type),
                marker=list(colors=c("blue","yellow","purple")),type="pie") %>% 
            layout(title="Types of Tweets by Microsoft") 
        
        
    })    
    
    
    output$tweet_freq <- renderPlot({
        ######Plot frequency of tweets using ts_plot from rwteet
        #Reference: https://www.rdocumentation.org/packages/rtweet/versions/0.3.7/topics/ts_plot
        rtweet::ts_plot(ms, "month") + theme_minimal()
        
        
    })    
    
    
    output$tweet_source <- renderPlotly({
        ######Visualize where Microsoft tweets from
        source_tweets <- ms %>% 
            select(source) %>% 
            group_by(source) %>%
            summarize(count=n())
        plot_ly(source_tweets,values=~count,labels=~factor(source),marker=list(colors=c("light blue","purple")),type="pie") %>% layout(title="Source of Tweets by Microsoft")
        
        
        
    })    
    
    output$tweet_words <- renderPlotly({
        originaltweet <- ms[ms$is_retweet==FALSE & is.na(ms$reply_to_status_id), ] 
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
        
        
        plot_ly(wordfreq, x = ~freq, y = ~word, type = 'bar', orientation = 'h') %>% 
            layout(title="Count of Most frequent Words in Microsoft's Tweets",
                   xaxis = list(title = ""),
                   yaxis = list(title =""))
        
        
        
    })
    
    output$most_tweet_accounts <- renderPlot({
    retweets <- ms[ms$is_retweet==TRUE,]
    wordcloud(retweets$retweet_screen_name,
              min.freq=5,
              scale=c(3, 1),
              colors = brewer.pal(3, 'Dark2'),
              random.order=FALSE,
    )
    
    })
    
    
    output$top_tweet_terms <- renderPlot({
        load("./data/top_tweet_terms.RData")
        top_tweet_terms %>%
            mutate(term = reorder_within(term, beta, topic)) %>%
            ggplot(aes(term, beta, fill = factor(topic))) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~ topic, scales = "free") +
            coord_flip() +
            scale_x_reordered()
        
        
    })
    
    #WordCloud on Microsoft Followers
    output$followers_word <- renderPlot({
    load("./data/MSFT_Frequency.RData")
    wordcloud(MSFT_Frequency$word, MSFT_Frequency$freq,
              min.freq = 2, 
              max.words=50,
              colors = brewer.pal(3, 'Dark2'),
              scale=c(3,1))
    
    })
    
    output$twitter_sent <- renderPlot({
    load("./data/MSFT_summarySentiment.RData")        
    MSFT_summarySentiment %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
        coord_flip()
    })
    
    output$twitter_sent_desc <- renderPlot({
    load("./data/MSFT_summarySentiment.RData")   
        MSFT_users <- mutate(followers, message = gsub(x = followers$description, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))
        MSFT_tokenized <- MSFT_users %>% unnest_tokens(output = "word",
                                                       input = description, 
                                                       token = "words", 
                                                       drop=FALSE,to_lower=TRUE) 
        
        MSFT_tokenized <- filter(MSFT_tokenized, substr(word, 1, 1) != '#', substr(word, 1, 1) != '@')
        MSFT_tokenized <- MSFT_tokenized %>% anti_join(get_stopwords())
        MSFT_tokenized <- MSFT_tokenized %>% count(id, word)
    MSFT_usersSentiment <- inner_join(MSFT_tokenized,get_sentiments("bing"))
    MSFT_statusSentiment <- MSFT_usersSentiment %>%
        count(id, sentiment) %>%                # count the positives and negatives per id (status)
        spread(sentiment, n, fill = 0) %>%      # we want overall sentiment, and here we have text (positive and negative), making it difficult to direclty calculate sentiment. 
        mutate(sentiment = positive - negative)
    
    ggplot(MSFT_statusSentiment,
           aes(x=sentiment), bins) + geom_histogram(bins=4, color = "white")
    
    })
    
    
    output$twitter_topics_pos <- renderPlot({
    load("./data/MSFT_top_topics.RData")
    MSFT_top_topics %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered()
    
})
    output$model_stats <- renderPlot({
    load("./data/model_perf.RData")
    precrec_obj <- precrec::evalmod(scores = df_res$y_pred, labels = df_res$y_test, mode="basic")
    autoplot(precrec_obj)
    })
    
    
    output$wc_d <- renderPlot({
        load("./data/ht_ctf.RData")
        wordcloud(names(ht_ctf),ht_ctf,
                  max.words=50,
                  colors = brewer.pal(3, 'Dark2'),
                  scale=c(3,1))
    })
    
    output$wc_d2 <- renderPlot({
        load("./data/ht_cFreq.RData")
    wordcloud(ht_cFreq$word, ht_cFreq$freq,
              max.words=50,
              colors = brewer.pal(3, 'Dark2'),
              scale=c(3,1))
    })
    
    output$wc_d3 <- renderPlot({
        load("./data/ht_c_editBigramCount.RData")
        ht_c_editBiCount <- ht_c_editBigramCount %>% group_by(bigram) %>% summarize(freq = n())
        wordcloud(ht_c_editBiCount$bigram,ht_c_editBiCount$freq,
                  colors = brewer.pal(3, 'Dark2'),
                  max.words = 40)
        
    })
    
    output$wc_d4 <- renderPlot({
        load("./data/ht_c_summarySentiment_edited.RData")
    ht_c_summarySentiment_edited %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
        coord_flip()
    })
    
  
    output$wc_d5 <- renderPlot({
        load("./data/ht_sentiment_1.RData")
        load("./data/ht_statusSentiment_3.RData")
        lim <- max(abs(ht_sentiment))
        ht_time  <- ht_statusSentiment$created_at
        
        ht_sentiment <- ht_statusSentiment[order(ht_time),"Sentiment"]
        
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
        
})

    output$top_ten_tweets <- renderPlot({    
        load("./data/top_ht_c_retweet.RData")
        ggplot(top_ht_c_retweet, aes(x=reorder(text, public_metrics.retweet_count), y=public_metrics.retweet_count))+
            geom_bar(stat="identity", fill =  "#377F97")+
            ggtitle("Top 10 Most Retweeted Tweets")+
            labs(x="Organic Tweets", y=("Retweet Count"))+
            theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
                  plot.title = element_text(hjust = 0.5))+
            coord_flip()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 80))
    })
    
    
    output$top_ten_tweets2 <- renderPlot({    
        load("./data/top_ht_c_reply.RData")
        ggplot(top_ht_c_reply, aes(x=reorder(text, public_metrics.reply_count), y=public_metrics.reply_count))+
            geom_bar(stat="identity", fill =  "#377F97")+
            ggtitle("Top 10 Most Reply Tweets")+
            labs(x="Organic Tweets", y=("Reply Count"))+
            theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
                  plot.title = element_text(hjust = 0.5))+
            coord_flip()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 80))
        
    })
    
    
    output$top_ten_tweets3 <- renderPlot({ 
        load("./data/top_ht_c_like.RData")
    ggplot(top_ht_c_like, aes(x=reorder(text, public_metrics.like_count), y=public_metrics.like_count))+
        geom_bar(stat="identity", fill =  "#377F97")+
        ggtitle("Top 10 Most Like Tweets")+
        labs(x="Organic Tweets", y=("Like Count"))+
        theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
              plot.title = element_text(hjust = 0.5))+
        coord_flip()+
        scale_x_discrete(labels = function(x) str_wrap(x, width = 80))
        
    })
    
    output$top_ten_tweets4 <- renderPlot({ 
        load("./data/top_ht_c_quote.RData")
        ggplot(top_ht_c_quote, aes(x=reorder(text, public_metrics.quote_count), y=public_metrics.quote_count))+
            geom_bar(stat="identity", fill =  "#377F97")+
            ggtitle("Top 10 Most Quote Tweets")+
            labs(x="Organic Tweets", y=("Quote Count"))+
            theme(axis.text.y = element_text(face="bold", color="black", size=8, hjust = 1), 
                  plot.title = element_text(hjust = 0.5))+
            coord_flip()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 80))
        
        
    })
    
    output$ht_tmp2 <- renderPlot({ 
        load("./data/tmp_1.RData")
    ggplot(tmp, aes(x=reorder(Var1, Freq), y=Freq))+
        geom_bar(stat="identity", fill = "#377F97")+
        ggtitle("Popular Hashtags")+labs(x="Hashtags", y="Frequency")+
        theme(axis.text.x = element_text(angle = 70,face="bold", 
                                         color="black", size=12, hjust = 1),
              plot.title = element_text(hjust = 0.5))+coord_flip()
})
    
    }) #Server End
