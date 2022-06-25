#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http:/shiny.rstudio.com/

# Load packages
if(!require("shiny")) install.packages("shiny"); library("shiny")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("plotly")) install.packages("plotly"); library("plotly")
if(!require("readxl")) install.packages("readxl"); library("readxl")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("DT")) install.packages("DT"); library("DT")
if(!require("haven")) install.packages("haven"); library("haven")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
if(!require("stringr")) install.packages("stringr"); library("stringr")
if(!require("maps")) install.packages("maps"); library("maps")
if(!require("maptools")) install.packages("maptools"); library("maptools")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("shinydashboard")) install.packages("shinydashboard"); library("shinydashboard")
if(!require("rsconnect")) install.packages("rsconnect"); library("rsconnect")
if(!require("shinythemes")) install.packages("shinythemes"); library("shinythemes")
if(!require("dashboardthemes")) install.packages("dashboardthemes"); library("dashboardthemes")
if(!require("sqldf")) install.packages("sqldf"); library("sqldf")
if(!require("rworldmap")) install.packages("rworldmap"); library("rworldmap")
if(!require("priceR")) install.packages("priceR"); library("priceR")
if(!require("rsconnect")) install.packages("rsconnect"); library("rsconnect")
if(!require("wordcloud")) install.packages("wordcloud"); library("wordcloud")
if(!require("textstem")) install.packages("textstem"); library("textstem")
if(!require("topicmodels")) install.packages("topicmodels"); library("topicmodels")
if(!require("tidytext")) install.packages("tidytext"); library("tidytext")




###################################### UI ###################################################

header <- dashboardHeader(title = "Social & Market Sentiment Profile: Microsoft")

sidebar <- dashboardSidebar( 
  
  #Sidebar
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("r-project")),
    menuItem("Summary", icon = icon("map"), tabName = "summary"),
    menuItem("News", tabName = "news", icon = icon("gamepad")),
    menuItem("Sentiment Stock Prediction", icon = icon("dice"), tabName = "st_predictions"),
    menuItem("Twitter Followers Analysis", icon = icon("map"), tabName = "hashtags"),
    menuItem("Twitter Tweets Analysis", icon = icon("map"), tabName = "tweets")
      )
  )


body <- dashboardBody(
  ### changing theme
  shinyDashboardThemes(
    theme = "blue_gradient"
  ),
  
  
  ############ Tab: Overview #################
  tabItems( 
    tabItem (tabName = "overview",
           mainPanel(
               fluidRow(
                 align = "center",
                 img(src='ieseg.png'),
                 h1(tags$span(style = "color: #fcba03;", "IESEG School of Management")),
                 br(),br(),
                 h1(tags$span(style = "color: #2f8f9c;", "Course: Social Media Analytics")),
                 br(),
                 h1(tags$span(style = "color: #2f8f9c;", "Social Media Profiling for Microsoft")),
                 h3(tags$span(style = "color: #3b8994;", "(Using social media and news data)")),
                 br(),br(),
                 h3(tags$span(style = "color: #2f8f9c;", "A group project by:")),
                 h3(tags$span(style = "color: #2f8f9c;", "RANA, Inderpreet, MA Muihan, HUSSAIN Hina; GALLET Charlotte")),
               ), width = 12
             )
    ),
    
    
    
    ############ Tab: summary #################    
    tabItem (tabName = "summary",
             fluidRow(
             h1("Summary"),
               align = "center",
             img(src='msft.png'),
               h1(tags$span(style = "color: #fcba03;", "Social Media Analysis for Microsoft")),
               h3(tags$span(style = "color: #2f8f9c;", "This Dashboard shows the social media analysis of Micorosft's Twitter handles.")),
               h3(tags$span(style = "color: #2f8f9c;", "We also used a news sentiment score based approach to predict the uptick or downtick in stock.:")),
             )
    ),
    

    ############ Tab: base table #################    
    tabItem (tabName = "basetable",
             downloadButton("downloadData", "Download"),
             div(style = 'overflow-x: scroll', DT::dataTableOutput('Basetable'))
             ),
    
    
    
    ############ Tab: News Analysis #################     
    tabItem (tabName = "news",  fluidRow(
      # Demographics page header
      titlePanel("Microsoft in News"),
      tabsetPanel(
        tabPanel("News coverage by topic", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 300,
                                   plotOutput("news", width = "100%")
                          )))),
        tabPanel("Topic Modeling on News", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   plotOutput("news_topics", width = "100%")
                          )))),
        tabPanel("News Word cloud", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 400,
                                   plotOutput("news_wordcloud", width = "100%")
                          )))),
        tabPanel("Sentiment over time", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   plotOutput("sent_news", width = "100%")
                          )))),
        
      ))),
    

    ############ Tab: stock predictions #################     
    tabItem (tabName = "st_predictions",  fluidRow(
      # Demographics page header
      titlePanel("Stock Predictions based on Sentiment Analysis"),
      tabsetPanel(
        tabPanel("Model Performance", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 300,
                                   plotOutput("model_perf", width = "100%")
                          )))),
          tabPanel("Model Evaluation", br(),
                   mainPanel(
                     column(width=11,
                            fluidRow(height = 600,
                                     plotOutput("model_stats", width = "100%")
                            )))),
      ))),  
    
    
    
      
#############################################
tabItem (tabName = "conclusion",
         fluidRow(
           img(src='conc.png', align = "left", height = 80),
           br(),br(),br(),br(),
           box(
           br(),
           h3("The most relevant insights from a business perspective that our analysis brought to the forefront are as follow:"),
           br(),
           h4(tags$div(
            tags$ul(tags$li("We have a small population of loyalists with a mid-range betting frequency, who seldomly win big, but bet a lot of money, making them our most profitable gamblers.")
                          )),
           h4(tags$div(
           tags$ul(tags$li("We have large population of passive or casual betters with low-frequency betting, whom are winning at a larger rate substantially than our loyalists")
          ))),
           h4(tags$div(
           tags$ul(tags$li("RFM is weakly correlated with profit for Bwin, and gamblers with a mid-range RFM value are our
            most profitable segment of gamblers.")))),
          h4(tags$div(
            tags$ul(tags$li("The majority of the customer base is from Germany or from Europe, hence we need to focus more on the customers from other regions and analyze why they don't use our products")))),
          h4(tags$div(
            tags$ul(tags$li("We can do a further analysis of the customer segments if we get more data points such as customer Age, Income etc."))))

           )
           , width = 12) )),

    
############ Tab: Twitter Hashtags #################     
tabItem (tabName = "hashtags",  fluidRow(
  # Demographics page header
  titlePanel("Social Media Analysis - Twitter"),
  tabsetPanel(
    
    tabPanel("Microsoft Follower's Map", br(),
               img(src='map.png')
               ),
    
    tabPanel("Tweet Types", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 300,
                               plotlyOutput("tweet_type", width = "100%")
                      )))),
    tabPanel("Tweet Frequency", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 300,
                               plotOutput("tweet_freq", width = "100%")
                      )))),
    tabPanel("Tweet Sources - (Application used to tweet)", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 300,
                               plotlyOutput("tweet_source", width = "100%")
                      )))),
    
    tabPanel("Most used Words for Microsoft for Twitter", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 300,
                               plotlyOutput("tweet_words", width = "100%")
                      )))),
    tabPanel("Most Active Accounts by Microsoft", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 300,
                               plotOutput("most_tweet_accounts", width = "100%")
                      )))),
    
    tabPanel("Topics by Tweets", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 300,
                               plotOutput("top_tweet_terms", width = "100%")
                      )))),
    
    tabPanel("Twitter Sentiment Analysis", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 300,
                               plotOutput("twitter_sent", width = "100%")
                      )))),
    
    tabPanel("Twitter Sentiment Scores Distribution", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 300,
                               plotOutput("twitter_sent_desc", width = "100%")
                      )))),
    
    tabPanel("WordCloud - Microsoft's Followers", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 300,
                               plotOutput("followers_word", width = "100%")
                      )))),
    
    tabPanel("Topic Modeling on MSFT followers based on POS", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 300,
                               plotOutput("twitter_topics_pos", width = "100%")
                      )))),
  ))),




############ Tab: Twitter Hashtags #################     
tabItem (tabName = "tweets",  fluidRow(
  # Demographics page header
  titlePanel("Twitter Tweets Analysis"),
  tabsetPanel(
    tabPanel("Wordcloud based on original text", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 500,
                               plotOutput("wc_d", width = "100%")
                      )))),
    tabPanel("Wordcloud after pre-processing data", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 500,
                               plotOutput("wc_d2", width = "100%")
                      )))),
    
    tabPanel("Wordcloud bi-gram based", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 500,
                               plotOutput("wc_d3", width = "100%")
                      )))),
    
    tabPanel("Sentiment Analysis for Microsoft Tweets and replies", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 500,
                               plotOutput("wc_d4", width = "100%")
                      )))),
    
    tabPanel("Sentiment over time", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 500,
                               plotOutput("wc_d5", width = "100%")
                      )))),
    tabPanel("Top 10 Tweets", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 500,
                               plotOutput("top_ten_tweets", width = "100%"),
                               plotOutput("top_ten_tweets2", width = "100%"),
                               plotOutput("top_ten_tweets3", width = "100%"),
                               plotOutput("top_ten_tweets4", width = "100%")
                      )))),
    tabPanel("Tweets Timeline", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 500,
                               plotOutput("ht_tmp", width = "100%")
                      )))),
    tabPanel("Popular #Hashtags", br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 500,
                               plotOutput("ht_tmp2", width = "100%")
                      )))),
    
    
  )))




  ))# end of tab item and body      
shinyUI(fluidPage(
                  dashboardPage(header, sidebar, body)
))