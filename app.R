rm(list=ls())

# Load packages
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(tm)
library(RTextTools)
library(wordcloud)
library(ggplot2)
library(tidyr)
library(tidytext)
library(topicmodels)
library(reshape2)


# Read data in
vir <- read.csv("Virginia Beach.csv")
new_vir <- vir[,c(7,15)]

ann <- read.csv("Annapolis.csv")
new_ann <- ann[,c(7,15)]

tuc <- read.csv("Tuscon.csv")
new_tuc <- tuc[,c(7,15)]

ham <- read.csv("Hampton Data.csv")
new_ham <- ham[,c(7,15)]


# Define UI for application that plots features of movies 
ui <- navbarPage("Hampton Inn",
                   tabPanel("Problem & Method", 
                            mainPanel( 
                              h3("Brief Background and End User"),
                              p("The end user of the app will be the owner and the management team of the 3 Hampton Inns. 
                                Currently they are relying on the Likelihood to Recommend (LTR) to gauge their performance. 
                                They feel that numeric values such as LTR give the managers misleading  and oftentimes vague information about 
                                how they are doing in general and where they are doing wrong. In order to dig into the issue areas in depth, 
                                the managers believe text and sentiment analysis will be more helpful to generate useful information. 
                                They expect to have a better understanding of the areas are being talked about the most and and what guests say about
                                their hotels after a stay."),
                              h3("Problem the app is solving : Hampton Inn Guest Reviews "),
                              p("The managers of the Hotels feel that going over all reviews individually is cumbersome, mentally taxing and potentially more expensive as they will have to devote some time and energy only for this job, and manual process will be more prone to errors compared to text analysis. Text analysis will help them with a quick analysis of language and interpretation.

Text analysis will also help the managers to map and visualize with word frequency histograms, wordclouds and topics and categorize the terms (columns) as negative and positive and then run analysis. They could use the visualizations to understand the major areas that are being talked about or need attention and thereby can delve more energy and resources to strategies that will be relevant rather than relying on scattered inputs by humans via individual analysis of the reviews or LTR. This app will help analyze guest reviews that are critical for businesses, in our case a 3 hotels from the Hampton Inn chain to acquire insights about their services, competitors, and improvements. 
"),
                              br(),
                         
                              h3("Data and Dataset Modelling"),
                              p("We have pulled a dataset from https://www.kaggle.com/datafiniti/hotel-reviews. The dataset has over 8600 observations/reviews on hotels across the USA. Reviews are extracted from tripadvisor, hotels.com, expedia.com. We are only focusing on the reviews for 3/5 of the Hampton Inn Hotels for for our app."),
                              br(),
                              HTML("
                                   Hampton Inn - Virginia Beach Oceanfront North, VA - 334 Reviews/Documents </br>
                                   Hampton Inn - Tucson Airport, AZ - 184 Reviews/Documents  </br>
                                   Hampton Inn and Suites - Annapolis, MD - 171 Reviews/Documents  </br>"
                            ), #HTML close
                              br(),
                              p("We preprocessed the Data by removing symbols, white space, numbers, punctuations, Stopwords. 
                                Then we We restructure the dataset as one token-per-row format with R tidytext. 
                                In R tidy text format is defined as as being a table with one-token-per-row. 
                                A token is a meaningful unit of text, such as a word, that we are interested in using for analysis, 
                                and tokenization is the process of splitting text into tokens. .
                                "),
                              
                              h3("Text Analysis"),
                              p("Using tidy data principles is a powerful way to make handling data easier and more effective, and this is no less true when it comes to dealing with text. 
                                As described by Hadley Wickham (Wickham 2014), tidy data has a specific structure:"),
                              HTML('<b>Each variable is a column</b></br>'),
                              HTML('<b>Each observation is a row</b></br>'),
                              HTML('<b>Each type of observational unit is a table </b></br>'),
                              p(""),
                              p("Tidy text format is defined as being a table with one-token-per-row. 
                                A token is a meaningful unit of text, such as a word, 
                                that we are interested in using for analysis, and tokenization is the process of splitting text into tokens. 
                                This one-token-per-row structure is in contrast to the ways text is often stored in current analyses, perhaps as strings or in a document-term matrix. For tidy text mining, the token that is stored in each row is most often a single word, but can also be an n-gram, sentence, or paragraph. In the tidytext package, we provide functionality to tokenize by commonly used units of text like these and convert to a one-term-per-row format."),
                              p(""),
                              h3('Topic Modelling with Latent Dirichlet Allocation'),
                              p("We have also used Latent Dirichlet Allocation (LDA) for analyzing the reviews, which would essentially convert the document-term matrix from BoW to document-topics and document-term matrices. The document-topics matrix will then divide each document into topics and topic-term matrix will divide each topic by words that are representative of the topic. This model will assume that a topic will be the distribution of words and also assumes that each document will reflect the number of topics in different proportion and the presence of the topics in the reviews will be inferred via presence of words in the review."),
                              
                              h3("Sentiment Analysis"),
                              p("Once we finished creating the word frequencies and wordcloud that allowed us to analyze which words are used more frequently 
                                in the reviews and compare the reviews, we wanted to understand the emotional intents of the words and hence use a sentiment 
                                lexicon to dig a little deeper. With data in a tidy format, sentiment analysis can be done as an inner join. 
                                There are a variety of methods and dictionaries exist for evaluating the opinion or emotion in text. 
                                The tidytext package contains several sentiment lexicons in the sentiments dataset. 
                                We had an option to choose from AFINN, Bing and nrc that are based on unigrams or single words. 
                                AFINN lexicon assigns a word with score on a scale of -5 - 5, nrc categorized words in binary manner, 
                                for example yes and no. We however, decided to go with Bing because, although it categorizes words in binary fashion, 
                                the categorization closely matches with the sentiments (negative or positive) 
                                that we are trying to pull out from our analysis also the sentiments can also be easily understood by the management with “Positive” and “Negative” labels. ")
                              ) #main panel close
                              ), #tab panel close
                   tabPanel("App",
                            # Sidebar layout with a input and output definitions 
                            sidebarLayout(
                              
                              # Inputs
                              sidebarPanel(
                                
                                # Text instructions
                                #HTML(paste("Enter a value between 1 and", n_total)),
                                # Select variable for y-axis
                                selectInput(inputId = "hotel", 
                                            label = "Hotels",
                                            choices = c("Virginia Beach", "Annapolis", "Tucson"), 
                                            selected = "Virgina Beach"),
                              width = 2),
                              # Outputs
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Text Analysis",
                                                     fluidRow(align="center",
                                                       verticalLayout(
                                                         plotOutput(outputId = "wordcloud", height = 600, width=600),
                                                         plotOutput(outputId = "freq", height=600)
                                                       )#verticalLayout close
                                                     )#fluidRow close
                                                     ),#tab panel close for text analysis
                                            tabPanel("Topic Modeling", plotOutput(outputId = "topic_histogram", height = 800)
                                                     ),#tabPanel close
                                            tabPanel("Sentiment Analysis", 
                                                     fluidRow(
                                                       splitLayout(
                                                       plotOutput(outputId = "comparison_plot", height = 1000), 
                                                       plotOutput(outputId = "histogram", height = 1000)
                                                       )
                                                     )#fluidRow close
                                                     ),#tabPanel close for sentiment analysis
                                            tabPanel("View Table",
                                                     h3("Review Data"),
                                                     #tableOutput("table")
                                                     DT::dataTableOutput("reviewTable")
                                                     )#tabPanel close for view table
                                            )#tabset close
                                
                              )#main panel close
                        ) #sidebar layout close
                   ),#tabpanel close 
                  tabPanel("Interpretation Guide",
                           mainPanel(
                             h5("Here is a simple guide for interpreting the results:"),
                             h3("Text Analysis"),
                             HTML('<b>WordCloud</b>'),
                             p("The size of a word’s text in Figure is in proportion to its frequency within its sentiment. The biggest text/word in the wordcloud is the word that is being discussed the most number of times in the reviews."),
                             HTML('<b>Frequency Histogram</b>'),
                             p("Frequency histogram is a visualization of the most common words. From the histogram we can infer that the most commonly used words in the documents. The management can focus on certain words for further insights. "),
                             br(),
                             h3("Topic Modelling"),
                             p("The number on top of each histogram represent the topic, and the most frequently used words under each topic. Each topic will be a distribution of words and each review will reflect the number of topics in different proportion and the presence of the topics in the reviews will be inferred via the presence of words in the review. Looking at the topic histogram, if the word “Room” is being talked about the most in topic 1, then we can assume that topic 1 is representing the topic “Room”."),
                             br(),
                             h3("Sentiment Analysis"),
                             HTML("<b>Positive vs Negative Word </b>"),
                             p("As we have our data frame with both sentiment and word, we can now analyze word counts that contribute to each sentiment. Combining both word and sentiment, we find out the proportion of each sentiment h (negative or positive)  through  a histogram. "),
                             
                             HTML("<b>Comparison Cloud</b>"),
                             p("We have also created a wordcloud of positive and negative sentiments on top of the wordcloud for text analysis for a categorized visualization. We tag positive and negative words using an inner join, then found the most common positive and negative words. The wordcloud is to be interpreted in the similar fashion as the wordcloud from the text analysis.

                               ")
                             
                           ))
                            
                   
)

  
# Define server function required to create the scatterplot
server <- function(input, output) {
  
  #get d_data from dtm
  dtm_d = reactive({
    dtm_vir <- DocumentTermMatrix(word())
    findFreqTerms(dtm_vir, lowfreq = 200)
    dtm_vir1 <- TermDocumentMatrix(word())
    m_vir <- as.matrix(dtm_vir1)
    v_vir <- sort(rowSums(m_vir),decreasing=TRUE)
    d_vir <- data.frame(word = names(v_vir),freq=v_vir)
    return (d_vir)
  })
  
  bing_word_counts = reactive({
    bing_word_counts_tuc <- dtm_d() %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
  })
  
  ######sentiment analysis#######
  # Create comparison plot
  output$comparison_plot <- renderPlot({
    dtm_d() %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("red", "black"),scale=c(1,.5),
                       max.words = 300)
  }, height = 800, width = 600)
  
  # Create histogram
  output$histogram <- renderPlot({
   ggplot(bing_word_counts(),aes(x=sentiment, fill=sentiment))+
        geom_bar()+theme_classic()+scale_fill_manual(values = c("red","black"))
    
  }, height = 700, width = 500)

  
  #new_data
  datasetInput_cor <- reactive({
    switch(input$hotel,
           "Virginia Beach" = new_vir$reviews.text,
           "Annapolis" = new_ann$reviews.text,
           "Tucson" = new_tuc$reviews.text)
  })
  
  
  #corpus
  word = reactive({
    myCorpus = Corpus(VectorSource(datasetInput_cor()))
    toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) # a function to clean /,@,\\,|
    corpus_vir <- tm_map(myCorpus, toSpace, enc2utf8("/|@|\\|"))
    corpus_vir <- tm_map(corpus_vir, content_transformer(tolower))
    corpus_vir <- tm_map(corpus_vir, removePunctuation)
    corpus_vir <- tm_map(corpus_vir, removeNumbers)
    corpus_vir <- tm_map(corpus_vir, stripWhitespace)
    corpus_vir <- tm_map(corpus_vir, removeWords, stopwords(kind="en"))
    corpus_vir <- tm_map(corpus_vir, removeWords, c("ive"))
    
  })
  
  
  #dtm_data
  dtm = reactive({
    dtm_vir <- DocumentTermMatrix(word())
    findFreqTerms(dtm_vir, lowfreq = 200)
    dtm_vir1 <- TermDocumentMatrix(word())
    m_vir <- as.matrix(dtm_vir1)
    v_vir <- sort(rowSums(m_vir),decreasing=TRUE)
    d_vir <- data.frame(word = names(v_vir),freq=v_vir)
    #head(d_vir, 10)
    row_vir <- apply(dtm_vir , 1, sum) #Find the sum of words in each Document
    dtm_vir.new   <- dtm_vir[row_vir> 0, ]  
  })

  
  #top 20 word
  toptwenty = reactive({
    dtm_vir <- DocumentTermMatrix(word())
    findFreqTerms(dtm_vir, lowfreq = 200)
    dtm_vir1 <- TermDocumentMatrix(word())
    m_vir <- as.matrix(dtm_vir1)
    v_vir <- sort(rowSums(m_vir),decreasing=TRUE)
    d_vir <- data.frame(word = names(v_vir),freq=v_vir)
    #head(d_vir, 10)
    row_vir <- apply(dtm_vir , 1, sum) #Find the sum of words in each Document
    dtm_vir.new   <- dtm_vir[row_vir> 0, ]  
    top_vir<- d_vir[1:20,]
    top_vir$word <- factor(top_vir$word, levels = top_vir$word[order(top_vir$freq)])
    return (top_vir)
  })
  
 
  #LDA
  lda = reactive({
    set.seed(234)
    lda_vir <- LDA(dtm(), k = 10, method = "Gibbs", control = NULL)
  })
  
  #topics
  top = reactive({
    topics_vir <- tidy(lda(), matrix = "beta")
  })
  
  #topics term
  top_terms = reactive({
    top_terms_vir <- top() %>%
      group_by(topic) %>%
      top_n(5, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)
    top_terms_vir <- top_terms_vir %>%
      mutate(term = reorder(term, beta))
  })
  
  
  ######topics modeling########## 
  # Create topics histogram
  output$topic_histogram <- renderPlot({
    ggplot(top_terms(), aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip()+theme_bw()
  })
  
  
  ##########text analysis##########
  # Create wordcloud for each hotel
  output$wordcloud <- renderPlot({
    set.seed(1234)
    wordcloud(words=word(), min.freq=2, max.words=150, scale=c(1.5, .5),colors=brewer.pal(6, "Dark2"),random.order=FALSE)
  }, height = 500, width = 500, res=100)

  
  # frequency histogram
  output$freq <- renderPlot({
    ggplot(toptwenty(), aes(word, freq, fill=word))+
      geom_bar(stat="identity")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ylab("Number of Times Word Appears in reviews")+
      xlab("")+
      guides(fill=FALSE)+ coord_flip()
    },  height = 500, width = 500)
  
  # Show table
  output$reviewTable = DT::renderDataTable({
    new_ham[1:20,] #show only the first 10 rows of the data
  })
}


#shinyApp object
shinyApp(ui=ui, server=server, options = list(height = 1080))
