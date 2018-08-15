
phraseCreator<-function(df,commentCol,removeStopWords=FALSE,phraseLength=2){
  library(dplyr)
  library(tidytext)
  
  df$temporaryKey<-1:length(df[[commentCol]])
  
  tidy_df<-unnest_tokens_(df,"word",commentCol)
  
  if(removeStopWords)
  {
    x=1
    data("stop_words")
    tidy_df<-tidy_df %>% anti_join(stop_words,by=c("word"="word"))
  }
  
  tidy_df$temporaryKey2<-1:length(tidy_df$word)
  temp<-select(tidy_df,temporaryKey,temporaryKey2,word)
  temp$temporaryKey2<-temp$temporaryKey2-1
  
  out<-left_join(tidy_df,temp,by=c("temporaryKey"="temporaryKey","temporaryKey2"="temporaryKey2"))
  
  if(phraseLength==3)
  {
    temp$temporaryKey2=temp$temporaryKey2-1
    out<-left_join(out,temp,by=c("temporaryKey"="temporaryKey","temporaryKey2"="temporaryKey2"))
    out$phrase<-paste(out$word.x,out$word.y,out$word,sep=" ")
  }else
  {
    out$phrase<-paste(out$word.x,out$word.y,sep=" ")
  }
  
  
  out$temporaryKey<-NULL
  out$temporaryKey2<-NULL
  
  return(out)
}


gg.gauge <- function(pos,breaks=c(0,50,100)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="#F8766D")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="#00BFC4")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(c(-1,0,1))))+
    annotate("text",x=0,y=0,label=paste0((pos-50)/50),vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
}

library(shiny)
library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(DT)
library(shinydashboard)

# Words that change the sentiment of the following word.
sentimentChangeWords<-c("not","wouldn't","couldn't")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ), 
  
   # Application title
   titlePanel("Text Miner"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        selectInput("selector1","Would you like to upload a file or paste text?",
                    c("Paste text"="paste","Upload a file"="upFile"),selected = "paste"),
        # Text box for pasting text in.
        
        conditionalPanel(
          condition = "input.selector1 == 'paste'",
          textAreaInput("text","Paste your text here:")
        ),
        
        conditionalPanel(
          condition = "input.selector1 == 'upFile'",
          # File Reader
          fileInput("file1", "Upload File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          
          checkboxInput("header", "Header", TRUE),
          
          # Input: Select separator ----
          radioButtons("sep", "Separator",
                       choices = c(Comma = ",",
                                   Semicolon = ";",
                                   Tab = "\t"),
                       selected = ","),
          
          # Select which column contains the text
          uiOutput("columnControls"),
          
          uiOutput("columnControls2")
        ), width =2
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        column(8,plotOutput("wordcloud",width="100%",height="500px")),
        column(4,plotOutput("sentimentGauge")),
        column(6,plotOutput("phrasePlot")),
        column(6,
               box(
                 title = NULL, width = NULL, status = "primary",
                 div(style = 'height:475px; overflow-y: scroll;overflow-x: scroll', 
                     DT::dataTableOutput('commentsTable'))
               )
               )
        
        
         
         #tableOutput("text1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Sets maximum file size to 100 MB
  options(shiny.maxRequestSize=100*1024^2) 
  
   text<-reactive({
     req(input$file1)
     temp<-read.csv(input$file1$datapath,
                    header = input$header,
                    sep = input$sep,stringsAsFactors = F)
     temp$key<-1:nrow(temp)
     temp
   })
   
   # Scores sentiment based on the entire comment
   sentiment<-reactive({
     text() %>% phraseCreator(input$selector2) %>%
       left_join(get_sentiments("bing"),by=c('word.y'='word')) %>% mutate(score=case_when(
         sentiment=="positive" ~ 1,
         sentiment=="negative" ~ -1,
         TRUE ~ 0
       )) %>% mutate(score=case_when(
         word.x %in% sentimentChangeWords ~ score*-1,
         TRUE ~ score
       )) %>% filter(score!=0) %>%
       group_by(key) %>% summarise(sentiment=mean(score))
   })
   
   output$wordPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     #if(input$text!="")
     {
       if(input$selector1=="paste")
       {
         req(input$text)
         text<-data.frame(text=input$text,stringsAsFactors = F)
         text %>% phraseCreator("text") %>% left_join(get_sentiments("bing"),by=c("word.y"="word")) %>%
           mutate(sentiment=case_when(
             word.x %in% sentimentChangeWords & sentiment=="positive" ~ "negative",
             word.x %in% sentimentChangeWords & sentiment=="negative" ~ "positive",
             TRUE ~ sentiment
           )) %>% mutate(word=word.y) %>% filter(!is.na(sentiment)) %>%
           count(sentiment,word) %>% mutate(topN=rank(desc(n),ties.method = "first")) %>% ungroup() %>%
           filter(topN<=30) %>% mutate(word=reorder(word,n)) %>%
           ggplot(aes(x=word,y=n,fill=sentiment)) + 
           geom_col(show.legend = FALSE) +
           facet_wrap(~sentiment, scales = "free") +
           coord_flip()
       }else
       {
         req(input$file1)
         text() %>% phraseCreator(input$selector2,removeStopWords=TRUE) %>% 
           left_join(sentiment()) %>% 
           mutate(sentiment=case_when(
             is.na(sentiment) ~ 0,
             TRUE ~ sentiment
           )) %>% group_by(word.y) %>%
           summarise(sentiment=mean(sentiment),n=length(word.y)) %>% 
           filter(!is.na(word.y)) %>%
           mutate(topN=rank(desc(n),ties.method = "first")) %>% ungroup() %>%
           filter(topN<=30) %>% mutate(word.y=reorder(word.y,n)) %>%
           ggplot(aes(x=word.y,y=n,fill=sentiment)) + 
           geom_col() +
           coord_flip()
       }
       
     }
   })
   
   
   
   output$phrasePlot <- renderPlot({
     # generate bins based on input$bins from ui.R
     #if(input$text!="")
     {
       if(input$selector1=="paste")
       {
         req(input$text)
         text<-data.frame(text=input$text,stringsAsFactors = F)
         text %>% phraseCreator("text",phraseLength = 3) %>% left_join(get_sentiments("bing"),by=c("word.y"="word")) %>%
           mutate(sentiment=case_when(
             word.x %in% sentimentChangeWords & sentiment=="positive" ~ "negative",
             word.x %in% sentimentChangeWords & sentiment=="negative" ~ "positive",
             TRUE ~ sentiment
           )) %>% mutate(phrase=paste(word.y,word,sep=" ")) %>% filter(!is.na(sentiment)) %>%
           count(sentiment,word) %>% mutate(topN=rank(desc(n),ties.method = "first")) %>% ungroup() %>%
           filter(topN<=30) %>% mutate(word=reorder(word,n)) %>%
           ggplot(aes(x=word,y=n,fill=sentiment)) + 
           geom_col(show.legend = FALSE) +
           facet_wrap(~sentiment, scales = "free") +
           coord_flip()
       }else
       {
         req(text())
         req(sentiment())
         text() %>% phraseCreator(input$selector2,removeStopWords=TRUE) %>% 
           left_join(sentiment()) %>% filter(!is.na(word.y)) %>% 
           mutate(sentiment=case_when(
             is.na(sentiment) ~ 0,
             TRUE ~ sentiment
           )) %>% group_by(phrase) %>%
           summarise(sentiment=mean(sentiment),n=length(phrase)) %>% 
           mutate(topN=rank(desc(n),ties.method = "first")) %>% ungroup() %>%
           filter(topN<=30) %>% mutate(phrase=reorder(phrase,n)) %>%
           ggplot(aes(x=phrase,y=n,fill=sentiment)) + 
           geom_col() +
           coord_flip()
       }
       
     }
   })
   
   
   output$wordcloud<-renderPlot({
     # Word clout plots
     # issues: sometimes bottom words are cut out, fix the size of the wordcloud object?
     {
       if(input$selector1=="paste")
       {
         req(input$text)
         text<-data.frame(text=input$text,stringsAsFactors = F)
         temp<-text %>% phraseCreator("text") %>% left_join(get_sentiments("bing"),by=c("word.y"="word")) %>%
           mutate(sentiment=case_when(
             word.x %in% sentimentChangeWords & sentiment=="positive" ~ "negative",
             word.x %in% sentimentChangeWords & sentiment=="negative" ~ "positive",
             TRUE ~ sentiment
           )) %>% mutate(word=word.y) %>% filter(!is.na(sentiment)) %>% filter(!is.na(word)) %>%
           count(sentiment,word) %>% mutate(topN=rank(desc(n),ties.method = "first")) %>% ungroup() %>%
           filter(topN<=50)
         
         wordcloud(temp$word,temp$n,min.freq=1,colors="#00BFC4")
       }else
       {
         req(input$file1)
         temp<-text() %>% phraseCreator(input$selector2,removeStopWords=TRUE) %>% 
           left_join(sentiment()) %>% group_by(word.y) %>%
           summarise(sentiment=mean(sentiment),n=length(word.y)) %>% 
           filter(!is.na(word.y)) %>%
           mutate(topN=rank(desc(n),ties.method = "first")) %>% ungroup() %>%
           filter(topN<=50) %>% mutate(word=reorder(word.y,n))
         
         wordcloud(temp$word,temp$n,colors="#00BFC4")
       }
     }
   })
   
   
   output$sentimentGauge<-renderPlot({
     # sentiment gauge: shows sentiment on a scale of -1 to 1 (negative to positive)
     {
       if(input$selector1=="paste")
       {
         req(input$text)
         text<-data.frame(text=input$text,stringsAsFactors = F)
         temp<-text %>% phraseCreator("text") %>% left_join(get_sentiments("bing"),by=c("word.y"="word")) %>%
           mutate(sentiment=case_when(
             word.x %in% sentimentChangeWords & sentiment=="positive" ~ "negative",
             word.x %in% sentimentChangeWords & sentiment=="negative" ~ "positive",
             TRUE ~ sentiment
           )) %>% mutate(word=word.y) %>% filter(!is.na(sentiment)) %>% filter(!is.na(word)) %>%
           mutate(score=case_when(
             sentiment=="positive" ~ 1,
             sentiment=="negative" ~ -1,
             TRUE ~ 0
           ))
           
         
         gg.gauge(round(50*mean(temp$score)+50,2))
       }else
       {
         req(input$file1)
         
         gg.gauge(round(50*mean(sentiment()$sentiment)+50,2))
       }
     }
   })
   
   
   output$commentsTable<-DT::renderDataTable({
     # table showing comments
     # need to make it filter based on clicks
     {
       req(text())
       DT::datatable(data.frame(comments=text()[[input$selector2]],stringsAsFactors = F),
                     options = list(dom = 't',pageLength=5000))
     }
   })
   
   
   output$sentimentOverTime<-renderPlot({
     # sentiment over time
     timeSeries <- inner_join(text(),sentiment())
     timeSeries[[input$dateSelector]]<-as.Date(timeSeries[[input$dateSelector]])
     temp<-max(timeSeries[[input$dateSelector]])-min(timeSeries[[input$dateSelector]])
     
     if(temp/182>1)
     {
       
     }
   })
   
   output$columnControls <- renderUI({
     req(input$file1)
     cnames=colnames(text())
     selectInput("selector2","Which column contains the text you would like to analyze?",
                 cnames,selected = cnames[1])
   })
   
   output$columnControls2 <- renderUI({
     req(input$file1)
     cnames = colnames(text())
     selectInput("dateSelector","Which column contains the dates?",
                 cnames, selected = cnames[2])
   })
   
   output$text1 <- renderTable({
     req(input$file1)
     read.csv(input$file1$datapath,
                    header = input$header,
                    sep = input$sep)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

