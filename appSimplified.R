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
library(lubridate)
library(readxl)

bing<-get_sentiments("bing")
adds<-data.frame(word=c("maintenance","fee","fees","paid","costs","pressure","pressured"),
                 sentiment=c("negative","negative","negative","negative","negative","negative","negative"))
bing<-rbind(bing,adds)

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
      
      # Input: Select separator ----
      radioButtons("sep", "Upload File Type",
                   choices = c("Comma Delimited" = ",",
                               "Semicolon Delimited" = ";",
                               "Tab Delimited" = "\t",
                               "Excel" = 'xlsx'),
                   selected = ","),
      
      
      fileInput("file1", "Upload File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",".txt",".xlsx")),
      
      checkboxInput("header", "Header", TRUE),
      
      # Select which column contains the text
      uiOutput("columnControls"),
      conditionalPanel(
        condition = "input.time == true",
        uiOutput("columnControls2")
      ), width =2
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      column(6,
             h3("Sentiment Gauge"),
             plotOutput("sentimentGauge")),
      
      column(6,
             h3("Top Phrases"),
             plotOutput("phrasePlot"),
             downloadButton("downloadPhrases","Download Top Phrases")),
      
      column(12,
             h3("Sentiment by Comment"),
             downloadButton("downloadComments","Download Sentiment by Comment"),
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
    if(input$sep=="xlsx")
    {
      temp <- read_xlsx(input$file1$datapath)
    }else
    {
      temp<-read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,stringsAsFactors = F)
    }
    temp$key<-1:nrow(temp)
    temp
  })
  
  # Scores sentiment based on the entire comment
  sentiment<-reactive({
    text() %>% phraseCreator(input$selector2) %>%
      left_join(bing,by=c('word.y'='word')) %>% mutate(score=case_when(
        sentiment=="positive" ~ 1,
        sentiment=="negative" ~ -1,
        TRUE ~ 0
      )) %>% mutate(score=case_when(
        word.x %in% sentimentChangeWords ~ score*-1,
        TRUE ~ score
      )) %>% filter(score!=0) %>%
      group_by(key) %>% summarise(sentiment=mean(score))
  })
  
  
  output$sentimentGauge<-renderPlot({
    # sentiment gauge: shows sentiment on a scale of -1 to 1 (negative to positive)
    {
      req(input$file1)
      
      gg.gauge(round(50*mean(sentiment()$sentiment)+50,2))
    }
  })
  
  output$columnControls <- renderUI({
    req(input$file1)
    cnames=colnames(text())
    selectInput("selector2","Which column contains the text you would like to analyze?",
                cnames,selected = cnames[1])
  })
  
  
  output$phrasePlot <- renderPlot({
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
  })
  
  phrases <- reactive({
    phrases<-text() %>% phraseCreator(input$selector2,removeStopWords=TRUE) %>% 
      left_join(sentiment()) %>% filter(!is.na(word.y)) %>% 
      mutate(sentiment=case_when(
        is.na(sentiment) ~ 0,
        TRUE ~ sentiment
      )) %>% group_by(phrase) %>%
      summarise(sentiment=mean(sentiment),count=length(phrase)) %>% 
      ungroup() %>% arrange(desc(count))
  })
  
  output$downloadPhrases <- downloadHandler(
    
    filename = function() {
      paste("TopPhrases","_",Sys.time(),".csv",sep="")
    },
    
    content = function(file) {
      write.csv(phrases(),file,row.names=F)
    }
  )
  
  comments <- reactive({
    text() %>% left_join(sentiment()) %>% mutate(sentiment=case_when(
      is.na(sentiment) ~ 0,
      TRUE ~ sentiment
    ))
  })
  
  output$commentsTable<-DT::renderDataTable({
    # table showing comments
    # need to make it filter based on clicks
    {
      req(text())
      DT::datatable(data.frame(comments=comments(),stringsAsFactors = F),
                    options = list(dom = 't',pageLength=5000)) %>% 
        DT::formatStyle(columns = colnames(.),fontSize="10pt")
    }
  })
  
  output$downloadComments <- downloadHandler(
    
    filename = function() {
      paste("CommentsSentiment","_",Sys.time(),".csv",sep="")
    },
    
    content = function(file) {
      write.csv(comments(),file,row.names=F)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

#library(png)
#library(grid)
#img<-readPNG("Logo.png")
#g<-rasterGrob(img,interpolate=TRUE)
#image_read("https://github.com/twwebb3/Text-Miner-App/blob/master/Logo.png")

#fcast$mean
#fcast$x
#out<-data.frame(fcast)
#out$x<-1:nrow(out)
#ggplot(out,aes(x=x,y=Point.Forecast)) +
#  annotation_custom(g,xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf) + geom_line()