
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

library(shiny)
library(dplyr)
library(tidytext)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
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
          uiOutput("columnControls")
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("mainPlot")
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
     read.csv(input$file1$datapath,
                    header = input$header,
                    sep = input$sep,stringsAsFactors = F)
   })
   
   output$mainPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     #if(input$text!="")
     {
       if(input$selector1=="paste")
       {
         req(input$text)
         text<-data.frame(text=input$text,stringsAsFactors = F)
         text %>% unnest_tokens(word,text) %>% inner_join(get_sentiments("bing")) %>%
           count(sentiment,word) %>% mutate(topN=rank(desc(n),ties.method = "first")) %>% ungroup() %>%
           filter(topN<=30) %>% mutate(word=reorder(word,n)) %>%
           ggplot(aes(x=word,y=n,fill=sentiment)) + 
           geom_col(show.legend = FALSE) +
           facet_wrap(~sentiment, scales = "free") +
           coord_flip()
       }else
       {
         req(input$file1)
         text() %>% mutate(key=1:nrow(text())) %>%
           phraseCreator(input$selector2,removeStopWords=TRUE) %>% 
           left_join(get_sentiments("bing"),by=c("word.y","word")) %>%
           mutate(score=case_when(
             sentiment=="positive" ~ 1,
             sentiment=="negative" ~ -1,
             TRUE ~ 0
           )) %>%
           count(sentiment,word) %>% mutate(topN=rank(desc(n),ties.method = "first")) %>% ungroup() %>%
           filter(topN<=30) %>% mutate(word=reorder(word,n)) %>%
           ggplot(aes(x=word,y=n,fill=sentiment)) + 
           geom_col(show.legend = FALSE) +
           facet_wrap(~sentiment, scales = "free") +
           coord_flip()
       }
       
     }
   })
   
   output$columnControls <- renderUI({
     req(input$file1)
     cnames=colnames(text())
     selectInput("selector2","Which column contains the text you would like to analyze?",
                 cnames,selected = cnames[1])
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

