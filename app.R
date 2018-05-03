library(shiny)
source("functions.R")

server <- function(input, output, session) {

  output$text <- renderText({
    paste("Input text is:", input$text)
  })

  observe({
    iniTime <- Sys.time()
    
    textCleansed <- clean(input$text)
    if(textCleansed != " ") 
    {
      output$cleaned <- renderText({
        paste0("Cleansed text: [",textCleansed,"]")
      })
      
      
      textCleansed <- gsub(" \\* "," ",textCleansed)    
      predictWords <- predict_model(textCleansed)
      updateSelectInput(session = session, inputId = "predicts", choices = predictWords)
      
      
      endTime <- Sys.time()
      output$msg <- renderText({
        paste(msg, "\n", sprintf("- Total time processing = %6.3f msecs",1000*(endTime-iniTime)))
      })
      gc()
    }  
  })
}


ui <- fluidPage(
  # Application title
  titlePanel("N-gram Based Shiny App for Next Word Predicton"),
  
  # User interface 
  sidebarLayout(
    sidebarPanel(
      p("Input a word(s) and hit the Go button to display the next pedicted word(s)"),	
      textInput(inputId="text", label = ""),
      submitButton("Go"),
      HTML('<script type="text/javascript"> 
           document.getElementById("text").focus();
           </script>')
      ),
    
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Output", 
                 conditionalPanel(condition = "input.text != ''",
                                  verbatimTextOutput("text"),
                                  verbatimTextOutput("cleaned"), verbatimTextOutput("msg"),
                                  selectInput("predicts","Word predictions:",choices=c(""))
                 )
        )
      )
    )
  ),
  
  fluidRow(HTML("<div style='margin-left:18px;margin-bottom:12px;color:red;'><strong> May 2018</strong></div>") ),
  fluidRow(HTML("<div style='margin-left:18px;margin-bottom:12px;margin-top:-12px;color:white;'><strong><big>By <a title='Get back to me!...' 
                href='mailto:harris2010@live.in'>Harris Panakkal</a></big></strong><style> body{background-color:#F5A9BC}</style></div>")
              )
  )          



shinyApp(ui = ui, server = server)