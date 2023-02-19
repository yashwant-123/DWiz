library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)



ui <- fluidPage(theme = shinytheme("superhero"),
  sidebarLayout(
    sidebarPanel(
      #for custom inputting file
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      checkboxInput("header", "Header", TRUE)
    ),
    sidebarPanel(
      HTML("<h3>Input parameters</h3>"),
      #for dropdown Box
      selectInput("Plot", label = "Type of Plot : ",
                  choices = list("ScatterPlot" = "ScatterPlot","ScatterPlot_2" = "ScatterPlot_2" , "Histogram" = "Histogram", "BarPlot" = "BarPlot"),
                  selected = "BarPlot"),
      #side by side textboxes using css
      tags$div(textInput("txt1", "Parameter 1 :","NULL"),  style="display:inline-block"),
      tags$div(textInput("txt2", "Parameter 2(optional) :","NULL"),  style="display:inline-block"),
      
      #For ScatterPlot Necessary inputs
      conditionalPanel(
        condition = "input.Plot == 'ScatterPlot'",
        tags$div(textInput("title", "title of graph",), style="display:inline-block"),
        tags$div(textInput("X_a", "lable of x axis"), style="display:inline-block"),
        tags$div(textInput("Y_a", "lable of y axis"), style="display:inline-block"),
        tags$div(numericInput("Size", "Size",13,min=1,max=15),style="display:inline-block"),
        tags$div(textInput("Color", "Color",'blue'), style="display:inline-block")
        
      ),
      
      #For ScatterPlot_2
      conditionalPanel(
        condition = "input.Plot == 'ScatterPlot_2'",
        tags$div(textInput("Add_param", "Additional_Parameter : ","NULL"),style="display:inline-block"),
        tags$div(numericInput("Size", "Size",6,min=1,max=25),style="display:inline-block"),
        tags$div(textInput("Color1", "Color 1 :",'blue'), style="display:inline-block"),
        tags$div(textInput("Color2", "Color 2 :",'red'), style="display:inline-block")
        
      ),
      
      #submit Button
      actionButton("submitbutton", "Submit", class = "btn btn-primary")
    )
    
    
  ),
  mainPanel(
    conditionalPanel(
      condition = "input.submitbutton > 0",
      HTML("<h2>Head of data : </h2>"),
      tableOutput("contents"),

      HTML("<h3>Plot : </h3>"),
      plotOutput(outputId = "Plot"),
      downloadButton('downloadPlot', 'Download Plot')
    )

    
  )
  
)

server <- function(input, output , session) {
  
  #Reading data from csv file.
  Indata <- reactive({
    # input$file1 will be NULL initially. After the user selects and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath' column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath,sep =",", header = input$header)
  })
  #for displaying data
  output$contents <-renderTable({
    if (input$submitbutton>0) { 
      isolate(head(Indata()))
    }
  })
  
  
  #For displaying plot
  plot <- reactive({
    if (input$submitbutton>0) { 
      
      param1 <- gsub(" ",".",input$txt1)
      param2 <- gsub(" ",".",input$txt2)
      
      #ScatterPlot
      if(input$Plot == "ScatterPlot"){
        isolate(
          ggplot(Indata(), aes_string(x = param1, y = param2)) +
            geom_point(color = input$Color, size = input$Size)+
            ggtitle(input$title)+
              labs(y=input$Y_a , x= input$X_a)
          
        )

      }
      
      #ScatterPlot_2
      else if(input$Plot == "ScatterPlot_2"){
        param3 <- gsub(" ","",input$Add_param)
        isolate(
          ggplot(Indata())+
            geom_point(aes_string(x=param1 , y=param2), color = input$Color1, size = input$Size )+
            geom_point(aes_string(x=param1, y=param3), color = input$Color2 , size = input$Size)
        )
      }
      
     else{ 
      isolate(ggplot(data = Indata())+ 
                geom_bar(mapping = aes_string(x= param1)) )
     }
    } 
    
  })
  
  output$Plot <- renderPlot({
    plot()
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$Plot, '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot(), device = "png")
    }
  )
  
}

shinyApp(ui=ui, server=server)