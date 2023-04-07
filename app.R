library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(forcats)



ui <- fluidPage(theme = shinytheme("superhero"),
                #[code omitted]
                tags$figure(
                  align = "left",
                  tags$img(
                    src = "logo.jpg",
                    width = 70,
                    height = 70,
                    alt = "My Logo"
                  ),
                ),
                #[code omitted]
                #[code omitted]

                sidebarLayout(
                  #[code omitted]


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
                                choices = list("ScatterPlot" = "ScatterPlot","ScatterPlot_2" = "ScatterPlot_2" , 
                                               "Histogram" = "Histogram", "BarPlot" = "BarPlot", "LinePlot" = "LinePlot","PieChart" = "PieChart"),
                                selected = "ScatterPlot"),
                    #side by side textboxes using css
                    tags$div(textInput("txt1", "Parameter 1 :","NULL"),  style="display:inline-block"),
                    tags$div(textInput("txt2", "Parameter 2 :","NULL"),  style="display:inline-block"),
                    
                    #For ScatterPlot Necessary inputs
                    conditionalPanel(
                      condition = "input.Plot == 'ScatterPlot'",
                      tags$div(textInput("S_title", "title of graph"), style="display:inline-block"),
                      tags$div(textInput("S_X_a", "lable of x axis"), style="display:inline-block"),
                      tags$div(textInput("S_Y_a", "lable of y axis"), style="display:inline-block"),
                      tags$div(numericInput("S_Size", "Size",13,min=1,max=15),style="display:inline-block"),
                      tags$div(textInput("S_Color", "Color",'blue'), style="display:inline-block")
                      
                    ),
                    
                    #For ScatterPlot_2
                    conditionalPanel(
                      condition = "input.Plot == 'ScatterPlot_2'",
                      tags$div(textInput("Add_param", "Additional_Parameter : ","NULL"),style="display:inline-block"),
                      tags$div(numericInput("Size", "Size",6,min=1,max=25),style="display:inline-block"),
                      tags$div(textInput("Color1", "Color 1 :",'blue'), style="display:inline-block"),
                      tags$div(textInput("Color2", "Color 2 :",'red'), style="display:inline-block")
                      
                    ),
                    
                    #For Histogram
                    conditionalPanel(
                      condition = "input.Plot == 'Histogram'",
                      tags$div(textInput("title", "title of graph"), style="display:inline-block"),
                      tags$div(textInput("X_a", "lable of x axis"), style="display:inline-block"),
                      tags$div(textInput("Y_a", "lable of y axis"), style="display:inline-block"),
                      tags$div(numericInput("nb", "No.of Bins",10,min=1,max=60),style="display:inline-block"),
                      tags$div(textInput("Color", "Color",'blue'), style="display:inline-block"),
                      tags$div(textInput("B_Fill", "Fill",'blue'), style="display:inline-block")
                      
                    ),
                    
                    #For BarPlot
                    conditionalPanel(
                      condition = "input.Plot == 'BarPlot'",
                      tags$div(textInput("B_title", "title of graph"), style="display:inline-block"),
                      tags$div(textInput("B_X_a", "lable of x axis"), style="display:inline-block"),
                      tags$div(textInput("B_Y_a", "lable of y axis"), style="display:inline-block"),
                      tags$div(textInput("B_Color", "Border Color",'blue'), style="display:inline-block"),
                      tags$div(textInput("B_Fill", "Fill",'blue'), style="display:inline-block")
                    ),
                    
                    #For LinePlot
                    conditionalPanel(
                      condition = "input.Plot == 'LinePlot'",
                      tags$div(textInput("L_title", "title of graph"), style="display:inline-block"),
                      tags$div(textInput("L_X_a", "lable of x axis"), style="display:inline-block"),
                      tags$div(textInput("L_Y_a", "lable of y axis"), style="display:inline-block"),
                      tags$div(textInput("L_Color", "Line Color",'black'), style="display:inline-block"),
                      tags$div(textInput("L_Type", "Line Type" , "solid"), style="display:inline-block"),
                      tags$div(numericInput("L_Size", "Size",1,min=1,max=25),style="display:inline-block")                
                      
                    ),
                    
                    #For PieChart
                    conditionalPanel(
                      condition = "input.Plot=='PieChart'",
                      tags$div(textInput("P_title", "title of Chart"), style="display:inline-block")
                    ),
                    
                    #submit Button
                    actionButton("submitbutton", "Submit", class = "btn btn-primary")
                  )
                  
                  
                ),
                mainPanel(
                  
                  conditionalPanel(
                    condition = "input.submitbutton == 0",
                    h6("Developed by Yash", align = "right"),
                  ),
                  
                  conditionalPanel(
                    condition = "input.submitbutton > 0",
                    HTML("<h2>Head of data : </h2>"),
                    tableOutput("contents"),
                    
                    HTML("<h3>Plot : </h3>"),
                    plotOutput(outputId = "Plot"),
                    downloadButton('downloadPlot', 'Download Plot'),
                    h6("Developed by Yash", align = "right"),
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
            geom_point(color = input$S_Color, size = input$S_Size)+
            ggtitle(input$S_title)+
            labs(y=input$S_Y_a , x= input$S_X_a)
          
        )
        
      }
      
      #ScatterPlot_2
      else if(input$Plot == "ScatterPlot_2"){
        param3 <- gsub(" ","",input$Add_param)
        isolate(
          ggplot(Indata())+
            geom_point(aes_string(x=param1 , y=param2), color = input$Color1, size = input$Size )+
            geom_point(aes_string(x=param1, y=param3), color = input$Color2 , size = input$Size)+
            #To access col of data using string we use data[[colname]]
            scale_x_discrete(limits=Indata()[[param1]])+
            ggtitle(input$title)+
            labs(y=input$Y_a , x= input$X_a)
        )
      }
      
      #For Histogram
      else if(input$Plot == "Histogram"){
        isolate(
          ggplot(Indata())+
            geom_histogram(aes_string(x=param1), color = input$Color, fill = input$B_Fill, bins = input$nb)
          +ggtitle(input$title)+
            labs(y=input$Y_a , x= input$X_a)
        )
      }
      
      #For BarPlot
      else if(input$Plot == "BarPlot"){
        isolate(
          ggplot(Indata())+
            geom_bar(stat='identity' , aes_string(x=param1, y =param2) , color=input$B_Color , fill=input$B_Fill)+
            #To access col of data using string we use data[[colname]]                          
            scale_x_discrete(limits=Indata()[[param1]])+
            ggtitle(input$B_title)+
            labs(y=input$B_Y_a , x= input$B_X_a)
        )
      }
      
      #For LinePlot
      else if(input$Plot == "LinePlot"){
        isolate(
          ggplot(Indata() , aes_string(x=param1, y=param2 , group ='1'))+
            geom_line( linetype= input$L_Type , color = input$L_Color , size=input$L_Size)+
            geom_point(size=3*input$L_Size)+
            #To access col of data using string we use data[[colname]]
            #scale_x_discrete(limits=Indata()[[param1]])+
            ggtitle(input$L_title)+
            labs(y=input$L_Y_a , x= input$L_X_a)
          
        )
        
      }
      
      #For PieChart
      else if(input$Plot == "PieChart"){
        lab.ypos = cumsum(Indata()[[param2]]) - 0.6*Indata()[[param2]]
        
        isolate(
          ggplot(Indata(), aes_string(x="''", y=param2, fill=param1)) +
            geom_bar(stat="identity", width=1, color="white") +
            coord_polar("y", start=0) +
            geom_text(aes(y = lab.ypos, label = Indata()[[param2]]), color = "black")+
            ggtitle(input$P_title)
        )
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