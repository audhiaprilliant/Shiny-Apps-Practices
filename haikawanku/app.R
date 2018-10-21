library(shiny)

ui = fluidPage(
  # WEBPAGE DISPLAY
  titlePanel("Statistics Talent Club Project"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "data1", label = "Input data"),
      checkboxInput(inputId = 'data2', label = "I'm not a robot"),
      fileInput(inputId = "data3", label = "Input your file!", buttonLabel = "Browse...",
                placeholder = "No file selected")
    ),
    mainPanel(
      # OUTPUT DISPLAY
      verbatimTextOutput(outputId = "value"),
      plotOutput(outputId = "distribution")
    )
  )
)

server = function(input, output, session){
  output$value = renderPrint({
     # INPUT FOR data1
     x = input$data1
     x = strsplit(x, " ") # Make list
     x = unlist(x) # Unlist list we have made into vector
     x = as.numeric(x) # Change element's vector as numeric
     summary(x) # Summary value x
    
    # INPUT FOR data3
    if(is.null(input$data3)){ # R will not read data if it has not been uploaded
      return('Input your dataset')
    }
    x = input$data3$datapath
    x = read.csv(file = x, header = TRUE, sep = ',')
    summary(x)
  })
  
  # OUTPUT HISTOGRAM
  output$distribution = renderPlot({
    if(is.null(input$data3)){
      return(NULL)
    }
    y = input$data3$datapath
    y = read.csv(file = y, header = TRUE, sep = ',')
    hist(y[,1], main = "Histogram of Age", xlab = "Age", ylab = "Frequency")
  })
}

# Read R Script become R Shiny Script
shinyApp(ui = ui, server = server)