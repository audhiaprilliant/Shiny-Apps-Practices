library(shiny)

ui = fluidPage(
  # WEBPAGE DISPLAY
  titlePanel("Audhi Aprilliant"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "data1", label = "Input your file!", buttonLabel = "Browse...",
                placeholder = "No file selected"),
      checkboxInput(inputId = 'data2', label = "Checklist if you don not want to read first row as data!")
    ),
    mainPanel(
      # OUTPUT DISPLAY
      verbatimTextOutput(outputId = "value"),
      plotOutput(outputId = "barplot")
    )
  )
)

server = function(input, output, session){
  output$value = renderPrint({
    # INPUT FOR data1
    if(is.null(input$data1)){ # R will not read data if it has not been uploaded
      return('Input your dataset')
    }
      x = input$data1$datapath
      x = read.csv(file = x, header = input$data2, sep = ',')
      x = data.frame(table(x))
      x
    })
  
  # OUTPUT BARPLOT
  output$barplot = renderPlot({
    if(is.null(input$data1)){
      return(NULL)
    }
    x = input$data1$datapath
    x = read.csv(file = x, header = input$data2, sep = ',')
    x = table(x)
    barplot(x)
  })
}

# Read R Script become R Shiny Script
shinyApp(ui = ui, server = server)
