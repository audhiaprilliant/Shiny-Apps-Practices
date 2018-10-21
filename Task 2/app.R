library(shiny)

ui = fluidPage(
  # WEBPAGE DISPLAY
  titlePanel("Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "data1", label = "Input your file!", buttonLabel = "Browse...",
                placeholder = "No file selected"),
      selectInput(inputId = 'dropdown', label = 'Choose separator', 
                  choices = c('Comma' = ',','Titik Koma'=';','Space'=' ')),
      selectInput(inputId = 'dropdown_column', label = "Choose column's name!",choices = c('')),
      checkboxInput(inputId = 'data2', label = "Checklist if you do not want to read first row as data!")
      
    ),
    mainPanel(
      # OUTPUT DISPLAY
      tableOutput(outputId = "value"),
      plotOutput(outputId = "histogram"),
      plotOutput(outputId = "boxplot")
    )
  )
)

server = function(input, output, session){
  x = reactive({
    if(is.null(input$data1)){ # R will not read data if it has not been uploaded
      return('Input your dataset')
    }
    read.csv(file = input$data1$datapath, header = input$data2, sep = input$dropdown) 
  })
  observe({
    rm = names(x())
    updateSelectInput(session = session, inputId = 'dropdown_column', choices = rm)
  })
  
  output$value = renderTable({
    # INPUT FOR data1
    head(x())
  })
  
  # OUTPUT HISTOGRAM
  output$histogram = renderPlot({
    hist(table(x()[input$dropdown_column]), main = paste('Histogram of', input$dropdown_column), xlab = 
           paste(input$dropdown_column))
  })
  
  # OUTPUT BOXPLOT
  output$boxplot = renderPlot({
    boxplot(x()[input$dropdown_column], main = paste('Boxplot of', input$dropdown_column), xlab = 
           paste(input$dropdown_column))
  })
}

# Read R Script become R Shiny Script
shinyApp(ui = ui, server = server)
