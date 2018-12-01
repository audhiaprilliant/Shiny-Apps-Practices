library(shiny)

ui = fluidPage(# theme = "bootstrap.css",
  # WEBPAGE DISPLAY
  titlePanel(windowTitle = "Haikawanku", 
             title = div(
               column(3,
                      img(src = 'task2_logo.png', width = '30%')),
               column(10,
                      h1('Data Exploration')
                      ))
             ),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "data1", label = "Input your file!", buttonLabel = "Browse...",
                placeholder = "No file selected"),
      selectInput(inputId = 'dropdown', label = 'Choose separator', 
                  choices = c('Comma' = ',','Semicolon'=';','Space'=' ')),
      checkboxInput(inputId = 'data2', label = "Checklist if you want to read first row as data!"),
      selectInput(inputId = 'dropdown_column', label = "Choose column's name!",choices = c('')),
      downloadButton(outputId = 'dwn', label = 'Download Data Iris')
    ),
    
    # OUTPUT DISPLAY
    mainPanel(
      tabsetPanel(type = 'tab', 
                  tabPanel('Data', tableOutput(outputId = "value")),
                  tabPanel('Summary Data',
                           sidebarPanel(textAreaInput(inputId = 'data', label = 'Input your data!', placeholder = 'Separate with space and use point (.) for decimal'),
                                        actionButton(inputId = 'process', label = 'Process')),
                           mainPanel(tableOutput(outputId = "descriptive"))),
                  tabPanel('Histogram', plotOutput(outputId = "histogram")),
                  tabPanel('Boxplot', plotOutput(outputId = "boxplot"))
    )
  )
)
)

server = function(input, output, session){
  output$dwn = downloadHandler(
    filename = function(){
      paste('Download', Sys.Date(), sep = '_')
    },
    content = function(downloadfolder){
      write.csv(iris, downloadfolder, row.names = FALSE)
      #file.copy('task2_logo.PNG',con)
      })
  
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
  
  # SUMMARY DATA
  inputdata = eventReactive(input$process, {
    x = input$data
    x = gsub('  ',' ',x)
    x = strsplit(x, ' ')
    x = unlist(x)
    x = as.numeric(x)
    x
  })
  
  observe({
    x = inputdata()
    if(sum(is.na(x)) >= 1){
      showModal(
        modalDialog(title = 'Error', style = 'color:red;font-weight:bold',
                    'You input more than one space!',
                    footer = modalButton('OK'),
                    easyClose = TRUE))
    }
  })
  
  # SHOW DESCRIPTIVE STATISTICS FROM OUR DATA
  output$descriptive = renderTable({
    x = inputdata()
    if(inputdata() == '' | length(x) == 1 | sum(is.na(x)) >= 1){
      return(NULL)
    }
    data.frame(minimum = min(x), 
               Q1 = quantile(x, 0.25, names = FALSE), 
               median = median(x), 
               mean = mean(x), 
               standard.deviation = sd(x), 
               Q3 = quantile(x, 0.75, names = FALSE))
  })
}

# Read R Script become R Shiny Script
shinyApp(ui = ui, server = server)