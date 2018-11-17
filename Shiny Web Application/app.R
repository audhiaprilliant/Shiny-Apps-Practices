library(shiny)

ui = fluidPage(
   titlePanel("Buttons in R-Shiny Apps"),
   sidebarLayout(
      sidebarPanel(
        textAreaInput(inputId = 'data', label = 'Input your data!', placeholder = 'Separate with space and use point (.) for decimal'),
        actionButton(inputId = 'process', label = 'Process')
      ),
      
      mainPanel(
        tableOutput(outputId = "descriptive")
      )
   )
)

server = function(input, output, session) {
  # INPUT OUR DATA
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
shinyApp(ui = ui, server = server)
