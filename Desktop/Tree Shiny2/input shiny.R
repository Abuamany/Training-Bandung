library(shiny)

ui <- shinyUI(fluidPage(
  
  
  titlePanel("HTML"),
  
  
  sidebarLayout(
    sidebarPanel(
      textInput("length",
                "Enter your length:"),
      textInput("weight",
                "Enter your weigth:")
      
    ),
    
    
    mainPanel(
      htmlOutput("testHTML")
    )
  )
))


server <- shinyServer(function(input, output) {
  
  output$testHTML <- renderText({
    paste("<b>Your length is: ", input$length, "<br>", "Your weight is: ", input$weight, "</b>")
  })
})


shinyApp(ui = ui, server = server)

