library(shiny)

ui <- fluidPage(
  titlePanel("DATA2902 Student Survey Explorer"),
  sidebarLayout(
    sidebarPanel(
      helpText("This app will allow interactive visualisation and hypothesis testing.")
    ),
    mainPanel(
      textOutput("welcome")
    )
  )
)

server <- function(input, output) {
  output$welcome <- renderText("Hello! Shiny app scaffold set up.")
}

shinyApp(ui = ui, server = server)

