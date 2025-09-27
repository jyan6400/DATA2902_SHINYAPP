library(shiny)

# Placeholder variables — later to replace
categorical_vars <- c("Alcohol Consumption", "Grade Aim", "Sleep Group", "Exercise Group")
numeric_vars     <- c("Study Hours", "Sleep Hours", "Exercise Hours")

ui <- fluidPage(
  titlePanel("DATA2902 Student Survey Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select variables to explore relationships."),
      
      # Variable selectors
      selectInput("cat1", "Categorical variable:", choices = categorical_vars),
      selectInput("cat2", "Second categorical variable (for χ² test):", choices = categorical_vars),
      selectInput("numvar", "Numeric variable:", choices = numeric_vars),
      
      # Test type radio button
      radioButtons("test_type", "Hypothesis test:",
                   choices = c("Chi-square (cat vs cat)" = "chi2",
                               "t-test (num vs cat)" = "ttest"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", 
                 textOutput("summary_text")),
        tabPanel("Visualisation", 
                 plotOutput("plot")),
        tabPanel("Test Results", 
                 verbatimTextOutput("test_output"))
      )
    )
  )
)

server <- function(input, output) {
  output$summary_text <- renderText({
    paste("Selected test:", input$test_type,
          "| Var1:", input$cat1,
          "| Var2:", input$cat2,
          "| Numeric:", input$numvar)
  })
  
  output$plot <- renderPlot({
    plot(1:10, main = "Placeholder plot — will update later")
  })
  
  output$test_output <- renderPrint({
    cat("Placeholder test results.\n",
        "Chi2 will be shown if categorical vs categorical.\n",
        "T-test if numeric vs categorical.")
  })
}

shinyApp(ui = ui, server = server)
