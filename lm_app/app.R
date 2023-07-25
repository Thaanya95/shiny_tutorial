library(shiny)

ui <- fluidPage(
  titlePanel("Old Faithful Geyser Data"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      tags$hr(),
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      # Add the button for modeling the data
      actionButton("model_button", "Linear model")
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("lmPlot"),
      tableOutput("coefficients_table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  dataInput <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    return(df)
  })
  
  # Add a reactive value to store the linear model results
  model_results <- reactiveVal(NULL)

  # Function to fit a linear model
  fit_linear_model <- function() {
    data <- dataInput()
    # Assuming your dependent variable is "y" and independent variable is "x"
    lm_result <- lm(y ~ x, data = data)
    return(lm_result)
  }

  # When the "Model Data" button is clicked
  observeEvent(input$model_button, {
    # Fit the linear model
    model <- fit_linear_model()

    # Update the reactive value with the model results
    model_results(model)
  })

  output$distPlot <- renderPlot({
    plot(dataInput()$x, dataInput()$y)
  })

  output$lmPlot <- renderPlot({
    model <- model_results()

    # Check if the model is NULL (not yet calculated) or if it failed to fit
    if (is.null(model)) {
      return(NULL)
    } else if (inherits(model, "try-error")) {
      return(NULL)
    }
    
    # Plot the original data
    plot(dataInput()$x, dataInput()$y)

    # Add the linear regression line to the plot
    abline(model, col = "blue")
    
    # Output the slope, intercept, and correlation coefficient
    coef_table <- data.frame(
      Slope = coef(model)[["x"]],
      Intercept = coef(model)[["(Intercept)"]],
      Correlation_Coefficient = cor(dataInput()$x, dataInput()$y)
    )
    output$coefficients_table <- renderTable({
      return(coef_table)
    })
  })

  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    if (input$disp == "head") {
      return(head(dataInput()))
    } else {
      return(dataInput())
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
