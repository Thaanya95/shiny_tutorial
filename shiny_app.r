install.packages("shiny")

packageVersion("htmltools")


install.packages("htmltools")


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Regrex_Scatterplot"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents")
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
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     print(bins)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
    })
    
    output$lmtPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
    })
    
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)

ui <- fluidPage(
  # Other UI elements here (e.g., data input, plots, etc.)
  actionButton("model_button", "Model the Data")
)


server <- function(input, output) {
  # Other server logic here

  # Reactive expression for modeling
  model_regrex1.csv <- reactive({
    if (input$model_button > 0) {
      # Perform your linear modeling here
      # Replace "data" with your actual data frame
      # Replace "response_var" and "predictor_vars" with the appropriate column names
      lm_model <- lm(y ~ x, data = data)
      return(lm_model)
    }
  })
}


server <- function(input, output) {
  # Other server logic here

  # Reactive expression for modeling
  model_regrex1.csv <- reactive({
    if (input$model_button > 0) {
      # Perform your linear modeling here
      # Replace "data" with your actual data frame
      # Replace "response_var" and "predictor_vars" with the appropriate column names
      lm_model <- lm(y ~ x, data = data)
      return(lm_model)
    }
  })

  # Display the model summary when the button is clicked
  output$model_summary <- renderPrint({
    summary(model_data())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

ui <- fluidPage(
  # Other UI elements here (e.g., data input, plots, etc.)
  actionButton("model_button", "Model the Data"),
  verbatimTextOutput("model_summary")
)



