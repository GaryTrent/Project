# Import libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(randomForest)
library(data.table)

# Read data
weather <- read.csv("weather-weka.txt")
weather$outlook <- as.factor(weather$outlook)
weather$play <- as.factor(weather$play)

# Build model
model <- randomForest(play ~ ., data = weather, ntree = 500, mtry = 4, importance = TRUE)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Detection Application"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Details", tabName = "details")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(title = "Play Badminton or Not?",
                    status = "primary", solidHeader = TRUE,
                    width = 12,
                    collapsible = TRUE,
                    selectInput("outlook", label = "Choose Weather from below options:", 
                                choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy", "Arid" = "arid","Cloudy" = "cloudy"), 
                                selected = "Cloudy"),
                    sliderInput("temperature", "Choose Temperature from slider:",
                                min = 64, max = 86,
                                value = 70),
                    sliderInput("humidity", "Choose Humidity from slider:",
                                min = 65, max = 96,
                                value = 90),
                    selectInput("windy", label = "Whelter Windy or not:", 
                                choices = list("Yes" = "TRUE", "No" = "FALSE"), 
                                selected = "TRUE"),
                    actionButton("submitbutton", "Submit", class = "btn btn-primary")
                )
              ),
              fluidRow(
                box(title = "Prediction Status",
                    status = "info", solidHeader = TRUE,
                    width = 12,
                    verbatimTextOutput('contents')
                )
              ),
              fluidRow(
                box(title = "Prediction Results",
                    status = "success", solidHeader = TRUE,
                    width = 12,
                    tableOutput('tabledata')
                )
              )
              
      ), 
      tabItem(tabName = "details",
              fluidRow(
                box(title = "Summary",
                    status = "warning", solidHeader = TRUE,
                    width = 6,
                    verbatimTextOutput("sum")
                ),
                box(title = "Structure",
                    status = "danger", solidHeader = TRUE,
                    width = 6,
                    verbatimTextOutput("str")
                )
                
              )
              
      )
      
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  datasetInput <- reactive({  
    df <- data.frame(
      Name = c("outlook",
               "temperature",
               "humidity",
               "windy"),
      Value = as.character(c(input$outlook,
                             input$temperature,
                             input$humidity,
                             input$windy)),
      stringsAsFactors = FALSE)
    play <- "play"
    df <- rbind(df, play)
    input <- transpose(df)
    write.table(input, "input.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    test <- read.csv(paste("input", ".csv", sep = ""), header = TRUE)
    test$outlook <- factor(test$outlook, levels = c("overcast", "rainy", "sunny", "arid", "cloudy"))
    Output <- data.frame(Prediction = predict(model, test), round(predict(model, test, type = "prob"), 3))
    print(Output)
  })
  
  output$contents <- renderPrint({
    if (input$submitbutton > 0) {
      isolate("Calculation complete.")
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) {
      isolate(datasetInput())
    }
  })
  
  output$str <- renderPrint({
    str(weather)
  })
  
  output$sum <- renderPrint({
    summary(weather)
  })
  
   output$dataset_info <- renderPrint({
    # Display dataset information
    datasetInput()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
