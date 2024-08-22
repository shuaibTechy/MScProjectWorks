
# Load required libraries
library(shiny)
library(randomForest)

library(data.table)

# Load the dataset
data1 <- read.csv("churnMLData.csv")


# Define the UI
ui <- fluidPage(
  titlePanel("Churn Rate Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("productCategory", "Product Category", choices = unique(data1$productCategory)),
      selectInput("asin", "Product ID", choices = unique(data1$asin)),
      selectInput("product.name", "Product Name", choices = unique(data1$product.name)),
      # Add more input fields for other attributes as needed
      actionButton("predictButton", "Predict")
    ),
    
    mainPanel(
      verbatimTextOutput("predictionText")
    )
  ))

# Define the server
server <- function(input, output) {
  # Function to predict churn rate
  predictChurnRate <- function(productCategory, asin, product.name) {
    # Subset the dataset based on user input
    subsetData <- subset(data1, productCategory == productCategory & asin == asin & product.name == product.name)
   
    # Train a random forest model on the subset of data
    model <- randomForest(churnRate ~., data = subsetData)
    
    # Predict churn rate for the user input
    prediction <- predict(model, subsetData, type = "response")
    
    # Round the prediction to two decimal places
    roundedPrediction <- round(prediction, 2)
    
    # Return the churn rate prediction
    return(roundedPrediction)
    output$churn_gauge <- renderGauge({
      gauge(round(prediction() * 100, 2), min = 0, max = 100, label = "Churn Probability")
    })
  }
  
  # Event handler for predict button click
  observeEvent(input$predictButton, {
    productCategory <- input$productCategory
    asin <- input$asin
    product.name <- input$product.name
    
    # Call the predictChurnRate function and display the prediction
    output$predictionText <- renderText({
      paste("Churn Rate Prediction:", predictChurnRate(productCategory, asin, product.name))
    })
  })
}

shinyApp(ui = ui,server = server)
