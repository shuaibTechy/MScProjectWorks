
# Load required libraries
library(shiny)
library(randomForest)

library(data.table)

# Load the dataset
data1 <- read.csv("modelData.csv")


# Define the UI
ui <- fluidPage(
  titlePanel("Purchase Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("asin", "Product ID", choices = unique(data1$asin)),
      selectInput("productCategory", "Product Category", choices = unique(data1$productCategory, readOnly=TRUE)),
      
      selectInput("product.name", "Product Name", choices = unique(data1$product.name, readOnly=TRUE)),
      
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
  predictPurchaseProbability <- function(productCategory, asin, product.name) {
    # Subset the dataset based on user input
    subsetData <- subset(data1, productCategory == productCategory & asin == asin & product.name == product.name)
   
    # Train a random forest model on the subset of data
    model <- randomForest(noPurchaseProbability ~ date+asin+product.name+productCategory, data = subsetData)
    
    # Predict churn rate for the user input
    prediction <- predict(model, subsetData, type = "response")
    
    # Average Prediction
    avgPrediction <- mean(prediction)
    
    # Round the prediction to two decimal places
    roundedPrediction <- round(avgPrediction, 4)
    
    # Return the churn rate prediction
    return(roundedPrediction)
    output$purchaseProbability_gauge <- renderGauge({
      gauge(round(avgPrediction() * 100, 4), min = 0, max = 100, label = "Purchase Probability")
    })
  }
  
  # Event handler for predict button click
  observeEvent(input$predictButton, {
    productCategory <- input$productCategory
    asin <- input$asin
    product.name <- input$product.name
    
    # Call the predictPurchase function and display the prediction
    output$predictionText <- renderText({
      paste("Probability of Purchase:", predictPurchaseProbability(productCategory, asin, product.name))
    })
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$predictButton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$predictButton>0) { 
      isolate(datasetInput()) 
    } 
  })
}

shinyApp(ui = ui,server = server)
