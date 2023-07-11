library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Disease Severity Index (DSI)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_plants", "Total number of plants:", value = 50),
      numericInput("max_index", "Maximal disease index:", value = 5),
      textInput("rating_scores", "Scores of rating classes (separated by spaces):"),
      textInput("class_freq", "Frequency of rating classes (separated by spaces):"),
      actionButton("calculate", "Calculate DSI")
    ),
    mainPanel(
      plotOutput("barplot"),
      textOutput("dsi_output")
    )
  )
)

# Define server
server <- function(input, output) {
  observeEvent(input$calculate, {
    # Get input values
    num_plants <- input$num_plants
    max_index <- input$max_index
    rating_scores <- as.numeric(strsplit(input$rating_scores, " ")[[1]])
    class_freq <- as.numeric(strsplit(input$class_freq, " ")[[1]])

    # Calculate the Disease Severity Index (DSI)
    numerator <- sum(class_freq * rating_scores)
    denominator <- num_plants * max_index
    dsi <- (numerator / denominator) * 100

    # Generate the bar plot
    barplot_data <- data.frame(rating_scores, class_freq)
    barplot <- ggplot(barplot_data, aes(x = rating_scores, y = class_freq)) +
      geom_bar(stat = "identity") +
      xlab("Rating Scores") +
      ylab("Frequency") +
      ggtitle("Rating Class Frequency") +
      geom_text(aes(label = paste("DSI =", sprintf("%.2f", dsi), "%")),
                x = 0.7, y = max(class_freq), vjust = 0.9, hjust = 0,
                size = 5, color = "black", fontface = "bold", show.legend = FALSE) +
      theme_bw()

    # Display the results
    output$barplot <- renderPlot({
      barplot
    })
    output$dsi_output <- renderText({
      paste("The Disease Severity Index (DSI) is:", sprintf("%.2f", dsi), "%")
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
