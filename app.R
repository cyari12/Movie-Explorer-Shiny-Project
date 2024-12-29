#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load libs
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

#read data
movies <- read.csv("/Users/cyarinaamatya/Desktop/Shiny-App/dataset/cleaned_movie.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Movie Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("type_filter", "Type (Movie or TV Show):", 
                  choices = c("All", unique(movies$type)), selected = "All"),
      sliderInput("min_votes", "Minimum Number of Votes:", 
                  min = 0, max = max(movies$imdbNumVotes), value = 5000),
      sliderInput("year_range", "Release Year Range:", 
                  min = min(movies$releaseYear), max = max(movies$releaseYear), 
                  value = c(1980, 2000)),
      textInput("genre_filter", "Genre Contains:", value = ""),
      textInput("country_filter", "Available Countries Contains:", value = ""),
      selectInput("x_var", "X-axis Variable:", 
                  choices = c("releaseYear", "imdbAverageRating", "imdbNumVotes")),
      selectInput("y_var", "Y-axis Variable:", 
                  choices = c("releaseYear", "imdbAverageRating", "imdbNumVotes"))
    ),
    
    mainPanel(
      plotlyOutput("scatterPlot"),
      textOutput("movieCount")
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  # Reactive expression to filter the dataset
  filteredData <- reactive({
    data <- movies
    # Filter by type
    if (input$type_filter != "All") {
      data <- data[data$type == input$type_filter, ]
    }
    # Filter by votes
    data <- data[data$imdbNumVotes >= input$min_votes, ]
    # Filter by year range
    data <- data[data$releaseYear >= input$year_range[1] & 
                   data$releaseYear <= input$year_range[2], ]
    # Filter by genre
    if (input$genre_filter != "") {
      data <- data[grepl(input$genre_filter, data$genres, ignore.case = TRUE), ]
    }
    # Filter by country
    if (input$country_filter != "") {
      data <- data[grepl(input$country_filter, data$availableCountries, ignore.case = TRUE), ]
    }
    data
  })
  
  # Render interactive scatter plot
  output$scatterPlot <- renderPlotly({
    plot_ly(
      data = filteredData(),
      x = ~get(input$x_var),
      y = ~get(input$y_var),
      text = ~paste(
        "Title: ", title, "<br>",
        "Genre: ", genres, "<br>",
        "IMDB Rating: ", imdbAverageRating
      ),
      hoverinfo = "text",
      type = "scatter",
      mode = "markers",
      color = ~type,  # Different colors for movies and TV shows
      marker = list(size = 10, opacity = 0.7)
    ) %>%
      layout(
        title = "Movie Explorer",
        xaxis = list(title = input$x_var),
        yaxis = list(title = input$y_var)
      )
  })
  
  # Render the number of filtered movies
  output$movieCount <- renderText({
    paste("Number of movies found:", nrow(filteredData()))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
