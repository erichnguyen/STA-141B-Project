library(shiny)
library(DT)
library(jsonlite)
library(httr)
library(stringr)
library(dplyr)
source("service.R")

# Define UI for app that draws a histogram ----
cuisines <- c("American", "Asian", "British", "Caribbean", "Central Europe", "Chinese", "Eastern Europe", 
              "French", "Indian", "Italian", "Japanese", "Kosher", "Mediterranean", "Mexican", 
              "Middle Eastern", "Nordic", "South American", "South East Asian")
meals <- c("Breakfast", "Lunch", "Dinner", "Snack")
calories <- c("Under 200 kcals", "200-300 kcals", "300-400 kcals", "400-500 kcals", "More than 500 kcals")

ui <- fluidPage(

  # App title ----
  titlePanel("Food Recipe with Ingredients and nutrition Facts"),
  sidebarLayout(
    sidebarPanel(
      textInput("query", "Query:", placeholder = "example: chicken"),
      selectInput("cuisine", "Cuisine Type:", choices = cuisines, selected = cuisines[1]),
      selectInput("meal", "Meal Type:", choices = meals, selected = meals[1]),
      selectInput("calorie", "Calories:", choices = calories, selected = calories[1]),
      sliderInput("num", "Number of records:",  
                  min = 1, max = 200, value = 10),
      numericInput("index", "Recipe Index", min=1, step=1, value=1),
      actionButton("action", "Submit")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Recipes",
          DTOutput('recipes')
        ),
        tabPanel(
          title = "Detail",
          fluidRow(
            column(12,
              h4("Ingredients"),
              DTOutput("ingredients")
            ),
            column(6,
              h4("Directions"),
              tableOutput('direction')
            ),
            column(6, 
              h4("Nutrition"),
              plotOutput("pie")
            )
          )
        )
      )

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  dataset <- reactive({
    input$action
    query <- input$query
    cuisine <- input$cuisine
    meal <- input$meal
    calorie <- input$calorie
    num <- as.numeric(input$num)
    low <- 0
    high <- 100
    if(calorie=="Under 200 kcals"){
      high <- 200
    }else if(calorie=="200-300 kcals"){
      low <- 200
      high <- 300
    }else if(calorie=="300-400 kcals"){
      low <- 300
      high <- 400
    }
    else if(calorie=="400-500 kcals"){
      low <- 400
      high <- 500
    }else{
      low <- 500
      high <- 20000
    }
    getRecipes(query, meal, cuisine, 0, num, low, high)
  })
  
  
  output$recipes = renderDT({
    dat <- dataset()
    df <- getDT(dat)
    df
  })
  
  output$ingredients <- renderDT({
    dat <- dataset()
    index <- as.numeric(input$index)
    dat$recipe$ingredients[[index]]
  })
  
  output$direction <- renderTable({
    dat <- dataset()
    index <- as.numeric(input$index)
    lines <- dat$recipe$ingredientLines[[index]]
    print(lines)
    df <- data.frame(index=1:length(lines), steps=lines)
    df
  })
  
  output$pie <- renderPlot({
    dat <- dataset()
    index <- as.numeric(input$index)
    nutrients <- dat$recipe$totalNutrients
    x <- unlist(c(nutrients[index, 1][2], nutrients[index, 2][2], nutrients[index, 3][2]))
    labels <- c("energy", "fat", "Saturated")
    pie(x, labels = labels)
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
