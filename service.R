user <- unname(Sys.info()["user"])
if (user == "shiny") {
  
  # Set library locations
  .libPaths(c(
    "C:/Users/User/Documents/R/win-library/3.6","C:/Program Files/R/R-3.6.2/library"
  )
  )
  
}

library(jsonlite)
library(httr)
library(stringr)
library(dplyr)
library(shiny)

app_id <- "0c99b187"
app_key <- "1f2132dee6bef6a6f4f7b0313edc531a"


getRecipes <- function(key, mealType, cuisineType, from=0 ,to=10, low, high){
  url <- "https://api.edamam.com/search?q={key}&app_id={app_id}&app_key={app_key}&from={from}&to={to}&mealType={mealType}&cuisineType={cuisineType}&calories={low}-{high}"
  url  <- str_glue(url, key=key, app_id=app_id, app_key=app_key, mealType=mealType, cuisineType=cuisineType,
                   from=from, to=to, low=low, high=high)
  resp <- GET(url)
  info <- content(resp, "text")
  data <- fromJSON(info)
  data$hit
}



getDT <- function(dat){
  recipes <- dat$recipe
  recipes %>% select(label, dietLabels, calories, totalWeight, totalTime,dishType ) %>% rename(title=label)
}

# dat <- getRecipes("chicken", "dinner", "chinese", 0, 10, 0, 200)
# getDT(dat)