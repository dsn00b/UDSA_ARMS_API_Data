#' @import httr
#' @import jsonlite
#' @export

refresh_metadata <- function() {
  
  # initialise

  key_parameter <- "api_key=UzCB1lR6AR7XyiUpyIlhYYHuodrznWx1NqdHJgtz"
  
  # get 'years' metadata
  
  end_point <- "https://api.ers.usda.gov/data/arms/year?"
  URL <- paste0(end_point, key_parameter)
  years <- jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))$data
  
  # get 'states' metadata
  
  end_point <- "https://api.ers.usda.gov/data/arms/state?"
  URL <- paste0(end_point, key_parameter)
  states <- 
    jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))$data[, c("name", "code")]
  
  # get 'reports' metadata
  
  end_point <- "https://api.ers.usda.gov/data/arms/report?"
  URL <- paste0(end_point, key_parameter)
  reports <- jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))$data$name
  
  # get 'farmtypes' metadata
  
  end_point <- "https://api.ers.usda.gov/data/arms/farmtype?"
  URL <- paste0(end_point, key_parameter)
  farmtypes <- 
    jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))$data[, c("id", "name")]
  
  # get 'categories' / 'category values' (or 'sub-categories') metadata
  
  end_point <- "https://api.ers.usda.gov/data/arms/category?"
  URL <- paste0(end_point, key_parameter)
  categories <- 
    jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))$data$name
  
  # get 'variables' metadata
  
  end_point <- "https://api.ers.usda.gov/data/arms/variable?"
  URL <- paste0(end_point, key_parameter)
  variables <- jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))$data
  
  # return metadata
  
  return(list(years = years, states = states, reports = reports,
              farmtypes = farmtypes, categories = categories, variables = variables))
  
}