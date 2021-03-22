#' @import httr
#' @import jsonlite
#' 
#' @title Refresh metadata corresponding to the USDA ARMS API
#' @return A \code{list} of metadata items
#' @export

refresh_metadata <- function() {
  
  # initialise

  key_parameter <- "api_key=xxxxxxxxxxxxxxxxx"
  
  # get 'years' metadata
  
  end_point <- "https://api.ers.usda.gov/data/arms/year?"
  URL <- paste0(end_point, key_parameter)
  parsed_get_object <- jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))
  if ("error" %in% names(parsed_get_object)) {
    
    if (parsed_get_object$error$code == "OVER_RATE_LIMIT") {
      
      return("Unfortunately, the API limit has been exceeded, try later")
      
    } else {
      
      return(parsed_get_object$error$message)
      
    }
    
  }
  years <- parsed_get_object$data
  
  # get 'states' metadata
  
  end_point <- "https://api.ers.usda.gov/data/arms/state?"
  URL <- paste0(end_point, key_parameter)
  parsed_get_object <- jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))
  if ("error" %in% names(parsed_get_object)) {
    
    if (parsed_get_object$error$code == "OVER_RATE_LIMIT") {
      
      return("Unfortunately, the API limit has been exceeded, try later")
      
    } else {
      
      return(parsed_get_object$error$message)
      
    }
    
  }
  states <- parsed_get_object$data[, c("name", "code")]
  
  # get 'reports' metadata
  
  end_point <- "https://api.ers.usda.gov/data/arms/report?"
  URL <- paste0(end_point, key_parameter)
  parsed_get_object <- jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))
  if ("error" %in% names(parsed_get_object)) {
    
    if (parsed_get_object$error$code == "OVER_RATE_LIMIT") {
      
      return("Unfortunately, the API limit has been exceeded, try later")
      
    } else {
      
      return(parsed_get_object$error$message)
      
    }
    
  }
  reports <- parsed_get_object$data$name
  
  # get 'farmtypes' metadata
  
  end_point <- "https://api.ers.usda.gov/data/arms/farmtype?"
  URL <- paste0(end_point, key_parameter)
  parsed_get_object <- jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))
  if ("error" %in% names(parsed_get_object)) {
    
    if (parsed_get_object$error$code == "OVER_RATE_LIMIT") {
      
      return("Unfortunately, the API limit has been exceeded, try later")
      
    } else {
      
      return(parsed_get_object$error$message)
      
    }
    
  }
  farmtypes <- parsed_get_object$data[, c("id", "name")]
  
  # get 'categories' / 'category values' (or 'sub-categories') metadata
  
  end_point <- "https://api.ers.usda.gov/data/arms/category?"
  URL <- paste0(end_point, key_parameter)
  parsed_get_object <- jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))
  if ("error" %in% names(parsed_get_object)) {
    
    if (parsed_get_object$error$code == "OVER_RATE_LIMIT") {
      
      return("Unfortunately, the API limit has been exceeded, try later")
      
    } else {
      
      return(parsed_get_object$error$message)
      
    }
    
  }
  categories <- parsed_get_object$data$name
  
  # get 'variables' metadata
  
  end_point <- "https://api.ers.usda.gov/data/arms/variable?"
  URL <- paste0(end_point, key_parameter)
  parsed_get_object <- jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))
  if ("error" %in% names(parsed_get_object)) {
    
    if (parsed_get_object$error$code == "OVER_RATE_LIMIT") {
      
      return("Unfortunately, the API limit has been exceeded, try later")
      
    } else {
      
      return(parsed_get_object$error$message)
      
    }
    
  }
  variables <- parsed_get_object$data
  
  # return metadata
  
  return(list(years = years, states = states, reports = reports,
              farmtypes = farmtypes, categories = categories, variables = variables))
  
}
