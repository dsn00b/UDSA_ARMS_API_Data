#' @import httr
#' @import jsonlite

pull_data <- function(year, report, state = "all", farmtype = 1, variable = "all") {
  
  # initialise basic variables
  
  end_point <- "https://api.ers.usda.gov/data/arms/surveydata?"
  key_parameter <- "api_key=UzCB1lR6AR7XyiUpyIlhYYHuodrznWx1NqdHJgtz"
  
  # collect and process inputs
  
  year <- paste(year, collapse = ",") # allow for multi-select in the Shiny app
  report <- gsub(" ", "+", tolower(paste(report, collapse = ","))) # allow for multi-select in the Shiny app
  state <- if ("all" %in% state) {"all"} else paste(state, collapse = ",")
  farmtype <- if (farmtype == "Farm Operator Households") {1} else if (farmtype == "Farm Businesses") {2} else {1} # allow for single-select in the Shiny app
  variable_id <- 
    if ("all" %in% variable) {"all"} else 
      {paste(unique(metadata$variables[metadata$variables$name %in% variable, "id"]), sep = ",")}  # allow for multi-select in the Shiny app; assumes that metadata list is available
  
  # formulate api call
  
  if (variable_id == "all") {
    
    URL <- paste0(end_point, "year=", year, "&report=", gsub(" ", "+", tolower(report)), 
                  "&variable=", variable_id, "&state=", state, "&farmtype=", farmtype, "&", key_parameter)
    
  } else {
    
    URL <- paste0(end_point, "year=", year, "&report=", gsub(" ", "+", tolower(report)), 
                  "&state=", state, "&farmtype=", farmtype, "&", key_parameter)
    
  }
  
  # get api data
  
  parsed_get_object <- jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))
  
  if ("error" %in% parsed_get_object) {
    
    if (parsed_get_object$error$code == "OVER_RATE_LIMIT") {
      
      return("Unfortunately, API limit has been exceeded, try later")
    
    } else {
      
      return(parsed_get_object$error$message)
      
    }
  
  } else {
    
    return(parsed_get_object$data)
    
  }
  
}