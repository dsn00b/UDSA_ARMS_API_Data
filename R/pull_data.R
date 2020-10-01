#' @import httr
#' @import jsonlite
#' @export

pull_data <- function(year, report, state = "all", farmtype = 1, variable = "all") {
  year=as.numeric(year)
  stopifnot(all(!is.na(year)))
  
  stopifnot(all(is.character(report),is.character(state),is.character(variable)))
  
  if(is.numeric(farmtype)){
    stopifnot(all(((farmtype%%1)==0),(farmtype<3),(farmtype>-1)))
  }else{
    stopifnot(is.character(farmtype))
  }
  
  americanStates <- c("all","AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
  stopifnot(all((state %in% americanStates)))
  
  
  # initialise basic variables
  
  end_point <- "https://api.ers.usda.gov/data/arms/surveydata?"
  key_parameter <- "api_key=UzCB1lR6AR7XyiUpyIlhYYHuodrznWx1NqdHJgtz"
  
  if (!exists("metadata")) {metadata <- refresh_metadata()}
  
  # collect and process inputs
  
  year <- paste(year, collapse = ",") # allow for multi-select in the Shiny app
  report <- gsub(" ", "+", tolower(paste(report, collapse = ","))) # allow for multi-select in the Shiny app
  state <- if ("all" %in% state) {"all"} else paste(state, collapse = ",")
  farmtype <- sapply(farmtype, 
                     function(x) {if (x == "Farm Operator Households") {1} 
                       else if (x == "Farm Businesses") {2} else {1}})  # allow for multi-select in the Shiny app
  variable_id <- 
    if ("all" %in% variable) {"all"} else 
      {paste(unique(metadata$variables[metadata$variables$name %in% variable, "id"]), sep = ",")}  # filtering on variable not allowed on Shiny app due to inherent ambiguous relationship between 'variable' and 'report'; variable related code is still made available, just in case
  
  # formulate api call
  
  if (!variable_id == "all") {
    
    URL <- paste0(end_point, "year=", year, "&report=", gsub(" ", "+", tolower(report)), 
                  "&variable=", variable_id, "&state=", state, "&farmtype=", farmtype, "&", key_parameter)
    
  } else {
    
    URL <- paste0(end_point, "year=", year, "&report=", gsub(" ", "+", tolower(report)), 
                  "&state=", state, "&farmtype=", farmtype, "&", key_parameter)
    
  }
  
  # get api data

  response <- httr::GET(URL)
  parsed_get_object <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  
  #throw an error if the API-ratelimit is low
  if (as.numeric(response$headers[["x-ratelimit-remaining"]])<50){
    warning("Slow down there cowboy! You're getting close to exceeding your hourly rate limit.")
  }
  
  
  
  if ("error" %in% names(parsed_get_object)) {
    
    if (parsed_get_object$error$code == "OVER_RATE_LIMIT") {
      
      return("Unfortunately, API limit has been exceeded, try later")
    
    } else {
      
      return(parsed_get_object$error$message)
      
    }
  
  } else {
    
    # get and clean the data
    data <- parsed_get_object$data
    data$var_rep <- paste0(data$variable_id, " (", data$report, ")") # disambiguate variable-report relationship
    data <- data[!is.na(data$estimate), ] # remove NAs
    data <- data[(!data$variable_is_invalid & data$unreliable_estimate == 0), ] # remove invalid data
    
    return(data)
    
  }
  
}