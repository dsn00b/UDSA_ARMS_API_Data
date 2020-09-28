#' @import httr
#' @import jsonlite

key_parameter <- "api_key=UzCB1lR6AR7XyiUpyIlhYYHuodrznWx1NqdHJgtz"

# get list of all 'states' in the survey database

end_point <- "https://api.ers.usda.gov/data/arms/state?"
URL <- paste0(end_point, key_parameter)
states <- jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))$data$code

# get list of all 'years' in the survey database

end_point <- "https://api.ers.usda.gov/data/arms/year?"
URL <- paste0(end_point, key_parameter)
years <- jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))$data

# get list of all 'reports' in the survey database

end_point <- "https://api.ers.usda.gov/data/arms/report?"
URL <- paste0(end_point, key_parameter)
reports <- jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))$data$name

# get list of all 'farmtypes' in the survey database

end_point <- "https://api.ers.usda.gov/data/arms/farmtype?"
URL <- paste0(end_point, key_parameter)
farmtypes <- jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))$data$id

# get survey data: query by year, report, state and farmtype

end_point <- "https://api.ers.usda.gov/data/arms/surveydata?"
data <- 0 # initialise
i <- 0 # to debug
j <- 1 # to debug
get_obj <- list() # to debug

for (year in years) {
  for (report in reports) {
    for (state in states) {
      for (farmtype in farmtypes) {
        URL <- paste0(end_point, "year=", year, "&report=", gsub(" ", "+", tolower(report)), 
                      "&state=", state, "&farmtype=", farmtype, "&", key_parameter)
        
        Sys.sleep(0.05) # so as to not hit API call limit
        
        if (is.numeric(data)) {
          get_obj[[j]] <- httr::GET(URL) # to debug
          #print(get_obj)
          data <- jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))$data  
        } else {
          get_obj[[j]] <- httr::GET(URL) # to debug
          #print(get_obj)
          data <- rbind(data, jsonlite::fromJSON(httr::content(httr::GET(URL), "text", encoding = "UTF-8"))$data)
        }
        
        j <- j + 1
        
      }
    }
  }
  i <- i + 1
  print(paste0("Year ", i, "/", length(years), " complete")) # to debug
}
