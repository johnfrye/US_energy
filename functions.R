readEIA <- function(url) {
  require("RCurl")
  require("jsonlite")
  require("data.table")
  require("dplyr")
  
  json.raw <- getURL(url)
  json.procd <- fromJSON(json.raw)
  data.raw <- json.procd$series$data
  
  if (is.null(data.raw)) {
    return(data.table())
  } else {
    data.procd <- data.table(matrix(unlist(data.raw), 
                                    nrow = length(data.raw[[1]]), 
                                    byrow = TRUE)) %>%
      mutate(YEAR = as.numeric(substr(V1, 1, 4)),
             MONTH = as.numeric(substr(V1, 5, 6)),
             CONSUMPTION = as.numeric(as.character(V2))) %>%
      select(-V1, -V2)
    
    return(data.procd)
  }
}

readFRED <- function(url) {
  require("RCurl")
  require("jsonlite")
  require("data.table")
  require("dplyr")
  
  json.raw <- getURL(url)
  json.procd <- fromJSON(json.raw)
  data.raw <- json.procd$observations
  data.procd <- data.table(YEAR = year(data.raw$date),
                           POPULATION = 1000 * as.numeric(data.raw$value)) 
  
  data.procd
}