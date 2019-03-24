#' Function for Lab3 using Lab2 Team5 function
#' 
#' @name team_7
#' @aliases team_7
#' @title team_7
#' @usage team_7(file,tolerance)
#' @import dplyr maptools purrr sf tidyverse tools
#' @importFrom "methods" "as"
#' @param file A path to the .shp file of a selected country. Default value is Australia.
#' @param tolerance A numeric number of thinning parameter. Default value is 0.1.
#' @return A dataframe for plotting the map of the selected country (with longitude, latitude, and grouping indeces of states/provinces).
#' @examples
#' fpath <- system.file("extdata","gadm36_AUS_1.shp",package="Lab3R")
#' team_7(fpath,0.1)
#' @export

team_7 <- function(file = system.file("extdata","gadm36_AUS_1.shp", package="Lab3R"), tolerance = 0.1){
  
  # TESTS
  # invalid arguement of file
  if (!file.exists(file)) {
    warning("file does not exist")
    return(NA)
  }
  
  # invalid arguement of file type
  if (file_ext(file) != "shp" ) {
    warning("file is not a shapefile")
    return(NA)
  }
  
  # invalid arguement of tolerance
  if (!is.numeric(tolerance)) {
    warning("tolerance is not numeric")
    return(NA)
  }
  
  # negative tolerance
  if (tolerance <= 0) { 
    warning("tolerance must be a positive number")
    return(NA)
  }
  
  # FUNCTION
  ozbig <- read_sf(file)
  oz_st <- thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
  oz <- st_as_sf(oz_st)
  
  f <- function(dframe){
    dframe <- data.frame(order = c(1:nrow(dframe)), long = dframe$x, lat = dframe$y)
  }
  
  ## Here our depth is 3, but that could change depending on the file
  ozplus <- oz$geometry %>% 
    modify_depth(3,data.frame) %>% 
    modify_depth(3,f) %>%
    flatten() %>%
    flatten() %>%
    bind_rows(.id = "group")
  
  # Bind the country name and territory or state names
  nrep <- oz$geometry %>% 
    map_depth(3, data.frame) %>% map_depth(3, nrow) %>% 
    map_depth(2, unlist) %>%
    map_depth(1, unlist) %>% map_depth(1, sum) %>% 
    unlist()
  
  df.info <- data.frame(country = rep(oz$NAME_0, nrep),
                        name = rep(oz$NAME_1, nrep))
  ozplus <- cbind(df.info, ozplus)
  
  # return
  return(ozplus)
}
