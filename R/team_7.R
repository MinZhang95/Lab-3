#' @name team_7
#' @aliases team_7
#' @title team_7
#' @usage team_7(file,tolerance)
#' @import dplyr maptools purrr sf tidyverse
#' @importFrom "methods" "as"
#' @param file A path to a shape file
#' @param tolerance The value used to thin the polygon
#' @return A data frame of the geometry file \code{file}
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
  
  return(ozplus)
}
