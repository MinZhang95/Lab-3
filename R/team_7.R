#' @name team_7
#' @aliases team_7
#' @title  team_7
#' @usage team_7(file,tolerance)
#' @import magrittr
#' @param file A path to a shape file
#' @param tolerance The value used to thin the polygon
#' @return A data frame of the geometry file \code{file}
#' @examples
#' tolerance <- 0.1
#' file <- "./data/gadm36_AUS_shp/gadm36_AUS_1.shp"
#' team_7(file,tolerance)
#' @export

team_7 <- function(file, tolerance){
  
  ozbig <- sf::read_sf(file)
  oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
  oz <- sf::st_as_sf(oz_st)
  
  f <- function(dframe){
    dframe <- data.frame(order = c(1:nrow(dframe)), long = dframe$x, lat = dframe$y)
  }
  
  ## Here our depth is 3, but that could change depending on the file
  ozplus <- oz$geometry %>% 
    purrr::modify_depth(3,data.frame) %>% 
    purrr::modify_depth(3,f) %>%
    purrr::flatten() %>%
    purrr::flatten() %>%
    bind_rows(.id = "group")
  
  return(ozplus)
}
