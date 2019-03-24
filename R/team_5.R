#' Function for Lab3 using Lab2 Team5 function
#'
#' @name team_5
#' @aliases team_5
#' @title  team_5
#' @usage team_5(file,tolerance)
#' @import dplyr maptools purrr sf
#' @importFrom "methods" "as"
#' @importFrom "stats" "rnorm"
#' @param file A path to the .shp file of a selected country. Default value is Australia.
#' @param tolerance A numeric number of thinning parameter. Default value is 0.1.
#' @import tidyverse dplyr purrr sf
#' @return A dataframe for plotting the map of the selected country (with longitude, latitude, and grouping indeces of states/provinces). A data frame of the geometry file \code{file}
#' @examples
#' fpath <- system.file("extdata","gadm36_AUS_1.shp",package="Lab3R")
#' team_5(fpath,0.1)
#' @export

team_5 <- function(file=system.file("extdata","gadm36_AUS_1.shp",package="Lab3R"), tolerance = 0.1) {
  
  #the following are test before running the function
  
  #invalid arguement of file
  if (!file.exists(file)) {
    warning("file does not exist")
    return(NA)
  }
  # invalid arguement of tolerance
  if (!is.numeric(tolerance)) {
    warning("tolerance is not numeric")
    return(NA)
  }
  
  #negative tolerance
  if(tolerance <= 0) {
    warning("tolerance must be a positive number")
    return(NA)
  }
  
  #the function
  ozbig <- read_sf(file)
  oz_st <- maptools::thinnedSpatialPoly(
    as(ozbig, "Spatial"), tolerance = tolerance, 
    minarea = 0.001, topologyPreserve = TRUE)
  oz <- st_as_sf(oz_st)
  
  Mat2Df <- function(Mat){
    long <- Mat[,1]
    lat <- Mat[,2]
    order <- 1:nrow(Mat)
    group <- rep(rnorm(1),nrow(Mat))
    df <- data.frame(long=long,lat=lat,group=group,order=order)
    df
  }
  oz_flatten <- flatten(flatten(oz$geometry))
  ozplus <- purrr::map_df(.x=oz_flatten,.f=Mat2Df)
  
  return(ozplus)
}
