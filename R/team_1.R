#' Function from Lab2 Team1
#'
#' @param file A path to the .shp file of a selected country. Default value is Australia.
#' @param tolerance A numeric number of thinning parameter. Default value is 0.1.
#' @import tidyverse dplyr purrr sf
#' @importFrom methods as
#' @export
#' @return A dataframe for plotting the map of the selected country (with longitude, latitude, and grouping indeces of states/provinces).
#' @examples
#' file = system.file("extdata", "gadm36_AUS_1.shp", package = "Lab3R")
#' tolerance = 0.1
#' team_1(file = file, tolerance = tolerance)

team_1 <- function(file = system.file("extdata", "gadm36_AUS_1.shp", package = "Lab3R"),
                   tolerance = 0.1) {
  # invalid arguement of file
  if (!file.exists(file)) {
    warning("file does not exist")
    return(NA)
  }
  # invalid arguement of tolerance
  if (!is.numeric(tolerance)) {
    warning("tolerance is not numeric")
    return(NA)
  } else {
    if (tolerance < 0) {
      warning("tolerance must be a positive number")
      return(NA)
    }
  }
  
  x = y = NULL
  
  # start Lab2 Team1's content
  ozbig <- read_sf(file)
  oz_st <- maptools::thinnedSpatialPoly(
    as(ozbig, "Spatial"), tolerance = tolerance,
    minarea = 0.001, topologyPreserve = TRUE)
  oz <- st_as_sf(oz_st)
  df.oz.purr <- oz$geometry %>%
    map_depth(3, data.frame) %>%
    purrr::flatten() %>%
    purrr::flatten() %>%
    dplyr::bind_rows(.id = "group") %>%
    dplyr::rename("lat" = y, "long" = x)
  # additional info
  nrep <- oz$geometry %>% 
    map_depth(3, data.frame) %>% map_depth(3, nrow) %>% 
    map_depth(2, unlist) %>%
    map_depth(1, unlist) %>% map_depth(1, sum) %>% 
    unlist()
  df.info <- data.frame(country = rep(oz$NAME_0, nrep),
                        name = rep(oz$NAME_1, nrep))
  df.oz.purr <- cbind(df.info, df.oz.purr)
  # return
  return(df.oz.purr)
}
