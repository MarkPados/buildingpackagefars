#' Read in Fatality Analysis Reporting System data file#'
#'
#' @return Read in Fatality Analysis Reporting System data file from a csv file
#'
#' @param filename csv type database
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @details The function will read the data from the path as data frame tibble. If it doesn't exist an error message will be returned.
#'
#' @examples
#' WD <- setwd(system.file("extdata", package = "buildingpackagefars"))
#' fars_read("accident_2013.csv.bz2")
#' setwd(WD)
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(system.file("inst/extdata", filename, package="buildingpackagefars"), progress = FALSE)
  })
  dplyr::tbl_df(data)
}



#' Create filename with a provided year
#'
#' @return Create a filename for the accident csv file based on a year.
#'
#' @param year year to add to the filename
#'
#' @details Create a filename with a provided year. E.g. if 2015 is provided then it will return "accident_2015_csv.bz2".
#'
#' @examples
#' WD <- setwd(system.file("extdata", package = "buildingpackagefars"))
#' make_filename(2015)
#' setwd(WD)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
  system.file("inst/extdata", file, package="buildingpackagefars")
}



#' Read in multiple Fatality Analysis Reporting System data files
#'
#' @return Read in multiple Fatality Analysis Reporting System data files
#'
#' @param years one or multiple years relating to the file names to be read in
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @details The function will read multiple data from the path as data frame tibble. If it doesn't exist an error message will be returned.
#'
#' @examples
#' WD <- setwd(system.file("extdata", package = "buildingpackagefars"))
#' fars_read_years(2013:2015)
#' setwd(WD)
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}



#' Summarize observations by multiple years
#'
#' @return Read data and summarize observations by one or more years
#'
#' @param years one or multiple years relating to the file names to be read in and summarize
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @details The function will read multiple data from the path as data frame tibble nad summerize it by years
#'
#' @examples
#' WD <- setwd(system.file("extdata", package = "buildingpackagefars"))
#' fars_summarize_years(2013:2015)
#' setwd(WD)
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}



#' Plot State Accidents
#'
#' @return Plot State Accidents by year ans state
#'
#' @param state.num ID number of a state
#' @param year selected year
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @details The function will read data and create a map of accidents for a given state and year.
#'
#' @examples
#' WD <- setwd(system.file("extdata", package = "buildingpackagefars"))
#' fars_map_state(2, "2016")
#' setwd(WD)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
