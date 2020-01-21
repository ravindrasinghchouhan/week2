#' @title fars_read
#' @description
#' Create a DataFrame from a File. If the File doesn't exist
#' the function will stop with an error message.
#'
#' @param filename A string.
#' @return This function returns a DataFrame from reading \code{filename}.
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' }
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
#' @title make_filename
#' @description
#' Select a filename according to the year. The Value of \code{year}
#' must be integer otherwise an error message will be in result.
#'
#' @param year A integer Representing the year
#' @return This function returns a string according to \code{year}.
#' @examples
#' make_filename(2014)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
#' @title fars_read_years
#' @description
#' Read files into a list of DataFrames with two columns, MONTH and Year,
#' over the vector of years input. All values in the vector years need to be
#' convertable into an integer otherwise the make_filename function wll error.
#'
#' @param years A vector of numbers representing years for analysis
#' @return This function returns a list of DataFrames with the
#'    length of the vector \code{years} and two columns: MONTH and year.
#' @examples
#' \dontrun{
#' fars_read_years(c(2014,2015))
#' }
#' @importFrom dplyr mutate
#' @importFrom dplyr select
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
#' @title fars_summarize_years
#' @description
#' Summarizes the count of Accidents by Month and Year for the
#' input vector \code{years}.
#'
#' @param years A vector of numbers shows years for analysis
#' @return This function returns a DataFrame with Months as rows
#'    and Years as columns. Each combination contains the count of
#'    Accidents.
#' @examples
#' \dontrun{
#' fars_summarize_years(2012:2014)
#' }
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
#' @title fars_map_state
#' @description
#' This function plots the number of Accidents for a Perticular state.
#' This function returns an error message if either the \code{state.num} or
#' \code{year} do not exist in Dataset
#'
#' @param state.num A number representing a state in the FARS data
#' @param year A number representing the year for analysis
#' @return This function returns a plot of the number of accidents
#' for the specified state.
#' @examples
#' \dontrun{
#' fars_map_state('01',2014)
#' }
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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
