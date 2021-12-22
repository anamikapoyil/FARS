#' Read data
#'
#' This function reads data in CSV format into R.
#' First, it checks whether the file already exists in the working directory.
#' You get an error message if the file does not exist, y
#' Second, the CSV file is read into R if it exists.
#' Third, the data are converted to a tibble.
#'
#' @param filename A string with the full name of a file in CSV format. This
#' function returns an error, if the file does not exist in the directory.
#'
#' @return This function returns a data frame in tibble format.
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv")
#' fars_read("accident_2014.csv")
#' fars_read("accident_2015.csv")
#'}
#' @importFrom readr read_csv
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

#' Name file
#'
#' This function makes a file name.
#' First, the year that is imputted will be converted to an integer, the value of which must be
#' a simple year without quotes.
#' Second, the year will be added to the middle of a string that represents the
#' main pattern of the file name, in the placeholder for an integer.
#'
#' @param year A number representing the desired year.
#'
#' @return This function returns a character vector with a combination of the
#' main string and the year inputted.
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename(2014)
#' make_filename(2015)
#'}
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read years
#'
#' This function reads specific variables from the data.
#' It reads two specific columns, MONTH and year, from the input files, and
#' then stores them as data frames in a list. It works by calling the function
#' \code{make_filename} from within
#'
#' @param years A vector of numbers, which represents the years to be analyzed.
#' Can be also a sequence of numbers. This function returns an error, if an
#' year number if invalid.
#'
#' @return A list with the data frames in tibble format for the selected years.
#' The list has the length of the input vector and each data frame contains
#' only the selected columns.
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' fars_read_years(c(2013,2014))
#' fars_read_years(2013:2015)
#'}
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
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

#' Summarize years
#'
#' This function summarizes the accidents by year and month for the data frames
#' imported. It calls the function \code{fars_read_years} from within.
#'
#' @param years A vector of numbers, which represents the years to be analyzed.
#' Can be also a sequence of numbers.
#'
#' @return This function returns a data frame in tibble format with months as
#' rows and years as columns, in which the data are the counts of accidents.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013,2014))
#' fars_summarize_years(2013:2015)
#'}
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom magrittr %>%
#' @importFrom rlang .data
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

#' Display accidents map by state and year
#'
#' Displays a plot with a state map including the accidents location by year
#' If the \code{state.num} is invalid the function shows an error
#'
#' @param state.num A number representing the FARS code of the designated state.
#' This function returns an error, if the state number if invalid.
#' #' \tabular{cc}{
#'   \strong{State Code} \tab \strong{State Name}    \cr
#'   01 \tab  Alabama              \cr
#'   02 \tab  Alaska               \cr
#'   04 \tab  Arizona              \cr
#'   05 \tab  Arkansas             \cr
#'   06 \tab  California           \cr
#'   08 \tab  Colorado             \cr
#'   09 \tab  Connecticut          \cr
#'   10 \tab  Delaware             \cr
#'   11 \tab  District of Columbia \cr
#'   12 \tab  Florida              \cr
#'   13 \tab  Georgia              \cr
#'   15 \tab  Hawaii               \cr
#'   16 \tab  Idaho                \cr
#'   17 \tab  Illinois             \cr
#'   18 \tab  Indiana              \cr
#'   19 \tab  Iowa                 \cr
#'   20 \tab  Kansas               \cr
#'   21 \tab  Kentucky             \cr
#'   22 \tab  Louisiana            \cr
#'   23 \tab  Maine                \cr
#'   24 \tab  Maryland             \cr
#'   25 \tab  Massachusetts        \cr
#'   26 \tab  Michigan             \cr
#'   27 \tab  Minnesota            \cr
#'   28 \tab  Mississippi          \cr
#'   29 \tab  Missouri             \cr
#'   30 \tab  Montana              \cr
#'   31 \tab  Nebraska             \cr
#'   32 \tab  Nevada               \cr
#'   33 \tab  New Hampshire        \cr
#'   34 \tab  New Jersey           \cr
#'   35 \tab  New Mexico           \cr
#'   36 \tab  New York             \cr
#'   37 \tab  North Carolina       \cr
#'   38 \tab  North Dakota         \cr
#'   39 \tab  Ohio                 \cr
#'   40 \tab  Oklahoma             \cr
#'   41 \tab  Oregon               \cr
#'   42 \tab  Pennsylvania         \cr
#'   43 \tab  Puerto Rico          \cr
#'   44 \tab  Rhode Island         \cr
#'   45 \tab  South Carolina       \cr
#'   46 \tab  South Dakota         \cr
#'   47 \tab  Tennessee            \cr
#'   48 \tab  Texas                \cr
#'   49 \tab  Utah                 \cr
#'   50 \tab  Vermont              \cr
#'   51 \tab  Virginia             \cr
#'   52 \tab  Virgin Islands       \cr
#'   53 \tab  Washington           \cr
#'   54 \tab  West Virginia        \cr
#'   55 \tab  Wisconsin            \cr
#'   56 \tab  Wyoming
#' }
#' @param year A number representing the expected year.
#'
#' @return This function returns a map with the locations of accidents for
#' the selected state and year.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013,2014))
#' fars_summarize_years(2013:2015)
#'}
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom rlang .data
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
