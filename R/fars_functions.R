#` Read a file into a dataframe
#`
#` This function checks that a specified file (\code{filename}) exists and if so, reads the data from the file into a dataframe.
#`
#` @importFrom readr read_csv
#` @importFrom dplyr tbl_df
#`
#` @param filename A character string representing a filename.
#`
#` @return This function returns a dataframe containing the data read from the specified file.
#`   As a side effect, this function also prints the data to the console.
#`
#` @note This function has dependencies on \code{\link[readr]{read_csv}} and \code{\link[dplyr]{tbl_df}}
#` @note If the filename supplied does not exist in the user's working direcrtory then this funtion returns an error.
#`
#` @examples
#`   fars_read(newfile.txt)
#`   fars_read(oldfile.csv)
#`
#` @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#` Dynamically generate a file name
#`
#` This function creates a filename based on a year (\code{year}) input by the user.
#`
#` @param year An integer value representing a year.
#`
#` @return This function returns a file name as a string.
#`
#` @note If the user does not supply an input which can be treated as an integer then this function returns an error.
#`
#` @examples
#`   make_filename(2015)
#`   make_filename(2016)
#`
#` @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#` Read a file or files into a list of dataframes.
#`
#` This function reads in a vector of filename(s) (\code{years}) and generates a dataframe for each one. 
#`   The dataframes consist of two columns, MONTH and year. The dataframes have the same number of rows as the source file.
#`   Each dataframe is held as an item in a list.
#`
#` @importFrom dplyr mutate select
#`
#` @param years An integer vector of years
#`
#` @return This function returns a list of dataframes. It will error if an invalid year value is supplied.
#`
#` @note This function has dependencies on \code{\link[dplyr]{mutate}} and \code{\link[dplyr]{select}}
#` @note If the user supplies a year for which no data exists then this function returns an error.
#`
#` @examples
#`   fars_read_years(c(2013,2014,2015))
#`  
#`   yrs <- c(2013,2014,2015)
#`   fars_read_years(yrs)
#`
#` @export

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

#` Summarise motor vehicle fatality information.
#`
#` This function produces a table of counts of motor vehicle fatalities by year and month, for a vector of years provided by the user (\code{years})
#`
#` importFrom dplyr bind_rows group_by summarize
#` importFrom tidyr spread
#`
#` @inheritParams fars_read_years
#`
#` @return This function returns a dataframe containing counts of motor vehicle fatalities by month and year.
#`
#` @note This function has dependencies on \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarize}} and \code{\link[tidyr]{spread}}
#` @note This function will return an error if the user provides a year for which data does not exist as input.
#`
#` @examples
#`   df <- fars_summarize_years(c(2013,2014,2015)) 
#`
#`   yrs <- c(2013,2014,2015)
#`   fars_summarize_years(yrs)
#`
#` @export


fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#` Plot motor vehicle fatalities on a map.
#`
#` This function  plots motor vehicle fatalities for a specific state of the USA and year (\code{state.num}, \code{year}) and plots them on a map.
#` If no fatalities occured in a given state/year combination the user is shown a message which explains this.
#`
#` @importFrom maps map
#` @importFrom graphics points
#`
#` @param state.num An integer value identifying a particular US state
#`
#` @inheritParams make_filename
#`
#` @return This function returns a map (plot)
#`
#` @note This function has dependencies on \code{\link[maps]{map}} and \code{\link[graphics]{points}}
#` @note This function will return an error if an invalid state code is provided as input.
#`
#` @examples
#`   fars_map_state(1,2014)
#`   fars_map_state(12,2015)
#`
#` @export

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