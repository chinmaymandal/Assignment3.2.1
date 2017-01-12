#' Read from a CSV file
#'
#' The function loads a csv file.
#'
#' @param filename A character string providing the full path of a CSV file.
#'
#' @return This function returns a data frame of records.
#'
#' @description
#' The function first validate the file path by \code{filename} and abort
#' the process if the path is incorrect with an error message.
#' If the filepath is valid it loads the file in memory.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
#'

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Construct a filename
#'
#' The function contructs a new filename.
#'
#' @param year A numerical input representing a year.
#'
#' @return This function returns a Character sting of new file name.
#'
#' @description
#' The function takes year as input and contructs a new file name by attaching the year \code{year} value
#' The expected result should be like accident_<year>.csv.bz2
#'
#' @examples
#' \dontrun{
#' makefilename(2013)
#' }
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read month and year from accident_<year>.csv
#'
#' The function contructs a new filename.
#'
#' @param years A vectorof year.
#'
#' @return This function returns a data frame of month and year as columns.
#'
#' @description
#' The function takes year vector as input and reads data for the specific year csv files and
#' returns a data frame of MONTH and year found in all of the year range csv files.
#' It reports with an error if any invalid year is specified.
#'
#'
#' @importFrom dplyr %>% mutate select
#'
#'

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

#' Summary of accidents
#'
#' The function contructs a new filename.
#'
#' @param years A numeric vector.
#'
#' @return This function returns a data frame of month, year and total number of incedents as columns.
#'
#' @description
#' This function summarizes the accident data like the number of cases permonth in a given year.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2016)
#' }
#'
#' @importFrom dplyr %>% bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
#'

fars_summarize_years <- function(years) {
        n<- 0
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by_("year", "MONTH") %>%
                dplyr::summarize_(n = ~n()) %>%
                tidyr::spread_("year", "n")
}

#' Plot the data over map
#'
#' The function plots the information over a map.
#'
#' @param state.num The number of a state
#' @param year The year value
#'
#' @description
#' Based on the \code{state.num} and \code{year} a summary of accidents occured in a given month and year is prepared.
#' The same information is filtered for a given valid state number and the result is
#' plotted over a state map.
#'


#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~STATE == state.num)
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
