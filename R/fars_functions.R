#'Read in a csv file as a tibble
#'
#'The function checks if the file exists and returns a warning if it doens't. If the file does exist, it is read in and converted to a tibble. The function
#'relies on the read_csv and tbl_df functions from the readr and dplyr packages, respectively. The function assumes that the data is formatted as a csv. If
#'it isn't, the function may return an error
#'
#'@param filename A Character string of the file's location.
#'
#'@return  The function returns a tibble containing the data in the file.
#'
#'@examples
#'
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}
#'
#'Create filename for annual accident data
#'
#'This function takes a year (in the form of a string or number) and returns a filename for a csv.bs2 file containing accident data for that year.
#'Note that the function does not return the file, just a string that can be passed to a write function. While the function retruns errors if the input is
#'the wrong type, is doesn't check to see if the input is a four-digit year.  make_filename is not dependant on any functions other than ones in base R.
#'
#'@param year A string or number of the year to be used in the filename
#'
#'@return The function returns a string of a filename for a file contain accident data of the given year.
#'
#'@examples
#'make_filename("2015")
#'make_filename(2014)
make_filename <- function(year) {
        year <- as.integer(year)
        end_of_filename <- sprintf("accident_%d.csv.bz2", year)
        system.file("extdata", end_of_filename, package="packagesfinalproject")
}
#'
#'Extract month values for given year(s)
#'
#'This function takes a vector of years and returns a list of tibbles, one tibble for each year.  Each tibble contains the year and the month value for the
#'accident dataset for that year.  This function relies on the make_filename and fars_read function.  The latter function depends on functions from
#'the dplyr and readr packages.  The fars_read_years also imports the mutate and slect functions from dplyr.The function checks, for year item in the
#'vector of years, that a file with the correct name exists.  If not, it returns a warning.
#'
#'@param years A vector of years to pass to make_filename.
#'
#'@return The function returns a list of tibbles containing the month values for each year's accident dataset along with that year.
#'
#'@examples
#'fars_read_years(2013)
#'fars_read_years(2013:2015)
#'fars_read_years(c("2013","2014"))
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
#'
#'Summarize accident data with number of observations for each month.
#'
#'This function takes a vector of years and returns a tidy tibble containing the number of observations in the accident data for each month/year combination.  In
#'addition to the dependancies of the fars_read_years function, fars_summarize_years imports two functions from dplyr (group_by and summarize) and one
#'from tidyr (spread).  If an input for year doesn't have a corresponding file, a warning is return (thanks to helper function fars_read_years).
#'
#'@param years A vector of years to pass to fars_read_years.
#'
#'@return The function returns a tibble containing the number of observations for each month (rows)/year (column) combination.
#'
#'@examples
#'fars_summarize_years(2013)
#'fars_summarize_years(2013:2015)
#'fars_summarize_years(c("2013","2014"))
#'
#'@export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}
#'
#'Create a map of all the accidents in a given state and year.
#'
#'This function takes a state code and year (both can be either a string or number).  It then reads in the accident data for that year and creates a plot
#'of the accidents from the given state in that year.  In addition to the error checking of the helper function fars_read, fars_map_state also checks to
#'see if the state code is valid and if there are accidents in the given state.  If either of those conditions is not met, the function returns an error.
#'In additon to the importing make_filename and fars_read, and thus the latter's importing of functions from dplyr and readr, fars_map_state imports
#'points from graphics (part of base R) and map from maps.
#'
#'@param state.num The numeric code of the desired state (can be a charecter string, e.g. '4')
#'@param year A string or number of the year to be used
#'
#'@return The function returns a map (techically a side effect of the function) of all accidents in the given state and year.
#'
#'@examples
#'fars_map_state(4, 2013)
#'fars_map_state(1, 2013)
#'
#'@export
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
0
