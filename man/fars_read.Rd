% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Read from a CSV file}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A character string providing the full path of a CSV file.}
}
\value{
This function returns a data frame of records.
}
\description{
The function first validate the file path by \code{filename} and abort
the process if the path is incorrect with an error message.
If the filepath is valid it loads the file in memory.
}
\details{
The function loads a csv file.
}

