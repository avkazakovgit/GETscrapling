% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DB_appendTibble.R
\name{DB_appendTibble}
\alias{DB_appendTibble}
\title{Append a Tibble to a Database Table Atomically}
\usage{
DB_appendTibble(db_con, db_table, df_append)
}
\arguments{
\item{db_con}{A valid \code{DBIConnection} to the target database.}

\item{db_table}{Character(1). Name of the existing table to append to.}

\item{df_append}{A data.frame or tibble containing rows to insert.}
}
\value{
Invisibly returns \code{TRUE} on success. Throws an error if the
connection is invalid, the table does not exist, or \code{df_append} is not a
suitable data.frame.
}
\description{
Appends the rows of a data.frame or tibble to an existing database table,
wrapping the operation in a single transaction for atomicity.
}
\examples{
con <- DB_load("mydb.sqlite")
new_data <- tibble::tibble(x = 1:3, y = letters[1:3])
DB_appendTibble(con, "tab_crawl", new_data)
}
