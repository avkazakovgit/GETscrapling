% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBinit_initMetadata.R
\name{DBinit_initMetadata}
\alias{DBinit_initMetadata}
\title{Initialize Metadata Table from Column Definitions}
\usage{
DBinit_initMetadata(con, tab_defs)
}
\arguments{
\item{con}{A valid \code{DBIConnection} to a SQLite (or other DBI) database.}

\item{tab_defs}{Named list of table definitions, where each element is itself
a named list of column definitions. Each column definition may include a
\code{role} entry (a non-empty character string) indicating its role.}
}
\value{
Invisibly returns \code{NULL}. The \code{metadata} table will be created
(if not present) and populated with unique \verb{(col_name, col_role)} pairs.
}
\description{
Creates (or replaces) a \code{metadata} table that lists each column's role
as specified in your table definitions. This helps downstream functions
understand which columns to treat as inputs, outputs, statuses, etc.
}
