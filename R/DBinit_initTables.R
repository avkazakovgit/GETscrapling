#' Initialize Database Tables from Definitions
#'
#' Create multiple tables in a SQLite database according to a nested list of
#' table definitions. Each top‐level element of `tab_defs` is the name of a
#' table; its value is a named list of column definitions, where each column
#' definition must contain an `init` element (an SQL type + constraints string).
#'
#' @param con A valid `DBIConnection` to a SQLite (or other DBI‐compliant) database.
#' @param tab_defs Named list of table definitions. Example:
#'   ```r
#'   list(
#'     users = list(
#'       id   = list(init = "INTEGER PRIMARY KEY"),
#'       name = list(init = "TEXT NOT NULL")
#'     ),
#'     posts = list(
#'       id       = list(init = "INTEGER PRIMARY KEY"),
#'       user_id  = list(init = "INTEGER NOT NULL"),
#'       content  = list(init = "TEXT")
#'     )
#'   )
#'   ```
#' @return Invisibly returns `NULL`. Tables are created if they did not already exist.
#' @export
DBinit_initTables    =    function(  con     ,  tab_defs                       ) {

  dbWithTransaction( con ,
                     {
                       iwalk(  tab_defs,

                               ~ {
                                 col_defs <- imap_chr( .x , ~ sprintf("%s %s",   .y ,  .x $ init  )  )

                                 sql      <-  sprintf(  "CREATE TABLE IF NOT EXISTS %s (\n  %s\n);",  .y,  str_c( col_defs, collapse = ",\n  " ) )

                                 dbExecute( con , sql )
                               })

                     })

}
