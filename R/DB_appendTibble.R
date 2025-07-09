#' Append a Tibble to a Database Table Atomically
#'
#' Appends the rows of a data.frame or tibble to an existing database table,
#' wrapping the operation in a single transaction for atomicity.
#'
#' @param db_con A valid `DBIConnection` to the target database.
#' @param db_table Character(1). Name of the existing table to append to.
#' @param df_append A data.frame or tibble containing rows to insert.
#'
#' @return Invisibly returns `TRUE` on success. Throws an error if the
#'   connection is invalid, the table does not exist, or `df_append` is not a
#'   suitable data.frame.
#' @examples
#' con <- DB_load("mydb.sqlite")
#' new_data <- tibble::tibble(x = 1:3, y = letters[1:3])
#' DB_appendTibble(con, "tab_crawl", new_data)
#' @export
DB_appendTibble      =   function(   db_con    ,
                                     db_table  ,
                                     df_append      ) {


  assert_that(
    DBI::dbIsValid(db_con),
    is.string(db_table),
    is.data.frame(df_append) && nrow(df_append) > 0,
    msg = "Invalid arguments to DB_appendTibble(): ensure a valid connection, existing table name, and non-empty data.frame."
  )
  # Check table exists
  if (!DBI::dbExistsTable(db_con, db_table)) {
    stop(sprintf("Table '%s' does not exist in the database.", db_table))
  }

  # ensure each batch is its own atomic append
  dbWithTransaction(  db_con  , {

    DBI::dbAppendTable(  db_con , db_table  ,   df_append  )

  })

}
