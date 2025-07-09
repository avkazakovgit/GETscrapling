#' Add a New Column to an Existing Table
#'
#' Alters a table by adding a new column with the specified type, default value,
#' and optional NOT NULL constraint.
#'
#' @param db_con A valid `DBIConnection` to the target database.
#' @param table Character(1). Name of the existing table to alter.
#' @param column Character(1). Name of the new column to add.
#' @param type Character(1). SQL data type for the column (e.g., "INTEGER", "TEXT").
#' @param default Scalar. Default value for the new column (quoted or unquoted as
#'   appropriate for the SQL type).
#' @param not_null Logical(1). If TRUE, adds a NOT NULL constraint; otherwise allows NULLs.
#'
#' @return Invisibly returns TRUE on success. Throws an error if the operation fails
#'   or if the column already exists.
#' @examples
#' con <- DB_load("mydb.sqlite")
#' DB_addColumn(con, "tab_queue", "retry_count", type = "INTEGER", default = 0)
#' @export
DB_addColumn         =   function(   db_con              ,
                                     table               ,
                                     column              ,
                                     type     = "INTEGER",
                                     default  = 0,
                                     not_null = TRUE          ) {

  assert_that(
    DBI::dbIsValid(db_con),
    is.string(table),
    is.string(column),
    is.string(type),
    is.flag(not_null),
    msg = "Invalid arguments to DB_addColumn()"
  )
  # Check table exists
  if (!DBI::dbExistsTable(db_con, table)) {
    stop(sprintf("Table '%s' does not exist.", table))
  }
  # Check column does not already exist
  cols <- DBI::dbGetQuery( db_con, glue::glue_sql(
    "PRAGMA table_info({`table`});",
    table = table, .con = db_con
  ))$name
  if (column %in% cols) {
    stop(sprintf("Column '%s' already exists in table '%s'.", column, table))
  }



  null_clause <- if (not_null) "NOT NULL" else ""

  sql <- glue_sql(
    "ALTER TABLE {`table`}
       ADD COLUMN {`column`} {type} {null_clause} DEFAULT {default};",
    table   = table,
    column  = column,
    type    = type,
    default = default,
    .con    = db_con
  )
  dbExecute( db_con , sql )

  invisible(TRUE)

}
