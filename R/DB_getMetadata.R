#' Retrieve Column Role Metadata
#'
#' Fetches the mapping of database column names to their semantic roles from the
#' `metadata` table.
#'
#' @param db_con A valid `DBIConnection` to a database containing a `metadata` table.
#' @return A named character list where names are roles (e.g., "col_in", "col_out")
#' and values are the corresponding column names.
#' @examples
#' con <- DB_load("mydb.sqlite")
#' metadata <- DB_getMetadata(con)
#' @export
DB_getMetadata       =   function(   db_con                 ) {


  assert_that( DBI::dbIsValid(db_con),  msg = "`db_con` must be a valid DBI connection."  )

  # Ensure metadata table exists
  if ( ! DBI::dbExistsTable( db_con, "metadata" ) ) {
    stop("The 'metadata' table does not exist in this database.")
  }

  df   =   DBI::dbGetQuery(db_con, "SELECT col_role, col_name FROM metadata;")

  assert_that(  all(c("col_role", "col_name") %in% names(df)),  msg = "'metadata' table must contain 'col_role' and 'col_name' columns." )

  # Check for duplicate roles
  dup <- df $ col_role [ duplicated( df $ col_role ) ]
  if ( length(dup) > 0) {
    stop("Duplicate roles found in 'metadata': ", paste(unique(dup), collapse = ", "))
  }


  result <- setNames( as.list( df $ col_name ),
                      df $ col_role   )

  result

}
