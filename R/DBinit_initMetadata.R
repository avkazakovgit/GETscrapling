#' Initialize Metadata Table from Column Definitions
#'
#' Creates (or replaces) a `metadata` table that lists each column's role
#' as specified in your table definitions. This helps downstream functions
#' understand which columns to treat as inputs, outputs, statuses, etc.
#'
#' @param con A valid `DBIConnection` to a SQLite (or other DBI) database.
#' @param tab_defs Named list of table definitions, where each element is itself
#'   a named list of column definitions. Each column definition may include a
#'   `role` entry (a non-empty character string) indicating its role.
#'
#' @return Invisibly returns `NULL`. The `metadata` table will be created
#'   (if not present) and populated with unique `(col_name, col_role)` pairs.
#' @export
DBinit_initMetadata  =    function(  con     ,  tab_defs                       ) {


  assertthat::assert_that(  DBI::dbIsValid(con),  nzlist( tab_defs )  ,  msg = "`tab_defs` must be a non-empty named list of table defs." )



  roles_df  =
    tab_defs  %>%
    purrr::map_dfr(
      ~  {
        purrr:: map_dfr(   .x    , ~ {  .x $ role  }) %>%

          tidyr::pivot_longer(   cols      = everything() ,
                                 names_to  = 'col_name' ,
                                 values_to = 'col_role'
          )
      })  %>%
    distinct

  assertthat::assert_that(  nrow(roles_df) > 0,  msg = "No column roles found in `tab_defs`; at least one `role` is required." )


  DBI::dbWithTransaction(con, {

    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS metadata (
        col_name TEXT NOT NULL,
        col_role TEXT NOT NULL,
        PRIMARY KEY (col_name, col_role)
      );
    ")

    DBI::dbExecute(con, "DELETE FROM metadata;")

    DBI::dbWriteTable(
      con,
      name      = "metadata",
      value     = roles_df,
      append    = TRUE,
      row.names = FALSE
    )

  })

  invisible(NULL)


}
