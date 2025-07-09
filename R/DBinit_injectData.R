#' Populate Tables with Initial Data
#'
#' Given a named list of tibbles, append each tibble to its corresponding
#' table in the database. Tables must already exist with compatible schemas.
#'
#' @param con A valid `DBIConnection` (e.g. to a SQLite database).
#' @param init_data A named list where names are table names (strings) and
#'   values are tibbles or data.frames containing rows to insert.  Other tables
#'   are left untouched.  If `NULL` or empty, nothing happens.
#'
#' @return Invisibly returns `NULL`.  On error, no partial inserts occur (all
#'   are wrapped in a single transaction).
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RSQLite::SQLite(), "mydb.sqlite")
#' init_data <- list(
#'   tab1 = tibble(x = 1:3, y = letters[1:3]),
#'   tab2 = tibble(a = c(TRUE, FALSE))
#' )
#' DBinit_injectData(con, init_data)
#' DBI::dbDisconnect(con)
#' }
#' @export
DBinit_injectData    =    function(  con     ,              init_data          ) {


  assertthat::assert_that(  DBI::dbIsValid(con)     ,  msg = "`con` must be a valid DB connection."    )


  if ( is.null( init_data ) ) {

    return( invisible( NULL ) )

  } else {

    DBI::dbWithTransaction( con, {  purrr::iwalk( init_data ,   ~   DBI::dbAppendTable(  con ,  .y , .x  )  )  })

    return( invisible( NULL ) )

  }




}
