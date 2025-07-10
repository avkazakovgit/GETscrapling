#' Initialize SQLite PRAGMA Settings for Concurrency and Performance
#'
#' Applies a set of SQLite PRAGMA statements to tune the database connection
#' for parallel writes, efficient indexing, and overall performance.
#'
#' @param db_con  A valid `DBIConnection` to a SQLite database.
#' @param pragmas Optional named character vector of PRAGMA settings
#'   (e.g. `c(journal_mode = "WAL", synchronous = "NORMAL")`).
#'   Defaults are tuned for parallel scraping workloads.
#'
#' @return Invisibly returns `NULL`. The PRAGMA settings are applied immediately.
#' @export
DBinit_initPRAGMA <- function(  db_con,
                                pragmas = c(
                                  busy_timeout = 30000,     # wait up to 30s on locks
                                  journal_mode = "WAL",    # allow concurrent readers/writers
                                  synchronous  = "NORMAL", # fast commits in WAL mode
                                  temp_store    = "MEMORY",# use memory for temp tables/indexes
                                  foreign_keys  = "ON",    # enforce FK constraints
                                  page_size     = 16384,     # 16 KB pages
                                  cache_size    = -2000      # approx 32MB cache (2000 pages)
                                )) {


  assert_that( DBI::dbIsValid( db_con  ), is.character(pragmas), length(pragmas) > 0,  msg = "`pragmas` must be a named character vector of settings." )

  purrr::walk( pragmas, ~ {

    sql <- glue::glue("PRAGMA {.x};")

    DBI::dbExecute( con , sql )

  })

  invisible(NULL)

}
