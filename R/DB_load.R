#' Open and Configure an Existing SQLite Database Connection
#'
#' Opens a connection to an existing SQLite file and applies a set of PRAGMA
#' settings optimized for concurrent reads/writes and performance.
#'
#' @param db_path Character scalar. Path to the existing SQLite database file.
#' @param pragmas Named character or numeric vector of PRAGMA settings to apply.
#'   Defaults are tuned for web-scraping concurrency (WAL mode, normal sync,
#'   memory temp storage, etc.).
#' @return A `DBIConnection` object with the specified PRAGMAs applied. The
#'   caller is responsible for closing the connection via `DBI::dbDisconnect()`.
#' @examples
#'
#' # Open and configure:
#' con <- DB_load("mydata.sqlite")
#' # ... use con ...
#' DBI::dbDisconnect(con)
#' @export
DB_load              =   function(   db_path   ,
                                     pragmas = c(
                                       busy_timeout = 30000,     # wait up to 30s on locks
                                       journal_mode = "WAL",    # allow concurrent readers/writers
                                       synchronous  = "NORMAL", # fast commits in WAL mode
                                       temp_store    = "MEMORY",# use memory for temp tables/indexes
                                       foreign_keys  = "ON",    # enforce FK constraints
                                       page_size     = 16384,     # 16 KB pages
                                       cache_size    = -2000      # approx 32MB cache (2000 pages)
                                     )
                                  ) {

  assert_that(
    is.character(db_path), length(db_path) == 1,
    file.exists (db_path),
    is.character(pragmas) || is.numeric(pragmas),
    !is.null(names(pragmas)),
    msg = "`db_path` must be a single existing file and `pragmas` a named vector."
  )

  con <- DBI::dbConnect( SQLite(),  db_path )

  DBinit_initPRAGMA ( db_con  =  con    ,
                      pragmas =  pragmas   )

  con

}
