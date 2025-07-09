#' Initialize Database Indexes based on Definitions
#'
#' Creates all specified indexes in the database, each inside a single transaction.
#'
#' @param db_con A valid `DBIConnection` to a SQLite (or other DBI) database.
#' @param ind_defs Named list: names are table names, values are named lists of
#'   index definitions. Each index definition maps an index name to a character
#'   vector of column names. Example:
#'   ```r
#'   list(
#'     tab_queue = list(
#'       idx_pending_order = c("crawl_pending", "orderid"),
#'       idx_url           = c("URL")
#'     ),
#'     tab_crawl = list(
#'       idx_url_success   = c("URL", "crawl_success")
#'     )
#'   )
#'   ```
#'
#' @return Invisibly returns `NULL`. On error, no indexes are created.
#' @export
DB_initIndex         =   function(   db_con   ,
                                     ind_defs               ) {

  dbWithTransaction(  db_con, {

    imap(  ind_defs ,   function( index_list,  table_name ) {

      imap(  index_list  , function(  cols ,   index_name  ) {

        # build a CREATE INDEX IF NOT EXISTS ... ON table(col1, col2, ...)

        sql <- glue_sql(

          "CREATE INDEX IF NOT EXISTS {`index_name`}
             ON {`table_name`} ({`cols`*});",

          index_name  = index_name,
          table_name  = table_name,
          cols        = cols,
          .con        = db_con
        )

        dbExecute(db_con, sql)

      })
    })
  })

}
