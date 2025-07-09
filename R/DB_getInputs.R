#' Fetch and Claim a Batch of Pending URLs
#'
#' Atomically selects the next `n` pending URLs from the queue, marks them
#' as `in_progress`, and returns their values. Uses a single CTE + UPDATE +
#' RETURNING statement to avoid race conditions in parallel sessions.
#'
#' @param db_con A valid `DBIConnection` (SQLite ≥ 3.35 for RETURNING support).
#' @param get_N Integer(1). Number of URLs to fetch and claim.
#' @param tab_queue Character(1). Name of the queue table (default: "tab_queue").
#'
#' @return A character vector of claimed URLs. If none are pending, returns
#'   a zero-length character vector.
#' @examples
#' con <- DB_load("mydb.sqlite")
#' urls <- DB_getInputs(con, get_N = 20, tab_queue = "tab_queue")
#' @export
DB_getInputs         =   function(   db_con                     ,
                                     get_N       =  10          ,
                                     tab_queue   = 'tab_queue'   ) {


  assert_that(  DBI::dbIsValid( db_con    ),
                is.count( get_N     ),
                is.character( tab_queue ),
                length( tab_queue ) == 1,   msg = "Invalid arguments: ensure `db_con` is valid, `get_N` is a positive integer, and `tab_queue` is a string."  )


  metadata  =  DB_getMetadata( db_con )

  col_in      <- metadata $ col_in
  col_pending <- metadata $ col_crawpending
  col_order   <- metadata $ col_order

  sql <- glue_sql("
    WITH batch AS (
      SELECT id
        FROM {`tab_queue`}
       WHERE {`col_pending`} = 'pending'
    ORDER BY {`col_order`}
       LIMIT {n}
    )
    UPDATE {`tab_queue`}
       SET {`col_pending`} = 'in_progress'
     WHERE id IN (SELECT id FROM batch)
    RETURNING {`col_in`};
  ",
                  tab_queue   = tab_queue,
                  col_pending = col_pending,
                  col_order   = col_order,
                  n           = get_N,
                  col_in      = col_in,
                  .con        = db_con
  )

  dbGetQuery( db_con ,  sql )  [[ col_in ]]

  # dbWithTransaction( db_con , {
  #
  #   sel_sql <- glue_sql(
  #     "SELECT id, {`col_in`}
  #        FROM {`table`}
  #       WHERE {`col_pending`} = 'pending'
  #    ORDER BY {`col_order`}
  #       LIMIT {n}",
  #
  #     table       =            tab_queue         ,
  #     col_order   = metadata $ col_order       ,
  #     col_in      = metadata $ col_in          ,
  #     col_pending = metadata $ col_crawpending ,
  #     n           = get_N                      ,
  #     .con        = db_con
  #   )
  #
  #   rows <- dbGetQuery( db_con , sel_sql )
  #
  #   if ( nrow( rows ) == 0  )  return( character() )
  #
  #   upd_sql <- glue_sql(
  #
  #     "UPDATE {`table`}
  #         SET {`col_pending`} = 'in_progress'
  #       WHERE id IN ({ids*})",
  #
  #     table        =             tab_queue         ,
  #     col_pending  =  metadata $ col_crawpending ,
  #     ids          =      rows $ id              ,
  #     .con         =             db_con
  #   )
  #   dbExecute( db_con , upd_sql )
  #
  #   # 3) return the requested column
  #   rows [[ metadata $ col_in ]]
  #
  # })

}
