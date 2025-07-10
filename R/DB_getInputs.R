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
                                     tab_queue   = 'tab_queue'  ,
                                     update      =  TRUE        ,
                                     order_by    = 'default'       ) {



  assertthat::assert_that(
    DBI::dbIsValid(db_con),
    assertthat::is.count(get_N),
    is.character(tab_queue), length(tab_queue)==1,
    is.logical(update),  length(update)==1,
    (is.null(order_by) ||
       (is.character(order_by) && length(order_by)==1)),
    msg = "Check: db_con valid, get_N positive integer, tab_queue string, update TRUE/FALSE, order_by NULL or single string."
  )

  metadata  =  DB_getMetadata( db_con )

  col_in      <- metadata $ col_in
  col_pending <- metadata $ col_crawpending
  col_order   <- metadata $ col_order

  # Build the ORDER BY clause
          if (   is.null( order_by )            ) {   order_sql   =   DBI::SQL("RANDOM()")
  } else  if ( identical( order_by , "default") ) {   order_sql   =   DBI::SQL( DBI::dbQuoteIdentifier( db_con , col_order ) )
  } else                                          {   order_sql   =   DBI::SQL( DBI::dbQuoteIdentifier( db_con , order_by  ) )
  }

  if (  update  ) {

    sql <- glue_sql("
      WITH batch AS (
        SELECT id
          FROM {`tab_queue`}
         WHERE {`col_pending`} = 'pending'
      ORDER BY {order_clause}
         LIMIT {n}
      )
      UPDATE {`tab_queue`}
         SET {`col_pending`} = 'in_progress'
       WHERE id IN (SELECT id FROM batch)
      RETURNING {`col_in`};
    ",
                    tab_queue   = tab_queue,
                    col_pending = col_pending,
                    order_clause= order_sql,
                    n           = get_N,
                    col_in      = col_in,
                    .con        = db_con
    )


  }  else {

    sql <- glue::glue_sql("
      SELECT {`col_in`}
        FROM {`tab_queue`}
       WHERE {`col_pending`} = 'pending'
    ORDER BY {order_clause}
       LIMIT {n};
    ",
                    tab_queue   = tab_queue,
                    col_pending = col_pending,
                    order_clause= order_sql,
                    n           = get_N,
                    col_in      = col_in,
                    .con        = db_con
    )


  }


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
