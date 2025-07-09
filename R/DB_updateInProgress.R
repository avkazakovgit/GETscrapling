#' Update All In-Progress URLs Based on Crawl Results
#'
#' Re-evaluate every URL currently marked "in_progress" in the queue table and
#' update its status according to the crawl outcomes. Specifically:
#' 1. URLs never crawled → reset to "pending".
#' 2. URLs with any successful crawl → marked "done".
#' 3. URLs with all failures and retry_count > max_retries → marked "failed".
#' 4. Otherwise → stay or revert to "pending".
#'
#' @param db_con A valid `DBIConnection`.
#' @param max_retries Integer(1). Maximum retry count before marking "failed".
#' @param tab_queue Character(1). Name of the queue table (default: "tab_queue").
#' @param tab_crawl Character(1). Name of the crawl results table (default: "tab_crawl").
#'
#' @return Invisibly returns `TRUE`. Throws an error for invalid inputs.
#' @examples
#' con <- DB_load("mydb.sqlite")
#' DB_updateInProgress(con, max_retries = 3)
#' @export
DB_updateInProgress  =   function(   db_con                    ,
                                     max_retries =  3          ,
                                     tab_queue   = 'tab_queue' ,
                                     tab_crawl   = 'tab_crawl'    ) {


  assert_that(
    DBI::dbIsValid(db_con),
    is.count(max_retries),
    is.string(tab_queue),
    is.string(tab_crawl),
    msg = "Invalid arguments to DB_updateInProgress()"
  )

  metadata  =  DB_getMetadata(  db_con  )

  dbWithTransaction( db_con, {

    sql <- glue_sql(
      "
    UPDATE {`queue_table`}
       SET {`col_pending`} = CASE
         -- 1) never scraped → back to pending
         WHEN NOT EXISTS (
           SELECT 1
             FROM {`crawl_table`}
            WHERE {`crawl_table`}.{`col_in`} = {`queue_table`}.{`col_in`}
         ) THEN 'pending'

         -- 2) any success → done
         WHEN EXISTS (
           SELECT 1
             FROM {`crawl_table`}
            WHERE {`crawl_table`}.{`col_in`} = {`queue_table`}.{`col_in`}
              AND {`col_success`} = 1
         ) THEN 'done'

         -- 3) all attempts failed & retries exceeded → failed
         WHEN (
           SELECT MAX({`col_success`})
             FROM {`crawl_table`}
            WHERE {`crawl_table`}.{`col_in`} = {`queue_table`}.{`col_in`}
         ) = 0
         AND {`queue_table`}.{`col_retry`} > {max_retries} THEN 'failed'

         -- 4) otherwise (all scraped but not yet maxed out) → pending
         ELSE 'pending'
       END
     WHERE {`queue_table`}.{`col_pending`} = 'in_progress';
    ",

      queue_table =              tab_queue       ,
      crawl_table =              tab_crawl       ,
      col_in      =   metadata $ col_in          ,
      col_pending =   metadata $ col_crawpending ,
      col_retry   =   metadata $ col_retry       ,
      col_success =   metadata $ col_crawsucces  ,
      max_retries =              max_retries     ,
      .con        =               db_con
    )

    dbExecute(  db_con  ,  sql  )

  })

  invisible(TRUE)

}
