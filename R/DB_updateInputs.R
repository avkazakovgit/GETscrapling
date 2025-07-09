#' Update Queue Statuses Based on Crawl Results
#'
#' For a given batch of input URLs, atomically reconcile their statuses in the
#' queue table against the crawl table. URLs are moved to:
#'  - "pending" if never crawled,
#'  - "done" if any crawl_success = 1,
#'  - "failed" if all attempts failed and retry_count > max_retries,
#'  - otherwise remain or revert to "pending".
#'
#' @param db_con A valid `DBIConnection`.
#' @param batch_inputs Character vector of URLs to update.
#' @param max_retries Integer(1). Maximum retry count before marking "failed".
#' @param tab_queue Character(1). Name of the queue table (default: "tab_queue").
#' @param tab_crawl Character(1). Name of the crawl results table (default: "tab_crawl").
#'
#' @return Invisibly returns TRUE. Throws an error if inputs are invalid.
#' @examples
#' con <- DB_load("mydb.sqlite")
#' urls <- c("https://...1", "https://...2")
#' DB_updateInputs(con, urls, max_retries = 3)
#' @export
DB_updateInputs      =   function(   db_con                    ,
                                     batch_inputs              ,
                                     max_retries =  3          ,
                                     tab_queue   = 'tab_queue' ,
                                     tab_crawl   = 'tab_crawl'    ) {


  # con  %>%  tbl( 'tab_queue' )  %>%  filter( crawl_pending == 'pending'     )  #  'https://xfirm.ru/company/6670073301/founders/'
  # con  %>%  tbl( 'tab_queue' )  %>%  filter( crawl_pending == 'in_progress' )  #  'https://xfirm.ru/company/0268069694/founders/'
  # con  %>%  tbl( 'tab_queue' )  %>%  filter( crawl_pending == 'done'        )  #  'https://xfirm.ru/company/0100002932/founders/'
  #
  # batch_inputs  =  c( 'https://xfirm.ru/company/6670073301/founders/', 'https://xfirm.ru/company/0268069694/founders/', 'https://xfirm.ru/company/0100002932/founders/' )

  assert_that(
    DBI::dbIsValid(db_con),
    is.character(batch_inputs), length(batch_inputs) >= 0,
    is.count(max_retries),
    is.string(tab_queue),
    is.string(tab_crawl),
    msg = "Invalid arguments to DB_updateInputs()"
  )

  # If no URLs to update, nothing to do
  if (length(batch_inputs) == 0) {
    return(invisible(TRUE))
  }

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
       WHERE {`queue_table`}.{`col_in`} IN ({batch*});
      ",
      queue_table =   tab_queue                  ,
      crawl_table =   tab_crawl                  ,
      col_in      =   metadata $ col_in          ,
      col_pending =   metadata $ col_crawpending ,
      col_retry   =   metadata $ col_retry       ,
      col_success =   metadata $ col_crawsucces  ,
      max_retries =   max_retries                ,
      batch       =   batch_inputs               ,
      .con        =   db_con
    )
    dbExecute( db_con ,  sql )

  })

}
