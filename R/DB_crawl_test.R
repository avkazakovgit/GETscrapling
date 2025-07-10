#’ Dry-run N URLs through your crawler without touching the queue
#’
#’ @param db_con     A `DBIConnection` to your SQLite file.
#’ @param sample_n   How many pending URLs to pull for this test.
#’ @param get_func     Your GET scraper (e.g. `craw_GETscrapling`).
#’ @param status_func  Your status‐extractor (e.g. `craw_crawStatus`).
#’ @param success_func Your success‐flag extractor (e.g. `craw_crawSuccess`).
#’ @param proxy_args   Named list of extra proxy args, if any.
#’ @param ...          Further args passed to `get_func`.
#’ @return A list with
#’   * `urls` the sampled URLs,
#’   * `results` a tibble exactly like your `crawl_crawlPendingInputs()` output,
#’   * `summary` a 2×1 tibble of success/failure counts.
#’ @export
DB_crawl_test        =    function(  db_con                            ,
                                     batch_size   =  20                ,
                                     sleep        =  5                 ,

                                     tab_queue     =   'tab_queue'     ,
                                     tab_crawl     =   'tab_crawl'     ,

                                     get_func  =  GETscrapling      ,
                                     success_func  =  craw_crawSuccess  ,
                                     status_func  =  craw_crawStatus   ,
                                     ...
                          ) {
  library(dplyr)
  library(glue)

  # pull just the URLs (no UPDATE)
  metadata     =   DB_getMetadata(  db_con )
  col_in       =         metadata $ col_in
  col_pending  =         metadata $ col_crawpending
  col_order    =         metadata $ col_order

  sel_sql <- glue_sql(

    "SELECT {`col_in`} FROM {`tbl`}
       WHERE {`col_pending`} = 'pending'
    ORDER BY {`col_order`}
       LIMIT {n}",

    tbl         = tab_queue   ,
    col_in      = col_in      ,
    col_pending = col_pending ,
    col_order   = col_order   ,
    n           = sample_n    ,
    .con        = db_con
  )

  urls <- dbGetQuery(  db_con  ,  sel_sql  ) [[ col_in ]]

  if (length(urls) == 0) {
    message("No pending URLs found.")
    return(invisible(NULL))
  }

  # build the same tibble your crawler would
  batch_proxies <- PROXY_Rotator_maker(proxy_args$proxies %||% character(0),
                                       n_pools = 1, interval_secs = 0)()

  batch_results <- get_func(
    urls    = urls,
    proxies = batch_proxies,
    ...
  )

  # mimic crawl_crawlPendingInputs()’s unwrapping into a tibble
  cr <- batch_results %>%
    keep(~ is.list(.x)) %>%
    flatten() %>%
    keep(~ "body" %in% names(.x))

  crawled_inputs <- batch_results %>%
    keep(is.list) %>%
    flatten() %>%
    map_lgl(~ "body" %in% names(.x)) %>%
    { urls[.] }

  result_df <- tibble::tibble(
    !!col_in         := crawled_inputs,
    !!md$col_out     := map_chr(cr,  ~ .x$body),
    !!md$col_respstatus := map_int(cr,  status_func),
    !!md$col_crawsucces := map_int(cr, success_func),
    crawled_at        = Sys.time()
  )

  # summary of success/fail
  summary_tbl <- result_df %>%
    count(!!sym(md$col_crawsucces), name = "count") %>%
    rename(crawl_success = !!sym(md$col_crawsucces))

  list(
    urls    = urls,
    results = result_df,
    summary = summary_tbl
  )
}
