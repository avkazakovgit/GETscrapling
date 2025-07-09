#' Process a Batch of Pending URLs: Scraping and Storage
#'
#' Fetches a batch of in-progress URLs and scrapes each one using the provided
#' `get_func`. Evaluates success and status with `success_func` and `status_func`,
#' then atomically appends the results to the crawl table.
#'
#' @param db_con A valid `DBIConnection` to the SQLite database.
#' @param batch_inputs Character vector of URLs to crawl (already marked in_progress).
#' @param tab_crawl Character(1). Name of the crawl results table (default: "tab_crawl").
#' @param get_func Function(URLs, proxies, ...) returning a list of responses.
#' @param success_func Function(response) → logical indicating success.
#' @param status_func Function(response) → integer status code.
#' @param proxies_fn Function() → vector/list of proxies for `get_func`.
#' @param ... Additional args passed to `get_func`.
#'
#' @return A tibble with columns:
#'   - URL (input URL),
#'   - HTML_TEXT (body),
#'   - resp_status,
#'   - crawl_success,
#'   - crawled_at (timestamp).
#' @export
DB_crawlPendingInputs  =   function(    db_con                             ,
                                        batch_inputs                       ,
                                        tab_queue     =  'tab_queue'       ,
                                        tab_crawl     =  'tab_crawl'       ,

                                            get_func  =  GETscrapling      ,
                                        success_func  =  craw_crawSuccess  ,
                                         status_func  =  craw_crawStatus   ,
                                        ...                                       ) {



  db_metadata       =   DB_getMetadata(  db_con  )
  col_in            =   db_metadata $ col_in
  col_crawpending   =   db_metadata $ col_crawpending
  col_out           =   db_metadata $ col_out
  col_respstatus    =   db_metadata $ col_respstatus  # resp_status
  col_crawsucces    =   db_metadata $ col_crawsucces

  suppressMessages({
    suppressWarnings({

      batch_proxies  =    PROXY_Rotator()
      batch_results  =    get_func   (  batch_inputs  ,
                                        batch_proxies ,
                                        ...             )

      crawled_results        =    batch_results  %>%   keep( ~ is_list(.x) ) %>%  squash   %>%  keep    (  ~  'body'  %in%  names( .x )  )
      crawled_inputs         =    batch_results  %>%   keep( ~ is_list(.x) ) %>%  squash   %>%  map_lgl (  ~  'body'  %in%  names( .x )  )  %>%   {  batch_inputs [ . ]  }
      crawled_df   =  tibble(
        !! col_in             :=  crawled_inputs                                                     ,
        !! col_out            :=  crawled_results  %>%  map ( ~              .x $ body ) %>%  unlist       ,
        !! col_respstatus     :=  crawled_results  %>%  map ( ~  status_func(.x)       ) %>%  unlist       ,
        !! col_crawsucces     :=  crawled_results  %>%  map ( ~ success_func(.x)       ) %>%  as.integer   ,
        crawled_at             =  Sys.time()
      )

      DB_appendTibble(  db_con    =  db_con    ,
                        db_table  =  tab_crawl ,
                        df_append =  crawled_df   )


      if( ! is.null( batch_results $ success_share ) ) {

        cat('\n', '\t', 'Success Share : ' ,  batch_results $ success_share , ' %' )

      }
      if( ! is.null( batch_results $ time_per_page ) ) {

        cat('\t', '\t', 'Secs per URL : '  ,  batch_results $ time_per_page        )

      }


    })
  })



}
