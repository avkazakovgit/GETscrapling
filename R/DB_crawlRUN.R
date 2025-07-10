DB_crawlRUN          =    function(  db_con                            ,
                                     batch_size   =  20                ,
                                     sleep        =  5                 ,

                                     tab_queue     =   'tab_queue'     ,
                                     tab_crawl     =   'tab_crawl'     ,

                                         get_func  =  GETscrapling      ,
                                     success_func  =  craw_crawSuccess  ,
                                      status_func  =  craw_crawStatus   ,
                                     ...
) {



  repeat {

    tryCatch({

      cat('\n', 'o---' )

      batch_inputs  =
      DB_getInputs            (    db_con      =    db_con     ,
                                     get_N     =    batch_size ,
                                     tab_queue =    tab_queue  ,
                                     update    =    TRUE       ,
                                     order_by  =    'default'    )
      if ( ! nzlen( batch_inputs ) ) {  message( 'No more pending URLs – exiting!' )
        break
      }


      DB_crawlPendingInputs   (    db_con                             ,
                                   batch_inputs                       ,

                                   get_func     =  craw_GETscrapling  ,
                                   success_func =  craw_crawSuccess   ,
                                   status_func  =  craw_crawStatus    ,
                                   ...                                  )  ;  cat('\n', '+++' )

      DB_updateInputs        (     db_con       =   db_con       ,
                                   batch_inputs =   batch_inputs ,
                                   max_retries  =   3            ,
                                   tab_queue    =   tab_queue    ,
                                   tab_crawl    =   tab_crawl        )


      cat( '\n', '    ---->O' )

    },
    error = function(e){

      message("  ⚠️  Caught error: ", conditionMessage(e))
      NULL

    })

  }

  return( invisible(TRUE) )

}
