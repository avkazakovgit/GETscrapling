#' Initialize a new SQLite database file with tables, metadata, and optional data
#'
#' Creates a fresh SQLite database at the specified path, applying PRAGMA settings,
#' initializing table schemas, populating the metadata table, and injecting any
#' provided initial data. Optionally replaces an existing file.
#'
#' @param db_path Character scalar; path to the SQLite database file.
#' @param tab_defs Named list of table definitions for DBinit_initTables().
#' @param init_data Optional named list of tibbles for DBinit_injectData().
#' @param replace Logical scalar; if TRUE, deletes any existing file before init.
#' @return Invisibly TRUE on success, FALSE if file existed and replace=FALSE.
#' @export
DB_init              =   function(   db_path,
                                     tab_defs,
                                     init_data  =  NULL  ,
                                     replace    =  FALSE    ) {



  DBinit_validate      =    function(  db_path ,  tab_defs  , init_data, replace ) {


    is_named_list  =  function( list ) {

      list_names  =  names( list )

      is.list( list       ) &&
        ! is.null( list_names ) &&
        all( nzchar( list_names ) )


    }

    tab_names  =   names(  tab_defs  )

    assertthat::assert_that(  assertthat::is.string(  db_path  )     ,  msg = "`db_path` must be a single non-NA string."                )
    assertthat::assert_that(  assertthat::is.flag  (  replace  )     ,  msg = "`replace` must be TRUE or FALSE (no NA)."                 )
    assertthat::assert_that(        is_named_list  (  tab_defs )     ,  msg = "`tab_defs` must be a named list (each name is a table)."  )

    # Each table definition
    purrr::imap( tab_defs , ~ {

      assertthat::assert_that(             is_named_list  (  .x )               ,  msg = "`tab_defs` must be a named list"  )

    })

    # Each column definitions
    purrr::imap( tab_defs , ~ {

      purrr::imap( .x , ~  {

        assertthat::assert_that(   any( str_detect( names( .x ) ,  'init' ) ) ,  msg = "Colum definitions must include element named 'init'"  )

      })
    })



    # init_data (if provided): named list, names ⊆ tab_defs, all data.frames
    if (  ! is.null( init_data )  ) {

      assertthat::assert_that(        is_named_list  (  tab_defs  ) ,
                                      all(      names(  init_data ) %in%
                                                  names(  tab_defs  ) )      ,  msg = "`init_data` must be a named list with names matching `tab_defs`."  )
      assertthat::assert_that(        all(     vapply(  init_data  ,
                                                        is.data.frame,
                                                        logical(1) )   )   ,  msg = "All elements of `init_data` must be data frames."  )

    }

    invisible(TRUE)

  }



  DBinit_validate(  db_path, tab_defs, init_data, replace  )

  {
    if   (  file.exists( db_path ) ) {
      if ( !             replace   ) {
        warning("DB already exists at '", db_path, "'. Use replace = TRUE to overwrite." ) ;  return ( invisible( FALSE ) )
      } else {
        unlink ( db_path )
      }
    }
  }

  dbConnect ( SQLite(),  db_path )   ->   con   ;    on.exit( DBI::dbDisconnect( con ) , add = TRUE )

  DBinit_applyPRAGMA  (  con               )

  DBinit_initTables   (  con  ,  tab_defs  )
  DBinit_initMetadata (  con  ,  tab_defs  )
  DBinit_injectData   (  con  ,  init_data )



  invisible(TRUE)


}
