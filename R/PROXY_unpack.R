#' Unpack Proxy Strings into Platform‐Specific Formats
#'
#' Parses a proxy definition string according to a user‐defined template and
#' returns either an `httr::use_proxy()` object for R or a list suitable for
#' Python/Scrapling. Supports arbitrary order and delimiters via templates.
#'
#' @param proxy_full Character(1). The raw proxy string (e.g. "user:pass@host:port").
#' @param template Character(1). A template specifying the format using
#'   placeholders in curly braces, e.g. "{user}:{pass}@{host}:{port}".
#' @param platform Character(1). Either "R" (returns an `httr::use_proxy`)
#'   or "scrapling" (returns a `list(server, username, password)`).
#'
#' @return For platform = "R", an `httr::use_proxy()` object; for platform =
#'   "scrapling", a named list with components `server`, `username`, `password`.
#' @examples
#' PROXY_unpack("alice:pw@10.0.0.1:8080",
#'               template = "{user}:{pass}@{host}:{port}",
#'               platform = "R")
#' PROXY_unpack("10.0.0.1:8080:alice:pw",
#'                template = "{host}:{port}:{user}:{pass}",
#'               platform = "scrapling")
#' @export
PROXY_unpack               =   function(  proxy_full          ,
                                          template            =  '{host}:{port}:{user}:{pass}' ,
                                          platform            =  c('R', 'scrapling')             ) {


  assertthat::assert_that(
                               is.string( proxy_full ),
                               is.string( template   ),
                            is.character( platform   ),
                                  length( platform   ) == 1  ,  msg = "`proxy_full` and `template` must be single strings; `platform` a single choice."  )


  parse_with_template <- function( proxy_full, template ) {


    fields  <- stringr::str_match_all  ( template ,          "\\{([^}]+)\\}"             ) [[ 1 ]] [ , 2 ]         # Extract placeholder names
    esc     <- stringr::str_replace_all( template , "([.\\+*?^$()\\[\\]\\|])", "\\\\\\1" )                         # Escape regex metachars
    pattern <- esc                                                                                                 # Replace each '{field}' with '(.+?)'
    for (  fld  in  fields  ) {
      pattern <- sub(paste0("\\{", fld, "\\}"), "(.+?)", pattern, perl = TRUE)
    }
    pattern <- paste0("^", pattern, "$")


    m <- stringr::str_match(  proxy_full  ,  pattern  )                                                             # Match and extract captures
    if (      is.na( m [ 1 ,  1 ] ) ) {
      stop("Proxy string does not match template: ",   template  )
    }
    vals <- as.list( m [ 1 , -1 ] )
    names(vals) <- fields
    vals

  }                       # Helper: parse full string by template


  parts  =  parse_with_template(  proxy_full =   proxy_full ,
                                  template   =   template      )

  platform  =   match.arg( platform )
  switch( platform ,
          R = {

            parts $ port   =  as.integer( parts $ port )                        # Validate numeric port
            if ( is.na( parts $ port ) ) stop("Port must be an integer")

            use_proxy(
              url      = parts $ ip   %||%  parts $ host ,
              port     = parts $ port ,
              username = parts $ user ,
              password = parts $ pass
            )

          },
          scrapling = {

            hostport  =   str_c(  parts $ ip     %||%
                                    parts $ host , ':'  ,
                                  parts $ port             )
            if ( ! grepl( '^https?://', hostport ) ){

              hostport  =  'http://'  %>%  paste0( hostport )

            }
            list(
              server   = hostport     ,
              username = parts $ user ,
              password = parts $ pass
            )

          }
  )


}
