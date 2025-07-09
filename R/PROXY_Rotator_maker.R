#' Create a Rotating Proxy Pool Selector
#'
#' Given a vector of proxy addresses, returns a function that, on each call,
#' yields one “pool” of proxies—cycling through sub‐pools at a fixed time
#' interval.  You can either split your proxies into \code{n_pools} equally‐sized
#' chunks, or request a \code{pool_size} (for single‐proxy providers).
#'
#' @param proxies Character vector of raw proxy strings (e.g. \"user:pass@host:port\").
#' @param n_pools Integer >= 1 or \code{NULL}. Number of sub‐pools to cycle through.
#'   Must be \emph{exactly one} of \code{n_pools} or \code{pool_size}.
#' @param pool_size Integer >= 1 or \code{NULL}. If you have a single proxy provider
#'   but need, say, 5 concurrent connections, set \code{pool_size = 5}. Must be
#'   exactly one of \code{n_pools} or \code{pool_size}.
#' @param interval_secs Numeric >= 0. Minimum seconds between rotations.
#'
#' @return A zero‐argument function. Each time you call it, you get back a
#'   character vector of proxies appropriate for that “tick.”  Internally it
#'   advances to the next pool only when \code{interval_secs} have elapsed.
#'
#' @examples
#' # Round‐robin across 3 sub‐pools:
#' rot <- make_proxy_rotator(proxies = paste0(\"p\", 1:12), n_pools = 3, interval_secs = 10)
#' rot()  # first 4 proxies
#' Sys.sleep(10); rot()  # next 4
#'
#' # Single peer provider but need 5 simultaneous connections:
#' rot2 <- make_proxy_rotator(proxies = \"single:provider:credentials\", pool_size = 5)
#' rot2()  # returns rep(\"single:provider:credentials\", 5)
#'
#' @export
PROXY_Rotator_maker =  function( proxies               ,
                                 n_pools        = NULL ,
                                 pool_size      = NULL ,
                                 interval_secs  =  0     ) {

  # ---- 1. Validate ----

  assert_that(   is.character( proxies ),
                       length( proxies ) >= 1          ,   msg = "`proxies` must be a non-empty character vector."  )
  assert_that( xor( ! is.null( n_pools   ) ,
                    ! is.null( pool_size )   )         ,   msg = "Specify exactly one of `n_pools` or `pool_size`." )
  assert_that(        is.null( n_pools   ) ||
                  (  is.count( n_pools   ) &&
                       length( proxies ) >=  n_pools  ), msg = "`n_pools` must be <= length(proxies)."              )

  # ---- 2. Build the list of proxy-pools ----
  if (!is.null(n_pools)) {

    pools   =  split(          proxies , cut( seq_along(proxies), breaks = n_pools, labels = FALSE) )    # split proxies into n_pools roughly equal chunks

  } else {

    if (length(proxies) == 1) {
      pools =   list(   rep(   proxies ,                                   pool_size              ) )
    } else {
      pools =  split(          proxies ,    ceiling( seq_along(proxies)  / pool_size              ) )
    }

  }


  # ---- 3. Rotation state ----
  last_rotate <- Sys.time()
  current_idx <- 1L
  n_pools_len <- length(pools)

  # ---- 4. The closure ----
  function() {

    now <- Sys.time()

    if (  as.numeric( difftime( now, last_rotate, units = "secs") ) >= interval_secs ) {

      current_idx <<- if (current_idx < n_pools_len) current_idx + 1L else 1L           # advance index, wrap around
      last_rotate <<- now

    }

    pools[[ current_idx ]]

  }

}
