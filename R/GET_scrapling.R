#' Fetch Multiple URLs Concurrently via Scrapling's StealthyFetcher
#'
#' High-performance wrapper that uses Python's asyncio + uvloop + Scrapling
#' to scrape many pages concurrently. Supports optional per-URL proxies,
#' configurable browser/HTTP options, and concurrency throttling via a semaphore.
#'
#' @param urls Character vector of target URLs. Must be length ≥ 1.
#' @param proxies Optional character vector of proxy strings matching `urls` length,
#'   or `NULL` for no proxy. Strings are parsed via `PROXY_unpack()` and passed
#'   to Scrapling as Python dicts.
#' @param proxy_template Template (e.g. `"{host}:{port}:{user}:{pass}"`) for unpacking
#'   each proxy string into `server`, `username`, `password` fields.
#' @param geoip,headless,block_webrtc,allow_webgl,os_randomize,disable_resources,
#'   block_images,network_idle Logical flags controlling StealthyFetcher behavior.
#' @param humanize Numeric. Artificial delay factor (seconds) between page actions.
#' @param timeout Integer timeout in milliseconds.
#' @param wait_selector Optional CSS selector string to wait for before returning.
#' @param wait_selector_state One of `"attached"`,`"visible"`,`"hidden"`,`"detached"`.
#' @param max_concurrent Integer ≥ 1. Maximum simultaneous fetches in one batch.
#' @return A list:
#'   - `pages`: a list of Python page/responses or exception objects,
#'   - `time_per_page`: average seconds taken per URL,
#'   - `error`: top-level R error if invocation failed, else `NULL`.
#' @examples
#'
#' # Without proxies:
#' res <- GET_scrapling(c("https://httpbin.org/ip","https://httpbin.org/ip"))
#'
#' # With rotating proxies:
#' urls    <- c("https://site1.com","https://site2.com")
#' proxies <- c("user:pass@1.2.3.4:8080","user:pass@5.6.7.8:8080")
#' res <- GET_scrapling(urls, proxies)
#' @export
GET_scrapling <- function(
    urls,
    proxies             = NULL,
    proxy_template      = "{host}:{port}:{user}:{pass}",
    geoip               = TRUE,
    headless            = TRUE,
    block_webrtc        = TRUE,
    allow_webgl         = TRUE,
    os_randomize        = TRUE,
    humanize            = 0   ,
    disable_resources   = TRUE,
    block_images        = TRUE,
    network_idle        = FALSE,
    timeout             = 40000,
    wait_selector       = NULL,
    wait_selector_state = "visible",
    max_concurrent      = 20 ,
    success_func        = NULL
) {
  # --- Validate arguments ---
  assertthat::assert_that(
    is.character(urls) && length(urls) >= 1,
    is.null(proxies) || (is.character(proxies) && length(proxies) == length(urls)),
    assertthat::is.string(proxy_template),
    is.flag(geoip), is.flag(headless), is.flag(block_webrtc),
    is.flag(allow_webgl), is.flag(os_randomize),
    is.numeric(humanize) && humanize >= 0,
    is.flag(disable_resources), is.flag(block_images), is.flag(network_idle),
    assertthat::is.count(timeout),
    is.null(wait_selector) || assertthat::is.string(wait_selector),
    wait_selector_state %in% c("attached","visible","hidden","detached"),
    assertthat::is.count(max_concurrent)
  )

  # --- Prepare Python argument block ---
  py_bool <- function(x) if (isTRUE(x)) "True" else "False"
  proxy_arg <- if (is.null(proxies)) "" else "proxy = p,"

  args <- list(
    timeout             = timeout,
    humanize            = humanize,
    wait_selector       = if (is.null(wait_selector)) "None" else sprintf("'%s'", wait_selector),
    wait_selector_state = sprintf("'%s'", wait_selector_state),
    geoip               = py_bool(geoip),
    headless            = py_bool(headless),
    block_webrtc        = py_bool(block_webrtc),
    allow_webgl         = py_bool(allow_webgl),
    os_randomize        = py_bool(os_randomize),
    disable_resources   = py_bool(disable_resources),
    block_images        = py_bool(block_images),
    network_idle        = py_bool(network_idle)
  )
  max_name    <- max(nchar(names(args)))
  param_lines <- purrr::imap_chr(args, function(val, name) {
    glue::glue("{stringr::str_pad(name, max_name)} = {val}")
  })
  param_block <- paste(param_lines, collapse = ",\n                ")

  # --- Inject Python code ---
  py_run <- glue::glue(
    "import asyncio, uvloop
asyncio.set_event_loop_policy(uvloop.EventLoopPolicy())
from scrapling.fetchers import StealthyFetcher

async def fetch_all(urls, proxies):
    sem = asyncio.Semaphore({max_concurrent})
    async def bound(url, p):
        async with sem:
            return await StealthyFetcher.async_fetch(
                url,
                {proxy_arg}
                {param_block}
            )
    tasks = [bound(u, p) for u, p in zip(urls, proxies)]
    return await asyncio.gather(*tasks, return_exceptions=True)


def fetch_parallel(urls, proxies):
    return asyncio.run(fetch_all(urls, proxies))"
  )
  reticulate::py_run_string(py_run)

  # --- Marshal inputs to Python ---
  py_proxies <- reticulate::r_to_py(
    if (is.null(proxies)) rep(list(NULL), length(urls))
    else purrr::map(proxies, ~ PROXY_unpack(.x,
                                            platform = "scrapling",
                                            template = proxy_template))
  )
  py_urls <- as.list(urls)

  # --- Call Python and time execution ---
  ss <- Sys.time()

  py_res <- tryCatch(
                        reticulate::py $ fetch_parallel(  py_urls   ,
                                                          py_proxies  ),
    error = function(e) e
  )


  if( ! is.null( success_func ) ) {

    assertthat::assert_that(  is_function( success_func ) )

    success        =  py_res  %>%  map_lgl( success_func )
    success_share  =  sum( success )  /  length( urls ) * 100

    runtime        =  as.numeric( difftime( Sys.time(), ss , units = "secs") )  /     sum( success  )

  } else {

    success        =  NULL
    success_share  =  NULL

    runtime        =  as.numeric( difftime( Sys.time(), ss , units = "secs") )  /  length( urls )

  }




  # --- Return results ---
  list(
    pages         = py_res  ,
    success       = success ,
    success_share = success_share ,
    time_per_page = runtime ,
    error         = if (inherits(py_res, "error")) py_res else NULL
  )


}
