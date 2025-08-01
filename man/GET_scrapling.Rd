% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GET_scrapling.R
\name{GET_scrapling}
\alias{GET_scrapling}
\title{Fetch Multiple URLs Concurrently via Scrapling's StealthyFetcher}
\usage{
GET_scrapling(
  urls,
  proxies = NULL,
  proxy_template = "{host}:{port}:{user}:{pass}",
  geoip = TRUE,
  headless = TRUE,
  block_webrtc = TRUE,
  allow_webgl = TRUE,
  os_randomize = TRUE,
  humanize = 0,
  disable_resources = TRUE,
  block_images = TRUE,
  network_idle = FALSE,
  timeout = 40000,
  wait_selector = NULL,
  wait_selector_state = "visible",
  max_concurrent = 20,
  success_func = NULL
)
}
\arguments{
\item{urls}{Character vector of target URLs. Must be length ≥ 1.}

\item{proxies}{Optional character vector of proxy strings matching \code{urls} length,
or \code{NULL} for no proxy. Strings are parsed via \code{PROXY_unpack()} and passed
to Scrapling as Python dicts.}

\item{proxy_template}{Template (e.g. \code{"{host}:{port}:{user}:{pass}"}) for unpacking
each proxy string into \code{server}, \code{username}, \code{password} fields.}

\item{geoip, headless, block_webrtc, allow_webgl, os_randomize, disable_resources, }{block_images,network_idle Logical flags controlling StealthyFetcher behavior.}

\item{humanize}{Numeric. Artificial delay factor (seconds) between page actions.}

\item{timeout}{Integer timeout in milliseconds.}

\item{wait_selector}{Optional CSS selector string to wait for before returning.}

\item{wait_selector_state}{One of \code{"attached"},\code{"visible"},\code{"hidden"},\code{"detached"}.}

\item{max_concurrent}{Integer ≥ 1. Maximum simultaneous fetches in one batch.}
}
\value{
A list:
\itemize{
\item \code{pages}: a list of Python page/responses or exception objects,
\item \code{time_per_page}: average seconds taken per URL,
\item \code{error}: top-level R error if invocation failed, else \code{NULL}.
}
}
\description{
High-performance wrapper that uses Python's asyncio + uvloop + Scrapling
to scrape many pages concurrently. Supports optional per-URL proxies,
configurable browser/HTTP options, and concurrency throttling via a semaphore.
}
\examples{

# Without proxies:
res <- GET_scrapling(c("https://httpbin.org/ip","https://httpbin.org/ip"))

# With rotating proxies:
urls    <- c("https://site1.com","https://site2.com")
proxies <- c("user:pass@1.2.3.4:8080","user:pass@5.6.7.8:8080")
res <- GET_scrapling(urls, proxies)
}
