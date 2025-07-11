#’ Parse a single JSON string out of a larger character blob
#’
#’ Scans for the first `{` or `[` then finds its matching closing brace,
#’ extracts that balanced substring, and parses it with `jsonlite::fromJSON()`.
#’
#’ @param raw_str A length-1 character string containing at least one JSON
#’   object or array.
#’ @return The R object resulting from parsing the first balanced JSON
#’   substring (a list, vector, or data.frame).
#’ @examples
#’ txt <- "foo … {\"x\":[1,2,3]} … bar"
#’ json_parse(txt)
#’ @importFrom jsonlite fromJSON
#’ @export
json_parse <- function(raw_str) {
  # 1) find opening
  start <- stringr::str_locate(raw_str, "[\\{\\[]")[1]
  if (is.na(start)) stop("No JSON opening brace found in input.")
  # 2) walk to matching close
  chars <- strsplit(raw_str, "")[[1]]
  depth <- 0L; end <- NA_integer_
  for (i in seq.int(start, length(chars))) {
    c <- chars[i]
    if (c %in% c("{","[")) depth <- depth + 1L
    if (c %in% c("}","]")) depth <- depth - 1L
    if (depth == 0L) { end <- i; break }
  }
  if (is.na(end)) stop("Matching closing brace not found.")
  # 3) extract & parse
  json_text <- paste0(chars[start:end], collapse = "")
  jsonlite::fromJSON(json_text)
}
