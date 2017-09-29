#' @export
dst_subject <- function(subject = NULL, lang = NULL) {
  lang <- lang_helper(lang = lang)

  base_url <- "http://api.statbank.dk"
  version <- "V1"
  type <- "subjects"
  query <- list(lang = lang,
                format = "JSON")

  resp <- GET(base_url, path = c(version, type, subject), query = query)

  parsed <- fromJSON(content(resp, "text"))

  if (http_error(resp)) {
    rlang::abort(
      sprintf(
        "DST API request failed [%s]\n%s",
        status_code(resp),
        parsed$message
      )
    )
  }

  if (is.null(subject)) {
    as_tibble(parsed[c(1,2, 4)])
  } else {
    as_tibble(parsed$subjects[[1]][c(1,2, 4)])
  }
}
