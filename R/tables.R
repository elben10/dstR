#' @export
dst_tables <- function(subjects = NULL, lang = NULL, date_formatted = TRUE) {
  lang <- lang_helper(lang = lang)
  subjects <- subjects_helper(subjects = subjects)

  base_url <- "http://api.statbank.dk"
  version <- "V1"
  type <- "tables"
  query <- list(lang = lang,
                subjects = subjects,
                format = "JSON")

  resp <- GET(base_url, path = c(version, type, subjects), query = query)
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

  if (date_formatted) {
    tibble::tibble(id = parsed[[1]],
                   text = parsed[[2]],
                   unit = parsed[[3]],
                   updated = lubridate::parse_date_time(parsed[[4]], "YmdHMS"),
                   first_start = date_helper(parsed[[5]]),
                   first_end = date_helper(parsed[[5]], FALSE),
                   last_start = date_helper(parsed[[6]]),
                   last_end = date_helper(parsed[[6]], FALSE))
  } else {
    tibble::tibble(id = parsed[[1]],
                   text = parsed[[2]],
                   unit = parsed[[3]],
                   updated = lubridate::parse_date_time(parsed[[4]], "YmdHMS"),
                   first = parsed[[5]],
                   last = parsed[[5]])
  }
}
