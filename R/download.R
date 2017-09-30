#' @export
dst_download <- function(tableID, variableID = NULL, ..., ascending = TRUE, lang = NULL) {
  lang <- lang_helper(lang = lang)

  base_url <- "http://api.statbank.dk"
  version <- "V1"
  type <- "data"

  order <-  ifelse(ascending, "Ascending", "Descending")

  query <- c(list(lang = lang,
                  timeOrder = order,
                  delimiter = "Semicolon"),
                  variableID)
  query <- query[rlang::have_name(query)]

  resp <- GET(base_url, path = c(version, type, tableID, "CSV"), query = query)

  if (http_error(resp)) {
    parsed <- fromJSON(content(resp, "text"))
    rlang::abort(
      sprintf(
        "DST API request failed [%s]\n%s",
        status_code(resp),
        parsed$message
      )
    )
  }

  readr::read_csv2(content(resp, "text"), ...)
}
