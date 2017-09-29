lang_helper <- function(lang) {
  if (!is.null(lang)) {
    rlang::arg_match(lang, c("da", "en"))
  }

  if(!cache_missing() & is.null(lang)) {
    lang <- cache_env$cache$lang
  }

  if (!is.null(lang)) {
    if (lang == "da") {
      lang <- NULL
    }
  }

  lang
}

subjects_helper <- function(subjects) {
  if (!is.character(subjects) & !is.null(subjects)) {
    rlang::abort("subjects need to be a character vector")
  }
  all_digit <- all(purrr::map_lgl(subjects, stringr::str_detect, "^[:digit:]{0,}$"))

  if (!all_digit) {
    rlang::abort("subjects need to be character vector only containing digits")
  }

  if (!is.null(subjects)) {
    subjects <- stringr::str_c(subjects, collapse = ",")
  }

  subjects
}

date_parse_helper <- function(x, start = TRUE) {
  if (str_detect(x, "^[:digit:]{4,4}(K|Q)[1234]$")) {
    return(lubridate::parse_date_time(stringr::str_replace(x, "(Q|K)", "-"), "Y-q!*"))
  }

  if (str_detect(x, "^[:digit:]{4,4}$")) {
    return(lubridate::parse_date_time(x, "Y"))
  }

  if (str_detect(x, "^[:digit:]{4,4}M[:digit:]{2,2}$")) {
    return(lubridate::parse_date_time(str_replace(x, "M", "-"), "Y-m!*"))
  }

  if (str_detect(x, "^[:digit:]{4,4}(/|:)[:digit:]{4,4}$")) {
    if (start) {
      return(parse_date_time(str_sub(x, 1, 4), "Y"))
    } else {
      return(parse_date_time(str_sub(x, 6, 9), "Y"))
    }
  }

  if (str_detect(x, "^[:digit:]{4,4}M[:digit:]{2,2}D[:digit:]{2,2}$")) {
    return(lubridate::parse_date_time(stringr::str_replace(x, "(Q|K|D)", "-"), "Y-m!*-d!"))
  }

  if (str_detect(x, "^[:digit:]{4,4}(H|Q)[:digit:]$")) {
    str <- str_c(str_sub(x, 1, 4), "-", ifelse(str_sub(x, -1, -1) == "1", "1", "6"))
    return(lubridate::parse_date_time(str, "Y-m!*"))
  }

  if (str_detect(x, "^[:digit:]{4,4}(U|W)[:digit:]{1,2}-[:digit:]{1,2}$")) {
    if (start) {
      str <- str_c(str_sub(x, 1, 4), "-", str_sub(x, 6,7), "-1")
      return(lubridate::parse_date_time(str, "Y-U-u"))
    } else {
      str <- str_c(str_sub(x, 1, 4), "-", str_sub(x, -2,-1), "-1")
      return(lubridate::parse_date_time(str, "Y-U-u"))
    }
  }
}

date_helper <- function(x, start = TRUE) {
  pattern <- str_c("(",
                   c("^[:digit:]{4,4}(K|Q)[1234]$",
                     "^[:digit:]{4,4}$",
                     "^[:digit:]{4,4}M[:digit:]{2,2}$",
                     "^[:digit:]{4,4}:[:digit:]{4,4}$",
                     "^[:digit:]{4,4}(U|W)[:digit:]{1,2}-[:digit:]{1,2}$",
                     "^[:digit:]{4,4}/[:digit:]{4,4}$",
                     "^[:digit:]{4,4}M[:digit:]{2,2}D[:digit:]{2,2}$",
                     "^[:digit:]{4,4}(H|Q)[:digit:]$")
                   , ")", collapse = "|")
  if (!all(purrr::map_lgl(x, str_detect, pattern))) {
    return(x)
  } else
    as_date(do.call("c", map(x, date_parse_helper)))


}

