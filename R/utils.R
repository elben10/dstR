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

