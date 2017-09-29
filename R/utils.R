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
