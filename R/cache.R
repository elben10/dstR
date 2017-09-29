cache_env <- rlang::new_environment()
NULL

#' @export
dst_cache <- function(lang = "da") {
  rlang::arg_match(lang, c("da", "en"))
  cache_env$cache <- list(lang = lang)
}

cache_missing <- function() {
  rlang::is_empty(cache_env$cache)
}
