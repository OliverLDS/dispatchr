.dispatchr_env <- function(name) {
  value <- Sys.getenv(name, unset = "")
  if (identical(value, "")) {
    return(NULL)
  }
  value
}

.dispatchr_compact_list <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

.dispatchr_stop_missing <- function(fields, channel) {
  stop(
    sprintf(
      "Missing required %s config field(s): %s.",
      channel,
      paste(fields, collapse = ", ")
    ),
    call. = FALSE
  )
}

.dispatchr_validate_scalar_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop(sprintf("`%s` must be a non-empty character string.", name), call. = FALSE)
  }
}

.dispatchr_validate_string_vector <- function(x, name) {
  if (!is.character(x) || length(x) < 1L || any(is.na(x)) || any(!nzchar(trimws(x)))) {
    stop(
      sprintf("`%s` must be a character vector with at least one non-empty value.", name),
      call. = FALSE
    )
  }
}

.dispatchr_validate_file <- function(path, name) {
  .dispatchr_validate_scalar_string(path, name)
  if (!file.exists(path)) {
    stop(sprintf("File not found for `%s`: %s", name, path), call. = FALSE)
  }
}

.dispatchr_http_get <- function(...) {
  httr::GET(...)
}

.dispatchr_http_post <- function(...) {
  httr::POST(...)
}

.dispatchr_result <- function(ok,
                              channel,
                              action,
                              request_summary,
                              response = NULL,
                              error = NULL,
                              rate_limit = NULL) {
  structure(
    list(
      ok = isTRUE(ok),
      channel = channel,
      action = action,
      request_summary = request_summary,
      response = response,
      error = error,
      rate_limit = rate_limit
    ),
    class = "dispatchr_result"
  )
}

#' @export
print.dispatchr_result <- function(x, ...) {
  cat(sprintf("<dispatchr_result[%s/%s] ok=%s>\n", x$channel, x$action, x$ok))
  invisible(x)
}
