.dispatchr_config <- function(config = NULL) {
  if (is.null(config)) {
    return(list())
  }
  if (!is.list(config)) {
    stop("`config` must be NULL or a list.", call. = FALSE)
  }
  config
}

.dispatchr_resolve_email_config <- function(config = NULL) {
  config <- .dispatchr_config(config)
  resolved <- list(
    from = config$from %||% .dispatchr_env("DISPATCHR_EMAIL_FROM"),
    password = config$password %||% .dispatchr_env("DISPATCHR_EMAIL_PASSWORD"),
    host = config$host %||% .dispatchr_env("DISPATCHR_EMAIL_HOST") %||% "smtp.gmail.com",
    port = config$port %||% .dispatchr_env("DISPATCHR_EMAIL_PORT") %||% 587L,
    connecttimeout = config$connecttimeout %||% .dispatchr_env("DISPATCHR_EMAIL_CONNECTTIMEOUT") %||% 10,
    timeout = config$timeout %||% .dispatchr_env("DISPATCHR_EMAIL_TIMEOUT") %||% 30,
    max_times = config$max_times %||% .dispatchr_env("DISPATCHR_EMAIL_MAX_TIMES") %||% 1L
  )

  missing <- names(resolved)[vapply(resolved[c("from", "password")], is.null, logical(1))]
  if (length(missing) > 0L) {
    .dispatchr_stop_missing(missing, "email")
  }

  resolved$port <- as.integer(resolved$port)
  for (name in c("connecttimeout", "timeout", "max_times")) {
    value <- suppressWarnings(as.numeric(resolved[[name]]))
    if (length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0) {
      stop(sprintf("Email config `%s` must be a positive number.", name), call. = FALSE)
    }
    resolved[[name]] <- value
  }

  resolved
}

.dispatchr_resolve_x_config <- function(config = NULL) {
  config <- .dispatchr_config(config)
  resolved <- list(
    api_key = config$api_key %||% .dispatchr_env("DISPATCHR_X_API_KEY"),
    api_secret = config$api_secret %||% .dispatchr_env("DISPATCHR_X_API_SECRET"),
    access_token = config$access_token %||% .dispatchr_env("DISPATCHR_X_ACCESS_TOKEN"),
    access_secret = config$access_secret %||% .dispatchr_env("DISPATCHR_X_ACCESS_SECRET")
  )

  missing <- names(resolved)[vapply(resolved, is.null, logical(1))]
  if (length(missing) > 0L) {
    .dispatchr_stop_missing(missing, "X")
  }

  resolved
}

.dispatchr_resolve_telegram_config <- function(config = NULL) {
  config <- .dispatchr_config(config)
  resolved <- list(
    bot_token = config$bot_token %||% .dispatchr_env("DISPATCHR_TELEGRAM_BOT_TOKEN"),
    chat_id = config$chat_id %||% .dispatchr_env("DISPATCHR_TELEGRAM_CHAT_ID"),
    parse_mode = config$parse_mode %||% .dispatchr_env("DISPATCHR_TELEGRAM_PARSE_MODE")
  )

  missing <- names(resolved)[vapply(resolved[c("bot_token", "chat_id")], is.null, logical(1))]
  if (length(missing) > 0L) {
    .dispatchr_stop_missing(missing, "Telegram")
  }

  resolved
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
