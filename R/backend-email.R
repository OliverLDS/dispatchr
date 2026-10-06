.email_message_id <- function(from) {
  domain <- sub("^[^@]+@", "", from)
  emayili::message_id(domain = domain)
}

.email_build_message <- function(to, subject, body, html, from, message_id,
                                 reply_to_message_id = NULL) {
  email <- emayili::envelope() |>
    emayili::from(from) |>
    emayili::to(to) |>
    emayili::subject(subject)

  if (isTRUE(html)) {
    email <- emayili::html(email, body)
  } else {
    email <- emayili::text(email, body)
  }

  email <- emayili::id(email, message_id)
  if (!is.null(reply_to_message_id)) {
    subject_prefix <- if (grepl("^Re\\s*:", subject, ignore.case = TRUE)) "" else "Re: "
    email <- emayili::inreplyto(email, reply_to_message_id, subject_prefix = subject_prefix)
    email <- emayili::references(email, reply_to_message_id, subject_prefix = "")
  }

  email
}

.email_smtp_send_once <- function(email, config) {
  state <- new.env(parent = emptyenv())
  state$started <- proc.time()[["elapsed"]]
  state$stage <- "connecting"
  state$greeting_observed <- FALSE
  state$last_command <- NULL
  state$data_started <- FALSE
  state$payload_complete <- FALSE
  state$payload_expected_bytes <- 0L
  state$data_out_bytes <- 0
  state$terminator_window <- raw()
  state$terminator_observed <- FALSE
  state$accepted <- FALSE
  state$rejected <- FALSE
  callback <- .email_smtp_debug_callback(state, config$smtp_debug)

  upload <- .email_smtp_upload(
    email,
    on_complete = function() {
      state$payload_complete <- TRUE
      state$stage <- "payload supplied to libcurl"
      .email_emit_smtp_diagnostic(config$smtp_debug, NA_integer_, state$stage, state$started)
    }
  )
  state$payload_expected_bytes <- length(upload$data)
  on.exit(close(upload$connection), add = TRUE)

  recipients <- c(emayili::to(email), emayili::cc(email), emayili::bcc(email))
  smtp_url <- sprintf(
    "%s://%s:%s/",
    if (identical(as.integer(config$port), 465L)) "smtps" else "smtp",
    config$host,
    config$port
  )

  handle <- curl::new_handle(
    upload = TRUE,
    readfunction = upload$read,
    infilesize_large = length(upload$data),
    mail_from = emayili::raw(emayili::from(email)),
    mail_rcpt = emayili::raw(recipients),
    use_ssl = if (as.integer(config$port) %in% c(465L, 587L)) 1L else 0L,
    username = config$from,
    password = config$password,
    ssl_verifypeer = TRUE,
    forbid_reuse = FALSE,
    connecttimeout = config$connecttimeout,
    timeout = config$timeout,
    verbose = TRUE,
    debugfunction = callback
  )

  tryCatch(
    {
      response <- .email_curl_fetch(
        smtp_url,
        handle,
        callback,
        readfunction = upload$read,
        upload_size = length(upload$data)
      )
      if (!state$accepted) {
        stop("SMTP transaction ended without a server acceptance reply.", call. = FALSE)
      }
      response
    },
    error = function(err) {
      detail <- conditionMessage(err)
      timed_out <- grepl("timed out|timeout was reached|timeout", detail, ignore.case = TRUE)
      uncertain <- state$payload_complete && !state$accepted && !state$rejected
      failure <- structure(
        list(
          message = detail,
          call = NULL,
          smtp_stage = state$stage,
          smtp_timeout = timed_out,
          delivery_uncertain = uncertain,
          delivery_status = if (uncertain) {
            "uncertain"
          } else if (state$rejected) {
            "rejected"
          } else {
            "not_submitted"
          },
          smtp_retryable = !timed_out && !state$payload_complete && !state$rejected
        ),
        class = c("dispatchr_smtp_error", "error", "condition")
      )
      stop(failure)
    }
  )
}

.email_smtp_upload <- function(email, on_complete = NULL, on_eof = NULL) {
  # Supply only the RFC 5322 message; libcurl writes SMTP's DATA terminator at EOF.
  serialized <- enc2utf8(as.character(email, encode = TRUE))
  normalized <- gsub("\\r\\n|\\r|\\n", "\\r\\n", serialized, perl = TRUE)
  if (!endsWith(normalized, "\r\n")) normalized <- paste0(normalized, "\r\n")
  data <- charToRaw(normalized)
  connection <- rawConnection(data)
  expected_bytes <- length(data)
  bytes_read <- 0L
  payload_complete <- FALSE
  eof_reported <- FALSE
  read <- function(nbytes, ...) {
    chunk <- readBin(connection, raw(), nbytes)
    bytes_read <<- bytes_read + length(chunk)
    if (!payload_complete && bytes_read == expected_bytes) {
      payload_complete <<- TRUE
      if (is.function(on_complete)) on_complete()
    }
    if (!length(chunk) && !eof_reported) {
      eof_reported <<- TRUE
      if (is.function(on_eof)) on_eof()
    }
    chunk
  }

  list(
    data = data,
    connection = connection,
    read = read,
    is_complete = function() payload_complete,
    eof_observed = function() eof_reported
  )
}

.email_curl_fetch <- function(url, handle, callback, readfunction, upload_size) {
  curl::curl_fetch_memory(url, handle = handle)
}

.email_smtp_send_emayili <- function(email, config) {
  started <- proc.time()[["elapsed"]]
  if (isTRUE(config$smtp_debug)) {
    .email_emit_smtp_diagnostic(TRUE, NA_integer_, "emayili send started", started)
  }

  tryCatch(
    {
      smtp <- emayili::server(
        host = config$host,
        port = config$port,
        username = config$from,
        password = config$password,
        connecttimeout = config$connecttimeout,
        timeout = config$timeout,
        max_times = 1,
        reuse = TRUE
      )
      response <- smtp(email, verbose = FALSE)
      if (isTRUE(config$smtp_debug)) {
        .email_emit_smtp_diagnostic(TRUE, NA_integer_, "emayili send accepted", started)
      }
      response
    },
    error = function(err) {
      detail <- conditionMessage(err)
      timed_out <- grepl("timed out|timeout was reached|timeout", detail, ignore.case = TRUE)
      failure <- structure(
        list(
          message = detail,
          call = NULL,
          smtp_stage = "unknown (emayili transport)",
          smtp_timeout = timed_out,
          delivery_uncertain = timed_out,
          delivery_status = if (timed_out) "uncertain" else "failed",
          smtp_retryable = FALSE
        ),
        class = c("dispatchr_smtp_error", "error", "condition")
      )
      if (isTRUE(config$smtp_debug)) {
        .email_emit_smtp_diagnostic(TRUE, NA_integer_, "emayili send failed; stage unavailable", started)
      }
      stop(failure)
    }
  )
}

.email_smtp_send_custom <- function(email, config) {
  last_error <- NULL
  for (attempt in seq_len(as.integer(config$max_times))) {
    result <- tryCatch(
      .email_smtp_send_once(email, config),
      error = function(err) {
        if (inherits(err, "dispatchr_smtp_error") &&
            isTRUE(err$smtp_retryable) && attempt < config$max_times) {
          last_error <<- err
          return(NULL)
        }
        stop(err)
      }
    )
    if (!is.null(result)) {
      return(result)
    }
    if (isTRUE(config$smtp_debug)) {
      .email_emit_smtp_diagnostic(
        TRUE, NA_integer_, "retrying before message submission", proc.time()[["elapsed"]]
      )
    }
    Sys.sleep(min(2^(attempt - 1L), 4))
  }

  stop(last_error)
}

.email_smtp_send <- function(email, config) {
  if (identical(config$email_transport, "emayili")) {
    return(.email_smtp_send_emayili(email, config))
  }
  .email_smtp_send_custom(email, config)
}

.email_emit_smtp_diagnostic <- function(enabled, reply_code, stage, started) {
  if (!isTRUE(enabled)) return(invisible(NULL))

  code <- if (length(reply_code) == 1L && !is.na(reply_code)) {
    as.character(as.integer(reply_code))
  } else {
    "NA"
  }
  elapsed <- proc.time()[["elapsed"]] - started

  message(sprintf(
    "[dispatchr SMTP] stage=%s reply_code=%s elapsed=%.3f",
    gsub("[^A-Za-z0-9 _/-]", "", stage),
    code,
    elapsed
  ))
  invisible(NULL)
}

.email_smtp_debug_callback <- function(state, enabled = FALSE) {
  function(type, msg) {
    tryCatch({
      event_type <- .email_normalize_curl_event_type(type)
      previous_stage <- state$stage

      reply_code <- NA_integer_
      if (event_type %in% c("HEADER_OUT", "HEADER_IN")) {
        text <- tryCatch({
          if (is.raw(msg)) rawToChar(msg) else paste(as.character(msg), collapse = "")
        }, error = function(...) "")
        lines <- strsplit(text, "\\r?\\n", perl = TRUE)[[1]]
        for (line in lines) {
          line <- sub("^\\s*[<>*]\\s*", "", trimws(line))
          if (!nzchar(line)) next

          if (identical(event_type, "HEADER_OUT")) {
            command <- toupper(line)
            if (grepl("^EHLO\\b|^HELO\\b", command)) {
              state$last_command <- "EHLO"
              state$stage <- "EHLO sent"
            } else if (grepl("^AUTH\\b", command)) {
              state$last_command <- "AUTH"
              state$stage <- "authentication pending"
            } else if (grepl("^MAIL FROM:", command)) {
              state$last_command <- "MAIL"
              state$stage <- "MAIL FROM pending"
            } else if (grepl("^RCPT TO:", command)) {
              state$last_command <- "RCPT"
              state$stage <- "RCPT TO pending"
            } else if (grepl("^DATA\\b", command)) {
              state$last_command <- "DATA"
              state$stage <- "DATA pending"
            }
          } else {
            code <- suppressWarnings(as.integer(substr(line, 1L, 3L)))
            if (is.na(code)) next
            reply_code <- code

            if (code == 220L) {
              state$greeting_observed <- TRUE
              state$stage <- "greeting received"
            } else if (code == 334L) {
              state$stage <- "authentication challenge"
            } else if (code == 235L) {
              state$stage <- "authenticated"
            } else if (code == 354L) {
              state$stage <- "DATA accepted"
            } else if (code >= 400L && code <= 599L) {
              state$rejected <- TRUE
              state$stage <- if (state$data_started) "server rejected message" else "SMTP server rejection"
            } else if (code == 250L && state$data_started && identical(state$last_command, "DATA")) {
              state$accepted <- TRUE
              state$stage <- "final acceptance"
            } else if (code == 250L && identical(state$last_command, "EHLO")) {
              state$stage <- "EHLO accepted"
            } else if (code == 250L && identical(state$last_command, "MAIL")) {
              state$stage <- "MAIL FROM accepted"
            } else if (code %in% c(250L, 251L) && identical(state$last_command, "RCPT")) {
              state$stage <- "RCPT TO accepted"
            }
          }
        }
      }

      if (identical(event_type, "DATA_OUT") && identical(state$last_command, "DATA")) {
        if (!state$data_started) {
          state$data_started <- TRUE
          if (state$payload_complete) {
            .email_emit_smtp_diagnostic(enabled, NA_integer_, "message bytes started", state$started)
          } else {
            state$stage <- "message bytes started"
          }
        }
        .email_smtp_observe_data_out(state, msg)
      }

      if (!identical(state$stage, previous_stage) || !is.na(reply_code)) {
        .email_emit_smtp_diagnostic(enabled, reply_code, state$stage, state$started)
      }
    }, error = function(...) NULL)

    invisible(NULL)
  }
}

.email_smtp_observe_data_out <- function(state, msg) {
  if (state$terminator_observed || !is.raw(msg)) return(invisible(NULL))

  chunk_start <- state$data_out_bytes
  state$data_out_bytes <- state$data_out_bytes + length(msg)
  terminator <- as.raw(c(13L, 10L, 46L, 13L, 10L))

  for (i in seq_along(msg)) {
    state$terminator_window <- c(state$terminator_window, msg[[i]])
    if (length(state$terminator_window) > length(terminator)) {
      state$terminator_window <- tail(state$terminator_window, length(terminator))
    }

    current_offset <- chunk_start + i
    if (current_offset > state$payload_expected_bytes &&
        identical(state$terminator_window, terminator)) {
      state$terminator_observed <- TRUE
      state$stage <- "SMTP DATA terminator written"
      break
    }
  }

  invisible(NULL)
}

.email_normalize_curl_event_type <- function(type) {
  if (is.numeric(type) && length(type) == 1L && !is.na(type)) {
    event_types <- c(
      "TEXT", "HEADER_IN", "HEADER_OUT", "DATA_IN", "DATA_OUT",
      "SSL_DATA_IN", "SSL_DATA_OUT"
    )
    index <- as.integer(type) + 1L
    if (index >= 1L && index <= length(event_types)) return(event_types[[index]])
    return("OTHER")
  }
  if (!is.character(type) || length(type) != 1L || is.na(type)) return("OTHER")
  normalized <- toupper(trimws(type))
  normalized <- sub("^CURLINFO_", "", normalized)
  if (normalized %in% c("TEXT", "HEADER_IN", "HEADER_OUT", "DATA_IN", "DATA_OUT",
                       "SSL_DATA_IN", "SSL_DATA_OUT")) normalized else "OTHER"
}
