test_that("send_email returns structured success result", {
  local_mocked_bindings(
    .email_smtp_send = function(email, config) list(status = "queued"),
    .package = "dispatchr"
  )

  result <- send_email(
    to = "friend@example.com",
    subject = "Hello",
    body = "Hi there",
    config = list(from = "sender@example.com", password = "secret")
  )

  expect_s3_class(result, "dispatchr_result")
  expect_true(result$ok)
  expect_equal(result$channel, "email")
  expect_equal(result$action, "send")
  expect_equal(result$response$status, "queued")
})

test_that("email body survives message ID assignment for text and HTML", {
  for (html in c(FALSE, TRUE)) {
    marker <- if (html) "unique-html-marker" else "unique-text-marker"
    body <- if (html) paste0("<b>", marker, "</b>") else marker
    email <- dispatchr:::.email_build_message(
      to = "friend@example.com",
      subject = "Hello",
      body = body,
      html = html,
      from = "sender@example.com",
      message_id = "test-id@example.com"
    )
    serialized <- as.character(email, encode = TRUE)

    expect_true(grepl("\r\n\r\n", serialized, fixed = TRUE))
    expect_true(grepl(marker, serialized, fixed = TRUE))
    expect_true(grepl("Message-ID:", serialized, fixed = TRUE))
  }
})

test_that("reply retains the body and threading headers without duplicating Re", {
  for (subject in c("Original subject", "Re: Original subject")) {
    email <- dispatchr:::.email_build_message(
      to = "friend@example.com",
      subject = subject,
      body = "unique-reply-marker",
      html = FALSE,
      from = "sender@example.com",
      message_id = "reply-id@example.com",
      reply_to_message_id = "parent-id@example.com"
    )
    serialized <- as.character(email, encode = TRUE)

    expect_identical(as.character(emayili::subject(email)), "Re: Original subject")
    expect_true(grepl("In-Reply-To:", serialized, fixed = TRUE))
    expect_true(grepl("References:", serialized, fixed = TRUE))
    expect_true(grepl("<parent-id@example.com>", serialized, fixed = TRUE))
    expect_true(grepl("unique-reply-marker", serialized, fixed = TRUE))
  }
})

test_that("send_email uses emayili by default", {
  withr::local_envvar(c(DISPATCHR_EMAIL_TRANSPORT = NA))
  called <- FALSE
  local_mocked_bindings(
    .email_smtp_send_emayili = function(email, config) {
      called <<- TRUE
      expect_identical(config$email_transport, "emayili")
      expect_equal(config$max_times, 1)
      list(status = "queued")
    },
    .email_smtp_send_custom = function(email, config) {
      stop("Custom curl transport was selected unexpectedly.")
    },
    .package = "dispatchr"
  )

  result <- send_email(
    to = "friend@example.com",
    subject = "Hello",
    body = "Hi there",
    config = list(from = "sender@example.com", password = "secret")
  )

  expect_true(called)
  expect_true(result$ok)
})

test_that("send_email validates required inputs", {
  expect_error(
    send_email(
      to = "",
      subject = "Hello",
      body = "Hi there",
      config = list(from = "sender@example.com", password = "secret")
    ),
    "`to`"
  )
})

test_that("SMTP diagnostics redact credentials and message contents", {
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
  callback <- dispatchr:::.email_smtp_debug_callback(state, enabled = TRUE)

  output <- capture.output({
    callback("HEADER_OUT", charToRaw("> AUTH PLAIN secret-auth-token\r\n"))
    callback("HEADER_IN", charToRaw("< 334 sensitive-challenge-text\r\n"))
    callback("HEADER_OUT", charToRaw("> base64-password-value\r\n"))
    callback("HEADER_OUT", charToRaw("> DATA\r\n"))
    callback("HEADER_IN", charToRaw("< 354 continue secret-server-text\r\n"))
    callback("DATA_OUT", charToRaw("private message body"))
  }, type = "message")

  expect_true(any(grepl("stage=authentication pending reply_code=NA", output, fixed = TRUE)))
  expect_true(any(grepl("stage=authentication challenge reply_code=334", output, fixed = TRUE)))
  expect_true(any(grepl("stage=message bytes started reply_code=NA", output, fixed = TRUE)))
  expect_true(all(grepl("elapsed=[0-9.]+$", output)))
  expect_true(all(grepl("^\\[dispatchr SMTP\\] stage=[A-Za-z0-9 _/-]+ reply_code=(NA|[0-9]{3}) elapsed=[0-9.]+$", output)))
  expect_false(any(grepl("event_type=|HEADER_IN|HEADER_OUT|DATA_OUT", output)))
  expect_false(any(grepl("secret-auth-token|sensitive-challenge-text|base64-password-value|private message body|secret-server-text", output)))
  expect_true(state$data_started)
})

test_that("SMTP timeout before the greeting is not submitted", {
  local_mocked_bindings(
    .email_curl_fetch = function(url, handle, callback, readfunction, upload_size) {
      stop(simpleError("Timeout was reached [smtp.example.test]: server response timeout"))
    },
    .package = "dispatchr"
  )

  output <- capture.output(
    result <- send_email(
      to = "friend@example.com",
      subject = "Hello",
      body = "Hi there",
      config = list(from = "sender@example.com", password = "secret", smtp_debug = TRUE,
                    email_transport = "curl")
    ),
    type = "message"
  )

  expect_false(result$ok)
  expect_identical(result$smtp_stage, "connecting")
  expect_identical(result$delivery_status, "not_submitted")
  expect_identical(result$delivery_uncertain, FALSE)
  expect_match(result$error, "SMTP message was not submitted")
})

test_that("curl callback accepts the server greeting and tracks message acceptance", {
  local_mocked_bindings(
    .email_curl_fetch = function(url, handle, callback, readfunction, upload_size) {
      callback(1L, charToRaw("< 220 ready\r\n"))
      callback(2L, charToRaw("> EHLO dispatchr.invalid\r\n"))
      callback(1L, charToRaw("< 250 hello\r\n"))
      callback(2L, charToRaw("> AUTH LOGIN\r\n"))
      callback(1L, charToRaw("< 334 Username:\r\n"))
      callback(2L, charToRaw("> base64-user\r\n"))
      callback(1L, charToRaw("< 334 Password:\r\n"))
      callback(2L, charToRaw("> base64-pass\r\n"))
      callback(1L, charToRaw("< 235 authenticated\r\n"))
      callback(2L, charToRaw("> MAIL FROM:<sender@example.com>\r\n"))
      callback(1L, charToRaw("< 250 sender accepted\r\n"))
      callback(2L, charToRaw("> RCPT TO:<friend@example.com>\r\n"))
      callback(1L, charToRaw("< 250 recipient accepted\r\n"))
      callback(2L, charToRaw("> DATA\r\n"))
      callback(1L, charToRaw("< 354 continue\r\n"))
      uploaded_chunks <- list()
      bytes_fed <- 0L
      while (bytes_fed < upload_size) {
        chunk <- readfunction(37L)
        expect_gt(length(chunk), 0L)
        bytes_fed <- bytes_fed + length(chunk)
        uploaded_chunks[[length(uploaded_chunks) + 1L]] <- chunk
        callback(4L, chunk)
      }
      uploaded <- do.call(c, uploaded_chunks)
      upload_text <- rawToChar(uploaded)
      expect_identical(length(uploaded), upload_size)
      expect_true(endsWith(upload_text, "\r\n"))
      expect_false(grepl("(?<!\\r)\\n", upload_text, perl = TRUE))
      callback(4L, as.raw(c(46L, 13L)))
      callback(4L, as.raw(10L))
      callback(1L, charToRaw("< 250 queued\r\n"))
      list(status_code = 250L)
    },
    .package = "dispatchr"
  )

  output <- capture.output(
    result <- send_email(
      to = "friend@example.com",
      subject = "Private subject",
      body = "private diagnostic body\r\n.\r\ncontinuation",
      config = list(
        from = "sender@example.com",
        password = "secret",
        smtp_debug = TRUE,
        email_transport = "curl"
      )
    ),
    type = "message"
  )

  expect_true(result$ok)
  expect_identical(result$delivery_status, "accepted")
  expect_true(any(grepl("stage=greeting received reply_code=220", output, fixed = TRUE)))
  expect_true(any(grepl("stage=authenticated reply_code=235", output, fixed = TRUE)))
  expect_true(any(grepl("stage=MAIL FROM accepted reply_code=250", output, fixed = TRUE)))
  expect_true(any(grepl("stage=RCPT TO accepted reply_code=250", output, fixed = TRUE)))
  expect_true(any(grepl("stage=DATA accepted reply_code=354", output, fixed = TRUE)))
  expect_true(any(grepl("stage=message bytes started reply_code=NA", output, fixed = TRUE)))
  expect_true(any(grepl("stage=payload supplied to libcurl reply_code=NA", output, fixed = TRUE)))
  expect_true(any(grepl("stage=SMTP DATA terminator written reply_code=NA", output, fixed = TRUE)))
  expect_true(any(grepl("stage=final acceptance reply_code=250", output, fixed = TRUE)))
  expect_false(any(grepl("secret|sender@example.com|friend@example.com|Private subject|private diagnostic body|Username:|Password:", output)))
  expect_true(all(grepl("^\\[dispatchr SMTP\\] stage=[A-Za-z0-9 _/-]+ reply_code=(NA|[0-9]{3}) elapsed=[0-9.]+$", output)))
})

test_that("SMTP upload records exact-length completion without requiring EOF", {
  email <- dispatchr:::.email_build_message(
    to = "friend@example.com",
    subject = "Hello",
    body = "A line\nwith LF.",
    html = FALSE,
    from = "sender@example.com",
    message_id = "test-id@example.com"
  )
  upload <- dispatchr:::.email_smtp_upload(email)
  on.exit(close(upload$connection), add = TRUE)

  bytes_fed <- 0L
  while (bytes_fed < length(upload$data)) {
    chunk <- upload$read(29L)
    expect_gt(length(chunk), 0L)
    bytes_fed <- bytes_fed + length(chunk)
  }

  expect_identical(bytes_fed, length(upload$data))
  expect_true(upload$is_complete())
  expect_false(upload$eof_observed())
  expect_length(upload$read(29L), 0L)
  expect_true(upload$eof_observed())
})

test_that("a single DATA_OUT callback after upload completion still accepts final 250", {
  local_mocked_bindings(
    .email_curl_fetch = function(url, handle, callback, readfunction, upload_size) {
      callback(2L, charToRaw("DATA\r\n"))
      callback(1L, charToRaw("354 continue\r\n"))
      payload <- readfunction(upload_size + 100L)
      expect_length(payload, upload_size)
      callback(4L, payload)
      callback(4L, charToRaw(".\r\n"))
      callback(1L, charToRaw("250 queued\r\n"))
      list(status_code = 250L)
    },
    .package = "dispatchr"
  )

  result <- send_email(
    to = "friend@example.com",
    subject = "Hi",
    body = "Hi.",
    config = list(from = "sender@example.com", password = "secret",
                  email_transport = "curl")
  )

  expect_true(result$ok)
  expect_identical(result$delivery_status, "accepted")
})

test_that("timeout after the full payload is supplied is uncertain", {
  local_mocked_bindings(
    .email_curl_fetch = function(url, handle, callback, readfunction, upload_size) {
      callback(2L, charToRaw("DATA\r\n"))
      callback(1L, charToRaw("< 354 continue\r\n"))
      bytes_fed <- 0L
      while (bytes_fed < upload_size) {
        chunk <- readfunction(31L)
        bytes_fed <- bytes_fed + length(chunk)
        callback(4L, chunk)
      }
      stop(simpleError("Timeout was reached [smtp.example.test]: server response timeout"))
    },
    .package = "dispatchr"
  )

  output <- capture.output(
    result <- send_email(
      to = "friend@example.com",
      subject = "Hello",
      body = "Hi there",
      config = list(
        from = "sender@example.com",
        password = "secret",
        smtp_debug = TRUE,
        email_transport = "curl"
      )
    ),
    type = "message"
  )

  expect_false(result$ok)
  expect_identical(result$smtp_stage, "payload supplied to libcurl")
  expect_identical(result$delivery_status, "uncertain")
  expect_true(result$delivery_uncertain)
  expect_true(any(grepl("stage=payload supplied to libcurl", output, fixed = TRUE)))
  expect_false(any(grepl("secret|sender@example.com|friend@example.com|Hi there", output)))
})

test_that("terminator detection spans outgoing callback chunks", {
  state <- new.env(parent = emptyenv())
  state$started <- proc.time()[["elapsed"]]
  state$stage <- "DATA accepted"
  state$last_command <- "DATA"
  state$data_started <- FALSE
  state$payload_complete <- TRUE
  payload <- charToRaw("payload\r\n.\r\ncontent\r\n")
  state$payload_expected_bytes <- length(payload)
  state$data_out_bytes <- 0
  state$terminator_window <- raw()
  state$terminator_observed <- FALSE
  callback <- dispatchr:::.email_smtp_debug_callback(state, enabled = TRUE)

  output <- capture.output({
    callback(4L, payload)
    expect_false(state$terminator_observed)
    callback(4L, as.raw(c(46L, 13L)))
    expect_false(state$terminator_observed)
    callback(4L, as.raw(10L))
  }, type = "message")

  expect_true(state$terminator_observed)
  expect_identical(state$stage, "SMTP DATA terminator written")
  expect_true(any(grepl("stage=SMTP DATA terminator written reply_code=NA", output, fixed = TRUE)))
  expect_false(any(grepl("payload", output, fixed = TRUE)))
  expect_true(all(grepl("^\\[dispatchr SMTP\\] stage=[A-Za-z0-9 _/-]+ reply_code=NA elapsed=[0-9.]+$", output)))
})

test_that("timeout after terminator emission remains uncertain pending final reply", {
  local_mocked_bindings(
    .email_curl_fetch = function(url, handle, callback, readfunction, upload_size) {
      callback(2L, charToRaw("DATA\r\n"))
      callback(1L, charToRaw("< 354 continue\r\n"))
      bytes_fed <- 0L
      while (bytes_fed < upload_size) {
        chunk <- readfunction(47L)
        bytes_fed <- bytes_fed + length(chunk)
        callback(4L, chunk)
      }
      callback(4L, as.raw(c(46L, 13L)))
      callback(4L, as.raw(10L))
      stop(simpleError("Timeout was reached [smtp.example.test]: server response timeout"))
    },
    .package = "dispatchr"
  )

  output <- capture.output(
    result <- send_email(
      to = "friend@example.com",
      subject = "Hello",
      body = "Hi there",
      config = list(from = "sender@example.com", password = "secret", smtp_debug = TRUE,
                    email_transport = "curl")
    ),
    type = "message"
  )

  expect_false(result$ok)
  expect_identical(result$smtp_stage, "SMTP DATA terminator written")
  expect_identical(result$delivery_status, "uncertain")
  expect_true(result$delivery_uncertain)
  expect_true(any(grepl("stage=SMTP DATA terminator written reply_code=NA", output, fixed = TRUE)))
})

test_that("a timeout after authentication retains the authenticated stage", {
  local_mocked_bindings(
    .email_curl_fetch = function(url, handle, callback, readfunction, upload_size) {
      callback(1L, charToRaw("< 220 ready\r\n"))
      callback(2L, charToRaw("> EHLO dispatchr.invalid\r\n"))
      callback(1L, charToRaw("< 250 hello\r\n"))
      callback(2L, charToRaw("> AUTH LOGIN\r\n"))
      callback(1L, charToRaw("< 235 authenticated\r\n"))
      stop(simpleError("Timeout was reached [smtp.example.test]: server response timeout"))
    },
    .package = "dispatchr"
  )

  output <- capture.output(
    result <- send_email(
      to = "friend@example.com",
      subject = "Hello",
      body = "Hi there",
      config = list(
        from = "sender@example.com",
        password = "secret",
        smtp_debug = TRUE,
        email_transport = "curl"
      )
    ),
    type = "message"
  )

  expect_false(result$ok)
  expect_identical(result$smtp_stage, "authenticated")
  expect_identical(result$delivery_status, "not_submitted")
  expect_true(any(grepl("stage=authenticated reply_code=235", output, fixed = TRUE)))
  expect_false(any(grepl("sender@example.com|friend@example.com|secret|Hi there", output)))
})

test_that("SMTP timeout after DATA returns uncertain delivery", {
  timeout <- structure(
    list(
      message = "Timeout was reached [smtp.example.test]: server response timeout",
      call = NULL,
      smtp_stage = "message body transmission / final acceptance",
      smtp_timeout = TRUE,
      delivery_uncertain = TRUE,
      delivery_status = "uncertain"
    ),
    class = c("dispatchr_smtp_error", "error", "condition")
  )

  local_mocked_bindings(
    .email_smtp_send = function(email, config) stop(timeout),
    .package = "dispatchr"
  )

  result <- send_email(
    to = "friend@example.com",
    subject = "Hello",
    body = "Hi there",
    config = list(from = "sender@example.com", password = "secret",
                  email_transport = "curl")
  )

  expect_false(result$ok)
  expect_identical(result$delivery_uncertain, TRUE)
  expect_identical(result$delivery_status, "uncertain")
  expect_match(result$error, "delivery is uncertain")
  expect_match(result$error, "Check the provider's Sent folder")
  expect_identical(result$smtp_stage, "message body transmission / final acceptance")
})

test_that("transport comparison uses matching SMTP settings", {
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .email_smtp_send_once = function(email, config) {
      seen$curl <- config
      list(status = "mock accepted")
    },
    .email_smtp_send_emayili = function(email, config) {
      seen$emayili <- config
      list(status = "mock accepted")
    },
    .package = "dispatchr"
  )

  shared_config <- list(
    from = "sender@example.com",
    password = "secret",
    host = "smtp.zoho.com",
    port = 465L,
    connecttimeout = 10,
    timeout = 30,
    max_times = 1,
    smtp_debug = TRUE
  )
  common_fields <- c(
    "from", "password", "host", "port", "connecttimeout",
    "timeout", "max_times", "smtp_debug"
  )

  send_email(
    to = "friend@example.com",
    subject = "Transport comparison",
    body = "Minimal body",
    config = c(shared_config, list(email_transport = "curl"))
  )
  send_email(
    to = "friend@example.com",
    subject = "Transport comparison",
    body = "Minimal body",
    config = c(shared_config, list(email_transport = "emayili"))
  )

  expect_equal(seen$curl[common_fields], seen$emayili[common_fields])
  expect_identical(seen$curl$email_transport, "curl")
  expect_identical(seen$emayili$email_transport, "emayili")
})

test_that("timeout after DATA permission but before body transmission is not submitted", {
  local_mocked_bindings(
    .email_curl_fetch = function(url, handle, callback, readfunction, upload_size) {
      callback("HEADER_IN", charToRaw("< 354 continue\r\n"))
      stop(simpleError("Timeout was reached [smtp.example.test]: server response timeout"))
    },
    .package = "dispatchr"
  )

  result <- send_email(
    to = "friend@example.com",
    subject = "Hello",
    body = "Hi there",
    config = list(from = "sender@example.com", password = "secret",
                  email_transport = "curl")
  )

  expect_false(result$ok)
  expect_identical(result$smtp_stage, "DATA accepted")
  expect_identical(result$delivery_status, "not_submitted")
  expect_false(result$delivery_uncertain)
})

test_that("live SMTP transport comparison (explicit opt-in)", {
  enabled <- identical(Sys.getenv("DISPATCHR_SMTP_COMPARE"), "1")
  testthat::skip_if_not(enabled, "Set DISPATCHR_SMTP_COMPARE=1 to send two comparison emails.")

  recipient <- Sys.getenv("DISPATCHR_SMTP_COMPARE_TO")
  from <- Sys.getenv("DISPATCHR_ZOHO_EMAIL")
  password <- Sys.getenv("DISPATCHR_ZOHO_PASSWORD")
  testthat::skip_if(
    any(!nzchar(c(recipient, from, password))),
    "Set the comparison recipient and Zoho SMTP credentials to run this test."
  )

  common <- list(
    from = from,
    password = password,
    host = "smtp.zoho.com",
    port = 465L,
    connecttimeout = 10,
    timeout = 30,
    max_times = 1,
    smtp_debug = TRUE
  )
  # The same harmless message is sent once per transport only when opted in.
  for (transport in c("emayili", "curl")) {
    result <- send_email(
      to = recipient,
      subject = "dispatchr SMTP transport comparison",
      body = "Minimal transport comparison message.",
      config = c(common, list(email_transport = transport))
    )

    expect_true(result$ok, info = paste(
      "transport", transport,
      "status", result$delivery_status,
      "stage", result$smtp_stage
    ))
  }
})
