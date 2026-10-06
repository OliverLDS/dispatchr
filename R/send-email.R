#' Send an Email
#'
#' Sends an email through SMTP using `emayili`. The current package defaults to
#' Gmail SMTP but allows the host and port to be overridden through `config`.
#'
#' Required email config fields are `from` and `password`. Missing fields are
#' resolved from environment variables:
#'
#' - `DISPATCHR_EMAIL_FROM`
#' - `DISPATCHR_EMAIL_PASSWORD`
#' - `DISPATCHR_EMAIL_HOST`
#' - `DISPATCHR_EMAIL_PORT`
#' - `DISPATCHR_EMAIL_CONNECTTIMEOUT` (default: 10 seconds)
#' - `DISPATCHR_EMAIL_TIMEOUT` (default: 30 seconds)
#' - `DISPATCHR_EMAIL_MAX_TIMES` (default: 1 attempt)
#'
#' @param to Recipient email address or vector of email addresses.
#' @param subject Email subject line.
#' @param body Email body content.
#' @param html Logical. If `TRUE`, send the body as HTML.
#' @param reply_to_message_id Optional `Message-ID` of the message being replied
#'   to. Sets `In-Reply-To` and `References` headers and prefixes the subject
#'   with `Re:`. This does not retrieve the original message.
#' @param config Optional named list with SMTP configuration. It may include
#'   `connecttimeout` and `timeout` in seconds and `max_times` for the maximum
#'   number of send attempts. Defaults are 10, 30, and 1 respectively.
#'
#' @return A `dispatchr_result` list with send metadata, backend response, and
#'   `message_id`, the ID assigned to this outgoing message. The ID is assigned
#'   before sending and is available even if the send fails.
#' @examples
#' \dontrun{
#' send_email(
#'   to = "friend@example.com",
#'   subject = "Hello from dispatchr",
#'   body = "<b>Test</b>",
#'   html = TRUE,
#'   config = list(
#'     from = Sys.getenv("DISPATCHR_EMAIL_FROM"),
#'     password = Sys.getenv("DISPATCHR_EMAIL_PASSWORD")
#'   )
#' )
#' }
#' @export
send_email <- function(to, subject, body, html = FALSE, config = NULL,
                       reply_to_message_id = NULL) {
  .dispatchr_validate_string_vector(to, "to")
  .dispatchr_validate_scalar_string(subject, "subject")
  .dispatchr_validate_scalar_string(body, "body")
  if (!is.null(reply_to_message_id)) {
    .dispatchr_validate_scalar_string(reply_to_message_id, "reply_to_message_id")
    normalized_id <- gsub("^<|>$", "", reply_to_message_id)
    if (!grepl("^[^@]+@[^@]+\\.[^@]+$", normalized_id)) {
      stop("`reply_to_message_id` must be a valid email Message-ID.", call. = FALSE)
    }
    reply_to_message_id <- normalized_id
  }

  email_config <- .dispatchr_resolve_email_config(config)
  message_id <- .email_message_id(email_config$from)
  email <- .email_build_message(
    to = to,
    subject = subject,
    body = body,
    html = html,
    from = email_config$from,
    message_id = message_id,
    reply_to_message_id = reply_to_message_id
  )

  request_summary <- list(
    to = to,
    subject = subject,
    html = isTRUE(html),
    from = email_config$from,
    reply_to_message_id = reply_to_message_id
  )

  tryCatch(
    {
      response <- .email_smtp_send(email, email_config)
      .dispatchr_result(
        ok = TRUE,
        channel = "email",
        action = "send",
        request_summary = request_summary,
        response = response,
        error = NULL,
        message_id = message_id
      )
    },
    error = function(err) {
      .dispatchr_result(
        ok = FALSE,
        channel = "email",
        action = "send",
        request_summary = request_summary,
        response = NULL,
        error = conditionMessage(err),
        message_id = message_id
      )
    }
  )
}
