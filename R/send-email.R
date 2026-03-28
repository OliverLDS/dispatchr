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
#'
#' @param to Recipient email address or vector of email addresses.
#' @param subject Email subject line.
#' @param body Email body content.
#' @param html Logical. If `TRUE`, send the body as HTML.
#' @param config Optional named list with SMTP configuration.
#'
#' @return A `dispatchr_result` list with send metadata and the backend response.
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
send_email <- function(to, subject, body, html = FALSE, config = NULL) {
  .dispatchr_validate_string_vector(to, "to")
  .dispatchr_validate_scalar_string(subject, "subject")
  .dispatchr_validate_scalar_string(body, "body")

  email_config <- .dispatchr_resolve_email_config(config)
  email <- .email_build_message(
    to = to,
    subject = subject,
    body = body,
    html = html,
    from = email_config$from
  )

  request_summary <- list(
    to = to,
    subject = subject,
    html = isTRUE(html),
    from = email_config$from
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
        error = NULL
      )
    },
    error = function(err) {
      .dispatchr_result(
        ok = FALSE,
        channel = "email",
        action = "send",
        request_summary = request_summary,
        response = NULL,
        error = conditionMessage(err)
      )
    }
  )
}
