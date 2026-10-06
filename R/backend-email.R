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
    email |> emayili::html(body)
  } else {
    email |> emayili::text(body)
  }

  email <- emayili::id(email, message_id)
  if (!is.null(reply_to_message_id)) {
    email <- emayili::inreplyto(email, reply_to_message_id)
    email <- emayili::references(email, reply_to_message_id, subject_prefix = "")
  }

  email
}

.email_smtp_send <- function(email, config) {
  smtp <- emayili::server(
    host = config$host,
    port = config$port,
    username = config$from,
    password = config$password,
    connecttimeout = config$connecttimeout,
    timeout = config$timeout,
    max_times = config$max_times
  )

  smtp(email)
}
