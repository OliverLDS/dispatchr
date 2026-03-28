.email_build_message <- function(to, subject, body, html, from) {
  email <- emayili::envelope() |>
    emayili::from(from) |>
    emayili::to(to) |>
    emayili::subject(subject)

  if (isTRUE(html)) {
    email |> emayili::html(body)
  } else {
    email |> emayili::text(body)
  }
}

.email_smtp_send <- function(email, config) {
  smtp <- emayili::server(
    host = config$host,
    port = config$port,
    username = config$from,
    password = config$password
  )

  smtp(email)
}
