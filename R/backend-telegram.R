.telegram_parse_response <- function(resp) {
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(...) list(raw = txt)
  )

  list(
    ok = identical(httr::status_code(resp), 200L) && isTRUE(parsed$ok),
    parsed = parsed,
    raw = txt
  )
}

.telegram_send_message_request <- function(text, config) {
  .dispatchr_http_post(
    sprintf("https://api.telegram.org/bot%s/sendMessage", config$bot_token),
    body = .dispatchr_compact_list(list(
      chat_id = config$chat_id,
      text = text,
      parse_mode = config$parse_mode
    )),
    encode = "form"
  )
}

.telegram_send_photo_request <- function(image_path, caption_text, config) {
  parse_mode <- config$parse_mode
  if (is.null(caption_text)) {
    parse_mode <- NULL
  }

  .dispatchr_http_post(
    sprintf("https://api.telegram.org/bot%s/sendPhoto", config$bot_token),
    body = .dispatchr_compact_list(list(
      chat_id = config$chat_id,
      caption = caption_text,
      photo = httr::upload_file(image_path),
      parse_mode = parse_mode
    )),
    encode = "multipart"
  )
}
