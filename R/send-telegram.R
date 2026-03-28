#' Send a Telegram Message
#'
#' Sends a text message to Telegram through the Bot API.
#'
#' Required config fields are `bot_token` and `chat_id`. Missing fields are
#' resolved from environment variables:
#'
#' - `DISPATCHR_TELEGRAM_BOT_TOKEN`
#' - `DISPATCHR_TELEGRAM_CHAT_ID`
#' - `DISPATCHR_TELEGRAM_PARSE_MODE`
#'
#' @param text Message text.
#' @param config Optional named list with Telegram configuration.
#'
#' @return A `dispatchr_result` list with API response metadata.
#' @examples
#' \dontrun{
#' send_telegram(
#'   text = "Hello from dispatchr",
#'   config = list(
#'     bot_token = Sys.getenv("DISPATCHR_TELEGRAM_BOT_TOKEN"),
#'     chat_id = Sys.getenv("DISPATCHR_TELEGRAM_CHAT_ID")
#'   )
#' )
#' }
#' @export
send_telegram <- function(text, config = NULL) {
  .dispatchr_validate_scalar_string(text, "text")
  telegram_config <- .dispatchr_resolve_telegram_config(config)

  tryCatch(
    {
      resp <- .telegram_send_message_request(text, telegram_config)
      parsed <- .telegram_parse_response(resp)
      .dispatchr_result(
        ok = parsed$ok,
        channel = "telegram",
        action = "send",
        request_summary = list(
          chat_id = telegram_config$chat_id,
          parse_mode = telegram_config$parse_mode,
          text = text
        ),
        response = parsed$parsed,
        error = if (parsed$ok) NULL else parsed$raw
      )
    },
    error = function(err) {
      .dispatchr_result(
        ok = FALSE,
        channel = "telegram",
        action = "send",
        request_summary = list(
          chat_id = telegram_config$chat_id,
          parse_mode = telegram_config$parse_mode,
          text = text
        ),
        response = NULL,
        error = conditionMessage(err)
      )
    }
  )
}

#' Send a Telegram Image
#'
#' Sends an image to Telegram with an optional caption.
#'
#' @param image_path Path to the image file.
#' @param caption Optional caption text.
#' @param config Optional named list with Telegram configuration.
#'
#' @return A `dispatchr_result` list with API response metadata.
#' @examples
#' \dontrun{
#' send_telegram_image(
#'   image_path = "plot.png",
#'   caption = "Performance update",
#'   config = list(
#'     bot_token = Sys.getenv("DISPATCHR_TELEGRAM_BOT_TOKEN"),
#'     chat_id = Sys.getenv("DISPATCHR_TELEGRAM_CHAT_ID")
#'   )
#' )
#' }
#' @export
send_telegram_image <- function(image_path, caption = NULL, config = NULL) {
  .dispatchr_validate_file(image_path, "image_path")
  if (!is.null(caption)) {
    .dispatchr_validate_scalar_string(caption, "caption")
  }

  telegram_config <- .dispatchr_resolve_telegram_config(config)

  tryCatch(
    {
      resp <- .telegram_send_photo_request(image_path, caption, telegram_config)
      parsed <- .telegram_parse_response(resp)
      .dispatchr_result(
        ok = parsed$ok,
        channel = "telegram",
        action = "send_image",
        request_summary = .dispatchr_compact_list(list(
          chat_id = telegram_config$chat_id,
          parse_mode = telegram_config$parse_mode,
          image_path = image_path,
          caption = caption
        )),
        response = parsed$parsed,
        error = if (parsed$ok) NULL else parsed$raw
      )
    },
    error = function(err) {
      .dispatchr_result(
        ok = FALSE,
        channel = "telegram",
        action = "send_image",
        request_summary = .dispatchr_compact_list(list(
          chat_id = telegram_config$chat_id,
          parse_mode = telegram_config$parse_mode,
          image_path = image_path,
          caption = caption
        )),
        response = NULL,
        error = conditionMessage(err)
      )
    }
  )
}
