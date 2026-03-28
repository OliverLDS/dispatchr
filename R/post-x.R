#' Post to X
#'
#' Posts text to X using OAuth 1.0a credentials. Missing config fields are
#' resolved from environment variables:
#'
#' - `DISPATCHR_X_API_KEY`
#' - `DISPATCHR_X_API_SECRET`
#' - `DISPATCHR_X_ACCESS_TOKEN`
#' - `DISPATCHR_X_ACCESS_SECRET`
#'
#' If `split_thread = TRUE`, longer content is split into a thread using the
#' existing sentence-based helper logic.
#'
#' @param text Post body.
#' @param config Optional named list of X credentials.
#' @param reply_to_id Optional tweet ID to reply to.
#' @param split_thread Logical. If `TRUE`, split a long post into a thread.
#'
#' @return A `dispatchr_result` list. For threaded posts, `response` contains a
#'   list of per-post API responses.
#' @examples
#' \dontrun{
#' post_x(
#'   text = "Hello from dispatchr",
#'   config = list(
#'     api_key = Sys.getenv("DISPATCHR_X_API_KEY"),
#'     api_secret = Sys.getenv("DISPATCHR_X_API_SECRET"),
#'     access_token = Sys.getenv("DISPATCHR_X_ACCESS_TOKEN"),
#'     access_secret = Sys.getenv("DISPATCHR_X_ACCESS_SECRET")
#'   )
#' )
#' }
#' @export
post_x <- function(text, config = NULL, reply_to_id = NULL, split_thread = FALSE) {
  .x_validate_text(text)
  x_config <- .dispatchr_resolve_x_config(config)
  oauth1 <- .x_build_oauth1(x_config)

  posts <- if (isTRUE(split_thread)) .x_split_thread_text(text) else text
  if (!isTRUE(split_thread) && .x_text_width(text) > 280) {
    stop("`text` exceeds X's 280-character weighted limit. Use `split_thread = TRUE` if appropriate.", call. = FALSE)
  }

  responses <- vector("list", length(posts))
  rate_limits <- vector("list", length(posts))
  current_reply_to <- reply_to_id

  tryCatch(
    {
      for (i in seq_along(posts)) {
        resp <- .x_post_tweet_request(
          text = posts[[i]],
          oauth1 = oauth1,
          reply_to_tweet_id = current_reply_to
        )

        parsed <- .x_parse_json_response(resp)
        if (httr::http_error(resp)) {
          stop(parsed$raw, call. = FALSE)
        }

        responses[[i]] <- parsed$parsed
        rate_limits[[i]] <- parsed$rate_limit

        tweet_id <- parsed$parsed$data$id %||% NULL
        if (!is.null(tweet_id)) {
          current_reply_to <- tweet_id
        }
      }

      .dispatchr_result(
        ok = TRUE,
        channel = "x",
        action = if (length(posts) > 1L) "thread" else "post",
        request_summary = .dispatchr_compact_list(list(
          text = if (length(posts) == 1L) text else NULL,
          pieces = length(posts),
          split_thread = isTRUE(split_thread),
          reply_to_id = reply_to_id
        )),
        response = if (length(responses) == 1L) responses[[1]] else responses,
        error = NULL,
        rate_limit = if (length(rate_limits) == 1L) rate_limits[[1]] else rate_limits
      )
    },
    error = function(err) {
      .dispatchr_result(
        ok = FALSE,
        channel = "x",
        action = if (length(posts) > 1L) "thread" else "post",
        request_summary = .dispatchr_compact_list(list(
          text = if (length(posts) == 1L) text else NULL,
          pieces = length(posts),
          split_thread = isTRUE(split_thread),
          reply_to_id = reply_to_id
        )),
        response = NULL,
        error = conditionMessage(err),
        rate_limit = NULL
      )
    }
  )
}

#' Post an Image to X
#'
#' Uploads an image and posts it to X with an optional text caption.
#'
#' @param text Post body.
#' @param image_path Path to the image file.
#' @param config Optional named list of X credentials.
#' @param reply_to_id Optional tweet ID to reply to.
#'
#' @return A `dispatchr_result` list with API response metadata.
#' @examples
#' \dontrun{
#' post_x_image(
#'   text = "Chart update",
#'   image_path = "plot.png",
#'   config = list(
#'     api_key = Sys.getenv("DISPATCHR_X_API_KEY"),
#'     api_secret = Sys.getenv("DISPATCHR_X_API_SECRET"),
#'     access_token = Sys.getenv("DISPATCHR_X_ACCESS_TOKEN"),
#'     access_secret = Sys.getenv("DISPATCHR_X_ACCESS_SECRET")
#'   )
#' )
#' }
#' @export
post_x_image <- function(text, image_path, config = NULL, reply_to_id = NULL) {
  .x_validate_text(text)
  .dispatchr_validate_file(image_path, "image_path")
  if (.x_text_width(text) > 280) {
    stop("`text` exceeds X's 280-character weighted limit.", call. = FALSE)
  }

  x_config <- .dispatchr_resolve_x_config(config)
  oauth1 <- .x_build_oauth1(x_config)

  tryCatch(
    {
      media_id <- .x_upload_media(oauth1, image_path)
      resp <- .x_post_tweet_request(
        text = text,
        oauth1 = oauth1,
        reply_to_tweet_id = reply_to_id,
        media_ids = media_id
      )

      parsed <- .x_parse_json_response(resp)
      if (httr::http_error(resp)) {
        stop(parsed$raw, call. = FALSE)
      }

      .dispatchr_result(
        ok = TRUE,
        channel = "x",
        action = "post_image",
        request_summary = .dispatchr_compact_list(list(
          text = text,
          image_path = image_path,
          reply_to_id = reply_to_id
        )),
        response = parsed$parsed,
        error = NULL,
        rate_limit = parsed$rate_limit
      )
    },
    error = function(err) {
      .dispatchr_result(
        ok = FALSE,
        channel = "x",
        action = "post_image",
        request_summary = .dispatchr_compact_list(list(
          text = text,
          image_path = image_path,
          reply_to_id = reply_to_id
        )),
        response = NULL,
        error = conditionMessage(err),
        rate_limit = NULL
      )
    }
  )
}

#' Get the Authenticated X Account
#'
#' Secondary helper that retrieves the authenticated account associated with the
#' configured credentials.
#'
#' @param config Optional named list of X credentials.
#'
#' @return Parsed X API response with attached rate-limit metadata.
#' @export
x_get_me <- function(config = NULL) {
  x_config <- .dispatchr_resolve_x_config(config)
  oauth1 <- .x_build_oauth1(x_config)

  resp <- .dispatchr_http_get(
    url = "https://api.x.com/2/users/me",
    oauth1
  )
  httr::stop_for_status(resp)
  out <- httr::content(resp, as = "parsed", encoding = "UTF-8")
  attr(out, "x_rate_limit") <- .x_parse_rate_limit(resp)
  out
}

#' Get the Authenticated Account Timeline
#'
#' Secondary helper that retrieves posts from the authenticated account.
#'
#' @param config Optional named list of X credentials.
#' @param max_results Maximum number of posts to return.
#' @param pagination_token Optional pagination token.
#' @param tweet_fields Fields requested from the X API.
#'
#' @return Parsed X API response with attached rate-limit metadata.
#' @export
x_get_my_timeline <- function(config = NULL,
                              max_results = 100,
                              pagination_token = NULL,
                              tweet_fields = c("created_at", "public_metrics", "text", "lang")) {
  me <- x_get_me(config = config)
  uid <- me$data$id

  query <- list(
    max_results = max_results,
    "tweet.fields" = paste(tweet_fields, collapse = ",")
  )
  if (!is.null(pagination_token)) {
    query$pagination_token <- pagination_token
  }

  resp <- .dispatchr_http_get(
    url = paste0("https://api.x.com/2/users/", uid, "/tweets"),
    .x_build_oauth1(.dispatchr_resolve_x_config(config)),
    query = query
  )
  httr::stop_for_status(resp)
  out <- httr::content(resp, as = "parsed", encoding = "UTF-8")
  attr(out, "x_rate_limit") <- .x_parse_rate_limit(resp)
  out
}

#' Inspect X Rate-Limit Metadata
#'
#' Extracts rate-limit information attached to X API helper results.
#'
#' @param x Object returned by an X helper.
#'
#' @return A rate-limit metadata list or `NULL`.
#' @export
x_rate_limit <- function(x) {
  attr(x, "x_rate_limit", exact = TRUE)
}
