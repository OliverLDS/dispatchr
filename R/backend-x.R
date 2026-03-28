.x_parse_rate_limit <- function(resp) {
  h <- httr::headers(resp)

  get_num <- function(name) {
    val <- h[[name]]
    if (is.null(val) || is.na(val) || identical(val, "")) {
      return(NA_real_)
    }
    as.numeric(val)
  }

  limit <- get_num("x-rate-limit-limit")
  remaining <- get_num("x-rate-limit-remaining")
  reset_epoch <- get_num("x-rate-limit-reset")
  reset_time <- if (is.na(reset_epoch)) {
    NA
  } else {
    as.POSIXct(reset_epoch, origin = "1970-01-01", tz = "UTC")
  }

  list(
    limit = limit,
    remaining = remaining,
    reset_epoch = reset_epoch,
    reset_time = reset_time
  )
}

.x_text_width <- function(x, url_length = 23L) {
  url_pattern <- "(https?://[^[:space:]]+)"
  m <- gregexpr(url_pattern, x, perl = TRUE)[[1]]
  n_urls <- if (m[1] == -1L) 0L else length(m)
  x_no_urls <- gsub(url_pattern, "", x, perl = TRUE)

  chars <- strsplit(x_no_urls, "")[[1]]
  if (length(chars) == 0L) {
    return(n_urls * url_length)
  }

  base_count <- sum(ifelse(nchar(chars, type = "width") > 1, 2L, 1L))
  base_count + n_urls * url_length
}

.x_validate_text <- function(text) {
  .dispatchr_validate_scalar_string(text, "text")
}

.x_build_oauth1 <- function(config) {
  httr::sign_oauth1.0(
    app = httr::oauth_app("x", key = config$api_key, secret = config$api_secret),
    token = config$access_token,
    token_secret = config$access_secret
  )
}

.x_split_thread_text <- function(text) {
  .x_validate_text(text)

  sentences <- trimws(paste0(regmatches(text, gregexpr("[^。]+", text))[[1]], "。"))
  if (length(sentences) == 0L) {
    sentences <- text
  }

  posts <- character()
  index <- 1L

  for (sentence in sentences) {
    if (.x_text_width(sentence) > 280) {
      stop("A single sentence exceeds X's 280-character weighted limit.", call. = FALSE)
    }

    proposed <- paste0(posts[index], sentence)
    if (!nzchar(posts[index]) || .x_text_width(proposed) <= 280) {
      posts[index] <- proposed
    } else {
      index <- index + 1L
      posts[index] <- sentence
    }
  }

  posts[nzchar(posts)]
}

.x_parse_json_response <- function(resp) {
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(...) list(raw = txt)
  )

  list(
    parsed = parsed,
    raw = txt,
    rate_limit = .x_parse_rate_limit(resp)
  )
}

.x_post_tweet_request <- function(text, oauth1, reply_to_tweet_id = NULL, media_ids = NULL) {
  body <- list(text = text)

  if (!is.null(reply_to_tweet_id)) {
    body$reply <- list(in_reply_to_tweet_id = as.character(reply_to_tweet_id))
  }

  if (!is.null(media_ids)) {
    body$media <- list(media_ids = as.list(media_ids))
  }

  .dispatchr_http_post(
    url = "https://api.x.com/2/tweets",
    oauth1,
    body = body,
    encode = "json"
  )
}

.x_upload_media <- function(oauth1, filepath) {
  resp <- .dispatchr_http_post(
    "https://upload.twitter.com/1.1/media/upload.json",
    oauth1,
    body = list(media = httr::upload_file(filepath))
  )

  httr::stop_for_status(resp)
  httr::content(resp, as = "parsed", encoding = "UTF-8")$media_id_string
}
