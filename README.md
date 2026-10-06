# dispatchr

`dispatchr` is a lightweight outbound communication package for agentic workflows.

Current channels:

- Email via Gmail SMTP using `emayili`
- X posting with OAuth 1.0a signing
- Telegram bot messaging

The package is intentionally narrow. It focuses on sending and posting rather than
building a full communication framework.

## Installation

```r
# install.packages("pak")
pak::pak("OliverLDS/dispatchr")
```

## Configuration

Each user-facing function accepts an explicit `config = list(...)`. Missing fields
are filled from environment variables when available.

### Email

Required fields:

- `from`
- `password`

Optional fields:

- `host` defaults to `smtp.gmail.com`
- `port` defaults to `587`
- `connecttimeout` defaults to `10` seconds
- `timeout` defaults to `30` seconds
- `max_times` defaults to `1` attempt

Environment variables:

- `DISPATCHR_EMAIL_FROM`
- `DISPATCHR_EMAIL_PASSWORD`
- `DISPATCHR_EMAIL_HOST`
- `DISPATCHR_EMAIL_PORT`
- `DISPATCHR_EMAIL_CONNECTTIMEOUT`
- `DISPATCHR_EMAIL_TIMEOUT`
- `DISPATCHR_EMAIL_MAX_TIMES`

### X

Required fields:

- `api_key`
- `api_secret`
- `access_token`
- `access_secret`

Environment variables:

- `DISPATCHR_X_API_KEY`
- `DISPATCHR_X_API_SECRET`
- `DISPATCHR_X_ACCESS_TOKEN`
- `DISPATCHR_X_ACCESS_SECRET`

### Telegram

Required fields:

- `bot_token`
- `chat_id`

Optional fields:

- `parse_mode`

Environment variables:

- `DISPATCHR_TELEGRAM_BOT_TOKEN`
- `DISPATCHR_TELEGRAM_CHAT_ID`
- `DISPATCHR_TELEGRAM_PARSE_MODE`

## Examples

```r
library(dispatchr)

send_email(
  to = "friend@example.com",
  subject = "Hello",
  body = "Sent from dispatchr",
  config = list(
    from = Sys.getenv("DISPATCHR_EMAIL_FROM"),
    password = Sys.getenv("DISPATCHR_EMAIL_PASSWORD")
  )
)
```

```r
post_x(
  text = "Hello from dispatchr",
  config = list(
    api_key = Sys.getenv("DISPATCHR_X_API_KEY"),
    api_secret = Sys.getenv("DISPATCHR_X_API_SECRET"),
    access_token = Sys.getenv("DISPATCHR_X_ACCESS_TOKEN"),
    access_secret = Sys.getenv("DISPATCHR_X_ACCESS_SECRET")
  )
)
```

```r
send_telegram(
  text = "Hello from dispatchr",
  config = list(
    bot_token = Sys.getenv("DISPATCHR_TELEGRAM_BOT_TOKEN"),
    chat_id = Sys.getenv("DISPATCHR_TELEGRAM_CHAT_ID")
  )
)
```

## Return values

The main send/post functions return a structured list with:

- `ok`
- `channel`
- `action`
- `request_summary`
- `response`
- `error`
- `rate_limit`

Email results also include `message_id`, the `Message-ID` assigned to the
outgoing message. Pass a previously stored ID as `reply_to_message_id` to set
the standard `In-Reply-To` and `References` headers when replying. `dispatchr`
does not retrieve the original message.

## Scope notes

Telegram chat sync is deliberately not part of the core exported API in this
version. The package is currently centered on outbound delivery only.
