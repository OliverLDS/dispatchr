test_that("email config falls back to environment variables", {
  withr::local_envvar(c(
    DISPATCHR_EMAIL_FROM = "sender@example.com",
    DISPATCHR_EMAIL_PASSWORD = "secret",
    DISPATCHR_EMAIL_TRANSPORT = NA
  ))

  config <- dispatchr:::.dispatchr_resolve_email_config()
  expect_equal(config$from, "sender@example.com")
  expect_equal(config$password, "secret")
  expect_equal(config$host, "smtp.gmail.com")
  expect_equal(config$port, 587L)
  expect_equal(config$connecttimeout, 10)
  expect_equal(config$timeout, 30)
  expect_equal(config$max_times, 1)
  expect_false(config$smtp_debug)
  expect_equal(config$email_transport, "emayili")
})

test_that("telegram config requires token and chat id", {
  withr::local_envvar(c(
    DISPATCHR_TELEGRAM_BOT_TOKEN = NA,
    DISPATCHR_TELEGRAM_CHAT_ID = NA
  ))

  expect_error(
    dispatchr:::.dispatchr_resolve_telegram_config(),
    "Missing required Telegram config field"
  )
})
