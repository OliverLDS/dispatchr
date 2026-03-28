test_that("send_telegram returns structured success result", {
  fake_response <- structure(
    list(
      status_code = 200L,
      headers = list(),
      content = charToRaw('{"ok":true,"result":{"message_id":1}}'),
      url = "https://api.telegram.org"
    ),
    class = "response"
  )

  local_mocked_bindings(
    .dispatchr_http_post = function(...) fake_response,
    .package = "dispatchr"
  )

  result <- send_telegram(
    text = "hello",
    config = list(bot_token = "token", chat_id = "chat")
  )

  expect_true(result$ok)
  expect_equal(result$channel, "telegram")
  expect_equal(result$response$result$message_id, 1)
})
