test_that("post_x returns structured success result", {
  fake_response <- structure(
    list(
      status_code = 201L,
      headers = list(
        "x-rate-limit-limit" = "100",
        "x-rate-limit-remaining" = "99",
        "x-rate-limit-reset" = "1700000000"
      ),
      content = charToRaw('{"data":{"id":"123","text":"hello"}}'),
      url = "https://api.x.com/2/tweets"
    ),
    class = "response"
  )

  local_mocked_bindings(
    .dispatchr_http_post = function(...) fake_response,
    .package = "dispatchr"
  )

  result <- post_x(
    text = "hello",
    config = list(
      api_key = "key",
      api_secret = "secret",
      access_token = "token",
      access_secret = "token-secret"
    )
  )

  expect_true(result$ok)
  expect_equal(result$channel, "x")
  expect_equal(result$response$data$id, "123")
  expect_equal(result$rate_limit$remaining, 99)
})
