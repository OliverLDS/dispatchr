test_that("send_email returns structured success result", {
  local_mocked_bindings(
    .email_smtp_send = function(email, config) list(status = "queued"),
    .package = "dispatchr"
  )

  result <- send_email(
    to = "friend@example.com",
    subject = "Hello",
    body = "Hi there",
    config = list(from = "sender@example.com", password = "secret")
  )

  expect_s3_class(result, "dispatchr_result")
  expect_true(result$ok)
  expect_equal(result$channel, "email")
  expect_equal(result$action, "send")
  expect_equal(result$response$status, "queued")
})

test_that("send_email validates required inputs", {
  expect_error(
    send_email(
      to = "",
      subject = "Hello",
      body = "Hi there",
      config = list(from = "sender@example.com", password = "secret")
    ),
    "`to`"
  )
})
