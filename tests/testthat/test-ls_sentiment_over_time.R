test_that("ls_sentiment_over_time defualt args work", {
  plot <- ls_sentiment_over_time(ls_example, sentiment, date_var = date, unit = "week")
  expect_s3_class(plot, "gg")
  expect_true("sentiment" %in% colnames(plot$data))
})

test_that("ls_sentiment_over_time doesn't render if bad column names provided",{
  expect_error(ls_example %>% ls_sentiment_over_time(sentiment_var = "Sentiment"))
  expect_error(ls_example %>% ls_sentiment_over_time(date_var = "Date"))
  expect_error(ls_example %>% ls_sentiment_over_time(unit = "superxd"))
})
