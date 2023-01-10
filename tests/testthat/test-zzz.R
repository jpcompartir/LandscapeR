# Create a sample data frame
df <- data.frame(V1 = 1:10, V2 = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"))

# Test that the function returns "yes" for a column with the correct data type
test_that("column_type_checker returns yes for correct data type", {
  result <- column_type_checker(df, "V1", "integer")
  expect_equal(result, "yes")
})

# Test that the function returns "no" for a column with the incorrect data type
test_that("column_type_checker returns no for incorrect data type", {
  result <- column_type_checker(df, "V1", "character")
  expect_equal(result, "no")
})

# Test that the function returns "no" for a non-existent column
test_that("column_type_checker does not return for non-existent column", {
  expect_error(column_type_checker(df, "V3", "numeric"))
})


# First, create a test dataframe
df <- data.frame(id = 1:5, url = c("www.google.com", "www.facebook.com", "www.twitter.com", "www.github.com", "www.instagram.com"))


test_that("ls_link_click modifies the dataframe as expected", {
  # Test that the function modifies the dataframe
  expect_s3_class(ls_link_click(df, "url"), "data.frame")

  df <- ls_link_click(df, url)
  # Test that the new column contains the correct values
  expect_equal(df$url[1], "<a href='www.google.com' target='blank'>Click to View</a>")
  expect_equal(df$url[2], "<a href='www.facebook.com' target='blank'>Click to View</a>")
  expect_equal(df$url[3], "<a href='www.twitter.com' target='blank'>Click to View</a>")
  expect_equal(df$url[4], "<a href='www.github.com' target='blank'>Click to View</a>")
  expect_equal(df$url[5], "<a href='www.instagram.com' target='blank'>Click to View</a>")

  # test the url_var doesn't exist in the dataframe
  expect_error(ls_link_click(df, "new_url"))
})
