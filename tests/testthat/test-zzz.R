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
