test_that("ls_link_click adds html code to a permalink", {
  ls_example <- LandscapeR::ls_example %>%
    dplyr::sample_n(20) %>%
    dplyr::mutate(permalink = stringr::str_remove(permalink, "<a href='"),
           permalink = stringr::str_remove(permalink, " target='blank'>Click to View</a"),
           permalink = stringr::str_remove(permalink, "'"))

  #Check it doesn't already have html code
  expect_true(all(!stringr::str_detect(ls_example$permalink, "<a href"))) # *X

  #Now check it does have the code when we run the function
  ls_example <- ls_example %>% ls_link_click(permalink)
  expect_false(all(!stringr::str_detect(ls_example$permalink, "<a href"))) #Check that it's false, that it does not have <a href - so this is a mirror image of *X
  expect_true(all(stringr::str_detect(ls_example$permalink, "<a href")))

})

test_that("ls_link_click modifies the dataframe as expected", {
  # First, create a test dataframe
  df <- data.frame(id = 1:5, url = c("www.google.com", "www.facebook.com", "www.twitter.com", "www.github.com", "www.instagram.com"))


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

test_that("ls_link_click allows a string input and non-string",{
  df <- data.frame(id = 1:5, url = c("www.google.com", "www.facebook.com", "www.twitter.com", "www.github.com", "www.instagram.com"))

  string_input <- df %>% ls_link_click("url")
  non_string_input <- df %>% ls_link_click(url)

  expect_s3_class(non_string_input, "data.frame")
  expect_s3_class(string_input, "data.frame")
  expect_identical(string_input$url, non_string_input$url)
})
