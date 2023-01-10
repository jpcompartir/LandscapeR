df <- ls_example

# Test that the function creates a ggplot object
test_that("ls_plot_static returns a ggplot object", {
  expect_s3_class(ls_plot_static(df), "gg")
})

# Test that the function uses the correct data
test_that("ls_plot_static uses the correct data", {
  plot_data <- ggplot2::ggplot_build(ls_plot_static(df))$data
  expect_equal(plot_data[[1]]$x, df$V1)
  expect_equal(plot_data[[1]]$y, df$V2)
})

# Test that the function uses the correct fill colour
test_that("ls_plot_static uses the correct fill colour", {
  plot_data <- ggplot2::ggplot_build(ls_plot_static(df))$data
  expect_equal(plot_data[[1]]$colour[[1]], "black")
})

test_that("ls_plot_static sets the x and y axis labels to be empty", {
  plot <- ls_plot_static(df)
  expect_equal(plot$labels$x, "")
  expect_equal(plot$labels$y, "")
})


# Test that the function sets the legend position to be "none"
test_that("ls_plot_static sets the legend position to be none", {
  plot <- ls_plot_static(df)
  expect_equal(plot$theme$legend.position, "none")
})
