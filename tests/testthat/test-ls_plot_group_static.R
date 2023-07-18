# Create a sample data frame
df <- data.frame(V1 = 1:10, V2 = 11:20, group = c("Group A", "Group B", "Group A", "Group B", "Group A", "Group B", "Group A", "Group B", "Group A", "Group B"))

# Test that the function creates a ggplot object
test_that("ls_plot_group_static returns a ggplot object", {
  expect_s3_class(ls_plot_group_static(df, group_var = group), "gg")
})

# Test that the function uses the correct data
test_that("ls_plot_group_static uses the correct data", {
  plot_data <- ggplot2::ggplot_build(ls_plot_group_static(df, group_var = group))$data
  expect_equal(sort(plot_data[[1]]$x), sort(df$V1))
  expect_equal(sort(plot_data[[1]]$y), sort(df$V2))
})

# Test that the function sets the x and y axis labels to be empty
test_that("ls_plot_group_static sets the x and y axis labels to be empty", {
  plot <- ls_plot_group_static(df, group_var = "group")
  expect_equal(plot$labels$x, "")
  expect_equal(plot$labels$y, "")
})

# Test that the function uses the correct point size
test_that("ls_plot_group_static uses the correct point size", {
  plot_point_size <- unique(ggplot2::ggplot_build(ls_plot_group_static(df, group_var = "group"))$data[[1]]$size)
  expect_equal(plot_point_size, 0.1)
})

# Test that the function sets the legend position to be "bottom"
test_that("ls_plot_group_static sets the legend position to be bottom", {
  plot <- ls_plot_group_static(df, group_var = "group")
  expect_equal(plot$theme$legend.position, "bottom")
})

# Test that the function sets the axis labelsto be blank
test_that("ls_plot_group_static sets the axis labels to be blank", {
  plot <- ls_plot_group_static(df, group_var = "group")
  expect_equal(plot$labels$x, "")
  expect_equal(plot$labels$y, "")
})
