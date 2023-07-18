test_that("ls_plot_group_facet is working as intended", {
  df <- ls_example

  # Check badly input arguments raise error
  expect_error(ls_plot_group_facet(df, x = v1))
  expect_error(ls_plot_group_facet(df, y = "blank"))

  # Check plot doesn't render if group_var isn't set
  expect_error(ls_plot_group_facet(df, nrow = 3))

  # outputs are returned properly
  plot <- ls_plot_group_facet(ls_example, group_var = cluster)
  expect_true("patchwork" %in% class(plot))
  plot <- ls_plot_group_facet(ls_example, group_var = cluster, output = "list_of_plots")
  expect_equal(length(plot), 7)
  expect_type(plot, "list")
  plot <- ls_plot_group_facet(ls_example, group_var = cluster, output = "wrapped_and_list")
  expect_true("wrapped" %in% names(plot) & "list_plots" %in% names(plot))
})
