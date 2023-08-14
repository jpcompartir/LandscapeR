test_that("Input validation works", {
  expect_error(ls_plot_density(ls_example, V1, V2, bins = "hello"),
               regexp = "numeric\\(bins\\)")
  expect_error(ls_plot_density(ls_example, V1, V2, legend_height = "hello"),
               regexp = "numeric\\(legend_height\\)")

  expect_error(ls_plot_density(ls_example, V1, V2, legend_width = "hello"),
               regexp = "numeric\\(legend_width\\)")
})


test_that("Renders a plot and plot has correct values & aesthetics", {

  #If these test fails, check that underlying data - ls_example hasn't been changed as well asls_plot_density()

  plot <- expect_silent(ls_plot_density(ls_example, V1, V2, bins = 50, legend_height = 0.3, legend_width = 2))

  plot_data <- ggplot2::ggplot_build(plot)

  #Check the density is correct in the first place
  expect_equal(plot_data$data[[1]]$count[[1]], 13)

  #And the 20th
  expect_equal(plot_data$data[[1]]$count[[20]], 1)


  #Legend is placed on top by default
  expect_equal(plot_data$plot$guides$fill$title.position, "top")

  #bins = 50 arg is working
  expect_equal(plot_data$plot$layers[[1]]$stat_params$bins, 50)

  #legend_width arg is working
  expect_equal(plot_data$plot$theme$legend.key.width, ggplot2::unit(2, "cm"))

  #legend_height arg is working
  expect_equal(plot_data$plot$theme$legend.key.size, ggplot2::unit(0.3, "cm"))
})
