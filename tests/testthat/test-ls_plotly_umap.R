test_that("function raises an error if arguments not called correctly", {
  expect_error(
    LandscapeR::ls_plotly_umap(
      data = LandscapeR::ls_example,
      x = V1,
      y = V2,
      type = "scattergl",
      group_var = "cluster",
      key = "document",
      text_var = "text",
      height = 600,
      width = 600
    ))
})

test_that('function returns a plotly object when called with correct arguments', {
  my_plotly <- LandscapeR::ls_plotly_umap(
    data = LandscapeR::ls_example,
    x = "V1",
    y = "V2",
    type = "scattergl",
    group_var = "cluster",
    key = "document",
    text_var = "text",
    height = 600,
    width = 600
  )
  expect_s3_class(my_plotly, "plotly")
})

