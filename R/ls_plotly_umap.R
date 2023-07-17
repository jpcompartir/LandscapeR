#' Render a landscape plot inside a shiny app
#'
#' Renders an interactive plotly plot with a
#'
#' @section Calling the function:
#' Due to plotly's non-standard evaluation you'll need to feed in any column
#' names, such as `color`, `x`, `y`, `text`, `key`, as strings.
#'
#' @param data Data Frame or tibble object, will likely need to be called
#' with brackets as it should be reactive
#' @param x X co-ordinate variable (as a string)
#' @param y Y co-ordinate variable (as a string)
#' @param type type of plotly chart to render, usually 'scattergl' for perforamcne
#' @param group_var Which variable to colouyr by (as a string)
#' @param key ID column (as a string)
#' @param text_var Text variable (as a string)
#' @param height Height of plot (as a strong)
#' @param width Width of plot (as a string)
#'
#' @return a plotly object
#'
#' @examples
#' \dontrun{
#' LandscapeR::ls_example %>%
#' dplyr::mutate(cluster = factor(cluster)) %>%
#'  ls_plotly_umap(x = "V1", y = "V2", type = "scattergl",
#'  key = "document", text_var = "text", group_var = "cluster",  height = 600, width = 800)
#'  }
ls_plotly_umap <- function(data, x, y, type, group_var, key, text_var,
                           height, width){

  y <- data[[y]]
  x <- data[[x]]
  text_var <- data[[text_var]]
  group_var <- data[[group_var]]

  axis_config <- list(showgrid = FALSE,
       showline = FALSE,
       zeroline = FALSE,
       linewidth = 0,
       tickwidth = 0,
       showticklabels = FALSE,
       title = "")

  data %>%
    plotly::plot_ly(x = x,
            y = y,
            type = type,
            color = group_var,
            key = key,
            text = ~ paste("<br> Post:", text_var),
            hoverinfo = "text",
            marker = list(size = 2),
            height = height,
            width = width) %>%
    plotly::layout(
      dragmode = "lasso",
      legend = list(itemsizing = "constant"),
      xaxis = axis_config,
      yaxis = axis_config,
      newshape=list(fillcolor="#ff5718", #Colour for shapes
                    linecolor = "black",
                    opacity=0.2)
    ) %>%
    plotly::config(
      displaylogo = FALSE,
      edits = list(shapePosition = TRUE,
                   annotation = TRUE), #Allow shapes to be edited
      # editable = TRUE,
      modeBarButtonsToAdd = #Allow drawing on shapes in app
        list(
          "drawline",
          "drawcircle",
          "drawrect",
          "eraseshape")) %>%
    plotly::event_register(event = "plotly_selected")
}

# library(tidyverse)
# LandscapeR::ls_example %>%
#   dplyr::mutate(cluster = factor(cluster)) %>%
#   ls_plotly_umap(x = "V1", y = "V2", type = "scattergl", key = "document", text_var = "text", height = 600, group_var = "cluster", width = 800)
