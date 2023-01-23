#' Create a landscape plot without segmenting by groups
#'
#' @param df Data Frame or Tibble object
#' @param x_var The variable containing x-coordinates
#' @param y_var The variable containing y-coordinates
#' @param fill_colour What colour the points should be
#'
#' @return a ggplot obje t
#' @export
#'
#' @examples
#' df <- ls_example
#' df %>% ls_plot_group_static()
ls_plot_static <- function(df, x_var = V1, y_var = V2, fill_colour = "black") {
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = {{ x_var }}, y = {{ y_var }})) +
    ggplot2::geom_point(shape = ".", colour = fill_colour) +
    HelpR::theme_microsoft_continuous() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "",
      y = ""
    )
}
