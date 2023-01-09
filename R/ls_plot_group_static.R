#' Create a landscape plot and colour in each level of a grouping variable.
#'
#' Default colour mapping ises the viridis palette, you can simply add different colour maps to the plot using standard ggplot syntax, as the return object is a ggplot object e.g. plot + `scale_colour_manual(values = ...)`
#'
#' @param df Data Frame or Tibble object
#' @param x_var The variable containing x-coordinates
#' @param y_var The variable containing y-coordinates
#' @param group_var The grouping variable to iteratively highlight
#' @param point_size How big each point should be
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' df <- ls_example %>% dplyr::mutate(cluster = factor(cluster))
#' df %>% ls_plot_group_static(group_var = cluster)
ls_plot_group_static <- function(df, x_var = V1, y_var = V2, group_var, point_size = 0.1){
  group_sym <- rlang::ensym(group_var)

  plot <- df %>%
    dplyr::slice_sample(n = nrow(df)) %>% #Do this to randomise which points are atop which other points
    ggplot2::ggplot(ggplot2::aes(x = {{x_var}}, y = {{y_var }}, colour = {{group_var}})) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::theme_bw() +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 4, alpha = 1))) +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid =  ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank()) +
    ggplot2::labs(x = "", y = "")

  return(plot)

}
