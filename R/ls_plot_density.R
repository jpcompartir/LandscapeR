#' @title Landscape hexplot coloured by density
#'
#' @description
#' An alternative to the ls_plot_static() function. The main benefit of the density plot is that regions od the landscape are coloured according to frequency, making it easier to detect potentially-meaningful clusters via visual inspection.
#'
#' @details
#' Setting bins to a lower value will mean the counts are higher, but the overall resolution of the plot is lower. The legend_* arguments specify the aesthetic appearance of the legend. You can override these parameters by simply calling the function and adding ggplot theme() and guides() inputs
#'
#' The data frame should be in un-summarised or long format, i.e. one row per document.
#'
#' @param df Data frame or tibble object
#' @param x_var Variable with your co-ordinates for the x-axis
#' @param y_var Variable with your co-ordinates for the y-axis
#' @param bins numeric vector giving number of bins in both vertical and horizontal directions. Set to 100 by default.
#' @param legend_height value in centimetres (decimals allowed)
#' @param legend_width value in centimetres (decimals allowed)
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' ls_example %>%
#'  ls_plot_density(V1, V2, bins = 150)
#'
#' #Change the legend:
#' ls_example %>%
#'  ls_plot_density(V1, V2, bins = 75) +
#'  ggplot2::theme(legend.position = "right", legend.key.width = ggplot2::unit(0.25, "cm"), legend.key.height = ggplot2::unit(1.5, "cm")) +
#' ggplot2::guides(fill = ggplot2::guide_colorbar(title.position = "right"))
ls_plot_density <- function(df, x_var, y_var, bins = 100, legend_height = 0.25, legend_width = 1.5) {

  stopifnot(
    is.numeric(bins) && bins > 0,
    is.numeric(legend_height) && legend_height > 0 && legend_height < 3,
    is.numeric(legend_width) && legend_width > 0.25 && legend_width < 5)

  x_sym <- rlang::ensym(x_var)
  y_sym <- rlang::ensym(y_var)

  plot <- df %>%
    ggplot2::ggplot(ggplot2::aes(x= !!x_sym, y = !!y_sym)) +
    ggplot2::geom_hex(bins = bins) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(fill = "Density") +
    ggplot2::theme(legend.key.size = ggplot2::unit(legend_height, "cm"),
                   legend.key.width = ggplot2::unit(legend_width, "cm"),
                   legend.text.align = 0.5,
                   legend.justification = "center",
                   legend.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(title.position = "top"))

  return(plot)
}
