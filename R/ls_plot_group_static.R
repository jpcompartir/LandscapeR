ls_plot_group_static <- function(df, x_var = V1, y_var = V2, group_var, point_size = 0.1){
  group_sym <- rlang::ensym(group_var)

  plot <- df %>%
    dplyr::slice_sample(n = nrow(df)) %>% #Do this to randomise which points are atop which other points
    ggplot2::ggplot(aes(x = {{x_var}}, y = {{y_var }}, colour = {{group_var}})) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::theme_bw() +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 4, alpha = 1))) +
    ggplot2::scale_colour_viridis_d(option = 'H') +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid =  ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank()) +
    ggplot2::labs(x = "", y = "")

  return(plot)

}
