ls_plot_static <- function(df, x_var = V1, y_var = V2, colour = "black"){

  df %>%
    ggplot2::ggplot(aes(x = {{x_var}}, y = {{y_var}})) +
    ggplot2::geom_point(shape = ".", colour = colour) +
    HelpR::theme_microsoft_continuous() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()) +
    ggplot2::labs(x = "",
                  y = "")

}

