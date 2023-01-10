#' Compare proportion of x in y
#'
#' Use this function to compare two grouping variables, for example to see what percentage of each cluster fall into each sentiment category (by volume).
#'
#' @param df Data Frame or Tibble object
#' @param x_var The grouping variable for the x axis.
#' @param y_var The grouping variable for the y axis.
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' ls_example %>% ls_plot_variation_matrix(cluster, sentiment)
ls_plot_variation_matrix <- function(df, x_var, y_var){
  requireNamespace("viridis")

  x_sym <- rlang::ensym(x_var)
  y_sym <- rlang::ensym(y_var)

  plotting_table <- df %>%
    #Coerce variables to factor:
    dplyr::mutate({{x_var}} := factor(!!x_sym),
                  {{y_var}} := factor(!!y_sym)) %>%
    #Count our variables separately (don't really need var2_n but...)
    dplyr::add_count({{x_var}}, name = "var1_n") %>%
    dplyr::add_count({{y_var}}, name = "var2_n") %>%
    #Count x in y
    dplyr::add_count({{x_var}}, {{y_var}}, name = "var1_var2_n") %>%
    #Get percentage of x in y
    dplyr::mutate(percentage_plot = .data$var1_var2_n/.data$var1_n * 100) %>%
    dplyr::select({{x_var}}, {{y_var }}, .data$var1_n, .data$var2_n, .data$var1_var2_n, .data$percentage_plot) %>%
    #Remove duplicates (we didn't summarise)
    dplyr::distinct({{x_var}}, {{y_var}}, .data$var1_var2_n, .keep_all = TRUE)

  #Generate plot
  plot <- plotting_table %>%
    ggplot2::ggplot(ggplot2::aes(x = {{x_var}}, y = {{y_var}}, fill = .data$percentage_plot)) +
    ggplot2::geom_tile(color = "white") + #For box trim
    ggplot2::theme_bw() +
    viridis::scale_fill_viridis() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(fill = "% of x in y",
                  subtitle = "Bright colours = high proportion.")

  return(plot)
}

