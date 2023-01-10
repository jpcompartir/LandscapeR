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
    dplyr::mutate(percentage_plot = var1_var2_n/var1_n * 100) %>%
    dplyr::select({{x_var}}, {{y_var }}, var1_n, var2_n, var1_var2_n, percentage_plot) %>%
    #Remove duplicates (we didn't summarise)
    dplyr::distinct({{x_var}}, {{y_var}}, var1_var2_n, .keep_all = TRUE)

  #Generate plot
  plot <- plotting_table %>%
    ggplot2::ggplot(aes(x = {{x_var}}, y = {{y_var}}, fill = percentage_plot)) +
    ggplot2::geom_tile(color = "white") + #For box trim
    ggplot2::theme_bw() +
    viridis::scale_fill_viridis() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(fill = "% of x in y",
                  subtitle = "Bright colours = high proportion.")

  return(plot)
}

