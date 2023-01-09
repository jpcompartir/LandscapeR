ls_plot_group_facet <- function(df, x_var = V1, y_var = V2, group_var, nrow, fill_colour = "blue",
                                output = c("wrapped_plots", "list_of_plots", "wrapped_and_list")){

  output <- match.arg(if (missing(output)) "wrapped_plots" else output, c("wrapped_plots", "list_of_plots", "wrapped_and_list"))

  group_sym <- rlang::ensym(group_var)

  group_names <- df %>% dplyr::pull({{group_var}}) %>% unique() %>% sort()

  facet_plots <- purrr::map(
    group_names,
    ~ df %>%
      dplyr::slice_sample(n = nrow(df)) %>%
      dplyr::mutate(plotting_var = as.character({{group_var}}),
                    highlight = ifelse(plotting_var == .x, 'A', 'B')) %>%
      ggplot2::ggplot(aes(x = {{x_var}}, y = {{y_var}}, colour = highlight, alpha = highlight)) +
      ggplot2::geom_point(shape = ".") +
      ggplot2::theme_bw() +
      ggplot2::scale_colour_manual(values = c(
        "A" = fill_colour,
        "B" = "grey80")) +
      ggplot2::scale_alpha_manual(values = c("A" = 1,
                                    "B" = 0.5)) +
      ggplot2::theme(
        legend.position = "none",
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
      ggplot2::labs(title = paste0(.x),
           x = "",
           y = "")
  )

  wrapped_plots <- patchwork::wrap_plots(facet_plots, nrow = nrow)

  names(facet_plots) <- group_names

  if(output == "wrapped_plots"){
    return(wrapped_plots)
  }

  if(output == "list_of_plots"){
    return(facet_plots)
  }

  if(output == "wrapped_and_list"){
    return(list(wrapped = wrapped_plots, list_plots = facet_plots))
  }

}

