#' View a sentiment and volume over time chart
#'
#' @param df Data Frame or Tibble object
#' @param sentiment_var  Name of your sentiment variable
#' @param date_var Name of your date variable
#' @param unit Time unit to count sentiment by
#'
#' @return a ggplot object
#' @export
#'
ls_sentiment_over_time <- function(df,
                                   sentiment_var = sentiment,
                                   date_var = date,
                                  unit = c("week", "day","month", "quarter", "year")){

  unit <- match.arg(unit)

  sent_sym <- rlang::ensym(sentiment_var)
  date_sym <- rlang::ensym(date_var)

  sent_string <- rlang::as_string(sent_sym)
  date_string <- rlang::as_string(date_sym)

if(!sent_string %in% colnames(df)){
  stop(paste0("Cannot find '", sent_string, "' in the data frame, did you mean `sentiment_var = sentiment`?"))
}
if(!date_string %in% colnames(df)){
  stop(paste0("Cannot find '", date_string, "' in the data frame, did you mean `date_var = date`?"))
}



  df <- df %>% dplyr:: mutate(
    plot_date = as.Date(!!date_sym),
    plot_date = lubridate::floor_date(plot_date, unit = unit))

  plot <- df %>%
    dplyr::count(plot_date,!!sent_sym) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n, fill = !!sent_sym)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_date(date_breaks = "1 months", date_labels = "%d-%b") +
    ggplot2::scale_fill_manual(aesthetics = c("fill", "colour"),
                               values = c("positive" = "#107C10",
                                          "negative" = "#D83B01",
                                          "neutral" = "#FFB900")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major = ggplot2::element_blank()) +
    ggplot2::labs(y = "n", x = paste0("Date by ", unit))

  return(plot)
}
