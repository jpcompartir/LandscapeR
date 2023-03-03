# Volume_time plot function ----
#' Title
#'
#' @param df Data Frame or Tibble object
#' @param .date_var Name of date variable
#' @param unit Unit of time
#' @param fill Colour - string or hexcode
#'
#' @return a ggplot object
#' @export
#'
#' @keywords internal
ls_plot_volume_over_time <- function(df, .date_var, unit = "week", fill = "#0f50d2") {

  date_quo <- rlang::enquo(.date_var)
  date_sym <- rlang::ensym(.date_var)

  df <- df %>% dplyr::mutate(plot_date = lubridate::floor_date(!!date_sym, unit = unit),
                             plot_date = as.Date(plot_date))

  df %>%
    dplyr::count(plot_date) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_date(date_breaks = "1 months", date_labels = "%d-%b") +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 90)
    )
}


# Token plot function ----
#' Title
#'
#' @param df Data Frame or Tibble object
#' @param text_var Name of text variable
#' @param top_n Number of tokens to show
#' @param fill Colour - string or hexcode
#'
#' @return ggplot object
#' @export
#'
#' @keywords internal

ls_plot_tokens_counter <- function(df, text_var = .data$mention_content, top_n = 20, fill = "#0f50d2") {

  text_quo <- rlang::ensym(text_var)

  df %>%
    tidytext::unnest_tokens(words, rlang::quo_name(text_quo)) %>%
    dplyr::count(words, sort = TRUE) %>%
    dplyr::slice_max(order_by = n, n = top_n, with_ties = FALSE) %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(words, n), y = n)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = NULL, y = "Word Count", title = "Bar Chart of Most Frequent Words")+
    ggplot2::theme_bw() +
    ggplot2::labs(x = NULL, y = "Word Count", title = "Bar Chart of Most Frequent Words") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
}

# Download box function ----
#' Title
#'
#' @param exportname Name of export as a string
#' @param plot The plot to download, which should be stored as a reactive.
#'
#' @return A download handler
#' @export
#'
#' @keywords internal
download_box <- function(exportname, plot, width = 300, height = 250) {
  shiny::downloadHandler(
    filename = function() {
      paste(exportname, Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggplot2::ggsave(file,
        plot = plot(), #make sure the reactive object's state is collected here, important!
        device = "png",
        width = width,
        height = height,
        units = "px",
        bg = "white",
        dpi = 100
      )
    }
  )
}
#---- Render Titles ----
#' Title
#'
#' @param plot_type Name of plot type as a string
#'
#' @return ggplot boilerplate code
#' @export
#'
#' @keywords internal
titles_render <- function(plot_type, input) {
  plot_type_title <- stringr::str_to_title(plot_type)

  shiny::renderUI({
    if (input[[paste0("toggle", plot_type_title, "titles")]]) {
      shiny::tagList(
        shiny::textInput(
          inputId = paste0(plot_type, "Title"), label = "Title",
          placeholder = "Write title here...", value = ""
        ),
        shiny::textInput(
          inputId = paste0(plot_type, "Subtitle"), label = "Subtitle",
          placeholder = "Write subtitle here...", value = ""
        ),
        shiny::textInput(
          inputId = paste0(plot_type, "Caption"), label = "Caption",
          placeholder = "Write caption here...", value = ""
        ),
        shiny::textInput(
          inputId = paste0(plot_type, "Xlabel"), label = "X axis title",
          placeholder = "Write the x axis title here..."
        ),
        shiny::textInput(
          inputId = paste0(plot_type, "Ylabel"), label = "Y axis title",
          placeholder = "Write the y axis title here"
        )
      )
    }
  })
}


#---- plot sentiment distribution ---- TODO add percent option
#' Title
#'
#' @param df Data Frame or Tibble Object
#' @param sentiment_var Name of sentiment variable =
#'
#' @return ggplot object
#' @export
#'
#' @keywords internal
ls_plot_sentiment_distribution <- function(df, sentiment_var = sentiment) {
  df %>%
    dplyr::filter({{ sentiment_var }} %in% c(
      "positive", "negative", "neutral", "POSITIVE", "NEGATIVE",
      "NEUTRAL", "Neutral", "Negative", "Positive"
    )) %>%
    dplyr::count({{ sentiment_var }}) %>%
    dplyr::rename(sentiment = 1) %>%
    dplyr::mutate(sentiment = tolower(sentiment)) %>%
    ggplot2::ggplot(ggplot2::aes(x = sentiment, y = n, fill = sentiment)) +
    ggplot2::geom_col() +
    ggplot2::theme_minimal() +
    HelpR::theme_microsoft_discrete() +
    ggplot2::ggplot(ggplot2::aes(x = sentiment, y = n, fill = sentiment)) +
    ggplot2::geom_col() +
    ggplot2::theme_minimal() +
    HelpR::theme_microsoft_discrete() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold"
      ),
      legend.position = "none"
    )
}

#' Prepare a URL column to be clickable in Shiny/Data Table
#'
#' Will allow you to click the hyperlink to load a URL, e.g. for selecting an image.
#' Make sure that DataTable is rendered with the argument 'escape = FALSE' or column will be all text.
#'
#' @param df Data Farame or Tibble Object
#' @param url_var URL Column
#'
#' @return data frame with URL column edited to be clickable
#' @export
#' @keywords internal
#'
ls_link_click <- function(df, url_var) {

  url_sym <- rlang::ensym(url_var)

  df %>%
    dplyr::mutate({{ url_var }} := paste0("<a href='", !!url_sym, "' target='blank'>", "Click to View", "</a>"))
}


#' Quick function for checking if a column is of the right type using data-masking
#'
#' @param data Data Frame or Tibble object
#' @param column Column you want to check
#' @param type `column`'s expected type
#'
#' @return a character vector
#' @export
#'
#' @examples
#' \dontrun{
#' check_text <- df %>% column_type_checker(text_var, "character")
#'
#' if (check_text == "no") stop("Wrong type")
#' }
column_type_checker <- function(data,
                                column,
                                type) {
  if (!all(class(data %>% dplyr::pull({{ column }})) == type)) {
    return("no")
  } else {
    return("yes")
  }
}


#' Programmatically generate reactive labels from a prefix
#'
#' @param prefix type of plot e.g. 'sentiment'
#' @param input reading from Shiny's server `input` object - the list of all inputs.
#'
#' @return ggplot boiler-plate code with dynamically generated inputs
#' @export
#'
reactive_labels <- function(prefix, input) {
  shiny::reactive({
    ggplot2::labs(
      title = input[[paste0(prefix, "Title")]],
      caption = input[[paste0(prefix, "Caption")]],
      subtitle = input[[paste0(prefix, "Subtitle")]],
      x = input[[paste0(prefix, "Xlabel")]],
      y = input[[paste0(prefix, "Ylabel")]]
    )
  })
}



#' Tibble of examples for LandscapeR functions & tests
#'
#'
#' @format A tibble with 10 rows and 5 columns
#' \describe{
#'   \item{document}{Post ID}
#'   \item{text}{Text variable for data table}
#'   \item{permalink}{Link to original post}
#'   \item{clean_text}{cleaner version of text variable for plots}
#'   \item{date}{Date variable in ymd_hms format}
#'   \item{setniment}{Sentiment variable}
#'   \item{V1}{x co-ordinates}
#'   \item{V2}{y co-ordinates}
#' }
#' @docType data
#' @name ls_example
#' @usage data("ls_example")
#' @keywords internal
"ls_example"

#' A LandscapeR version of ParseR's Weighted Log-odds
#'
#' Function should be used for identifying the differences between levels of a
#' grouping variable.
#'
#' @param df Data Frame or Tibble object
#' @param group_var The variable to group with e.g. topic, sentiment
#' @param text_var Your text variable
#' @param top_n Number of terms per plot
#' @param nrow Number of rows to display the plots across
#' @param top_terms_cutoff The top x words which should have WLOs calculated for them
#' @param text_size An integer determining text size, higher = larger
#'
#' @return a ggplot object
#' @export
#'
ls_wlos <- function(df,
                    group_var = cluster,
                    text_var = clean_text,
                    top_n = 30,
                    text_size = 4,
                    nrow = 4,
                    top_terms_cutoff = 5000){

    text_quo <- rlang::enquo(text_var)
    group_quo <- rlang::enquo(group_var)

    wlos <- df %>%
      tidytext::unnest_tokens(word, !!text_quo) %>%
      dplyr::rename(facet_var = !!group_quo) %>%
      dplyr::mutate(facet_var = factor(facet_var)) %>%
      dplyr::group_by(facet_var) %>%
      dplyr::count(word, sort = TRUE) %>%
      dplyr::ungroup() %>%
      tidylo::bind_log_odds(set = facet_var,
                            feature = word,
                            n = n)

    viz <- wlos %>%
      dplyr::slice_max(order_by = n,
                       n = top_terms_cutoff) %>%
      dplyr::group_by(facet_var) %>%
      dplyr::top_n(n = top_n,
                   wt = log_odds_weighted) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot(ggplot2::aes(x = n,
                                   y = log_odds_weighted,
                                   label = word)) +
      ggplot2::geom_hline(yintercept = 0,
                          lty = 2,
                          color = "gray50",
                          alpha = 0.5,
                          linewidth = 1.2) +
      ggrepel::geom_text_repel(size = text_size,
                               segment.size = 0.5,
                               color = 'black',
                               bg.color = "white") +
      ggplot2::geom_point(size = .4,
                          show.legend = F) +
      ggplot2::facet_wrap(~facet_var,
                          nrow = nrow,
                          scales = "free") +
      ggplot2::scale_x_log10() +
      ggplot2::labs(x = "Word frequency",
                    y = "Log odds ratio, weighted by uninformative Dirichlet prior") +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill="gray"))+
      ggplot2::theme_bw() +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white",
                                                              colour = "white"),
                     strip.text = ggplot2::element_text(face = "bold"),
                     panel.grid.minor = ggplot2::element_blank())

    return(viz)
}
