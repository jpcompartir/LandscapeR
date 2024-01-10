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

  df <- df %>% dplyr::mutate(
    plot_date = lubridate::floor_date(!!date_sym, unit = unit),
    plot_date = as.Date(plot_date)
  )

  df %>%
    dplyr::count(plot_date) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_date(date_breaks = "1 months", date_labels = "%d-%b") +
    DisplayR::dr_theme_microsoft(scale_type = "continuous") +
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
    ggplot2::labs(x = NULL, y = "Word Count", title = "Bar Chart of Most Frequent Words") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = NULL, y = "Word Count", title = "Bar Chart of Most Frequent Words") +
    DisplayR::dr_theme_microsoft(scale_type = "continuous") +
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
      ggplot2::ggsave(
        file,
        plot = plot(), # make sure the reactive object's state is collected here, important!
        device = "png",
        width = width(),
        height = height(),
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


#---- plot sentiment distribution ----
#TODO add percent option
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
    DisplayR::dr_theme_microsoft(scale_type = "discrete") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold"
      ),
      legend.position = "none"
    )
}


#ls_link_click ----
#' Prepare a URL column to be clickable in Shiny/Data Table
#'
#' Will allow you to click the hyperlink to load a URL, e.g. for selecting an image.
#' Make sure that DataTable is rendered with the argument 'escape = FALSE' or column will be all text.
#'
#' @param df Data Frame or Tibble Object
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

# column_type_checker ----
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


# reactive_labels ----
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

#ls_wlos ----
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
#' @param filter_by whether to perform initial filtering by frequency or association
#'
#' @return a ggplot object
#' @export
#'
ls_wlos <- function(df,
                    group_var = cluster,
                    text_var = clean_text,
                    top_n = 30,
                    text_size = 4,
                    filter_by = c("association", "frequency"),
                    nrow = 4,
                    top_terms_cutoff = 5000) {

  filter_by <- match.arg(filter_by)

  group_quo <- group_var # Weird tidy eval pattern when the columns are being generated dynamically with updateSelectInput. First get the variable (in this case a string) then convert to symbol.
  text_quo <- rlang::ensym(text_var)
  group_quo <- rlang::ensym(group_var)

  wlos <- df %>%
    tidytext::unnest_tokens(word, !!text_quo) %>%
    dplyr::count(!!group_quo, word) %>%
    dplyr::rename(facet_var = !!group_quo) %>%
    dplyr::mutate(facet_var = factor(facet_var)) %>%
    tidylo::bind_log_odds(set = facet_var, feature = word, n = n)

  if (filter_by == "association") {
    wlos <- wlos %>%
      dplyr::slice_max(order_by = n, n = top_terms_cutoff) %>%
      # Select the top_n words with the highest log odds ratio within each topic
      dplyr::slice_max(order_by = log_odds_weighted, n = top_n, by = facet_var, with_ties = FALSE)
  } else {
    wlos <- wlos %>%
      dplyr::slice_max(n = top_n, order_by = n, by = facet_var, with_ties = FALSE)
  }

  wlos <- wlos %>%
    dplyr::arrange(dplyr::desc(n))


  viz <- wlos %>%
    ggplot2::ggplot(ggplot2::aes(
      x = n,
      y = log_odds_weighted,
      label = word
    )) +
    ggplot2::geom_hline(
      yintercept = 0,
      lty = 2,
      color = "gray50",
      alpha = 0.5,
      linewidth = 1.2
    ) +
    ggrepel::geom_text_repel(
      size = text_size,
      segment.size = 0.5,
      color = "black",
      bg.color = "white"
    ) +
    ggplot2::geom_point(
      size = .4,
      show.legend = FALSE
    ) +
    ggplot2::facet_wrap(~facet_var,
                        nrow = nrow,
                        scales = "free"
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = "Word frequency",
      y = "Log odds ratio, weighted by uninformative Dirichlet prior"
    ) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "gray")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(
        fill = "white",
        colour = "white"
      ),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(viz)
}

#ls_plot_group_sent ----
#' Quickly plot group sentiment distributions as a percentage in LandscapeR shiny app
#'
#' @param df data frame
#' @param group_var Grouping variable, e.g. country, topic, cluster
#' @param sentiment_var Sentiment variable (categorical)
#' @param type Whether the plot should be of volume or py percentage. Accepts "percent" or "volume"
#' @param title The title of the plot, entered as a string.
#' @param bar_labels Whether to add the raw volume, percentage or neither to the bars
#'
#' @return Ggplot stacked bar chart with x and y co-ords flipped
#' @export
ls_plot_group_sent <- function(df,
                               group_var = cluster,
                               sentiment_var = sentiment,
                               type = c("percent", "volume"),
                               title = "Grouped Sentiment Chart",
                               bar_labels = c("none", "percent", "volume")) {
  group_sym <- group_var # Weird tidy eval pattern when the columns are being generated dynamically with updateSelectInput. First get the variable (in this case a string) then convert to symbol.
  group_sym <- rlang::ensym(group_var)
  sentiment_sym <- rlang::ensym(sentiment_var)

  bar_labels <- match.arg(if (missing(bar_labels)) "volume" else bar_labels, c("none", "percent", "volume"))
  type <- match.arg(if (missing(type)) "percent" else type, c("percent", "volume"))

  df <- df %>%
    dplyr::mutate(
      group_var = !!group_sym,
      sentiment_var = !!sentiment_sym,
      group_var = factor(group_var)
    ) %>%
    dplyr::count(group_var, sentiment_var) %>%
    dplyr::add_count(group_var, wt = n, name = ".total") %>%
    dplyr::mutate(
      percent = n / .total * 100,
      percent_character = paste0(round(percent, digits = 1), "%")
    )

  if (type == "percent") {
    plot <- df %>%
      ggplot2::ggplot(ggplot2::aes(
        x = reorder(group_var, n),
        y = percent,
        fill = sentiment_var
      )) +
      ggplot2::geom_col() +
      ggplot2::labs(
        fill = NULL, y = NULL, x = "% of Posts",
        title = title
      )
  } else if (type == "volume") {
    plot <- df %>%
      ggplot2::ggplot(ggplot2::aes(
        x = reorder(group_var, n),
        y = n,
        fill = sentiment_var
      )) +
      ggplot2::geom_col() +
      ggplot2::labs(
        fill = NULL, y = NULL, x = "Number of Posts",
        title = title
      )
  }

  plot <- plot +
    DisplayR::dr_theme_microsoft(scale_type = "discrete") +
    ggplot2::coord_flip() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0))

  if (bar_labels == "percent") {
    plot <- plot +
      ggplot2::geom_text(ggplot2::aes(label = percent_character),
                         colour = "white",
                         position = ggplot2::position_stack(0.5),
                         check_overlap = TRUE
      )
  }
  if (bar_labels == "volume") {
    plot <- plot +
      ggplot2::geom_text(ggplot2::aes(label = scales::comma(n)),
                         colour = "white",
                         position = ggplot2::position_stack(0.5),
                         check_overlap = TRUE
      )
  }
  return(plot)
}


#ls_plot_group_vol_time ----
#' Quickly plot faceted volume of groups over time in a LandscapeR Shiny App
#'
#' @param df Data frame or tibble
#' @param group_var grouping variable e.g. country, cluster, topic etc.
#' @param date_var Variable which contains date information (can be datetime too I think)
#' @param unit A single unit of time fed into lubridate::floor_date  "week", "day", "month","quarter", "year"
#' @param nrow How many rows the plot should be shown in
#'
#' @return ggplot object of faceted bar charts
#' @export
ls_plot_group_vol_time <- function(df, group_var = group, date_var = date, unit = c("day", "week", "month", "quarter", "year"), nrow = 4) {
  unit <- match.arg(unit)

  date_sym <- rlang::ensym(date_var)
  group_sym <- group_var # shiny tidy eval trick
  group_sym <- rlang::ensym(group_var)

  df <- df %>% dplyr::mutate(
    plot_date = lubridate::floor_date(!!date_sym, unit = unit),
    plot_date = as.Date(plot_date),
    facet_var = !!group_sym,
    facet_var = factor(facet_var)
  )

  df %>%
    dplyr::count(plot_date, facet_var) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n, fill = facet_var)) +
    ggplot2::geom_col() +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_date(date_breaks = "1 months", date_labels = "%b") +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 90),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = "Topic Volume over Time", x = NULL, y = "Number of Posts") +
    ggplot2::facet_wrap(~facet_var, nrow = nrow)
}


#note_area ----
#' Add a re-sizeable text area to take notes in!
#'
#' @return a movable shiny text box
#' @export
#'
#' @examples
#' \dontrun{
#' ui <- shiny::fluidPage(
#'   ls_create_note_area()
#' )
#' }
ls_create_note_area <- function() {
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      #container {
        width: 200px;
        height: 100px;
        border: 1px solid #ccc;
        padding: 5px;
      }
      #note_area {
        width: 100%;
        height: 100%;
      }
    ")),
    shinyjqui::jqui_resizable(
      shiny::div(
        id = "container",
        shiny::tags$textarea(id = "note_area", placeholder = "Write your notes here...")
      ),
    )
  )
}

#sentiment_over_time ----Ø
#' View a sentiment and volume over time chart
#'
#' Data is counted by the function, so input your data into uncounted, or unsummarised form, i.e. do not use `count()`, or `summarise(n())`
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
                                   unit = c("week", "day", "month", "quarter", "year")) {
  unit <- match.arg(unit)

  sent_sym <- rlang::ensym(sentiment_var)
  date_sym <- rlang::ensym(date_var)

  sent_string <- rlang::as_string(sent_sym)
  date_string <- rlang::as_string(date_sym)

  if (!sent_string %in% colnames(df)) {
    stop(paste0("Cannot find '", sent_string, "' in the data frame, did you mean `sentiment_var = sentiment`?"))
  }
  if (!date_string %in% colnames(df)) {
    stop(paste0("Cannot find '", date_string, "' in the data frame, did you mean `date_var = date`?"))
  }



  df <- df %>% dplyr::mutate(
    plot_date = as.Date(!!date_sym),
    plot_date = lubridate::floor_date(plot_date, unit = unit)
  )

  plot <- df %>%
    dplyr::count(plot_date, !!sent_sym) %>%
    dplyr::mutate(!!sent_sym := tolower(!!sent_sym)) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n, fill = !!sent_sym)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_date(date_breaks = "1 months", date_labels = "%d-%b") +
    DisplayR::dr_theme_microsoft(scale_type = "discrete") +
    ggplot2::scale_fill_manual(
      aesthetics = c("fill", "colour"),
      values = c(
        "positive" = "#107C10",
        "negative" = "#D83B01",
        "neutral" = "#FFB900"
      )
    ) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90)
    ) +
    ggplot2::labs(y = "n", x = paste0("Date by ", unit))

  return(plot)
}
