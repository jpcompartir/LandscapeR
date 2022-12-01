# Volume_time plot function ----
.plot_volume_over_time <- function(df, date_var , unit = "week",  fill = "#0f50d2"){

  df <- df %>% dplyr::mutate(plot_date = lubridate::floor_date(!!date_sym, unit = unit))
  df %>%
    dplyr::count(plot_date) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_date(date_breaks = "1 months", date_labels = "%d-%b") +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = element_text(angle = 90))
}


# Token plot function ----
.plot_tokens_counter <- function(df, text_var = .data$mention_content, top_n = 20, fill = "#0f50d2"){

  .text_var <- rlang::enquo(text_var)
  df %>%
    tidytext::unnest_tokens(words, rlang::quo_name(.text_var))%>%
    dplyr::count(words, sort = TRUE) %>%
    dplyr::top_n(top_n)%>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(words, n), y = n))+
    ggplot2::geom_col(fill = fill)+
    ggplot2::coord_flip()+
    ggplot2::theme_bw()+
    ggplot2::labs(x = NULL, y =  "Word Count", title = "Bar Chart of Most Frequent Words")+
    ggplot2::theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

#Download box function ----
download_box <- function(exportname, plot) {
  shiny::downloadHandler(
    filename = function() {
      paste(exportname, Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = plot, device = "png", width = 8)
    }
  )
}


#---- Render Titles ----
.titles_render <- function(plot_type){

  .plot_type <- stringr::str_to_title(plot_type)

  shiny::renderUI({
    if(eval(parse(text = paste0("input$toggle", .plot_type, "titles"))) == "TRUE"){
      shiny::tagList(
        shiny::textInput(inputId = paste0(plot_type, "Title"), label = "Title",
                         placeholder = "Write title here...", value = ""),
        shiny::textInput(inputId = paste0(plot_type, "Subtitle"), label = "Subtitle",
                         placeholder = "Write subtitle here...", value = ""),
        shiny::textInput(inputId = paste0(plot_type, "Caption"), label = "Caption",
                         placeholder = "Write caption here...", value = ""),
        shiny::textInput(inputId = paste0(plot_type, "Xlabel"), label = "X axis title",
                         placeholder = "Write the x axis title here..."),
        shiny::textInput(inputId = paste0(plot_type, "Ylabel"), label = "Y axis title",
                         placeholder = "Write the y axis title here")
      )
    }
  })
}


#---- Labs Render ----
.labs_render <- function(plot_type){
  df +
    ggplot2::labs(title = paste0(input$sentimentTitle),
                  caption = paste0(input$sentimentCaption),
                  subtitle = paste0(input$sentimentSubtitle),
                  x = paste0(input$sentimentXlabel),
                  y = paste0(input$sentimentYlabel))
}
