library(shiny)
library(LandscapeR)
library(readr)

data <- read_csv("~/Downloads/conversation_landscape_df.csv")
names(data)

plotting_heights <- "450px";  plotting_widths <- "400px"

#Get date ranges for volume
dates <- data %>% select(date) %>% summarise(min = min(date), max = max(date))
date_min <- as.Date(dates$min)
date_max <- as.Date(dates$max)
size <- 2
type = "scattergl"
data <- data %>%  mutate(cluster = factor(cluster, levels = c(1:7)),
                         date = as.Date(date)) %>%
  clean_text(clean_text) %>%
  mutate(clean_text = tm::removeWords(clean_text, tm::stopwords(kind = "smart")),
         clean_text = stringr::str_squish(clean_text))

# var_patterns <- c("{{id}}", "{{text_var}}", "{{colour_var}}", "clean_text", "date", "{{sentiment_var}}", "permalink",
#               "{{x_var}}", "{{y_var}}")
# var_replacements <- c("document", "text", "cluster", "clean_text", "date", "sentiment", "permalink", "V1", "V2")

#replace {{id}} with document
# hide ui----
ui <- shiny::navbarPage("Conversation Landscape", theme = shinythemes::shinytheme("cosmo"), position = "fixed-top",
                        tags$style(type="text/css", "body {padding-top: 70px;}"), #Prevents the navbar from eating body of app
                        #colours all 10  sliders orange
                        shinyWidgets::setSliderColor(color = rep("#ff7518", 10), sliderId = c(1:10)),
                        #---- Landscape Tab----
                        shiny::tabPanel("Survey the Landscape",
                                        shiny::fluidPage(
                                          gotop::use_gotop(),
                                          theme = shinythemes::shinytheme(theme = "cosmo"),
                                          shiny::fluidRow(
                                            shiny::column(2, style = "padding-right: 0px; border: none;",
                                                          shiny::textInput("remainingName", "All Data",
                                                                           value = NULL,
                                                                           placeholder = "filename")),
                                            shiny::column(1, style = "padding-left: 10px; padding-right: 20px;",
                                                          shiny::div(style = "margin-top: 25px;",
                                                                     shiny::downloadButton("downloadAll", "Download",
                                                                                           class = "btn btn-warning",
                                                                                           style = "background: #ff4e00; border-radius: 100px; color: #ffffff; border:none;"))),
                                            shiny::column(3, style = "padding-left: 20px; padding-right: 10px;", shinyWidgets::searchInput(
                                              inputId = "filterPattern",
                                              label = "Pattern to search text with",
                                              placeholder = "A placeholder",
                                              btnSearch = shiny::icon("search"),
                                              btnReset = shiny::icon("remove"),
                                              width = "100%",
                                              value = ""
                                            )),
                                            shiny::column(2, shiny::textInput("fileName", "Selected Data", value = NULL, placeholder = "filename excluding .csv")),
                                            shiny::column(2, shiny::div(style = "margin-top: 25px;",
                                                                        shiny::downloadButton("downloadData",
                                                                                              "Download",class = "btn btn-warning",
                                                                                              style = "background: #ff4e00; border-radius: 100px; color: #ffffff; border:none;")))
                                          ),
                                          shiny::column(6, style = "width:50%; height: 10000px; position: relative;",
                                                        div(id = "graph",
                                                            shinycssloaders::withSpinner(plotly::plotlyOutput("umapPlot", height = 600)),
                                                            div(id = "button",
                                                                shiny::fluidRow(
                                                                  shiny::uiOutput("deleteme"),
                                                                ),
                                                            ),
                                                            shiny::br(),
                                                            shiny::br(),
                                                            shiny::fluidRow(
                                                              shiny::column(6, div(id = "slider1",
                                                                                   style = "width: 100%;",
                                                                                   shiny::sliderInput("x1","V1 Range",step = 5,  -100, 100, c(-20, 20))),),
                                                              shiny::column(6,
                                                                            div(id = "slider2", style = "width: 100%;",
                                                                                shiny::sliderInput( "y1","V2 Range",step = 5, -100, 100, c(-20, 20)))
                                                              ),
                                                            )
                                                        )
                                          ),
                                          shiny::column(5, shinycssloaders::withSpinner(DT::dataTableOutput("highlightedTable"))),
                                        ),),
                        #---- Distribution Tab ----
                        shiny::tabPanel("Distribution Plots", shiny::fluidPage(theme = shinythemes::shinytheme('cosmo'),
                                                                               gotop::use_gotop()),

                                        shiny::p("In this tab you can view, and download if necessary, charts designed to help you understand your selections."),
                                        shiny::p("Below you will find four charts; sentiment distribution, volume over time, tokens counter and a sampled bigram network."),
                                        #---- Sentiment plot ----
                                        shiny::br(),
                                        shiny::titlePanel(title =  "Sentiment Distribution"),
                                        shiny::sidebarLayout(
                                          shiny::sidebarPanel(width = 2,
                                                              #Should functionise all of this and use map to render the UI elements.
                                                              shiny::sliderInput("sentimentHeight","Height",  min = 100, max = 800, value = 400, step = 50),
                                                              shiny::sliderInput("sentimentWidth","Width",  min = 100, max = 800, value = 400, step = 50),
                                                              shinyWidgets::materialSwitch(
                                                                inputId = "toggleSentimenttitles",
                                                                label = "Customise Titles?",
                                                                status = "primary",
                                                                right = TRUE
                                                              ),
                                                              shiny::uiOutput("sentimentTitles"),
                                                              shiny::downloadButton(outputId = "saveSentiment", class = "btn btn-warning",  style = "background: #ff4e00; border-radius: 100px; color: #ffffff; border:none;"),
                                          ),
                                          shiny::mainPanel(
                                            shinycssloaders::withSpinner(shiny::plotOutput("sentimentPlot",height = plotting_heights, width  = plotting_widths))
                                          )
                                        ),
                                        shiny::hr(),

                                        #Volume Over Time plot ----
                                        shiny::titlePanel(title = "Volume Over Time"),
                                        shiny::br(),
                                        shiny::sidebarLayout(
                                          shiny::sidebarPanel(width =  2,
                                                              shiny::sliderInput("volumeHeight","Height",  min = 100, max = 800, value = 400, step = 50),
                                                              shiny::sliderInput("volumeWidth","Width",  min = 100, max = 800, value = 400, step = 50),
                                                              shiny::dateRangeInput("dateRange", label = "Date Range",start = date_min, end = date_max),

                                                              shiny::selectInput(inputId = "dateBreak", label = "Unit", choices = c("day", "week", "month", "quarter", "year"), selected = "week"),
                                                              shiny::selectInput(inputId = "dateSmooth", label = "Smooth", choices = c("none", "loess", "lm", "glm", "gam"), selected = "none"),
                                                              shiny::uiOutput("smoothControls"),
                                                              shiny::textInput("volumeHex", "colour", value ="#107C10"),
                                                              shinyWidgets::materialSwitch(
                                                                inputId = "toggleVolumetitles",
                                                                label = "Customise Titles?",
                                                                status = "primary",
                                                                right = TRUE
                                                              ),
                                                              shiny::uiOutput("volumeTitles"),
                                                              shiny::downloadButton(outputId = "saveVolume", class = "btn btn-warning",  style = "background: #ff4e00; border-radius: 100px; color: #ffffff; border:none;"),
                                          ),
                                          shiny::mainPanel(
                                            shinycssloaders::withSpinner(shiny::plotOutput("volumePlot", height = plotting_heights,width  = plotting_widths)))
                                        ),
                                        shiny::br(),

                                        #Token Plot ----
                                        shiny::br(),
                                        shiny::titlePanel(title = "Token Distribution"),
                                        shiny::sidebarLayout(
                                          shiny::sidebarPanel(width = 2,
                                                              shiny::sliderInput("tokenHeight","Height",  min = 100, max = 800, value = 400, step = 50),
                                                              shiny::sliderInput("tokenWidth","Width",  min = 100, max = 800, value = 400, step = 50),
                                                              shinyWidgets::materialSwitch(
                                                                inputId = "toggleTokentitles",
                                                                label = "Customise Titles?",
                                                                status = "primary",
                                                                right = TRUE
                                                              ),
                                                              shiny::textInput("tokenHex", "colour", value ="#0f50d2"),
                                                              shiny::uiOutput("tokenTitles"),
                                                              shiny::downloadButton(outputId = "saveToken", class = "btn btn-warning",  style = "background: #ff4e00; border-radius: 100px; color: #ffffff; border:none;"),
                                          ),
                                          shiny::mainPanel(
                                            shinycssloaders::withSpinner(shiny::plotOutput("tokenPlot", height = plotting_heights, width  = plotting_widths)))
                                        ),
                        ),
                        shiny::br(),
                        #---- Bigram Tab ----
                        shiny::tabPanel("Bigram Network", shiny::fluidPage(theme = shinythemes::shinytheme('cosmo'),
                                                                           gotop::use_gotop()),

                                        shiny::br(),
                                        # Bigram plot ----
                                        shiny::fluidRow(
                                          shiny::column(4,
                                                        shiny::p("Below you'll find a bigram network, this network will help you estimate how clean your selected data is. Remember that long and connected chains of words may represent spam or unwanted mentions."),
                                                        shiny::br() ,
                                                        shiny::p("This bigram network is restricted to a maximum of 5,000 data points for speed and user experience. It is therefore not recommended to be saved or exported. If the data looks clean, download the selection and create the network in the standard way in R/Rstudio"),)
                                        ),

                                        shiny::sidebarPanel(width = 2,
                                                            shiny::sliderInput("bigramHeight","Height",  min = 100, max = 1200, value = 600, step = 50),
                                                            shiny::sliderInput("bigramWidth","Width",  min = 100, max = 1200, value = 800, step = 50),
                                        ),
                                        shiny::mainPanel(
                                          shinycssloaders::withSpinner(shiny::plotOutput("bigramPlot",height = plotting_heights,width  = plotting_widths)))
                        ),

)

# hide server ----
server <- function(input, output, session){
  #---- Pattern ----
  pattern <- shiny::reactiveVal(value = "",{})
  shiny::observeEvent(input$filterPattern, {
    pattern(input$Regex)
  })

  #---- Filter + Reset Pattern ----
  shiny::observeEvent(input$reset, {
    pattern(input$Regex)
    updateTextInput(session, "Regex", value = "")
  })
  shiny::observeEvent(input$reset, {
    pattern("")
  })

  #---- Delete IDS ----
  remove_range <- shiny::reactiveValues(
    keep_keys = data %>% dplyr::pull(document), #Get the original IDs saved and save an object for later adding selected points to remove
    remove_keys = NULL
  )

  shiny::observeEvent(input$delete,{  #Update remove_range's values on delete button press
    req(length(remove_range$keep_keys) > 0)
    remove_range$remove_keys <- selected_range()$key
    remove_range$keep_keys <- remove_range$keep_keys[!remove_range$keep_keys %in% remove_range$remove_keys]

  })
  #---- reactive data ---
  reactive_data <- shiny::reactive({
    data <- data %>%
      dplyr::filter(V1 > input$x1[[1]], V1 < input$x1[[2]], V2 > input$y1[[1]], V2 < input$y1[[2]]) %>%
      dplyr::filter(document %in% remove_range$keep_keys) %>%
      dplyr::filter(grepl(input$filterPattern, text, ignore.case = TRUE))
  })

  #---- UMAP Plot ----
  output$umapPlot = plotly::renderPlotly({
    reactive_data() %>%
      dplyr::rename(V1 = V1, #rename these variables to avoid problems with plotly NSE and not have to rename the eventual output's columns.
                    V2 = V2,
                    id_var = document,
                    text_var = text,
                    colour_var = cluster) %>%
      plotly::plot_ly(x = ~V1, y = ~V2,
                      type = type,
                      color = ~colour_var,
                      # colors = colour_mapping,
                      key = ~id_var,
                      text = ~paste("<br> Post:", text_var),
                      hoverinfo = "text", marker = list(size = size), height = 600) %>%
      plotly::layout(dragmode = "lasso",
                     legend= list(itemsizing='constant')) %>%
      plotly::event_register(event = "plotly_selected")
  })

  #Instantiate a reactive value, then update that value dynamically when points are selected. ----
  selected_range <- shiny::reactiveVal({})

  shiny::observeEvent(plotly::event_data("plotly_selected"),{
    selected_range(plotly::event_data("plotly_selected"))
  })

  #---- key ----
  key <- reactive({
    selected_range()$key
  })

  #---- filtered_df ----
  df_filtered <- reactive({
    df_filtered <- reactive_data() %>%
      dplyr::filter(document %in% key())
  })

  #---- Data Table ----
  #Now render the data table, selecting all points within our boundaries. Would need to update this for lasso selection.
  output$highlightedTable <- DT::renderDataTable({
    df <- df_filtered() %>%
      #Select the columns you want to see from your data
      dplyr::select(text,
                    cluster, permalink, sentiment)

    DT::datatable(df, filter = "top", options = list(pageLength = 25,
                                                     dom = '<"top" ifp> rt<"bottom"lp>', autoWidth = FALSE), #TODO check adding l worked
                  style = "bootstrap", rownames = FALSE,
                  escape = FALSE) #Add escape = False to ensure the HTML clicks work properly
  })

  #---- Download Handler Data ----
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      paste0(input$fileName, ".csv")
    },
    content = function(file) {
      utils::write.csv(df_filtered(), file)
    }
  )

  output$downloadAll <- shiny::downloadHandler(
    filename = function() {
      paste0(input$remainingName, ".csv")
    },
    content = function(file) {
      utils::write.csv(reactive_data(), file)
    }
  )

  delayedTokenHex <- shiny::reactive({input$tokenHex}) %>%
    shiny::debounce(500)
  delayedVolumeHex <- shiny::reactive({input$volumeHex}) %>%
    shiny::debounce(500)

  #---- Reactive plots + Observes ----
  #First create the reactive (this will be sent to download handler) then create the server's output for display in app

  sentiment_label <- reactive_labels("sentiment", input)
  sentiment_reactive <- reactive({
    df_filtered() %>%
      LandscapeR::ls_plot_sentiment_distribution(sentiment_var = sentiment) +
      sentiment_label()
  })

  #now create the server's output for display in app
  output$sentimentPlot <-
    shiny::renderPlot({
      sentiment_reactive()
    }, res = 100,
    width = function() input$sentimentWidth,
    height = function() input$sentimentHeight)

  #---- Token plot ----
  token_label <- reactive_labels("token", input)
  token_reactive <- reactive({
    df_filtered() %>%
      LandscapeR::ls_plot_tokens_counter(text_var = clean_text,
                                         top_n = 25,
                                         fill = delayedTokenHex()) +
      ggplot2::scale_fill_manual(values = input$tokenHex) +
      token_label()

  })

  output$tokenPlot <- shiny::renderPlot({
    token_reactive()
  }, res = 100,
  width = function()  input$tokenWidth,
  height = function() input$tokenHeight)

  #---- Volume Plot ----
  volume_label <- reactive_labels("volume", input)
  volume_reactive <- reactive({
    vol_data <- df_filtered() %>%
      dplyr::filter(date >= input$dateRange[[1]], date <= input$dateRange[[2]])

    vol_plot <- vol_data %>%
      LandscapeR::ls_plot_volume_over_time(
        .date_var = date,
        unit =  input$dateBreak,
        fill = delayedVolumeHex()
      ) +
      volume_label()

    if (!input$dateSmooth == "none") {
      if (input$smoothSe == "FALSE") {
        vol_plot <- vol_plot +
          ggplot2::geom_smooth(
            method = input$dateSmooth,
            se = FALSE,
            colour = input$smoothColour
          )
      } else {
        vol_plot <- vol_plot +
          ggplot2::geom_smooth(method = input$dateSmooth,
                               colour = input$smoothColour)}}

    return(vol_plot)

  })

  output$volumePlot <-
    shiny::renderPlot({volume_reactive()},
                      res = 100,
                      width = function() input$volumeWidth,
                      height = function() input$volumeHeight)

  #Volume plot smooth controls
  output$smoothControls <- shiny::renderUI({
    if (input$dateSmooth != "none") {
      shiny::tagList(
        shiny::selectInput(
          "smoothSe",
          "show standard error?",
          choices = c("TRUE", "FALSE"),
          selected = "TRUE"
        ),
        shiny::textInput("smoothColour", "Smooth colour", value = "#000000")
      )
    }

  })

  #Make delete button disappear when nothing selected
  output$deleteme <- shiny::renderUI({
    if (length(selected_range() > 1)) {
      shiny::tagList(
        shiny::actionButton(
          "delete",
          "Delete selections",
          class = 'btn-warning',
          style = "position: absolute; bottom 7px; right: 7px; background: #ff4e00; border-radius: 100px; color: #ffffff; border:none;"
        )
      )

    }
  })

  output$volumeTitles <- LandscapeR::titles_render("volume", input)
  output$sentimentTitles <- LandscapeR::titles_render("sentiment", input)
  output$tokenTitles <- LandscapeR::titles_render("token", input )

  bigram_reactive <- reactive({
    if (length(selected_range()) > 1) {
      if (!length(selected_range()) >= 5000) {
        bigram <- df_filtered() %>%
          JPackage::make_bigram_viz(
            text_var = clean_text,
            clean_text = FALSE,
            min = 5,
            remove_stops = FALSE
          )
      } else{
        bigram <- df_filtered() %>%
          dplyr::sample_n(5000) %>%
          JPackage::make_bigram_viz(
            text_var = clean_text,
            clean_text = FALSE,
            min = 5,
            remove_stops = FALSE
          )
      }
    }
    return(bigram)
  })
  output$bigramPlot <- shiny::renderPlot({
    bigram_reactive()
  }, res = 100,
  width = function() input$bigramWidth,
  height = function() input$bigramHeight)

  #---- Download boxes for plots ----

  output$saveVolume <- LandscapeR::download_box("volume_plot", volume_reactive())
  output$saveToken <-  LandscapeR::download_box("token_plot", token_reactive())
  output$saveSentiment <- LandscapeR::download_box(exportname = "sentiment_plot", plot = sentiment_reactive())
}

shiny::shinyApp(ui, server)

