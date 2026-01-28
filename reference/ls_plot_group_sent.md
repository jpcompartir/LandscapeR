# Quickly plot group sentiment distributions as a percentage in LandscapeR shiny app

Quickly plot group sentiment distributions as a percentage in LandscapeR
shiny app

## Usage

``` r
ls_plot_group_sent(
  df,
  group_var = cluster,
  sentiment_var = sentiment,
  type = c("percent", "volume"),
  title = "Grouped Sentiment Chart",
  bar_labels = c("none", "percent", "volume")
)
```

## Arguments

- df:

  data frame

- group_var:

  Grouping variable, e.g. country, topic, cluster

- sentiment_var:

  Sentiment variable (categorical)

- type:

  Whether the plot should be of volume or py percentage. Accepts
  "percent" or "volume"

- title:

  The title of the plot, entered as a string.

- bar_labels:

  Whether to add the raw volume, percentage or neither to the bars

## Value

Ggplot stacked bar chart with x and y co-ords flipped
