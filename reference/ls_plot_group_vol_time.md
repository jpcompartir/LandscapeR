# Quickly plot faceted volume of groups over time in a LandscapeR Shiny App

Quickly plot faceted volume of groups over time in a LandscapeR Shiny
App

## Usage

``` r
ls_plot_group_vol_time(
  df,
  group_var = group,
  date_var = date,
  unit = c("day", "week", "month", "quarter", "year"),
  nrow = 4
)
```

## Arguments

- df:

  Data frame or tibble

- group_var:

  grouping variable e.g. country, cluster, topic etc.

- date_var:

  Variable which contains date information (can be datetime too I think)

- unit:

  A single unit of time fed into lubridate::floor_date "week", "day",
  "month","quarter", "year"

- nrow:

  How many rows the plot should be shown in

## Value

ggplot object of faceted bar charts
