# View a sentiment and volume over time chart

Data is counted by the function, so input your data into uncounted, or
unsummarised form, i.e. do not use `count()`, or `summarise(n())`

## Usage

``` r
ls_sentiment_over_time(
  df,
  sentiment_var = sentiment,
  date_var = date,
  unit = c("week", "day", "month", "quarter", "year")
)
```

## Arguments

- df:

  Data Frame or Tibble object

- sentiment_var:

  Name of your sentiment variable

- date_var:

  Name of your date variable

- unit:

  Time unit to count sentiment by

## Value

a ggplot object
