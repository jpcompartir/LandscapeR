# Compare proportion of x in y

Use this function to compare two grouping variables, for example to see
what percentage of each cluster fall into each sentiment category (by
volume).

## Usage

``` r
ls_plot_variation_matrix(df, x_var, y_var)
```

## Arguments

- df:

  Data Frame or Tibble object

- x_var:

  The grouping variable for the x axis.

- y_var:

  The grouping variable for the y axis.

## Value

a ggplot object

## Examples

``` r
ls_example %>% ls_plot_variation_matrix(cluster, sentiment)
#> Loading required namespace: viridis
```
