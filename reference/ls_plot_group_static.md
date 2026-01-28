# Create a landscape plot and colour in each level of a grouping variable.

Default colour mapping uses the viridis palette, you can simply add
different colour maps to the plot using standard ggplot syntax, as the
return object is a ggplot object e.g. plot +
`scale_colour_manual(values = ...)`

## Usage

``` r
ls_plot_group_static(df, x_var = V1, y_var = V2, group_var, point_size = 0.1)
```

## Arguments

- df:

  Data Frame or Tibble object

- x_var:

  The variable containing x-coordinates

- y_var:

  The variable containing y-coordinates

- group_var:

  The grouping variable to iteratively highlight

- point_size:

  How big each point should be

## Value

a ggplot object

## Examples

``` r
df <- ls_example %>% dplyr::mutate(cluster = factor(cluster))
df %>% ls_plot_group_static(group_var = cluster)
```
