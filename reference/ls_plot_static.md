# Create a landscape plot without segmenting by groups

Create a landscape plot without segmenting by groups

## Usage

``` r
ls_plot_static(df, x_var = V1, y_var = V2, fill_colour = "black")
```

## Arguments

- df:

  Data Frame or Tibble object

- x_var:

  The variable containing x-coordinates

- y_var:

  The variable containing y-coordinates

- fill_colour:

  What colour the points should be

## Value

a ggplot object

## Examples

``` r
df <- ls_example
df %>% ls_plot_group_static()
```
