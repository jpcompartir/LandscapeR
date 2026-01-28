# Render a landscape plot inside a shiny app

Renders an interactive plotly plot with a

## Usage

``` r
ls_plotly_umap(data, x, y, type, group_var, key, text_var, height, width)
```

## Arguments

- data:

  Data Frame or tibble object, will likely need to be called with
  brackets as it should be reactive

- x:

  X co-ordinate variable (as a string)

- y:

  Y co-ordinate variable (as a string)

- type:

  type of plotly chart to render, usually 'scattergl' for performance

- group_var:

  Which variable to colour by (as a string)

- key:

  ID column (as a string)

- text_var:

  Text variable (as a string)

- height:

  Height of plot (as a strong)

- width:

  Width of plot (as a string)

## Value

a plotly object

## Calling the function

Due to plotly's non-standard evaluation you'll need to feed in any
column names, such as `color`, `x`, `y`, `text`, `key`, as strings.

## Examples

``` r
if (FALSE) { # \dontrun{
LandscapeR::ls_example %>%
  dplyr::mutate(cluster = factor(cluster)) %>%
  ls_plotly_umap(
    x = "V1", y = "V2", type = "scattergl",
    key = "document", text_var = "text", group_var = "cluster", height = 600, width = 800
  )
} # }
```
