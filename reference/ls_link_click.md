# Prepare a URL column to be clickable in Shiny/Data Table

Will allow you to click the hyperlink to load a URL, e.g. for selecting
an image. Make sure that DataTable is rendered with the argument 'escape
= FALSE' or column will be all text.

## Usage

``` r
ls_link_click(df, url_var)
```

## Arguments

- df:

  Data Frame or Tibble Object

- url_var:

  URL Column

## Value

data frame with URL column edited to be clickable
