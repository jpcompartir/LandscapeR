# Quick function for checking if a column is of the right type using data-masking

Quick function for checking if a column is of the right type using
data-masking

## Usage

``` r
column_type_checker(data, column, type)
```

## Arguments

- data:

  Data Frame or Tibble object

- column:

  Column you want to check

- type:

  `column`'s expected type

## Value

a character vector

## Examples

``` r
if (FALSE) { # \dontrun{
check_text <- df %>% column_type_checker(text_var, "character")

if (check_text == "no") stop("Wrong type")
} # }
```
