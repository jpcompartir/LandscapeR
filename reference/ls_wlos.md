# A LandscapeR version of ParseR's Weighted Log-odds

Function should be used for identifying the differences between levels
of a grouping variable.

## Usage

``` r
ls_wlos(
  df,
  group_var = cluster,
  text_var = clean_text,
  top_n = 30,
  text_size = 4,
  filter_by = c("association", "frequency"),
  nrow = 4,
  top_terms_cutoff = 5000
)
```

## Arguments

- df:

  Data Frame or Tibble object

- group_var:

  The variable to group with e.g. topic, sentiment

- text_var:

  Your text variable

- top_n:

  Number of terms per plot

- text_size:

  An integer determining text size, higher = larger

- filter_by:

  whether to perform initial filtering by frequency or association

- nrow:

  Number of rows to display the plots across

- top_terms_cutoff:

  The top x words which should have WLOs calculated for them

## Value

a ggplot object
