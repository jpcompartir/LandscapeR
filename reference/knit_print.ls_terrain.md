# Show a terrain map inside R Markdown, Quarto or a pkgdown article

The map is embedded as an `<iframe>` holding the whole page, so the
output document stays self-contained.

## Usage

``` r
# S3 method for class 'ls_terrain'
knit_print(x, options = list(), ...)
```

## Arguments

- x:

  An `ls_terrain` object.

- options:

  Chunk options. `out.height` sets the frame height (default "640px").

- ...:

  Not used.
