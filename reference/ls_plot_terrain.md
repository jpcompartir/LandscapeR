# Render a landscape as an interactive 3D terrain map

Bins a set of 2D co-ordinates onto a fine grid and renders them as a 3D
terrain, in the style of a population density map. Each bin becomes a
column whose height follows the number of documents nearby, so dense
regions rise as peaks and sparse regions sit as low plains. When a
grouping variable is supplied, each column is coloured by the groups
that occupy it and every group gets a label anchored to its highest
peak.

The output is a single self-contained HTML file. It is drawn with WebGL
in the browser, so it needs no JavaScript libraries and works offline
(fonts fall back to system fonts when there is no internet connection).

## Usage

``` r
ls_plot_terrain(
  df,
  x_var = V1,
  y_var = V2,
  group_var = NULL,
  text_var = NULL,
  bins = NULL,
  bandwidth = 1.6,
  roughness = 0.25,
  height_scale = c("relief", "sqrt", "linear", "log"),
  spread = 0.35,
  compress_outliers = TRUE,
  height_by = c("auto", "count", "density"),
  colour_by = c("auto", "group", "enrichment"),
  place_names = TRUE,
  exaggeration = 1,
  land_threshold = 0.2,
  colours = NULL,
  labels = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  theme = c("paper", "dark"),
  file = NULL,
  open = NULL
)
```

## Arguments

- df:

  Data frame or tibble, one row per document.

- x_var, y_var:

  Variables holding the x and y co-ordinates.

- group_var:

  Optional grouping variable (for example a cluster or topic) used to
  colour and label the terrain.

- text_var:

  Optional text variable. One example document per bin is shown in the
  hover tooltip.

- bins:

  Number of bins along the longer axis. Higher values give finer detail.
  By default it grows with the number of rows, from 200 up to 420.

- bandwidth:

  Smoothing applied to the counts, in bins. Larger values give rounder
  hills.

- roughness:

  Between 0 and 1. How much fine-grained texture from the raw counts is
  mixed into the smoothed surface.

- height_scale:

  How density maps onto height. "relief" (default) sets a sea level so
  sparse ground stays flat and dense regions rise steeply, with the very
  tallest peaks softened so one large cluster does not flatten the rest.
  "linear", "sqrt" and "log" are plain transforms of the density, each
  lifting the low ground more than the last.

- spread:

  How far to push groups apart, as a fraction of the distance between
  each group's centre and the overall centre. Gives clusters room to
  breathe. Groups that are mixed together are hardly moved. Set to 0 to
  keep the original co-ordinates.

- compress_outliers:

  Pull far-flung points and groups in towards the main landmass so that
  it fills the view. Groups are moved whole, so their shape is kept.

- height_by:

  "count" makes the peak of each group's mountain proportional to its
  number of documents, so heights compare topic sizes. "density" uses
  local density, so tight clusters rise higher than loose ones of the
  same size. "auto" (default) uses "count" when groups occupy their own
  regions, and "density" otherwise.

- colour_by:

  "group" colours each bin by the groups in it. "enrichment" colours
  each bin by the group that is most over-represented there compared
  with its overall share, and leaves bins close to the average neutral.
  This suits variables such as sentiment that are spread across the
  whole map. "auto" (default) picks "enrichment" when the groups are
  heavily mixed.

- place_names:

  When `text_var` is supplied, name regions of the map by their most
  distinctive words. Finer place names appear as you zoom in, and
  clicking a place opens a panel with its keywords and example
  documents.

- exaggeration:

  Multiplier for the height of the peaks.

- land_threshold:

  Controls how far the base plate extends around the points. Lower
  values give a larger landmass and more islands around outlying points.

- colours:

  Optional vector of colours for the groups. Can be named by group
  level.

- labels:

  Optional named character vector to rename group labels, for example
  `c("1" = "Browser privacy")`.

- title, subtitle, caption:

  Text for the poster-style header and footer. `subtitle` sits in small
  capitals above the title.

- theme:

  "paper" (light) or "dark".

- file:

  Path to write the HTML to. If `NULL` a temporary file is used.

- open:

  By default (`NULL`) the map is returned visibly, so it opens when
  printed at the console and is embedded when used in R Markdown, Quarto
  or a pkgdown article. `TRUE` opens it straight away and `FALSE` only
  writes the file; both return the result invisibly.

## Value

An object of class `ls_terrain` holding the file path. Printing it opens
the map.

## Details

In the browser you can drag to rotate, shift-drag (or right-drag) to pan
and scroll to zoom. Hovering over a column shows the documents and
groups beneath it. Clicking a group in the legend isolates it.
Shift-click adds groups to the selection. The toolbar can flatten the
map to a 2D view, rotate it slowly and download a high-resolution PNG.

Only the binned summary is written to the file, so large data sets
(millions of rows) produce files of roughly the same size as small ones.
The exception is `text_var`, which stores one example document per bin.

## Examples

``` r
if (FALSE) { # \dontrun{
ls_example %>%
  ls_plot_terrain(V1, V2, group_var = cluster, text_var = text,
                  title = "Browser Conversation", subtitle = "Landscape of")

# A single-colour density map
ls_example %>% ls_plot_terrain(V1, V2, title = "Density")
} # }
```
