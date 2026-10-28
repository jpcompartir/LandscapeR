# Terrain maps

[`ls_plot_terrain()`](https://jpcompartir.github.io/LandscapeR/reference/ls_plot_terrain.md)
turns a set of 2D co-ordinates, such as the output of UMAP, into an
interactive 3D map. Each small square of the map becomes a column, and
the height of the column shows how many documents sit there, so the busy
parts of a conversation rise as mountains and the quiet parts stay as
flat ground. When you give it a grouping variable, it colours the map by
group. When you give it a text variable, it names regions of the map by
their most distinctive words, so you can find your way around.

The function writes a single HTML file. The file works offline, needs no
other files, and opens in any modern browser, so you can send it to
someone as an attachment. In R, the only package it needs beyond
LandscapeR’s existing imports is `jsonlite`.

``` r

library(LandscapeR)
```

## A first map

The example data has 11,226 posts about web browsers, with UMAP
co-ordinates in `V1` and `V2` and a cluster number in `cluster`. The
call below builds a map coloured by cluster and named from the post
text.

``` r

ls_plot_terrain(
  ls_example,
  x_var = V1, y_var = V2,
  group_var = cluster,
  text_var = text,
  title = "Browser talk",
  subtitle = "A map of the conversation"
)
```

In an R session, the map opens in the RStudio viewer or your browser. In
R Markdown, Quarto or a pkgdown article, the map is embedded in the
page, as above. To keep the file, pass a path to `file`, and set
`open = FALSE` if you only want the file to be written.

``` r

ls_plot_terrain(ls_example, V1, V2, group_var = cluster, text_var = text,
                file = "browser_map.html")
```

## How to read the map

The height of the map depends on the kind of grouping variable you give
it.

- When each group has its own area of the map, as clusters and topics
  usually do, the highest point of each group’s mountain is proportional
  to the number of documents in the group. A cluster of 3,869 posts
  stands about five times as tall as a cluster of 701 posts, while each
  mountain keeps its own shape.
- When the groups are spread across the whole map, as sentiment usually
  is, the height shows how many documents of any group sit in that spot.

The colour also depends on the grouping variable. Clustered groups
colour their own area of the map, and the colours fade towards the low
ground. For spread-out groups, a colour marks the places where that
group is more common than it is across the whole map, and places close
to the average stay neutral. The function chooses between the two modes
by measuring how well the groups separate in space. You can set the
choice yourself with `colour_by` and `height_by`.

Group labels sit on each group’s highest point and show the group’s
size. When the group levels are plain numbers, as cluster ids usually
are, and you have not supplied `labels`, the function names each group
by its two most distinctive words.

## Exploring the map

The map is built to be explored in the same way as an online map. You
can move around it with the mouse or the keyboard:

- Drag to rotate, and shift-drag, right-drag or use the arrow keys to
  pan.
- Scroll, or press `+` and `-`, to zoom. Finer place names appear as you
  zoom in.
- Hover over the terrain to see the groups in that spot and an example
  document. The tooltip also gives the number of documents in that
  column, and in the 5 × 5 block of columns around it. Column heights
  come from a smoothed count, so a tall column can hold only a few
  documents when its neighbours are busy.
- Click a label or any part of the terrain to fly there. A panel opens
  with the place’s size, its mix of groups, its distinctive words, the
  smaller places inside it, and example documents.
- Press `/` to search group names, place names and words, and press
  Enter to fly to the first result.
- Click a group in the legend to isolate it. The other groups sink to
  the ground. Shift-click adds more groups, and Escape brings everything
  back.

The buttons on the right reset the view, switch between 3D and a flat 2D
view, turn on a slow rotation, and download a PNG at three times screen
resolution. The keyboard shortcuts for the buttons are `R`, `F`, `S` and
`P`.

## Groups that are spread across the map

Sentiment is a good example of a grouping variable that does not form
regions, because positive, neutral and negative posts appear in every
part of the conversation. For a variable like sentiment, the map shows
where each value is more common than usual, and the place names tell you
what people are talking about in that spot. Clicking a place shows how
far above or below its usual share each sentiment is.

``` r

ls_plot_terrain(
  ls_example, V1, V2,
  group_var = sentiment,
  text_var = text,
  theme = "dark",
  title = "Sentiment",
  subtitle = "Where the tone changes"
)
```

The function gives the levels “positive”, “negative” and “neutral”
green, red and amber by default. You can choose your own colours with
`colours`, either as a plain vector or a vector named by level.

## A map without groups

Without a grouping variable, the map shows document density on its own,
coloured from pale lavender on the low ground to pink on the highest
peaks.

``` r

ls_plot_terrain(ls_example, V1, V2, text_var = text,
                title = "Density", subtitle = "Documents per region")
```

## Choosing the example documents

By default, the tooltip and the place panels show example documents
picked at random. Social data often holds a lot of noise, such as spam
and bot posts, so random examples can be poor. If you have already
marked the good documents in your data, you can use `sample_by` to
choose the examples from them. It is evaluated using the columns of your
data, in the same way as an argument to
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
or
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html).

The calls below assume a data frame called `posts` with a `label`
column, holding values such as “clean” and “noise”, and a numeric
`relevance` column.

``` r

# Show only the documents labelled "clean".
ls_plot_terrain(posts, V1, V2, text_var = text, sample_by = label == "clean")

# Show the most relevant documents first.
ls_plot_terrain(posts, V1, V2, text_var = text, sample_by = relevance)

# Show the least relevant documents first.
ls_plot_terrain(posts, V1, V2, text_var = text, sample_by = -relevance)

# Show the most relevant of the clean documents.
ls_plot_terrain(posts, V1, V2, text_var = text,
                sample_by = ifelse(label == "clean", relevance, NA))
```

The function follows these rules:

- A logical result keeps the documents where it is `TRUE`.
- A numeric result ranks the documents from highest to lowest.
- Documents where the result is `NA` are never shown, and ties are
  broken at random.
- A date cannot be used to rank directly. Write `as.numeric(date)` to
  rank by it, or a comparison such as `date >= "2024-01-01"` to filter
  by it.

`sample_by` changes only the example text. The heights, colours and
place names still use every document, so if you want the noise removed
from the map itself, filter the data before you call
[`ls_plot_terrain()`](https://jpcompartir.github.io/LandscapeR/reference/ls_plot_terrain.md).
A strict filter can also leave many columns with no example to show when
you hover over them.

## Settings you may want to change

Most maps need no settings beyond the variables and a title. The table
lists the settings that change the look of the map most.

| Argument | Default | What it changes |
|:---|:---|:---|
| `labels` | none | Names for groups, e.g. `c("2" = "Edge on Windows")`. |
| `colours` | built-in palette | Group colours, as a plain or named vector. |
| `bins` | grows with the data, from 200 to 420 | The number of columns along the longer side of the map. More bins give finer detail. |
| `bandwidth` | 1.6 | How much the counts are smoothed. Larger values give rounder hills. |
| `roughness` | 0.25 | How much fine texture from the raw counts shows on top of the smooth surface. Set it to 0 for smooth hills. |
| `height_scale` | “relief” | How density turns into height when the map uses density. “relief” keeps sparse ground flat and makes busy areas rise steeply. “linear”, “sqrt” and “log” are plain transforms. |
| `exaggeration` | 1 | Multiplies the height of every column. |
| `spread` | 0.35 | Pushes groups apart to leave clear ground between them. Set it to 0 to keep the original co-ordinates. |
| `compress_outliers` | TRUE | Pulls far-away points and groups in towards the main landmass, so the landmass fills the view. |
| `place_names` | TRUE | Names regions from `text_var`. |
| `theme` | “paper” | “paper” for a light page, or “dark”. |

`spread` and `compress_outliers` move points on the map, so turn both
off if the exact distances between points matter for your analysis.

``` r

ls_plot_terrain(ls_example, V1, V2, group_var = cluster,
                spread = 0, compress_outliers = FALSE)
```

## Large data sets

The HTML file holds only a summary of each column of the map and a few
example documents per place, so the file size depends on the number of
bins and not on the number of documents. A map of one million documents
with 20 groups and text takes about 10 seconds to build and makes a file
of about 2.5 MB. For very large data sets the function finds place names
from a random sample of 200,000 documents. Stray documents in otherwise
empty space are left off the map, and the caption at the bottom of the
page says how many.

## Limitations

The map needs WebGL2, which current versions of Chrome, Edge, Firefox
and Safari support. The page loads the Archivo font from Google Fonts
and falls back to a system font when there is no internet connection.

Place names come from word counts, so they are only as clear as the
text. For example, a cluster of posts about Netflix errors comes out as
“Worked & Issue”. You can replace any group name with `labels`.
