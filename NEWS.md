# LandscapeR 0.1.0

LandscapeR 0.1.0 is the first version recorded in NEWS.md. For earlier
changes, see the git history.

## New features

* `ls_plot_terrain()` draws a landscape as an interactive 3D terrain map and
  writes it to a single HTML file (#32). The height of the map follows the
  number of documents in each spot, and a grouping variable colours the map
  and labels each group. When you give it a text variable, it names regions
  of the map by their most distinctive words, and you can hover or click to
  read example documents. See `vignette("terrain_maps")`.

* `ls_plot_terrain()` has a new `sample_by` argument, which chooses the
  example documents shown in the tooltip and the place panels. A logical
  expression keeps only the matching documents, e.g.
  `sample_by = label == "clean"`, and a numeric expression ranks them from
  highest to lowest, e.g. `sample_by = relevance`. It changes only the example
  text, so the heights and colours of the map still use every document.
