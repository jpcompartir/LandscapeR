#' Render a landscape as an interactive 3D terrain map
#'
#' @description
#' Bins a set of 2D co-ordinates onto a fine grid and renders them as a 3D
#' terrain, in the style of a population density map. Each bin becomes a
#' column whose height follows the number of documents nearby, so dense
#' regions rise as peaks and sparse regions sit as low plains. When a grouping
#' variable is supplied, each column is coloured by the groups that occupy it
#' and every group gets a label anchored to its highest peak.
#'
#' The output is a single self-contained HTML file. It is drawn with WebGL in
#' the browser, so it needs no JavaScript libraries and works offline (fonts
#' fall back to system fonts when there is no internet connection).
#'
#' @details
#' In the browser you can drag to rotate, shift-drag (or right-drag) to pan and
#' scroll to zoom. Hovering over a column shows the documents and groups beneath
#' it. Clicking a group in the legend isolates it. Shift-click adds groups to
#' the selection. The toolbar can flatten the map to a 2D view, rotate it slowly
#' and download a high-resolution PNG.
#'
#' Only the binned summary is written to the file, so large data sets (millions
#' of rows) produce files of roughly the same size as small ones. The exception
#' is `text_var`, which stores one example document per bin.
#'
#' @param df Data frame or tibble, one row per document.
#' @param x_var,y_var Variables holding the x and y co-ordinates.
#' @param group_var Optional grouping variable (for example a cluster or topic)
#'   used to colour and label the terrain.
#' @param text_var Optional text variable. One example document per bin is shown
#'   in the hover tooltip.
#' @param sample_by Optional expression that chooses which documents are shown
#'   as examples in the hover tooltip and the place panels. It is evaluated
#'   using the columns of `df`, as in `dplyr::filter()` or `dplyr::arrange()`.
#'   A logical result acts as a filter, so only rows where it is `TRUE` are
#'   shown, for example `label == "clean"`. A numeric result ranks the rows
#'   from highest to lowest, for example `relevance`. Use `-relevance` to rank
#'   from lowest to highest. To filter and rank at once, give `NA` to the rows
#'   you want left out, for example `ifelse(label == "clean", relevance, NA)`.
#'   A date cannot be used to rank directly: write `as.numeric(date)` to rank
#'   by it, or a comparison such as `date >= "2024-01-01"` to filter by it.
#'   Rows where the result is `NA` are never shown, and ties are broken at
#'   random. It changes only which text is shown, not the heights or colours
#'   of the map, so a strict filter can leave many columns with no example to
#'   show on hover.
#' @param bins Number of bins along the longer axis. Higher values give finer
#'   detail. By default it grows with the number of rows, from 200 up to 420.
#' @param bandwidth Smoothing applied to the counts, in bins. Larger values give
#'   rounder hills.
#' @param roughness Between 0 and 1. How much fine-grained texture from the raw
#'   counts is mixed into the smoothed surface.
#' @param height_scale How density maps onto height. "relief" (default) sets a
#'   sea level so sparse ground stays flat and dense regions rise steeply, with
#'   the very tallest peaks softened so one large cluster does not flatten the
#'   rest. "linear", "sqrt" and "log" are plain transforms of the density, each
#'   lifting the low ground more than the last.
#' @param spread How far to push groups apart, as a fraction of the distance
#'   between each group's centre and the overall centre. Gives clusters room to
#'   breathe. Groups that are mixed together are hardly moved. Set to 0 to keep
#'   the original co-ordinates.
#' @param compress_outliers Pull far-flung points and groups in towards the
#'   main landmass so that it fills the view. Groups are moved whole, so their
#'   shape is kept.
#' @param height_by "count" makes the peak of each group's mountain
#'   proportional to its number of documents, so heights compare topic sizes.
#'   "density" uses local density, so tight clusters rise higher than loose
#'   ones of the same size. "auto" (default) uses "count" when groups occupy
#'   their own regions, and "density" otherwise.
#' @param place_names When `text_var` is supplied, name regions of the map by
#'   their most distinctive words. Finer place names appear as you zoom in, and
#'   clicking a place opens a panel with its keywords and example documents.
#' @param colour_by "group" colours each bin by the groups in it. "enrichment"
#'   colours each bin by the group that is most over-represented there compared
#'   with its overall share, and leaves bins close to the average neutral. This
#'   suits variables such as sentiment that are spread across the whole map.
#'   "auto" (default) picks "enrichment" when the groups are heavily mixed.
#' @param exaggeration Multiplier for the height of the peaks.
#' @param land_threshold Controls how far the base plate extends around the
#'   points. Lower values give a larger landmass and more islands around
#'   outlying points.
#' @param colours Optional vector of colours for the groups. Can be named by
#'   group level.
#' @param labels Optional named character vector to rename group labels, for
#'   example `c("1" = "Browser privacy")`.
#' @param title,subtitle,caption Text for the poster-style header and footer.
#'   `subtitle` sits in small capitals above the title.
#' @param theme "paper" (light) or "dark".
#' @param file Path to write the HTML to. If `NULL` a temporary file is used.
#' @param open By default (`NULL`) the map is returned visibly, so it opens
#'   when printed at the console and is embedded when used in R Markdown,
#'   Quarto or a pkgdown article. `TRUE` opens it straight away and `FALSE`
#'   only writes the file; both return the result invisibly.
#'
#' @return An object of class `ls_terrain` holding the file path. Printing it
#'   opens the map.
#' @export
#'
#' @examples
#' \dontrun{
#' ls_example %>%
#'   ls_plot_terrain(V1, V2, group_var = cluster, text_var = text,
#'                   title = "Browser Conversation", subtitle = "Landscape of")
#'
#' # A single-colour density map
#' ls_example %>% ls_plot_terrain(V1, V2, title = "Density")
#' }
ls_plot_terrain <- function(df,
                            x_var = V1,
                            y_var = V2,
                            group_var = NULL,
                            text_var = NULL,
                            sample_by = NULL,
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
                            open = NULL) {
  height_scale <- match.arg(height_scale)
  colour_by <- match.arg(colour_by)
  height_by <- match.arg(height_by)
  theme <- match.arg(theme)
  if (!is.null(bins) && (!is.numeric(bins) || length(bins) != 1 || bins < 20 || bins > 600)) {
    stop("`bins` must be NULL or a single number between 20 and 600.")
  }

  stopifnot(
    is.data.frame(df),
    is.numeric(spread), spread >= 0,
    is.numeric(bandwidth), bandwidth > 0,
    is.numeric(roughness), roughness >= 0, roughness <= 1,
    is.numeric(exaggeration), exaggeration > 0,
    is.numeric(land_threshold), land_threshold > 0
  )

  x <- terrain_pull(df, rlang::enquo(x_var), "x_var")
  y <- terrain_pull(df, rlang::enquo(y_var), "y_var")
  g <- terrain_pull(df, rlang::enquo(group_var), "group_var")
  txt <- terrain_pull(df, rlang::enquo(text_var), "text_var")
  sample_by <- rlang::enquo(sample_by)
  pri <- terrain_priority(df, sample_by)
  if (is.null(txt) && !rlang::quo_is_null(sample_by)) warning("`sample_by` has no effect without `text_var`.", call. = FALSE)

  if (!is.numeric(x) || !is.numeric(y)) stop("`x_var` and `y_var` must be numeric.")

  keep <- is.finite(x) & is.finite(y)
  if (!is.null(g)) keep <- keep & !is.na(g)
  if (sum(keep) < 2) stop("Need at least two rows with finite co-ordinates.")
  x <- x[keep]
  y <- y[keep]
  if (!is.null(g)) g <- g[keep]
  if (!is.null(txt)) txt <- as.character(txt[keep])
  pri <- pri[keep]

  # Groups -------------------------------------------------------------------
  if (is.null(g)) {
    g <- factor(rep("All", length(x)))
    grouped <- FALSE
  } else {
    g <- droplevels(as.factor(g))
    grouped <- TRUE
  }
  lev <- levels(g)
  n_groups <- length(lev)
  if (grouped && n_groups > 64) stop("`group_var` has more than 64 levels; too many to colour.")
  gi <- as.integer(g)

  # Spread -------------------------------------------------------------------
  # Push each group away from the overall centre in proportion to how far its
  # own centre already sits from it. Groups that occupy their own region gain
  # clear ground between them; groups that are mixed together barely move
  # because their centres all sit near the middle.
  if (grouped && spread > 0) {
    gx <- tapply(x, gi, stats::median)
    gy <- tapply(y, gi, stats::median)
    x <- x + spread * (gx[gi] - stats::median(x))
    y <- y + spread * (gy[gi] - stats::median(y))
  }

  # Outliers ----------------------------------------------------------------
  # Far-flung points stretch the map so the main landmass ends up small. Pull
  # them in: whole groups that sit far out are moved rigidly (keeping their
  # shape), then any remaining stragglers are drawn in along their radius.
  if (compress_outliers) {
    mx <- stats::median(x)
    my <- stats::median(y)
    # Scale factor that moves a point at radius r to its pulled-in radius.
    pull_factor <- function(r, r0, s) {
      f <- rep(1, length(r))
      far <- which(r > r0)
      f[far] <- (r0 + s * log1p((r[far] - r0) / s)) / r[far]
      f
    }
    r_all <- sqrt((x - mx)^2 + (y - my)^2)
    r0 <- stats::quantile(r_all, 0.9, names = FALSE)
    if (grouped && r0 > 0) {
      gx <- tapply(x, gi, mean)
      gy <- tapply(y, gi, mean)
      rc <- sqrt((gx - mx)^2 + (gy - my)^2)
      f <- pull_factor(rc, r0, 0.3 * r0)
      x <- x + ((gx - mx) * (f - 1))[gi]
      y <- y + ((gy - my) * (f - 1))[gi]
      r_all <- sqrt((x - mx)^2 + (y - my)^2)
    }
    r1 <- stats::quantile(r_all, 0.995, names = FALSE)
    if (r1 > 0) {
      f <- pull_factor(r_all, r1, 0.5 * r1)
      far <- which(f < 1)
      x[far] <- mx + (x[far] - mx) * f[far]
      y[far] <- my + (y[far] - my) * f[far]
    }
  }

  # More data supports a finer grid.
  if (is.null(bins)) bins <- max(200, min(420, round(26 * length(x)^0.25)))

  # Grid ---------------------------------------------------------------------
  # Square cells, `bins` along the longer side, with padding so that smoothed
  # hills and the base plate do not touch the edge of the grid.
  mask_sigma <- max(2.5, bandwidth * 2.2)
  pad_cells <- ceiling(mask_sigma * 4.5) + 3
  span <- max(diff(range(x)), diff(range(y)))
  if (span == 0) span <- 1
  inner <- bins - 2 * pad_cells
  cell <- span / inner
  nx <- ceiling(diff(range(x)) / cell) + 2 * pad_cells + 1
  ny <- ceiling(diff(range(y)) / cell) + 2 * pad_cells + 1
  x0 <- min(x) - pad_cells * cell
  y0 <- min(y) - pad_cells * cell
  ix <- pmin(nx - 1L, as.integer(floor((x - x0) / cell)))
  iy <- pmin(ny - 1L, as.integer(floor((y - y0) / cell)))
  cell_id <- ix + iy * nx # 0-based, x varies fastest

  raw_total <- matrix(tabulate(cell_id + 1L, nbins = nx * ny), nx, ny)

  # Land mask: a wider blur of the counts, thresholded relative to the peak a
  # single isolated document would produce. With large data sets the faint
  # tails would cover the whole grid, so the cut-off also scales with the
  # densest region.
  land <- terrain_kernel(nx, mask_sigma) %*% raw_total %*% t(terrain_kernel(ny, mask_sigma))
  single_peak <- 1 / (2 * pi * mask_sigma^2)
  cutoff <- land_threshold * max(single_peak, 0.01 * max(land))
  # With small data every document clears the cut-off on its own, so nothing
  # is lost; with large data stray documents in empty space are left off the
  # plate rather than drawn as a field of specks.
  mask <- land > cutoff
  # Keep a clear border so the plate never touches the edge of the grid.
  mask[c(1:2, nx - 1:0), ] <- FALSE
  mask[, c(1:2, ny - 1:0)] <- FALSE
  cells <- which(mask) - 1L

  # Smoothing ----------------------------------------------------------------
  # Separable Gaussian blur written as two matrix products (Kx %*% M %*% t(Ky)).
  # Groups are smoothed one at a time and only their land bins are kept, so
  # memory never holds a full grid per group.
  kx_main <- terrain_kernel(nx, bandwidth)
  tky_main <- t(terrain_kernel(ny, bandwidth))
  kx_fine <- terrain_kernel(nx, 0.6)
  tky_fine <- t(terrain_kernel(ny, 0.6))
  ids_by_group <- split(cell_id + 1L, factor(gi, levels = seq_len(n_groups)))
  rm(ix, iy)

  total <- matrix(0, nx, ny)
  peak_main <- numeric(n_groups)
  dens_flat <- matrix(0, length(cells), n_groups)
  for (k in seq_len(n_groups)) {
    m <- matrix(tabulate(ids_by_group[[k]], nbins = nx * ny), nx, ny)
    main <- kx_main %*% m %*% tky_main
    peak_main[k] <- max(main)
    d <- (1 - roughness) * main + roughness * (kx_fine %*% m %*% tky_fine)
    total <- total + d
    dens_flat[, k] <- d[cells + 1L]
  }
  rm(ids_by_group, m, main, d)

  cell_total <- total[cells + 1L]
  share <- dens_flat / pmax(cell_total, 1e-12)

  # Sparse form for the browser: groups holding >1% of a cell, in cell order.
  sig_t <- t(share > 0.01 & dens_flat > 1e-6)
  n_sig <- colSums(sig_t)
  g_idx <- (which(sig_t) - 1L) %% n_groups
  g_val <- t(dens_flat)[sig_t]
  rm(sig_t)

  # Group summaries ------------------------------------------------------------
  group_n <- as.vector(table(g))
  base_share <- group_n / sum(group_n)

  # How well groups separate in space: the density-weighted purity of the bins,
  # rescaled so 0 means "no purer than the overall mix" and 1 means every bin
  # holds a single group.
  dominant <- max.col(share, ties.method = "first")
  purity <- sum(cell_total * share[cbind(seq_along(cells), dominant)]) / sum(cell_total)
  coherence <- if (n_groups > 1) (purity - max(base_share)) / (1 - max(base_share)) else 1
  if (colour_by == "auto") {
    colour_by <- if (grouped && coherence < 0.3) "enrichment" else "group"
  }
  if (!grouped) colour_by <- "group"
  # Count mode: each group's mountain peaks at a height proportional to its
  # number of documents, so a small tight cluster cannot out-climb a big one.
  if (height_by == "auto") {
    height_by <- if (grouped && colour_by == "group") "count" else "density"
  }
  group_scale <- (tabulate(gi, n_groups) / max(tabulate(gi, n_groups))) / pmax(peak_main, 1e-12)

  # Label anchors: the peak of each group's own territory, or for mixed groups
  # the dense spot where it is most over-represented (by odds ratio, so that
  # common and rare groups are treated alike).
  logit <- function(p) {
    p <- pmin(pmax(p, 1e-4), 1 - 1e-4)
    log(p / (1 - p))
  }
  anchors <- vapply(seq_len(n_groups), function(k) {
    score <- if (colour_by == "enrichment") {
      dens_flat[, k] * pmax(0, logit(share[, k]) - logit(base_share[k]))
    } else {
      dens_flat[, k] * share[, k]
    }
    cells[which.max(score)]
  }, integer(1))

  pal <- terrain_colours(lev, colours)
  label_text <- lev
  if (!is.null(labels)) {
    hit <- match(names(labels), lev)
    label_text[hit[!is.na(hit)]] <- as.character(labels[!is.na(hit)])
  }

  # Place names ------------------------------------------------------------------
  places <- NULL
  if (isTRUE(place_names) && !is.null(txt)) {
    reg <- terrain_regions(x, y, gi, txt, pri, n_groups, by_group = grouped && colour_by == "group")
    # Cell centres in data co-ordinates, for assigning cells to regions.
    ccx <- x0 + ((cells %% nx) + 0.5) * cell
    ccy <- y0 + ((cells %/% nx) + 0.5) * cell
    layer_out <- function(L, level) {
      R <- length(L$cx)
      if (all(is.na(L$owner))) {
        cr <- terrain_nearest(ccx, ccy, L$cx, L$cy)
      } else {
        cr <- rep(NA_integer_, length(cells))
        for (g in unique(L$owner)) {
          own <- which(L$owner == g)
          hit <- dominant == g
          if (any(hit)) cr[hit] <- own[terrain_nearest(ccx[hit], ccy[hit], L$cx[own], L$cy[own])]
        }
      }
      anchor <- vapply(seq_len(R), function(r) {
        idx <- which(cr == r)
        if (!length(idx)) return(cells[which.min((ccx - L$cx[r])^2 + (ccy - L$cy[r])^2)])
        cells[idx[which.max(cell_total[idx])]]
      }, integer(1))
      list(
        level = level,
        cellRegion = I(ifelse(is.na(cr), -1L, cr - 1L)),
        regions = lapply(seq_len(R), function(r) c(L$info[[r]], list(
          anchor = anchor[r],
          owner = if (is.na(L$owner[r])) -1L else L$owner[r] - 1L
        )))
      )
    }
    places <- list(groups = reg$groups, layers = list())
    # Groups named only by a number (cluster ids) get their keywords as a name.
    if (grouped && is.null(labels) && all(grepl("^[0-9]+$", lev))) {
      kw <- lapply(reg$groups, function(g) as.character(g$keywords))
      auto <- vapply(kw, function(k) if (length(k)) paste(utils::head(k, 2), collapse = " & ") else "", character(1))
      label_text[nzchar(auto)] <- gsub("(^|\\s)(\\p{L})", "\\1\\U\\2", auto[nzchar(auto)], perl = TRUE)
    }
    if (!is.null(reg$coarse)) places$layers <- c(places$layers, list(layer_out(reg$coarse, 1L)))
    places$layers <- c(places$layers, list(layer_out(reg$fine, 2L)))
  }

  # Example text per bin --------------------------------------------------------
  # The best-ranked document in each bin, so bins whose documents are all
  # filtered out by `sample_by` have no example.
  samples <- NULL
  if (!is.null(txt)) {
    ord <- terrain_order(pri)
    first <- ord[!duplicated(cell_id[ord])]
    s <- txt[first]
    s <- gsub("\\s+", " ", s)
    long <- nchar(s) > 180
    s[long] <- paste0(substr(s[long], 1, 177), "...")
    samples <- list(cell = I(cell_id[first]), text = I(s))
  }

  payload <- list(
    nx = nx, ny = ny,
    grouped = grouped,
    heightScale = height_scale,
    colourBy = colour_by,
    heightBy = height_by,
    coherence = round(coherence, 3),
    exaggeration = exaggeration,
    theme = theme,
    nDocs = length(x),
    cells = I(cells),
    raw = I(as.integer(raw_total[cells + 1L])),
    nDrawn = sum(raw_total[cells + 1L]),
    gCount = I(as.integer(n_sig)),
    gIdx = I(as.integer(g_idx)),
    gVal = I(signif(g_val, 4)),
    groups = list(
      name = I(lev),
      label = I(label_text),
      n = I(group_n),
      share = I(signif(base_share, 4)),
      colour = I(unname(pal)),
      anchor = I(anchors),
      scale = I(signif(group_scale, 5))
    ),
    places = places,
    samples = samples,
    title = title %||% "",
    subtitle = subtitle %||% "",
    caption = caption %||% ""
  )

  html <- terrain_html(payload)
  if (is.null(file)) file <- tempfile("ls_terrain_", fileext = ".html")
  writeLines(html, file, useBytes = TRUE)

  out <- structure(list(file = normalizePath(file)), class = "ls_terrain")
  if (is.null(open)) return(out)
  if (isTRUE(open)) print(out)
  invisible(out)
}

#' @export
print.ls_terrain <- function(x, ...) {
  viewer <- getOption("viewer")
  # The RStudio viewer only serves files inside the session's temp directory.
  in_temp <- startsWith(x$file, normalizePath(tempdir()))
  if (!is.null(viewer) && in_temp) {
    viewer(x$file)
  } else {
    utils::browseURL(x$file)
  }
  invisible(x)
}

#' Show a terrain map inside R Markdown, Quarto or a pkgdown article
#'
#' The map is embedded as an `<iframe>` holding the whole page, so the output
#' document stays self-contained.
#'
#' @param x An `ls_terrain` object.
#' @param options Chunk options. `out.height` sets the frame height (default
#'   "640px").
#' @param ... Not used.
#' @keywords internal
#' @exportS3Method knitr::knit_print
knit_print.ls_terrain <- function(x, options = list(), ...) {
  html <- paste(readLines(x$file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  html <- gsub("&", "&amp;", html, fixed = TRUE)
  html <- gsub("\"", "&quot;", html, fixed = TRUE)
  # One line with no raw angle brackets, so Markdown converters pass it through.
  html <- gsub("<", "&lt;", html, fixed = TRUE)
  html <- gsub(">", "&gt;", html, fixed = TRUE)
  html <- gsub("\n", "&#10;", html, fixed = TRUE)
  height <- options$out.height %||% "640px"
  knitr::asis_output(paste0(
    '<iframe srcdoc="', html, '" style="width: 100%; height: ', height,
    '; border: 0; border-radius: 12px;" loading="lazy" title="Terrain map"></iframe>'
  ))
}

# Helpers ----------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

terrain_pull <- function(df, quo, arg) {
  if (rlang::quo_is_null(quo)) return(NULL)
  col <- tryCatch(rlang::as_name(quo), error = function(e) NULL)
  if (is.null(col) || !col %in% names(df)) {
    stop("Cannot find the column given to `", arg, "` in the data frame.", call. = FALSE)
  }
  df[[col]]
}

# Display priority of each document's text, evaluated like a dplyr::filter() or
# dplyr::arrange() argument. Lower values are shown first and NA is never
# shown: a logical result keeps the TRUE rows, a number ranks highest first.
# With no expression every row ties, so the choice is random.
terrain_priority <- function(df, quo) {
  if (rlang::quo_is_null(quo)) return(numeric(nrow(df)))
  s <- rlang::eval_tidy(quo, df)
  if (length(s) != nrow(df)) {
    stop("`sample_by` must give one value per row of the data frame.", call. = FALSE)
  }
  if (is.logical(s)) return(ifelse(s, 0, NA_real_))
  if (is.numeric(s)) return(-as.numeric(s))
  if (inherits(s, c("Date", "POSIXt"))) {
    stop("`sample_by` cannot rank by a date directly. Write `as.numeric(date)` to show the newest ",
         "first, or a comparison such as `date >= \"2024-01-01\"` to keep a date range.", call. = FALSE)
  }
  stop("`sample_by` must give a logical or numeric value. To keep certain categories, ",
       "write a comparison such as `label == \"clean\"`.", call. = FALSE)
}

# Row order for picking example text: best priority first, ties at random, and
# rows with NA priority left out.
terrain_order <- function(priority) {
  order(priority, stats::runif(length(priority)), na.last = NA)
}

# Row-normalised Gaussian smoothing matrix, so mass is preserved away from edges.
terrain_kernel <- function(n, sigma) {
  d <- outer(seq_len(n), seq_len(n), "-")
  k <- exp(-d^2 / (2 * sigma^2))
  k[abs(d) > ceiling(4 * sigma)] <- 0
  k / sum(exp(-(-ceiling(4 * sigma):ceiling(4 * sigma))^2 / (2 * sigma^2)))
}

terrain_colours <- function(lev, colours) {
  base <- c(
    "#5468E7", "#E2467E", "#13A89E", "#F0A13A", "#8B5CF6", "#2E9BD6",
    "#E0643C", "#4CAF6E", "#C056C9", "#8A9A3B", "#D4A017", "#3D5A80",
    "#F27BA9", "#00897B", "#A0522D", "#7E8CF2", "#B8336A", "#5FB3B3",
    "#9C6ADE", "#C9A227"
  )
  n <- length(lev)
  sentiment <- c(positive = "#2E9E6B", negative = "#DD4A4F", neutral = "#E3A33A", mixed = "#8B5CF6")
  if (all(tolower(lev) %in% names(sentiment))) {
    base <- unname(sentiment[tolower(lev)])
  }
  pal <- if (n <= length(base)) base[seq_len(n)] else grDevices::hcl(seq(15, 375, length.out = n + 1)[-1], 70, 60)
  names(pal) <- lev
  if (!is.null(colours)) {
    if (is.null(names(colours))) {
      pal[seq_len(min(n, length(colours)))] <- colours[seq_len(min(n, length(colours)))]
    } else {
      hit <- intersect(names(colours), lev)
      pal[hit] <- colours[hit]
    }
  }
  # Normalise any colour name to hex.
  rgb <- grDevices::col2rgb(pal)
  grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ], maxColorValue = 255)
}

terrain_html <- function(payload) {
  dir <- system.file("terrain", package = "LandscapeR")
  if (!nzchar(dir)) dir <- file.path("inst", "terrain") # during development
  read <- function(f) paste(readLines(file.path(dir, f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, digits = NA, null = "null")
  json <- gsub("</", "<\\/", json, fixed = TRUE)

  page_title <- if (nzchar(payload$title)) payload$title else "Landscape"
  html <- read("template.html")
  html <- terrain_fill(html, "{{TITLE}}", terrain_escape(page_title))
  html <- terrain_fill(html, "{{CSS}}", read("terrain.css"))
  html <- terrain_fill(html, "{{DATA}}", json)
  html <- terrain_fill(html, "{{JS}}", read("terrain.js"))
  html
}

# Literal placeholder replacement (sub() would interpret backslashes in `value`).
terrain_fill <- function(html, key, value) {
  at <- regexpr(key, html, fixed = TRUE)
  if (at < 0) stop("Template placeholder ", key, " is missing.")
  paste0(substr(html, 1, at - 1), value, substr(html, at + nchar(key), nchar(html)))
}

terrain_escape <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  gsub(">", "&gt;", s, fixed = TRUE)
}
