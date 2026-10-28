terrain_payload <- function(out) {
  html <- paste(readLines(out$file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  json <- regmatches(html, regexpr('(?s)<script type="application/json" id="terrain-data">.*?</script>', html, perl = TRUE))
  json <- sub('^<script[^>]*>', "", sub("</script>$", "", json))
  jsonlite::fromJSON(json, simplifyVector = TRUE)
}

test_that("writes a self-contained HTML file with every placeholder filled", {
  out <- ls_plot_terrain(ls_example, V1, V2, group_var = cluster, open = FALSE)
  expect_s3_class(out, "ls_terrain")
  expect_true(file.exists(out$file))
  html <- paste(readLines(out$file, warn = FALSE), collapse = "\n")
  expect_false(grepl("{{", html, fixed = TRUE))
  expect_true(grepl("webgl2", html, fixed = TRUE))
})

test_that("payload keeps every document and every group", {
  out <- ls_plot_terrain(ls_example, V1, V2, group_var = cluster, open = FALSE)
  p <- terrain_payload(out)
  expect_equal(p$nDocs, nrow(ls_example))
  # Small data: every document lands on the plate.
  expect_equal(sum(p$raw), nrow(ls_example))
  expect_equal(p$nDrawn, nrow(ls_example))
  expect_equal(p$groups$n, as.vector(table(ls_example$cluster)))
  expect_equal(length(p$gIdx), sum(p$gCount))
  expect_equal(length(p$cells), length(p$raw))
  expect_true(all(p$groups$anchor %in% p$cells))
})

test_that("works without a grouping variable and keeps arrays as arrays", {
  out <- ls_plot_terrain(ls_example, V1, V2, open = FALSE)
  p <- terrain_payload(out)
  expect_false(p$grouped)
  expect_length(p$groups$name, 1)
  expect_type(p$groups$colour, "character")
})

test_that("labels, colours and text samples are applied", {
  out <- ls_plot_terrain(
    ls_example, V1, V2,
    group_var = sentiment, text_var = text,
    labels = c(positive = "Happy"), colours = c(negative = "red"),
    open = FALSE
  )
  p <- terrain_payload(out)
  expect_equal(p$groups$label[p$groups$name == "positive"], "Happy")
  expect_equal(p$groups$colour[p$groups$name == "negative"], "#FF0000")
  expect_true(length(p$samples$text) > 0)
  expect_true(all(nchar(p$samples$text) <= 180))
})

test_that("drops rows with missing co-ordinates and rejects bad input", {
  df <- ls_example
  df$V1[1:10] <- NA
  p <- terrain_payload(ls_plot_terrain(df, V1, V2, open = FALSE))
  expect_equal(p$nDocs, nrow(df) - 10)

  expect_error(ls_plot_terrain(ls_example, not_a_column, V2, open = FALSE), "x_var")
  expect_error(ls_plot_terrain(ls_example, V1, V2, bins = 5, open = FALSE))
  expect_error(ls_plot_terrain(ls_example[1, ], V1, V2, open = FALSE), "two rows")
})

test_that("text containing a closing script tag cannot break the page", {
  df <- ls_example[1:200, ]
  df$text[1] <- "</script><script>alert(1)</script>"
  out <- ls_plot_terrain(df, V1, V2, text_var = text, open = FALSE)
  html <- paste(readLines(out$file, warn = FALSE), collapse = "\n")
  expect_false(grepl("</script><script>alert", html, fixed = TRUE))
})

test_that("place names come from the text and cluster ids get keyword names", {
  out <- ls_plot_terrain(ls_example, V1, V2, group_var = cluster, text_var = text, open = FALSE)
  p <- terrain_payload(out)
  expect_equal(p$heightBy, "count")
  expect_length(p$groups$scale, length(unique(ls_example$cluster)))
  expect_false(any(grepl("^[0-9]+$", p$groups$label)))
  expect_true(length(p$places$layers) >= 1)
  fine <- p$places$layers$regions[[length(p$places$layers$regions)]]
  expect_true(all(fine$anchor %in% p$cells))
  expect_true(all(lengths(fine$keywords) > 0))
  expect_equal(length(p$places$layers$cellRegion[[1]]), length(p$cells))
})

test_that("explicit labels are kept and mixed groups use density heights", {
  out <- ls_plot_terrain(ls_example, V1, V2, group_var = sentiment, text_var = text, open = FALSE)
  p <- terrain_payload(out)
  expect_equal(p$colourBy, "enrichment")
  expect_equal(p$heightBy, "density")
  expect_setequal(p$groups$label, c("negative", "neutral", "positive"))
  expect_length(p$places$layers$level, 2)
})

test_that("place names can be switched off", {
  out <- ls_plot_terrain(ls_example, V1, V2, group_var = cluster, text_var = text,
                         place_names = FALSE, open = FALSE)
  expect_null(terrain_payload(out)$places)
})

test_that("sample_by as a filter only shows the chosen documents", {
  df <- ls_example
  df$keep <- df$sentiment == "positive"
  df$keep[1:50] <- NA
  df$tagged <- paste(ifelse(df$keep %in% TRUE, "KEEP", "DROP"), df$text)
  out <- ls_plot_terrain(df, V1, V2, group_var = cluster, text_var = tagged,
                         sample_by = keep, open = FALSE)
  p <- terrain_payload(out)
  panel <- c(unlist(p$places$groups$samples), unlist(lapply(p$places$layers$regions, `[[`, "samples")))
  expect_true(length(p$samples$text) > 0)
  expect_true(all(startsWith(p$samples$text, "KEEP")))
  expect_true(length(panel) > 0)
  expect_true(all(startsWith(panel, "KEEP")))
  # Filtering the text does not drop documents from the map.
  expect_equal(p$nDocs, nrow(df))
})

test_that("sample_by as a number ranks the documents, highest first", {
  df <- ls_example
  df$score <- sample(nrow(df))
  df$tagged <- paste0("#", df$score, " ", df$text)
  score_of <- function(s) as.integer(sub("^#([0-9]+) .*", "\\1", s))
  top5 <- function(f) unname(lapply(split(f(df$score), df$cluster), function(v) sort(v, decreasing = TRUE)[1:5]))

  p <- terrain_payload(ls_plot_terrain(df, V1, V2, group_var = cluster, text_var = tagged,
                                       sample_by = score, open = FALSE))
  expect_equal(lapply(p$places$groups$samples, score_of), top5(identity))

  p <- terrain_payload(ls_plot_terrain(df, V1, V2, group_var = cluster, text_var = tagged,
                                       sample_by = -score, open = FALSE))
  expect_equal(lapply(p$places$groups$samples, function(s) -score_of(s)), top5(`-`))
})

test_that("sample_by rejects values it cannot rank", {
  expect_error(ls_plot_terrain(ls_example, V1, V2, text_var = text, sample_by = sentiment, open = FALSE),
               "logical or numeric")
  expect_error(ls_plot_terrain(ls_example, V1, V2, text_var = text, sample_by = date, open = FALSE),
               "as.numeric")
  expect_error(ls_plot_terrain(ls_example, V1, V2, text_var = text, sample_by = c(TRUE, FALSE), open = FALSE),
               "one value per row")
  expect_warning(ls_plot_terrain(ls_example, V1, V2, sample_by = sentiment == "positive", open = FALSE),
                 "no effect without `text_var`")
})
