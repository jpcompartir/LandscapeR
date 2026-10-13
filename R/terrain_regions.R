# Place names for ls_plot_terrain() ---------------------------------------------
#
# Regions are found with k-means on the displayed co-ordinates and named by the
# words that most distinguish their documents from the rest of the corpus
# (weighted log-odds with an informative Dirichlet prior, as in tidylo).
# Everything here is base R plus `stats`.

terrain_stopwords <- c(
  "a", "about", "above", "after", "again", "against", "all", "also", "am", "an", "and",
  "any", "are", "aren't", "as", "at", "be", "because", "been", "before", "being", "below",
  "between", "both", "but", "by", "can", "can't", "cannot", "could", "couldn't", "did",
  "didn't", "do", "does", "doesn't", "doing", "don't", "down", "during", "each", "even",
  "ever", "every", "few", "for", "from", "further", "get", "gets", "getting", "got", "had",
  "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "her", "here", "hers",
  "herself", "him", "himself", "his", "how", "i'm", "i've", "i'd", "i'll", "if", "in",
  "into", "is", "isn't", "it", "it's", "its", "itself", "just", "know", "let", "let's",
  "like", "lol", "make", "many", "may", "me", "might", "more", "most", "much", "must", "my",
  "myself", "need", "new", "no", "nor", "not", "now", "of", "off", "on", "once", "one",
  "only", "or", "other", "our", "ours", "ourselves", "out", "over", "own", "really",
  "same", "say", "see", "she", "should", "shouldn't", "so", "some", "still", "such",
  "than", "that", "that's", "the", "their", "theirs", "them", "themselves", "then",
  "there", "there's", "these", "they", "they're", "thing", "things", "think", "this",
  "those", "through", "to", "too", "under", "until", "up", "use", "used", "using", "very",
  "via", "want", "was", "wasn't", "way", "we", "we're", "well", "were", "weren't", "what",
  "what's", "when", "where", "which", "while", "who", "whom", "why", "will", "with",
  "won't", "would", "wouldn't", "yeah", "yes", "yet", "you", "you're", "you've", "your",
  "yours", "yourself", "yourselves", "amp", "http", "https", "www", "com", "gt", "lt",
  "rt", "via", "don", "doesn", "didn", "isn", "ve", "ll", "re", "im", "ive", "dont",
  "cant", "wont", "thats", "go", "going", "good", "great", "time", "first", "two",
  "people", "anyone", "something", "anything", "though", "lot", "actually", "since"
)

# Document-level word presence: a two-column integer matrix of (doc, word id)
# plus the vocabulary. Rare words are dropped to keep the matrices small.
terrain_doc_words <- function(txt, min_docs = 5, max_vocab = 6000) {
  t <- tolower(txt)
  t <- gsub("https?://\\S+|www\\.\\S+|[@#]\\w+|&\\w+;", " ", t, perl = TRUE)
  t <- gsub("\u2019", "'", t, fixed = TRUE)
  # strsplit is far lighter on memory than regmatches(gregexpr()).
  w <- strsplit(t, "[^\\p{L}\\p{N}']+", perl = TRUE)
  rm(t)
  doc <- rep.int(seq_along(w), lengths(w))
  w <- unlist(w, use.names = FALSE)
  w <- sub("^'+|'+$", "", sub("'s$", "", w))
  keep <- nchar(w) >= 3 & grepl("^\\p{L}", w, perl = TRUE) & !w %in% terrain_stopwords
  doc <- doc[keep]
  w <- w[keep]
  # Count each word once per document, using integer keys rather than pasted strings.
  uw <- unique(w)
  id <- match(w, uw)
  dup <- duplicated(doc * (length(uw) + 1) + id)
  doc <- doc[!dup]
  id <- id[!dup]
  freq <- tabulate(id, nbins = length(uw))
  ord <- order(freq, decreasing = TRUE)
  ord <- utils::head(ord[freq[ord] >= min_docs], max_vocab)
  vocab <- uw[ord]
  word <- match(id, ord)
  ok <- !is.na(word)
  list(doc = doc[ok], word = word[ok], vocab = vocab)
}

# Top distinguishing words for each region. `region` is an integer vector (one
# per document, 1..R, NA for documents not in any region).
terrain_keywords <- function(dw, region, n_regions, top = 8) {
  if (!length(dw$vocab)) return(replicate(n_regions, character(0), simplify = FALSE))
  r <- region[dw$doc]
  ok <- !is.na(r)
  V <- length(dw$vocab)
  y <- matrix(tabulate((r[ok] - 1L) * V + dw$word[ok], nbins = n_regions * V),
              nrow = n_regions, byrow = TRUE)
  word_total <- colSums(y)
  n_r <- rowSums(y)
  n_all <- sum(y)
  alpha <- word_total * (min(1, 5000 / n_all)) + 0.5
  a0 <- sum(alpha)
  lapply(seq_len(n_regions), function(i) {
    yi <- y[i, ]
    yo <- word_total - yi
    ni <- n_r[i]
    no <- n_all - ni
    d <- log((yi + alpha) / (ni + a0 - yi - alpha)) - log((yo + alpha) / (no + a0 - yo - alpha))
    z <- d / sqrt(1 / (yi + alpha) + 1 / (yo + alpha))
    z[yi < max(2, 0.01 * ni / 8)] <- -Inf
    ord <- order(z, decreasing = TRUE)
    picked <- character(0)
    for (j in ord) {
      if (!is.finite(z[j]) || length(picked) >= top) break
      w <- dw$vocab[j]
      # Skip near-duplicates such as "extension" / "extensions".
      if (any(startsWith(w, picked) | startsWith(picked, w))) next
      picked <- c(picked, w)
    }
    picked
  })
}

# Nearest-centre assignment for a matrix of points.
terrain_nearest <- function(px, py, cx, cy) {
  best <- rep(1L, length(px))
  bd <- (px - cx[1])^2 + (py - cy[1])^2
  for (k in seq_along(cx)[-1]) {
    d <- (px - cx[k])^2 + (py - cy[k])^2
    hit <- d < bd
    best[hit] <- k
    bd[hit] <- d[hit]
  }
  best
}

# k-means on (a sample of) the points, returning centres.
terrain_kmeans <- function(x, y, k, max_n = 150000) {
  k <- min(k, length(x))
  if (k < 1) return(list(cx = numeric(0), cy = numeric(0)))
  if (k == 1) return(list(cx = mean(x), cy = mean(y)))
  idx <- if (length(x) > max_n) sample.int(length(x), max_n) else seq_along(x)
  pts <- cbind(x[idx], y[idx])
  # kmeans() fails when there are fewer distinct points than centres.
  repeat {
    km <- tryCatch(
      suppressWarnings(stats::kmeans(pts, centers = k, nstart = 3, iter.max = 40)),
      error = function(e) NULL
    )
    if (!is.null(km) || k == 1) break
    k <- max(1, k %/% 2)
  }
  if (is.null(km)) return(list(cx = mean(x), cy = mean(y)))
  list(cx = km$centers[, 1], cy = km$centers[, 2])
}

# Builds the place-name layers. Returns per-document region ids for each layer
# and the summaries the browser needs.
terrain_regions <- function(x, y, gi, txt, n_groups, by_group, n_samples = 5) {
  n <- length(x)
  k_coarse <- max(5, min(12, round(n^(1 / 3) / 2.5)))
  k_fine <- max(12, min(40, 3 * k_coarse))

  layers <- list()
  if (by_group) {
    # Sub-regions inside each group, more for bigger groups.
    size <- tabulate(gi, n_groups)
    k_g <- pmax(1, round(k_fine * sqrt(size) / sum(sqrt(size))))
    k_g[size < 30] <- 0
    region <- rep(NA_integer_, n)
    cx <- cy <- numeric(0)
    owner <- integer(0)
    for (g in which(k_g > 0)) {
      idx <- which(gi == g)
      km <- terrain_kmeans(x[idx], y[idx], k_g[g])
      region[idx] <- length(cx) + terrain_nearest(x[idx], y[idx], km$cx, km$cy)
      cx <- c(cx, km$cx)
      cy <- c(cy, km$cy)
      owner <- c(owner, rep(g, length(km$cx)))
    }
    layers$fine <- list(region = region, cx = cx, cy = cy, owner = owner)
  } else {
    for (lvl in c("coarse", "fine")) {
      km <- terrain_kmeans(x, y, if (lvl == "coarse") k_coarse else k_fine)
      layers[[lvl]] <- list(
        region = terrain_nearest(x, y, km$cx, km$cy),
        cx = km$cx, cy = km$cy, owner = rep(NA_integer_, length(km$cx))
      )
    }
  }

  # Keywords are stable on a sample, and tokenising is the slow step.
  kw_idx <- if (n > 2e5) sort(sample.int(n, 2e5)) else seq_len(n)
  dw <- if (!is.null(txt)) terrain_doc_words(txt[kw_idx]) else NULL
  summarise <- function(region, R) {
    kw <- if (!is.null(dw)) terrain_keywords(dw, region[kw_idx], R) else replicate(R, character(0), simplify = FALSE)
    lapply(seq_len(R), function(r) {
      idx <- which(region == r)
      s <- character(0)
      if (!is.null(txt) && length(idx)) {
        pick <- idx[sample.int(length(idx), min(n_samples, length(idx)))]
        s <- gsub("\\s+", " ", txt[pick])
        long <- nchar(s) > 260
        s[long] <- paste0(substr(s[long], 1, 257), "...")
      }
      list(
        n = length(idx),
        keywords = I(kw[[r]]),
        comp = I(tabulate(gi[idx], n_groups)),
        samples = I(s)
      )
    })
  }

  out <- list(groups = summarise(gi, n_groups))
  for (lvl in names(layers)) {
    L <- layers[[lvl]]
    out[[lvl]] <- list(
      region = L$region,
      cx = L$cx, cy = L$cy, owner = L$owner,
      info = summarise(L$region, length(L$cx))
    )
  }
  out
}
