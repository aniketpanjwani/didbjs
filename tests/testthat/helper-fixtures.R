fixture_path <- function(...) {
  testthat::test_path("..", "fixtures", ...)
}

fixtures_present <- function() {
  dir.exists(testthat::test_path("..", "fixtures", "parity", "f002-dynamic-horizons"))
}

read_fixture_csv <- function(...) {
  utils::read.csv(fixture_path(...), na.strings = c("", "NA"), check.names = FALSE)
}

read_covariance_matrix <- function(...) {
  covariance <- read_fixture_csv(...)
  terms <- unique(c(covariance$row_term, covariance$col_term))
  mat <- matrix(NA_real_, nrow = length(terms), ncol = length(terms), dimnames = list(terms, terms))
  for (idx in seq_len(nrow(covariance))) {
    mat[covariance$row_term[[idx]], covariance$col_term[[idx]]] <- covariance$value[[idx]]
  }
  mat
}
