#' Determine subsampling depth for robust correlation estimation
#'
#' Computes the minimum number of samples shared across all feature pairs
#' and returns a fixed subsampling depth for correlation estimation.
#'
#' @param feature_matrix Numeric matrix or data.frame (samples × features).
#' @param features Optional character vector of feature names (default: all columns).
#' @param min_required Integer. Minimum shared samples required to consider a pair.
#' @param safety_fraction Numeric in (0,1). Fraction of minimum overlap to use.
#'
#' @return Integer subsampling depth.
#'
#' @export
get_subsample_depth <- function(
    feature_matrix,
    features = colnames(feature_matrix),
    min_required = 5,
    safety_fraction = 0.8
) {
  stopifnot(is.matrix(feature_matrix) || is.data.frame(feature_matrix))

  mat <- as.matrix(feature_matrix)
  min_shared <- Inf
  n_feat <- length(features)

  for (i in seq_len(n_feat - 1)) {
    for (j in seq(i + 1, n_feat)) {

      x <- mat[, features[i]]
      y <- mat[, features[j]]
      shared <- sum(!is.na(x) & !is.na(y))

      if (shared >= min_required && shared < min_shared) {
        min_shared <- shared
      }
    }
  }

  if (!is.finite(min_shared)) {
    stop("No feature pairs met the minimum shared sample requirement.")
  }

  depth <- floor(safety_fraction * min_shared)

  message("Subsampling depth for correlation estimation: ", depth)

  depth
}
