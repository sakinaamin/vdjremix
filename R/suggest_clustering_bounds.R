#' Suggest clustering bounds for feature modularisation
#'
#' Provides data-driven default bounds for the number of clusters
#' and minimum module size, based on feature dimensionality.
#'
#' @param n_features Integer. Number of features to cluster.
#'
#' @return A list with suggested clustering parameters.
#'
#' @export
suggest_clustering_bounds <- function(n_features) {

  stopifnot(n_features > 5)

  # Conservative, interpretable defaults
  min_cluster_size <- max(3, floor(0.05 * n_features))

  bottom_k <- max(5, floor(0.10 * n_features))
  top_k    <- min(floor(0.40 * n_features), n_features - 1)

  list(
    min_cluster_size = min_cluster_size,
    bottom_k = bottom_k,
    top_k = top_k
  )
}
