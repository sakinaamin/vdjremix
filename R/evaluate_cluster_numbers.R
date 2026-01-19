#' Evaluate optimal number of clusters across a specified range
#'
#' Uses multiple clustering validity indices to suggest an
#' appropriate number of clusters.
#'
#' @param dist_mat Distance matrix (as.dist).
#' @param k_range Integer vector of candidate cluster numbers.
#' @param linkage Character. Linkage method for hclust.
#'
#' @return A data.frame summarising suggested cluster numbers per index.
#'
#' @export
evaluate_cluster_numbers <- function(
    dist_mat,
    k_range,
    linkage = "ward.D2"
) {
  stopifnot(inherits(dist_mat, "dist"))

  results <- list()

  for (k in k_range) {
    res <- NbClust::NbClust(
      diss = dist_mat,
      distance = NULL,
      min.nc = k,
      max.nc = k,
      method = linkage,
      index = c("dunn", "silhouette", "mcclain", "cindex", "frey")
    )

    best <- do.call(
      rbind,
      lapply(res$Best.nc, as.numeric)
    )

    results[[as.character(k)]] <- data.frame(
      k = k,
      suggested_k = best
    )
  }

  do.call(rbind, results)
}
