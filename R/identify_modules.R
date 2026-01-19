#' Identify feature modules from a correlation matrix
#'
#' Clusters repertoire features using hierarchical clustering
#' and Dynamic Tree Cut.
#'
#' @param correlation_matrix Symmetric feature–feature correlation matrix.
#' @param min_cluster_size Integer. Minimum module size.
#' @param deep_split Logical. Whether to use deep splitting in Dynamic Tree Cut.
#'
#' @return A list with:
#' \item{modules}{Named list of feature vectors (one per module).}
#' \item{assignment}{Data frame mapping features to modules.}
#' \item{linkage}{Selected hierarchical clustering method.}
#'
#' @export
identify_modules <- function(
    correlation_matrix,
    min_cluster_size = 10,
    deep_split = TRUE
) {
  stopifnot(is.matrix(correlation_matrix))

  features <- colnames(correlation_matrix)

  # Distance matrix
  dist_mat <- .compute_distance(correlation_matrix)

  # Select linkage method
  linkage <- .choose_linkage_method(dist_mat)
  method <- linkage$best_method

  # Hierarchical clustering
  hc <- fastcluster::hclust(dist_mat, method = method)

  # Dynamic Tree Cut
  cut <- dynamicTreeCut::cutreeDynamic(
    dendro = hc,
    distM = as.matrix(dist_mat),
    minClusterSize = min_cluster_size,
    method = "hybrid",
    deepSplit = deep_split,
    verbose = 0
  )

  # Build assignment table
  assignment <- data.frame(
    feature = features,
    cluster = cut,
    stringsAsFactors = FALSE
  )

  # Remove unassigned (cluster 0)
  assignment$cluster[assignment$cluster == 0] <- NA

  # Build module list
  modules <- split(
    assignment$feature[!is.na(assignment$cluster)],
    assignment$cluster[!is.na(assignment$cluster)]
  )

  list(
    modules = modules,
    assignment = assignment,
    linkage = linkage
  )
}
