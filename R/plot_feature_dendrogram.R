#' Plot feature dendrogram with module assignments
#'
#' Visualises the hierarchical clustering dendrogram used for
#' module identification, with features coloured by module.
#'
#' @param correlation_matrix Symmetric feature–feature correlation matrix.
#' @param module_assignment Data frame with columns 'feature' and 'cluster'.
#' @param linkage_method Character. Linkage method used for clustering.
#' @param show_labels Logical. Whether to show feature labels.
#'
#' @return A dendrogram plot.
#'
#' @export
#' @importFrom stats as.dendrogram
#' @importFrom WGCNA labels2colors


plot_feature_dendrogram <- function(
    correlation_matrix,
    module_assignment,
    linkage_method = "ward.D2",
    show_labels = FALSE
) {
  stopifnot(is.matrix(correlation_matrix))
  stopifnot(is.data.frame(module_assignment))
  stopifnot(all(c("feature", "cluster") %in% colnames(module_assignment)))

  # Distance matrix (same as clustering)
  dist_mat <- as.dist((1 - correlation_matrix) / 2)

  # Hierarchical clustering
  hc <- fastcluster::hclust(dist_mat, method = linkage_method)

  # Convert to dendrogram
  dend <- stats::as.dendrogram(hc)


  # Align module assignments to dendrogram order
  clusters <- module_assignment$cluster
  names(clusters) <- module_assignment$feature

  dend_labels <- labels(dend)
  cluster_vec <- clusters[dend_labels]

  # Replace NA / unassigned with "0"
  cluster_vec[is.na(cluster_vec)] <- 0

  # Convert to colors
  cluster_colors <- WGCNA::labels2colors(cluster_vec)

  # Plot dendrogram with colored labels
  dend <- dendextend::set(dend, "labels_col", cluster_colors)
  dend <- dendextend::set(dend, "labels_cex", ifelse(show_labels, 0.4, 0.01))

  plot(dend, main = "Feature dendrogram with module assignments")
}
