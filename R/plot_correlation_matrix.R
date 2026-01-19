#' Plot correlation and distance matrices for feature clustering
#'
#' Visualises the feature–feature correlation structure and the
#' corresponding distance matrix used for clustering.
#'
#' @param correlation_matrix Symmetric numeric correlation matrix.
#' @param max_features Integer. Maximum number of features to display.
#'
#' @return A named list with:
#' \item{correlation_heatmap}{Correlation heatmap.}
#' \item{distance_heatmap}{Distance heatmap.}
#'
#' @export
plot_correlation_matrix <- function(
    correlation_matrix,
    max_features = 100
) {
  stopifnot(is.matrix(correlation_matrix))

  # Subset for readability
  feat_idx <- seq_len(min(ncol(correlation_matrix), max_features))
  cor_sub <- correlation_matrix[feat_idx, feat_idx, drop = FALSE]

  # ---- Correlation heatmap ----
  p_cor <- pheatmap::pheatmap(
    cor_sub,
    color = colorRampPalette(c("blue", "white", "red"))(100),
    breaks = seq(-1, 1, length.out = 101),
    border_color = NA,
    show_rownames = FALSE,
    show_colnames = FALSE,
    main = "Feature correlation matrix"
  )

  # ---- Distance matrix (used for clustering) ----
  dist_mat <- (1 - cor_sub) / 2

  p_dist <- pheatmap::pheatmap(
    dist_mat,
    color = colorRampPalette(c("white", "black"))(100),
    border_color = NA,
    show_rownames = FALSE,
    show_colnames = FALSE,
    main = "Feature distance matrix"
  )

  list(
    correlation_heatmap = p_cor,
    distance_heatmap = p_dist
  )
}
