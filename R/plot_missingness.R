#' Plot missingness structure in a feature matrix
#'
#' Visualises missing data patterns across features and samples.
#'
#' @param feature_matrix Numeric matrix or data.frame (samples × features).
#' @param max_features Integer. Maximum number of features to display in heatmap.
#' @param max_samples Integer. Maximum number of samples to display in heatmap.
#'
#' @return A named list of ggplot objects:
#'   \item{feature_missingness}{Bar plot of missingness per feature.}
#'   \item{missingness_heatmap}{Heatmap of missingness structure.}
#'
#' @export
plot_missingness <- function(
    feature_matrix,
    max_features = 100,
    max_samples = 100
) {
  stopifnot(is.matrix(feature_matrix) || is.data.frame(feature_matrix))

  mat <- as.matrix(feature_matrix)

  # ---- Feature-level missingness ----
  feature_na <- colMeans(is.na(mat))
  df_feat <- data.frame(
    feature = names(feature_na),
    missing_fraction = feature_na
  )

  df_feat <- df_feat[order(df_feat$missing_fraction, decreasing = TRUE), ]

  p_feat <- ggplot2::ggplot(
    df_feat,
    ggplot2::aes(
      x = reorder(feature, missing_fraction),
      y = missing_fraction
    )
  ) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Feature",
      y = "Proportion missing",
      title = "Missingness per feature"
    )

  # ---- Heatmap (subsample for readability) ----
  feat_idx <- seq_len(min(ncol(mat), max_features))
  samp_idx <- seq_len(min(nrow(mat), max_samples))

  mat_sub <- mat[samp_idx, feat_idx, drop = FALSE]

  df_heat <- reshape2::melt(is.na(mat_sub))
  colnames(df_heat) <- c("sample", "feature", "missing")

  p_heat <- ggplot2::ggplot(
    df_heat,
    ggplot2::aes(x = feature, y = sample, fill = missing)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(
      values = c("FALSE" = "grey80", "TRUE" = "firebrick"),
      labels = c("Present", "Missing")
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "Features",
      y = "Samples",
      fill = "",
      title = "Missingness structure"
    )

  list(
    feature_missingness = p_feat,
    missingness_heatmap = p_heat
  )
}
