#' Plot variance explained by module eigengenes
#'
#' Visualises the proportion of variance explained by the first
#' principal component (PC1) for each module.
#'
#' @param variance_explained Data frame returned by compute_eigengenes(),
#'   containing columns 'module' and 'proportion_variance'.
#' @param reorder Logical. Whether to reorder modules by variance explained.
#'
#' @return A ggplot object.
#'
#' @export
plot_variance_explained <- function(
    variance_explained,
    reorder = TRUE
) {
  stopifnot(is.data.frame(variance_explained))
  stopifnot(all(c("module", "proportion_variance") %in% colnames(variance_explained)))

  df <- variance_explained

  if (reorder) {
    df$module <- factor(
      df$module,
      levels = df$module[order(df$proportion_variance, decreasing = TRUE)]
    )
  }

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = module, y = proportion_variance)
  ) +
    ggplot2::geom_col(fill = "steelblue", colour = "black") +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Module",
      y = "Proportion of variance explained (PC1)",
      title = "Variance explained by module eigengenes"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
}
