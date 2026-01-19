#' Plot feature loadings for a module eigengene
#'
#' Visualises PCA loadings for features within a given module,
#' highlighting features that contribute more than expected by chance.
#'
#' @param loadings Named list of PCA loadings per module,
#'   as returned by compute_eigengenes().
#' @param module Character. Name of the module to plot.
#'
#' @return A ggplot object.
#'
#' @export
plot_module_loadings <- function(
    loadings,
    module
) {
  stopifnot(is.list(loadings))
  stopifnot(module %in% names(loadings))

  w <- loadings[[module]]
  df <- data.frame(
    feature = names(w),
    loading = as.numeric(w)
  )

  # Sort by absolute loading
  df <- df[order(abs(df$loading), decreasing = TRUE), ]

  # Importance cutoff
  p <- nrow(df)
  cutoff <- sqrt(1 / p)

  df$important <- abs(df$loading) >= cutoff

  df$feature <- factor(df$feature, levels = df$feature)

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = feature, y = loading, colour = important)
  ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_hline(
      yintercept = c(-cutoff, cutoff),
      linetype = "dotted",
      colour = "red"
    ) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Feature",
      y = "PC1 loading",
      colour = "Important\ncontributor",
      title = paste("Feature contributions to", module)
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )
    )
}
