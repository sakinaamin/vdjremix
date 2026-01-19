#' Plot imputation benchmarking results
#'
#' Visualises reconstruction error (RMSE) across imputation methods
#' and missingness levels.
#'
#' @param benchmark_results Data frame returned by benchmark_imputation().
#' @param rmse_threshold Numeric. Optional RMSE threshold to highlight.
#'
#' @return A named list of ggplot objects.
#'
#' @export
plot_imputation_benchmark <- function(
    benchmark_results,
    rmse_threshold = 0.5
) {
  stopifnot(is.data.frame(benchmark_results))
  stopifnot(all(c("method", "rmse", "na_fraction") %in% colnames(benchmark_results)))

  df <- benchmark_results

  # ---- RMSE per method ----
  p_method <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = method, y = rmse, fill = method)
  ) +
    ggplot2::geom_violin(alpha = 0.5, trim = TRUE) +
    ggplot2::geom_boxplot(width = 0.2, outlier.shape = NA) +
    ggplot2::geom_hline(
      yintercept = rmse_threshold,
      colour = "red",
      linetype = "dashed"
    ) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Imputation method",
      y = "RMSE",
      title = "Imputation error by method"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )

  # ---- RMSE by method and NA frequency ----
  p_na <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = method, y = rmse, fill = method)
  ) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::facet_wrap(~ na_fraction) +
    ggplot2::geom_hline(
      yintercept = rmse_threshold,
      colour = "red",
      linetype = "dashed"
    ) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Imputation method",
      y = "RMSE",
      title = "Imputation error across missingness levels"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )

  # ---- RMSE per feature (summary) ----
  df_feat <- df |>
    dplyr::group_by(method, feature) |>
    dplyr::summarise(mean_rmse = mean(rmse, na.rm = TRUE), .groups = "drop")

  p_feat <- ggplot2::ggplot(
    df_feat,
    ggplot2::aes(x = feature, y = mean_rmse, fill = method)
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::geom_hline(
      yintercept = rmse_threshold,
      colour = "red",
      linetype = "dashed"
    ) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Feature",
      y = "Mean RMSE",
      title = "Mean imputation error per feature"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

  list(
    rmse_by_method = p_method,
    rmse_by_missingness = p_na,
    rmse_by_feature = p_feat
  )
}
