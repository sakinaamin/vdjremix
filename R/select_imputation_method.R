#' Select optimal imputation method based on benchmarking results
#'
#' Selects the imputation method with the lowest reconstruction error
#' across features and missingness levels.
#'
#' @param benchmark_results Output from benchmark_imputation().
#' @param metric Character. Summary metric used for comparison:
#'   "mean" (default) or "median".
#'
#' @return A list with:
#' \item{best_method}{Character name of selected imputation method.}
#' \item{summary}{Data frame summarising error per method.}
#'
#' @details
#' The optimal method is defined as the method with the lowest
#' aggregate RMSE across all features and NA-seeding frequencies.
#'
#' @export
select_imputation_method <- function(
    benchmark_results,
    metric = c("mean", "median")
) {
  metric <- match.arg(metric)

  stopifnot(is.data.frame(benchmark_results))
  stopifnot(all(c("method", "rmse") %in% colnames(benchmark_results)))

  summary_df <- benchmark_results |>
    dplyr::group_by(method) |>
    dplyr::summarise(
      mean_rmse = mean(rmse, na.rm = TRUE),
      median_rmse = median(rmse, na.rm = TRUE),
      .groups = "drop"
    )

  if (metric == "mean") {
    best_method <- summary_df$method[
      which.min(summary_df$mean_rmse)
    ]
  } else {
    best_method <- summary_df$method[
      which.min(summary_df$median_rmse)
    ]
  }

  list(
    best_method = best_method,
    summary = summary_df
  )
}
