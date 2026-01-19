#' Run the full VDJ-REMIX pipeline
#'
#' Executes preprocessing, imputation, robust correlation,
#' module identification, and eigengene computation for
#' immune repertoire feature matrices.
#'
#' @param feature_matrix Numeric matrix or data.frame (samples × features).
#' @param na_frequencies Numeric vector of NA fractions for imputation benchmarking.
#' @param run_imputation_benchmark Logical. Whether to benchmark imputation methods.
#' @param imputation_method Character or NULL. If NULL, method is selected automatically.
#' @param rmse_threshold Numeric. RMSE cutoff for feature filtering.
#' @param min_cluster_size Integer. Minimum module size.
#' @param n_repeats Integer. Subsampling repeats for correlation estimation.
#' @param n_cores Integer. Number of CPU cores to use (default: n-1).
#'
#' @return A list containing all intermediate and final outputs.
#'
#' @export
run_vdjremix <- function(
    feature_matrix,
    na_frequencies = seq(0.05, 0.5, by = 0.05),
    run_imputation_benchmark = TRUE,
    imputation_method = NULL,
    rmse_threshold = 0.5,
    min_cluster_size = 10,
    n_repeats = 10000,
    n_cores = parallel::detectCores() - 1
) {
  # 1. Preprocess features
  mat_filtered <- preprocess_features(feature_matrix)

  # 2. Benchmark imputation (optional)
  benchmark <- NULL
  if (run_imputation_benchmark) {
    benchmark <- benchmark_imputation(
      feature_matrix = mat_filtered,
      na_frequencies = na_frequencies
    )
  }

  # 3. Select imputation method
  if (is.null(imputation_method)) {
    if (is.null(benchmark)) {
      stop("Either run imputation benchmarking or provide imputation_method.")
    }
    imputation_method <- select_imputation_method(benchmark)$best_method
  }

  # 4. Filter features by imputation error
  filtered <- filter_features_by_imputation_error(
    feature_matrix = mat_filtered,
    benchmark_results = benchmark,
    method = imputation_method,
    rmse_threshold = rmse_threshold
  )

  mat_filtered2 <- filtered$filtered_matrix

  # 5. Impute features
  mat_imputed <- impute_features(
    feature_matrix = mat_filtered2,
    method = imputation_method
  )

  # 6. Determine subsampling depth
  depth <- get_subsample_depth(mat_imputed)

  # 7. Robust correlation
  cor_mat <- robust_correlation(
    feature_matrix = mat_imputed,
    subsample_depth = depth,
    n_repeats = n_repeats,
    n_cores = n_cores
  )

  # 8. Identify modules
  clustering <- identify_modules(
    correlation_matrix = cor_mat,
    min_cluster_size = min_cluster_size
  )

  # 9. Compute eigengenes
  eig <- compute_eigengenes(
    scaled_matrix = scale(mat_imputed),
    modules = clustering$modules
  )

  list(
    filtered_matrix = mat_filtered2,
    imputed_matrix = mat_imputed,
    imputation_method = imputation_method,
    imputation_benchmark = benchmark,
    subsample_depth = depth,
    correlation_matrix = cor_mat,
    modules = clustering$modules,
    cluster_assignment = clustering$assignment,
    eigengenes = eig$eigengenes,
    loadings = eig$loadings,
    variance_explained = eig$variance_explained,
    singletons = eig$singletons
  )
}
