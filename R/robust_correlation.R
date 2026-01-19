#' Compute robust subsampled correlation matrix (parallelised)
#'
#' Estimates a feature–feature correlation matrix using repeated subsampling
#' of shared non-missing samples, with optional parallelisation.
#'
#' @param feature_matrix Numeric matrix or data.frame (samples × features).
#' @param features Optional character vector of feature names.
#' @param subsample_depth Integer number of samples to subsample per repeat.
#' @param n_repeats Integer number of subsampling repeats.
#' @param method Correlation method (default "pearson").
#' @param n_cores Integer number of CPU cores to use (default: detectCores() - 1).
#' @param random_seed Integer for reproducibility.
#'
#' @return Symmetric correlation matrix (features × features).
#'
#' @export
robust_correlation <- function(
    feature_matrix,
    features = colnames(feature_matrix),
    subsample_depth,
    n_repeats = 10000,
    method = "pearson",
    n_cores = parallel::detectCores() - 1,
    random_seed = 1
) {
  stopifnot(is.matrix(feature_matrix) || is.data.frame(feature_matrix))
  stopifnot(subsample_depth > 1)

  set.seed(random_seed)

  mat <- as.matrix(feature_matrix)
  n_feat <- length(features)

  # Preallocate result
  corr_mat <- matrix(
    NA_real_,
    nrow = n_feat,
    ncol = n_feat,
    dimnames = list(features, features)
  )

  # Generate index pairs (upper triangle)
  pair_idx <- which(upper.tri(corr_mat, diag = TRUE), arr.ind = TRUE)

  # Setup parallel backend
  n_cores <- max(1, n_cores)
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)

  on.exit({
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
  })

  `%dopar%` <- foreach::`%dopar%`

  results <- foreach::foreach(
    k = seq_len(nrow(pair_idx)),
    .combine = rbind,
    .packages = c("stats")
  ) %dopar% {

    i <- pair_idx[k, 1]
    j <- pair_idx[k, 2]

    x <- mat[, features[i]]
    y <- mat[, features[j]]
    idx <- which(!is.na(x) & !is.na(y))

    if (length(idx) < subsample_depth) {
      return(c(i, j, NA_real_))
    }

    cor_vals <- replicate(
      n_repeats,
      {
        sel <- sample(idx, subsample_depth, replace = FALSE)
        suppressWarnings(cor(x[sel], y[sel], method = method))
      }
    )

    c(i, j, mean(cor_vals, na.rm = TRUE))
  }

  # Fill matrix
  for (r in seq_len(nrow(results))) {
    i <- results[r, 1]
    j <- results[r, 2]
    corr_mat[i, j] <- results[r, 3]
    corr_mat[j, i] <- results[r, 3]
  }

  corr_mat
}
