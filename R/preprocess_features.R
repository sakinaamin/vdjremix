#' Preprocess repertoire feature matrix
#'
#' Filters repertoire features by missingness and variability.
#'
#' @param feature_matrix Numeric matrix (samples × features)
#' @param missingness_threshold Maximum allowed fraction of missing values
#' @param min_unique_values Minimum number of unique values required
#'
#' @return Filtered feature matrix
#' @export
preprocess_features <- function(
    feature_matrix,
    missingness_threshold = 0.4,
    min_unique_values = 8
) {
  stopifnot(is.matrix(feature_matrix) || is.data.frame(feature_matrix))

  mat <- as.matrix(feature_matrix)

  keep_missing <- colMeans(is.na(mat)) <= missingness_threshold
  keep_unique <- apply(mat, 2, function(x) length(unique(na.omit(x))) >= min_unique_values)

  mat[, keep_missing & keep_unique, drop = FALSE]
}
