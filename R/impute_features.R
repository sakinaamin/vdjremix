#' Impute missing values in a repertoire feature matrix
#'
#' Applies a specified imputation method to a numeric feature matrix
#' to produce a complete matrix for downstream analysis.
#'
#' @param feature_matrix Numeric matrix or data.frame (samples × features).
#' @param method Character. Imputation method to apply.
#'   Supported methods:
#'   "mean", "median", "random", "knn",
#'   "missMDA_reg", "missMDA_EM", "missForest".
#' @param random_seed Integer for reproducibility.
#'
#' @return A numeric matrix with missing values imputed.
#'
#' @export
impute_features <- function(
    feature_matrix,
    method = "missForest",
    random_seed = 1
) {
  stopifnot(is.matrix(feature_matrix) || is.data.frame(feature_matrix))

  set.seed(random_seed)

  mat <- as.matrix(feature_matrix)
  mat <- apply(mat, 2, as.numeric)
  rownames(mat) <- rownames(feature_matrix)
  colnames(mat) <- colnames(feature_matrix)

  if (!any(is.na(mat))) {
    return(mat)
  }

  imputed <- NULL

  if (method == "mean") {
    imputed <- apply(mat, 2, function(x) Hmisc::impute(x, fun = mean))
  }

  if (method == "median") {
    imputed <- apply(mat, 2, function(x) Hmisc::impute(x, fun = median))
  }

  if (method == "random") {
    imputed <- apply(mat, 2, function(x) Hmisc::impute(x, fun = "random"))
  }

  if (method == "knn") {
    tmp <- VIM::kNN(mat, k = 5)
    imputed <- as.matrix(tmp[, colnames(mat)])
  }

  if (method == "missMDA_reg") {
    ncp <- missMDA::estim_ncpPCA(mat)$ncp
    imputed <- missMDA::imputePCA(mat, ncp = ncp)$completeObs
  }

  if (method == "missMDA_EM") {
    ncp <- missMDA::estim_ncpPCA(mat)$ncp
    imputed <- missMDA::imputePCA(mat, ncp = ncp, method = "EM")$completeObs
  }

  if (method == "missForest") {
    tmp <- missForest::missForest(mat)
    imputed <- tmp$ximp
  }

  if (is.null(imputed)) {
    stop("Unsupported imputation method: ", method)
  }

  as.matrix(imputed)
}
