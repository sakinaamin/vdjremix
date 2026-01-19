#' Compute module eigengenes from clustered features
#'
#' Computes eigengenes (PC1) for each feature module,
#' along with per-feature loadings and variance explained.
#'
#' @param scaled_matrix Numeric matrix (samples × features), scaled.
#' @param modules Named list of character vectors (features per module).
#' @param orientation Character. Reference for PC1 sign consistency:
#'   "mean" (default) or "pc1".
#' @param include_singletons Logical. Whether to retain singleton features.
#'
#' @return A list with:
#' \item{eigengenes}{Matrix of module eigengenes (samples × modules).}
#' \item{loadings}{Named list of PCA loadings per module.}
#' \item{variance_explained}{Data frame of PC1 variance per module.}
#' \item{singletons}{Character vector of singleton features.}
#'
#' @export
compute_eigengenes <- function(
    scaled_matrix,
    modules,
    orientation = c("mean", "pc1"),
    include_singletons = TRUE
) {
  orientation <- match.arg(orientation)

  stopifnot(is.matrix(scaled_matrix))
  stopifnot(is.list(modules))

  X <- scaled_matrix
  X[is.na(X)] <- 0

  eigengenes_list <- list()
  loadings_list <- list()
  var_explained <- list()
  singletons <- character()

  for (k in seq_along(modules)) {

    feats <- modules[[k]]
    mod_name <- names(modules)[k] %||% paste0("Module_", k)

    if (length(feats) < 2) {
      singletons <- c(singletons, feats)
      next
    }

    dat <- X[, feats, drop = FALSE]

    pca <- prcomp(dat, center = FALSE, scale. = FALSE)

    pc1 <- pca$x[, 1]
    load <- pca$rotation[, 1]

    # Orientation consistency
    if (orientation == "mean") {
      mean_vec <- rowMeans(dat)
      if (cor(mean_vec, pc1) < 0) {
        pc1 <- -pc1
        load <- -load
      }
    }

    eigengenes_list[[mod_name]] <- pc1
    loadings_list[[mod_name]] <- sort(load, decreasing = TRUE)

    ve <- (pca$sdev[1]^2) / sum(pca$sdev^2)

    var_explained[[mod_name]] <- data.frame(
      module = mod_name,
      proportion_variance = ve
    )
  }

  eigengenes <- do.call(cbind, eigengenes_list)
  variance_df <- do.call(rbind, var_explained)

  # Optionally include singleton features
  if (include_singletons && length(singletons) > 0) {
    singleton_mat <- X[, singletons, drop = FALSE]
    colnames(singleton_mat) <- paste0("Singleton_", colnames(singleton_mat))
    eigengenes <- cbind(eigengenes, singleton_mat)
  }

  list(
    eigengenes = eigengenes,
    loadings = loadings_list,
    variance_explained = variance_df,
    singletons = singletons
  )
}
