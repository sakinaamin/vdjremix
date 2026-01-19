.choose_linkage_method <- function(dist_mat,
                                   methods = c("average", "single", "complete", "ward", "weighted")) {

  ac_scores <- sapply(methods, function(m) {
    cluster::agnes(dist_mat, method = m)$ac
  })

  best <- names(ac_scores)[which.max(ac_scores)]
  if (best == "ward") best <- "ward.D2"

  list(
    best_method = best,
    scores = data.frame(
      method = methods,
      agglomerative_coefficient = ac_scores
    )
  )
}
