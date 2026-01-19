.compute_distance <- function(cor_mat) {
  stopifnot(is.matrix(cor_mat))
  as.dist((1 - cor_mat) / 2)
}
