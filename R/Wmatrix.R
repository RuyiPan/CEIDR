#build the nearest neighbors matrix for within-subject adjustment
buildWNNmatrix <- function(distMat, within.radius = 20) {
  p <- nrow(distMat)
  weights <- matrix(1,p,p)
  weights[distMat > within.radius] <- 0

  # Get nonzero entries for sparse matrix construction
  idx <- which(weights > 0, arr.ind = TRUE)

  vals <- weights[idx]
  i <- idx[, 1]
  j <- idx[, 2]

  LNNmatrix <- sparseMatrix(i = i, j = j, x = 1, dims = c(p, p))

  return(LNNmatrix)
}




# set.seed(1)
# p <- 5000
# coords <- matrix(runif(p * 2, 0, 10), ncol = 2)
# distMat <- as.matrix(dist(coords))
#
# microbenchmark(
#   original = buildNNmatrixWmatrixDist(distMat, max.radius = 20, sigma=1.5),
#   fast=buildNNmatrixWmatrixDist_cpp(distMat, max_radius = 20, sigma=1.5),
#   times = 3
# )
#
#
# microbenchmark(
#   # original = buildNNmatrixDist(distMat, max.radius = 20),
#   fast=buildNNmatrixDist_fast(distMat, max.radius = 20),
#   times = 3
# )
#
# original = buildNNmatrixDist(distMat, max.radius = 20)
# fast =buildNNmatrixDist_fast(distMat, max.radius = 20)
# original == stacked
# fast
# fast
#
# str(result$NNmatrix)
# str(result$Wmatrix)
#
# # Visualize non-zero connections
# image(result$Wmatrix, main = "Wmatrix (weighted neighbors)")
# image(result$NNmatrix, main = "NNmatrix (binary neighbors)")
#
# dnorm(distMat)


