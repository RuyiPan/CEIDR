#' @param m1 A matrix of modality 1 values (subjects × vertices).
#' @param m2 A matrix of modality 2 values (same dimensions as `m1`).
#' @param cov_df A data frame of subject-level covariates (rows aligned with subjects in `m1`/`m2`).
#' @param distmat A vertex × vertex spatial distance matrix.
#' @param cortex Optional vector of vertex indices to include. If `NULL`, all vertices are used.
#' @param cov.nuisance Character vector of column names in `cov_df` to adjust for as nuisance covariates.
#' @param cov.interest Character vector of column names in `cov_df` that are the covariates of interest.
#' @param between.mod For between-subject adjustment: a list specifying modeling formulas and GAMLSS families:
#'   \describe{
#'     \item{mu.formula1, mu.formula2}{Formulas for the mean of modality 1 and 2}
#'     \item{sigma.formula1, sigma.formula2}{Formulas for the variance of modality 1 and 2}
#'     \item{family1, family2}{GAMLSS families for each modality (e.g., \code{NO()})}
#'   }
#' @param within.radius Radius (in distance units) used to define the local neighborhood for within-subject adjustment.
#' @param max.radius Maximum radius for spatial cluster enhancement in the permutation step.
#' @param perm Type of permutation. Currently supports \code{"Manly"} or \code{"Draper–Stoneman"}.
#' @param nperm Number of permutations to run for inference.
#' @param alpha Significance level used in inference (default is 0.05).
#' @param parallel Logical, whether to use parallel processing for spatial permutation inference.
#' @param ncores Number of CPU cores to use when \code{parallel = TRUE}.
#' @param seed Random seed for reproducibility.
#'
#' @export
Ceidr=function(m1, m2, cov.df, distmat,
               cortex=NULL,
               cov.nuisance = NULL,
               cov.interest = NULL,
               between.mod = list(mu.formula1=~1, sigma.formula1=~1, family1=NO(),
                                 mu.formula2=~1, sigma.formula2=~1, family2=NO()),
               within.radius=5,
               max.radius = 15,
               perm="Manly",
               nperm = 2000,
               alpha = 0.05,
               parallel=F,
               ncores=1,
               seed=1) {
  V = ncol(m1)
  if (!is.null(cortex)){
    m1 = m1[, cortex]
    m2 = m2[, cortex]
    distmat = distmat[cortex, cortex]
  } else{
    cortex= 1:V
  }

  # stageI-step1, between-subject mean and variance adjustment
  res1 <- MeanVarBetween(m1, cov.df, between.mod$mu.formula1, between.mod$sigma.formula1, between.mod$family1)
  res2 <- MeanVarBetween(m2, cov.df, between.mod$mu.formula2, between.mod$sigma.formula2, between.mod$family2)

  # stageI-step2, within-subject mean and variance adjustment
  WNNmatrix <- buildWNNmatrix(distmat, within.radius = within.radius)
  res1 <- MeanVarWithin(res1, WNNmatrix)
  res2 <- MeanVarWithin(res2, WNNmatrix)

  #compute intermodal couplings
  rho <- res1 * res2

  mod0 <- if (is.null(cov.nuisance)) NULL else model.matrix(~ cov.df[, cov.nuisance])

  #testing the individual differences in couplings
  cider <- Clean(
    t(rho), distmat, cortex = NULL,
    mod0 = mod0,
    cov.interest = cov.df[, cov.interest],
    sacf = NULL,
    max.radius = 15,
    # perm=perm,
    nperm = 2000,
    alpha = 0.05,
    alternative = "two.sided",
    seed = 1,
    nngp = FALSE,
    nngp.J = 50,
    partition = TRUE,
    npartition = 2,
    parallel = FALSE,
    ncores = 1
  )
  cider$couplings <- rho
  return(cider)
}
