#' Cluster Enhancement for testing Individual Differences in intermodal coupling (r)
#'
#' Run CEIDR to test individual differences in spatially-structured intermodal coupling.
#'
#' @param m1 A matrix of modality 1 values (subjects × vertices).
#' @param m2 A matrix of modality 2 values (same dimensions as `m1`).
#' @param cov.df A data frame of subject-level covariates (rows aligned with subjects in `m1`/`m2`).
#' @param distmat A vertex × vertex spatial distance matrix.
#' @param cortex Optional vector of vertex indices to include. If `NULL`, all vertices are used.
#' @param cov.nuisance Character vector of column names in `cov.df` to adjust for as nuisance covariates.
#' @param cov.interest Character vector of column names in `cov.df` that are the covariates of interest.
#' @param between.mod For between-subject adjustment: a list specifying modeling formulas and GAMLSS families:
#'   If \code{between.mod = "default"}, the function will automatically construct a default linear model using all covariates
#'   provided via \code{cov.interest} and \code{cov.nuisance} as fixed effects. The same formula will be used for both
#'   the mean (\code{mu.formula1}, \code{mu.formula2}) and variance (\code{sigma.formula1}, \code{sigma.formula2})
#'   components. The GAMLSS families default to \code{NO()} for both modalities.
#'   If \code{between.mod = NULL}, the function will skipp between-subject adjustment
#'   \describe{
#'     \item{mu.formula1, mu.formula2}{Formulas for the mean of modality 1 and 2}
#'     \item{sigma.formula1, sigma.formula2}{Formulas for the variance of modality 1 and 2}
#'     \item{family1, family2}{GAMLSS families for each modality (e.g., \code{NO()})}
#'   }
#' @param within.radius Radius (in distance units) used to define the local neighborhood for within-subject adjustment.
#' @param max.radius Maximum radius for spatial cluster enhancement in the permutation step.
#' @param perm Type of permutation. Currently supports \code{"Draper–Stoneman"} (permute cov.interesst) or \code{"Manly"} (permute (cov.interesst, cov.nuisance)).
#' @param nperm Number of permutations to run for inference.
#' @param alpha Significance level used in inference (default is 0.05).
#' @param parallel Logical, whether to use parallel processing for spatial permutation inference.
#' @param ncores Number of CPU cores to use when \code{parallel = TRUE}.
#' @param seed Random seed for reproducibility.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{Tstat}{A length-\code{V} vector of cluster-enhanced test statistics obtained by CLEAN. Non-cortex vertices take the value 0.}
#'   \item{Tstat_thresholded}{Cluster-enhanced test statistics thresholded at a pre-set FWER level (\code{alpha}). Vertices not passing the threshold are shrunk to 0. Non-cortex vertices are 0.}
#'   \item{permMax}{A length-\code{nperm} vector of the maximum cluster-enhanced test statistic across all vertices for each permutation. Used to compute FWER-controlling threshold.}
#'   \item{permMin}{A length-\code{nperm} vector of the minimum cluster-enhanced test statistic across all vertices for each permutation.}
#'   \item{threshold}{FWER-controlling threshold.}
#'   \item{nperm}{Number of permutations used.}
#'   \item{seed}{Random seed used.}
#'   \item{nlocations}{Number of cortical vertices used in the analysis.}
#'   \item{alternative}{Type of alternative hypothesis: \code{"two.sided"}, \code{"greater"}, or \code{"less"}.}
#'   \item{couplings}{Estimated subject-level intermodal couplings.}
#' }
#'
#' @examples
#'
#' Ceidr(Data$m1, Data$m2, Data$cov.df, Data$distmat, cov.interest = c("x"),  cov.nuisance = c("z"))
#'
#' @references
#' TODO
#'
#' @export
Ceidr=function(m1, m2, cov.df, distmat,
               cortex=NULL,
               cov.nuisance = NULL,
               cov.interest = NULL,
               between.mod = list(mu.formula1=y~1, sigma.formula1=~1, family1=NO(),
                                 mu.formula2=y~1, sigma.formula2=~1, family2=NO()),
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
  if (is.null(cov.interest)) {
    stop("Error: cov.interest should be provided")
  }

  if (is.null(between.mod)) {
    res1.1 <- m1
    res2.1 <- m2
  } else if (identical(between.mod, "default")) {
    covariates <- c(cov.nuisance, cov.interest)
      formula_str <- paste("~", paste(covariates, collapse = " + "))
      mu.f <- as.formula(paste("y ~", paste(covariates, collapse = " + ")))
      sigma.f <- as.formula( paste("~", paste(covariates, collapse = " + ")))
      between.mod <- list(
        mu.formula1 = mu.f,
        sigma.formula1 = sigma.f,
        family1 = NO(),
        mu.formula2 = mu.f,
        sigma.formula2 = sigma.f,
        family2 = NO()
      )
    # stageI-step1, between-subject mean and variance adjustment
    res1.1 <- MeanVarBetween(m1, cov.df, between.mod$mu.formula1, between.mod$sigma.formula1, between.mod$family1)
    res2.1 <- MeanVarBetween(m2, cov.df, between.mod$mu.formula2, between.mod$sigma.formula2, between.mod$family2)
  } else {
    res1.1 <- MeanVarBetween(m1, cov.df, between.mod$mu.formula1, between.mod$sigma.formula1, between.mod$family1)
    res2.1 <- MeanVarBetween(m2, cov.df, between.mod$mu.formula2, between.mod$sigma.formula2, between.mod$family2)
  }



  # stageI-step2, within-subject mean and variance adjustment
  WNNmatrix <- buildWNNmatrix(distmat, within.radius = within.radius)
  res1.2 <- MeanVarWithin(res1.1, WNNmatrix)
  res2.2 <- MeanVarWithin(res2.1, WNNmatrix)

  #compute intermodal couplings
  rho <- res1.2 * res2.2

  mod0 <- if (is.null(cov.nuisance)) NULL else model.matrix(~ cov.df[, cov.nuisance])

  #testing the individual differences in couplings
  cider <- Clean(
    t(rho), distmat, cortex = NULL,
    mod0 = mod0,
    cov.interest = cov.df[, cov.interest],
    sacf = NULL,
    max.radius = 15,
    perm= perm,
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
