#' @export
Ceidr=function(m1, m2, cov_df, distmat,
               cortex=NULL,
               cov.nuisance = NULL,
               cov.interest = NULL,
               between.mod =list(mu.formula1=~1, sigma.formula1=~1, family1=NO(),
                                 mu.formula2=~1, sigma.formula2=~1, family2=NO()), #for mean and varaince modelling,
               within.radius=5, #if within mean and variance adjustment
               parallel=F,
               ncores=1,
               max.radius = 15, #for cluster enhancement
               perm="Manly", # "Draper–Stoneman" different way to do permutation.
               nperm = 2000, #for permutation
               alpha = 0.05,
               seed=1) {
  V = ncol(m1)
  if (!is.null(cortex)){
    m1 = m1[, cortex]
    m2 = m2[, cortex]
    distmat = distmat[cortex, cortex]
  } else{
    cortex= 1:V
  }
  print("stageI-step1")
  # stageI-step1, between-subject mean and variance adjustment
  res1 <- MeanVarBetween(m1, cov_df, between.mod$mu.formula1, between.mod$sigma.formula1, between.mod$family1)
  res2 <- MeanVarBetween(m2, cov_df, between.mod$mu.formula2, between.mod$sigma.formula2, between.mod$family2)

  # stageI-stepw, within-subject mean and variance adjustment
  WNNmatrix <- buildWNNmatrix(distmat, within.radius = within.radius)
  res1 <- MeanVarWithin(res1, WNNmatrix)
  res2 <- MeanVarWithin(res2, WNNmatrix)

  rho <- res1 * res2

  mod0 <- if (is.null(cov.nuisance)) NULL else model.matrix(~ cov_df[, cov.nuisance])

  cider <- Clean(
    t(rho), distmat, cortex = NULL,
    mod0 = mod0,
    cov.interest = cov_df[, cov.interest],
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
