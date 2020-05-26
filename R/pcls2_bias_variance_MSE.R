#' pcls2_bias_variance_MSE
#' @description Calculate the bias, the variance and the MSE of each component of the estimator of beta in the positively constrained least squares problem with 2 explanatory variables.
#' @param X a dataframe containing the explanatory variables (2 columns)
#' @param beta parameter vector
#' @param sigma standard deviation of the Gaussian noise
#' @return a dataframe containing the bias, the variance and the MSE of each component of the estimator of beta
#' @examples
#' results <- pcls2_bias_variance_MSE(X = data.frame(runif(3), runif(3)), beta = c(1,0.5), sigma = 2)
pcls2_bias_variance_MSE <- function(X, beta = c(0,1), sigma = 1)
{
  ### STOP ###
  if(ncol(X) != length(beta)){stop('erreur de dimension')}

  X <- as.matrix(X)

  # Calculs intermédiaires
  norme_X1 <- sqrt(sum(X[,1]^2))
  norme_X2 <- sqrt(sum(X[,2]^2))
  det_XTX <- norme_X1^2 * norme_X2^2 - as.double((t(X[,2]) %*% X[,1]))^2

  sigma_beta_nnls_1 <- (sigma/sqrt(det_XTX)) * norme_X2
  sigma_beta_nnls_2 <- (sigma/sqrt(det_XTX)) * norme_X1

  rho <- -(t(X[,2]) %*% X[,1])/(norme_X1 * norme_X2)

  #denom <- sqrt(det_XTX)/(norme_X1 * norme_X2)

  c1 <- beta[1]/sigma_beta_nnls_1
  c2 <- beta[2]/sigma_beta_nnls_2

  constante1 <- (c1 - rho * c2)/sqrt(1 - rho^2)
  constante2 <- (c2 - rho * c1)/sqrt(1 - rho^2)



  ### Biais ###



  # nnls_1
  true_bias_nnls_1 <- sigma_beta_nnls_1 * (
    dnorm(c1) * pnorm(constante2) + rho * dnorm(c2) * pnorm(constante1) +
      c1 * (pbivnorm::pbivnorm(-c1, -c2, rho = rho, recycle = FALSE) -
              pnorm(-c1) - pnorm(-c2)) + pnorm(-c2) * sqrt(1 - rho^2) * (
                dnorm(constante1) + constante1 * pnorm(constante1)
        )
    )

  # nnls_2
  true_bias_nnls_2 <- sigma_beta_nnls_2 * (
    dnorm(c2) * pnorm(constante1) + rho * dnorm(c1) * pnorm(constante2) +
      c2 * (pbivnorm::pbivnorm(-c2, -c1, rho = rho, recycle = FALSE) -
              pnorm(-c2) - pnorm(-c1)) + pnorm(-c1) * sqrt(1 - rho^2) * (
                dnorm(constante2) + constante2 * pnorm(constante2)
        )
  )



  ### MSE ###



  # nnls_1
  true_MSE_nnls_1 <- sigma_beta_nnls_1^2 * (
    c1^2 + pbivnorm::pbivnorm(c1, c2, rho = rho, recycle = FALSE) * (1 - c1^2) -
      c1 * dnorm(c1) * pnorm(constante2) -
      rho^2 * c2 * dnorm(c2) * pnorm(constante1) +
      rho * (sqrt(1 - rho^2)/sqrt(2 * pi)) *
      dnorm(sqrt(c1^2 - 2 * rho * c1 * c2 + c2^2)/sqrt(1 - rho^2)) +
      (1 - rho^2 - c1^2 + rho^2 * c2^2) * pnorm(-c2) * pnorm(constante1) -
      sqrt(1 - rho^2) * (c1 + rho * c2) * pnorm(-c2) * dnorm(constante1)
      )

  # nnls_2
  true_MSE_nnls_2 <- sigma_beta_nnls_2^2 * (
    c2^2 + pbivnorm::pbivnorm(c2, c1, rho = rho, recycle = FALSE) * (1 - c2^2) -
      c2 * dnorm(c2) * pnorm(constante1) -
      rho^2 * c1 * dnorm(c1) * pnorm(constante2) +
      rho * (sqrt(1 - rho^2)/sqrt(2 * pi)) *
      dnorm(sqrt(c1^2 - 2 * rho * c1 * c2 + c2^2)/sqrt(1 - rho^2)) +
      (1 - rho^2 - c2^2 + rho^2 * c1^2) * pnorm(-c1) * pnorm(constante2) -
      sqrt(1 - rho^2) * (c2 + rho * c1) * pnorm(-c1) * dnorm(constante2)
  )



  ### Variance ###



  # nnls_1
  true_variance_nnls_1 <- true_MSE_nnls_1 - true_bias_nnls_1^2
  # nnls_2
  true_variance_nnls_2 <- true_MSE_nnls_2 - true_bias_nnls_2^2



  ### Résultats ###



  results <-
    data.frame(
      biais = c(true_bias_nnls_1, true_bias_nnls_2),
      variance = c(true_variance_nnls_1, true_variance_nnls_2),
      MSE = c(true_MSE_nnls_1, true_MSE_nnls_2)
    )
  row.names(results) <- c("nnls_1", "nnls_2")


  return(results)


}




