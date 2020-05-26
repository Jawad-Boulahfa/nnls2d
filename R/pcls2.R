#' pcls2
#' @description Solve the positively constrained least squares problem with 2 explanatory variables.
#' @param X a dataframe containing the explanatory variables (2 columns)
#' @param Y a dataframe containing the response vector
#' @return a list containing the optimal parameter vector and the vector of Lagrange multipliers
#' @examples
#' results <- pcls2(X = data.frame(runif(3), runif(3)), Y = data.frame(1:3))
pcls2 <- function(X, Y)
{
  ### STOP ###
  if(ncol(X) != 2 | ncol(Y) != 1){stop('erreur de dimension')}
  if(nrow(X) != nrow(Y)){stop('erreur de dimension')}

  X <- as.matrix(X)
  Y <- as.matrix(Y)

  # Cas mu = (0,0)
  # Calcul de mu
  mu <- rep(0,2)

  # Calculs intermédiaires
  norme_X1 <- sqrt(sum(X[,1]^2))
  norme_X2 <- sqrt(sum(X[,2]^2))
  det_XTX <- norme_X1^2 * norme_X2^2 - as.double((t(X[,2]) %*% X[,1]))^2
  A <- c(norme_X2^2 * as.double(t(X[,1]) %*% Y) - (t(X[,2]) %*% X[,1]) * (t(X[,2]) %*% Y), norme_X1^2 * as.double((t(X[,2]) %*% Y)) - (t(X[,2]) %*% X[,1]) * (t(X[,1]) %*% Y))
  B <- c(1/norme_X1^2 * (t(X[,1]) %*% Y), 1/norme_X2^2 * (t(X[,2]) %*% Y))


  # Cas A_1 >= 0 et A_2 >= 0
  if(A[1] >= 0 & A[2] >= 0)
  {
    # Calcul de beta
    beta <- 1/det_XTX * A
    return(list(beta = beta, mu = mu))
  }

  # Cas A_1 < 0 et A_2 < 0
  if(A[1] < 0 & A[2] < 0)
  {
    # Calcul de mu
    mu[1] <- -norme_X1^2 * B[1]
    mu[2] <- -norme_X2^2 * B[2]
    # Calcul de beta
    #beta[1] <- 0
    #beta[2] <- 0
    beta <- rep(0,2)
    return(list(beta = beta, mu = mu))
  }

  # Cas A_1 >= 0 et A_2 < 0
  if(A[1] >= 0 & A[2] < 0)
  {
    if(B[1] >= 0)
    {
      # Calcul de mu
      mu[1] <- 0
      mu[2] <- -1/norme_X1^2 * A[2]
      # Calcul de beta
      #beta[1] <- B[1]
      #beta[2] <- 0
      beta <- c(B[1], 0)
    }
    else
    {
      # Calcul de mu
      mu[1] <- -norme_X1^2 * B[1]
      mu[2] <- -norme_X2^2 * B[2]
      # Calcul de beta
      #beta[1] <- 0
      #beta[2] <- 0
      beta <- rep(0,2)
    }
    return(list(beta = beta, mu = mu))
  }

  # Cas A_1 < 0 et A_2 >= 0
  if(A[1] < 0 & A[2] >= 0)
  {
    if(B[2] >= 0)
    {
      # Calcul de mu
      mu[1] <- -1/norme_X2^2 * A[1]
      mu[2] <- 0
      # Calcul de beta
      #beta[1] <- 0
      #beta[2] <- B[2]
      beta <- c(0,B[2])

    }
    else
    {
      # Calcul de mu
      mu[1] <- -norme_X1^2 * B[1]
      mu[2] <- -norme_X2^2 * B[2]
      # Calcul de beta
      #beta[1] <- 0
      #beta[2] <- 0
      beta <- rep(0,2)
    }
    return(list(beta = beta, mu = mu))
  }

}

