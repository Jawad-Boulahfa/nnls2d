#' pcls1
#' @description Solve the positively constrained least squares problem with 1 explanatory variable
#' @param X a list containing the explanatory variable
#' @param Y a list containing the response vector
#' @return a list containing the optimal parameter and the Lagrange multiplier
#' @examples
#' results <- pcls1(X = data.frame(runif(5)), Y = data.frame(1:5))
pcls1 <- function(X, Y)
{

  ### STOP ###
  if(ncol(X) != 1 | ncol(Y) != 1){stop('erreur de dimension')}
  if(nrow(X) != nrow(Y)){stop('erreur de dimension')}

  X <- as.matrix(X)
  Y <- as.matrix(Y)

  # Cas mu = 0
  mu <- 0

  # On calcule la norme de X puis beta
  # X^2 renvoie la liste où chaque élément est élevé au carré
  # On aurait pû utiliser t(X)%*%X au lieu de sum(X^2)
  # mais cela posait problème pour le produit avec un vecteur dans pcls2
  # il faut faire un as.double() avant)
  norme_X <- sqrt(sum(X^2))
  beta <- as.double((1/norme_X^2) * t(X) %*% Y)

  # Cas beta < 0
  if(beta < 0)
  {
    beta = 0
    mu <- as.double(-t(X) %*% Y)
  }

  return(list(beta = beta, mu = mu))
}

