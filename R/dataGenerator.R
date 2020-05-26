#' dataGenerator
#' @description Generate data for multiple linear regression (using the standard uniform distribution)
#' @param n number of data points
#' @param beta parameter vector
#' @param sigma standard deviation of the Gaussian noise
#' @return a list containing the response vector Y (size n), a dataframe for explanatory variables X (size nxp) and the vector of Gaussian noise (size n)
#' @examples
#' dataset <- dataGenerator(10, 1:3, 2)
dataGenerator <- function(n, beta = c(0,1,2), sigma = 1)
{
  # Calcul de p
  p <- length(beta)

  # Noms des colonnes et des lignes de X
  noms_colonnes <- paste("X", 1:p, sep = "_.")
  noms_lignes_X <- paste("X", 1:n, sep = "_")
  noms_lignes_X <- paste0(noms_lignes_X, ".")


  # Calcul de X
  X <- matrix(runif(n*p), nrow = n, ncol = p)

  # On nomme les colonnes puis les lignes de X
  colnames(X) <- noms_colonnes
  #rownames(X) <- noms_lignes_X

  # On stocke le bruit gaussien dans une variable epsilon
  epsilon <- rnorm(n, sd = sigma)

  # Calcul de Y
  Y <- as.matrix(X) %*% beta + epsilon
  Y <- as.data.frame(Y)

  # Y n'a qu'une colonne, on la nomme. On nomme aussi ses lignes.
  noms_lignes_Y <- paste("Y", 1:n, sep = "_")
  colnames(Y) <- "Y"
  #rownames(Y) <- noms_lignes_Y

  # On retourne Y, X, et epsilon
  return(list(Y = Y, X = X, epsilon = epsilon))
}



