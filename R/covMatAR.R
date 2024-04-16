#' Generate a covariance matrix with Autoregressive (AR) structure.
#'
#' This function generates generates an Autoregressive (AR) covariance structure
#' matrix of size \eqn{p \times p} based on the specified autoregressive
#' coefficient (\eqn{\rho}) and variance (\eqn{\sigma^2}).
#'
#' @param p An integer specifying the number of dimensions of the covariance matrix.
#' @param sigma2 A numeric value specifying the variance parameter (default = 1).
#' @param rho A numeric value specifying the autoregressive coefficient. If not
#' provided, a random value between 0 and 1 will be generated.
#'
#' The Autoregressive structure is defined as follows:
#'
#' \deqn{\Sigma = \Sigma_{AR} = \sigma^2 \begin{bmatrix}
#' 1 & \rho & \rho^2 & \cdots & \rho^{\lvert p-1 \rvert} \\
#' \rho & 1 & \rho & \cdots & \rho^{\lvert p-2 \rvert} \\
#' \vdots & \vdots & \vdots & \ddots & \vdots \\
#' \rho^{\lvert p-1 \rvert}  & \rho^{\lvert p-2 \rvert}  & \rho^{\lvert p-3 \rvert} \cdots & 1
#' \end{bmatrix}}
#' where \eqn{\Sigma } is the covariance matrix, \eqn{\sigma^2} is the variance parameter,
#' and \eqn{\rho } is the correlation parameter.
#'
#'
#' @return A \eqn{p \times p} numeric matrix representing the Autoregressive (AR) covariance structure.
#'
#' @examples
#' # generate a covariance matrix for \eqn{p = 5}, \eqn{\sigma^2 = 1}, and \eqn{\rho = 0.9}.
#' covMatAR(p = 5, rho = 0.9)
#'
#' # generate a covariance matrix for \eqn{p = 5},  \eqn{\sigma^2 = 5}, and \eqn{\rho = 0.9}.
#' covMatAR(p = 5, sigma2 = 5, rho = 0.9)
#'
#' # generate  covariance matrix for \eqn{p = 5},  and no value is considered for \eqn{\rho}
#' covMatAR(p = 5)
#'
#' @importFrom stats runif
#' @importFrom matrixcalc is.positive.definite
#' @export

covMatAR <- function(p, sigma2 = 1, rho) {
  if (missing(rho)) {
    rho <- stats::runif(1) # If rho is not provided, generate a random value between 0 and 1
  }

  # Initialize an empty covariance matrix
  covariance_matrix <- matrix(0, nrow = p, ncol = p)

  # Fill in the covariance matrix based on the AR structure
  for (i in 1:p) {
    for (j in 1:p) {
      covariance_matrix[i, j] <- rho^(abs(i - j))
    }
  }

  matrix <- sigma2 * covariance_matrix

  if (matrixcalc::is.positive.definite(matrix)) {
    message <- "The covariance matrix is positive definite."
  } else {
    message <- "The covariance matrix is not positive definite."
  }

  cat(message, "\n")
  cat("\n")
  return(matrix)
}
