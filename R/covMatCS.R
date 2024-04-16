#' Generate a covariance matrix with equivariance-equicorrelation or compound symmetry structure.
#'
#' This function generates a covariance matrix with equivariance-equicorrelation
#´ or compound symmetry (CS) structure of size \eqn{p \times p} based on the
#´ specified coefficient (\eqn{\rho}) and variance (\eqn{\sigma^2}).
#'
#' @param p An integer specifying the number of dimensions of the covariance matrix.
#' @param sigma2 A numeric value specifying the variance parameter (default = 1).
#' @param rho A numeric value specifying the correlation parameter. If not provided,
#'              a random value between 0 and 1 will be generated.
#'
#'
#' The compound symmetry structure is defined as follows:
#'
#' \deqn{\Sigma = \Sigma_{CS} = \sigma^2 \begin{bmatrix}
#' 1 & \rho & \cdots & \rho \\
#' \rho & 1 & \cdots & \rho \\
#' \vdots & \vdots & \ddots & \vdots \\
#' \rho & \rho & \cdots & \rho
#' \end{bmatrix}}
#' where \eqn{\Sigma } is the covariance matrix, \eqn{\sigma^2} is the variance parameter,
#' and \eqn{\rho } is the correlation parameter.
#'
#'
#' @return A \eqn{p \times p} numeric matrix representing the covariance matrix with
#' equivariance-equicorrelation or compound symmetry structure.
#' @examples
#' # generate a covariance matrix for \eqn{p = 5}, \eqn{\sigma^2 = 1}, and \eqn{\rho = 0.9}.
#' covMatCS(p = 5, rho = 0.9)
#'
#' # generate a covariance matrix for \eqn{p = 5},  \eqn{\sigma^2 = 5}, and \eqn{\rho = 0.9}.
#' covMatCS(p = 5, sigma2 = 5, rho = 0.9)
#'
#' # generate  covariance matrix for \eqn{p = 5},  and no value is considered for \eqn{\rho}
#' covMatCS(p = 5)
#'
#' @importFrom stats runif
#' @importFrom matrixcalc is.positive.definite
#'
#' @export
covMatCS <- function(p, sigma2 = 1, rho = NULL) {
  if (is.null(rho)) {
    rho <- stats::runif(1) # If rho is not provided, generate a random value between 0 and 1
  }

  # Equivariance-equicorrelation covariance matrix
  matrix <- sigma2 * (rho * matrix(1, nrow = p, ncol = p) + (1 - rho) * diag(p))


  if (matrixcalc::is.positive.definite(matrix)) {
    message <- "The covariance matrix is positive definite."
  } else {
    message <- "The covariance matrix is not positive definite."
  }

  message(message)

  return(matrix)
}
