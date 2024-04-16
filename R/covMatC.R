#' Generate a covariance matrix with Circular (C) structure.
#'
#' This function generates generates an Circular (C) covariance structure
#' matrix of size \eqn{p \times p} based on the specified sequence of
#' \eqn{\{b_1, b_2, \ldots, b_{\lfloor p/2 \rfloor}\}} where
#' \eqn{\lfloor \cdot \rfloor} represents the largest integer that is not
#' greater than the argument and \eqn{b_j = b_{p -j}}
#' that this sequence in this function is created by a controlling parameter \eqn{\rho} as well as variance
#' (\eqn{\sigma^2}).
#'
#' @param p An integer specifying the number of dimensions of the covariance matrix.
#' @param sigma2 A numeric value specifying the variance parameter (default = 1).
#' @param rho Parameter controlling the circular pattern. If not
#' provided, a random value between 0 and 1 will be generated.
#'
#' The Circular structure is defined as follows:
#'
#' \deqn{\Sigma = \Sigma_{C} = \begin{bmatrix}
#' \sigma^2  & b_1 & b_2 & \cdots & b_{p-1} \\
#' b_{p-1} & \sigma^2  & b_1 & \cdots & b_{p-2} \\
#' \vdots & \vdots & \vdots & \ddots & \vdots \\
#' b_1  & b_2  & b_3 \cdots & \sigma^2
#' \end{bmatrix}}
#' where \eqn{\Sigma } is the covariance matrix, \eqn{\sigma^2} is the variance parameter,
#' and \eqn{b_j} is the sequence that \eqn{b_j = b_{p-j}} for
#'    \eqn{j = 1, 2, \ldots, \lfloor p/2 \rfloor} where
#' \eqn{\lfloor \cdot \rfloor} represents the largest integer that is not
#' greater than the argument.
#'
#' @return A \eqn{p \times p} numeric matrix representing the Circular (C) covariance structure.
#'
#' @examples
#' # generate a covariance matrix for \eqn{p = 5}, \eqn{\sigma^2 = 1}, and \eqn{\rho = 0.9}.
#' covMatC(p = 5, rho = 0.9)
#'
#' # generate a covariance matrix for \eqn{p = 5},  \eqn{\sigma^2 = 5}, and \eqn{\rho = 0.9}.
#' covMatC(p = 5, sigma2 = 5, rho = 0.9)
#'
#' # generate  covariance matrix for \eqn{p = 5},  and no value is considered for \eqn{\rho}
#' covMatC(p = 5)
#'
#' @importFrom stats runif
#' @importFrom matrixcalc is.positive.definite
#' @export

covMatC <- function(p, sigma2 = 1, rho = NULL) {
  if (is.null(rho)) {
    rho <- stats::runif(1) # If rho is not provided, generate a random value between 0 and 1
  }
  # Create an empty matrix
  matrix <- matrix(0, nrow = p, ncol = p)

  # Populate the covariance matrix
  for (i in 1:p) {
    for (j in 1:p) {
      if (i == j) {
        matrix[i, j] <- sigma2
      } else {
        distance <- min(abs(i - j), p - abs(i - j))
        matrix[i, j] <- sigma2 * exp(-rho * distance)
      }
    }
  }


  if (matrixcalc::is.positive.definite(matrix)) {
    message <- "The covariance matrix is positive definite."
  } else {
    message <- "The covariance matrix is not positive definite."
  }



  cat(message, "\n")
  cat("\n")
  return(matrix)
}

mat = covMatC(5)
mat
