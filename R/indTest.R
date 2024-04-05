#' Independent Test for Covariance Matrix
#'
#' Performs an independent test for the covariance matrix to assess if it is significantly different from a given covariance matrix or the sample covariance matrix.
#'
#' @param X A numeric matrix or data frame containing the variables.
#' @param covMat Optional. A numeric matrix representing the covariance matrix to be tested against. If NULL, the sample covariance matrix is used (default is NULL).
#' @param alpha The significance level for the test (default is 0.05).
#' @return A data frame containing the test statistic, degrees of freedom, alpha value, p-value, and test result.
#' #' @references
#' Marques, F. J., Diogo, J., Norouzirad, M., & Bispo, R. (2023). Testing the independence of variables for specific covariance structures: A simulation study. Mathematical Methods in the Applied Sciences, 46(9), 10421–10434. DOI: 10.1002/mma.9130
#' @examples
#' # Example usage:
#'
#' library(MASS)
#'
#' n = 10 # Sample Size
#' p = 20  # number of variables
#' rho = 0.4
#' # Covariance structure with Autoregressive structure
#' cov_mat <- covMatAR(p = p, rho = rho)
#' data <- mvrnorm(n = n, mu = rep(0,p), Sigma = cov_mat)
#' indTest(data)
#' indTest(data, covMat = cov_mat)
#'
#' # Covariance structure with Compound Symmetry structure
#' cov_mat <- covMatCS(p = p, rho = rho)
#' data <- mvrnorm(n = n, mu = rep(0,p), Sigma = cov_mat)
#' indTest(data)
#' indTest(data, covMat = cov_mat)
#'
#' #' # Covariance structure with Circular structure
#' cov_mat <- covMatC(p = p, rho = rho)
#' data <- mvrnorm(n = n, mu = rep(0,p), Sigma = cov_mat)
#' indTest(data)
#' indTest(data, covMat = cov_mat)
#'
#'
#' @export
#' @importFrom stats cov var pchisq
indTest <- function(X, covMat = NULL, alpha = 0.05) {

  n <- nrow(X)
  p <- ncol(X)

  # Maximum likelihood estimator of covariance matrix
  A <- (n - 1) * stats::cov(X)

  if (is.null(covMat)) {
    covMat = stats::cov(X)
  }

  off_diag <- covMat[!row(covMat) == col(covMat)]
  if (any(off_diag > 0) && any(off_diag < 0)) {
    cat("Alert:")
    cat('\n')
    cat("The off-diagonal elements of the covariance matrix are not the same signs. The results may not be reliable.")
    cat("\n")
  }

  # Check if the covariance matrix is symmetric
  if (!isSymmetric(covMat)) {
    stop("Covariance matrix is not symmetric.")
  }

  sig <- diag(cov(X))
  W <- rowSums(X)

  if (is.null(covMat)) {
    # Sigma unknown
    test_statistic <- (n - 1) * stats::var(W) / sum(sig)
    df <- n - 1
  } else {
    # Sigma known
    test_statistic <- (n - 1) * stats::var(W) / sum(diag(covMat))
    df <- n - 1
  }


  # Calculate p-values for both tails
  p_values_upper <- stats::pchisq(test_statistic, df = df, lower.tail = FALSE)
  p_values_lower <- stats::pchisq(test_statistic, df = df, lower.tail = TRUE)

  # Calculate final p-value using the minimum of probabilities
  p_value <- 2 * min(p_values_upper, p_values_lower)

  # Create a data frame to store results
  result <- data.frame(Test_Statistic = test_statistic,
                        Degrees_of_Freedom = df,
                        Alpha = alpha,
                        P_Value = p_value,
                        Test_Result = ifelse(p_value <= alpha, "Reject H0", "Fail to Reject H0"))

  return(result)
}

