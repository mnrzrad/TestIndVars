#' Schott's Test for testing independency
#'
#' Performs Schott's test for the correlation matrix to assess if the correlation matrix is significantly different from an identity matrix.
#'
#' @param X A numeric matrix or data frame containing the variables.
#' @param alpha The significance level for the test (default is 0.05).
#' @return A data frame containing the test statistic, alpha value, p-value, and test result.
#'
#' @references
#' Schott, J. R.  (2005). Testing for complete independence in high dimensions, Biometrika, 92(4), 951–956.
#' @examples
#' library(MASS)
#'
#' n = 100 # Sample Size
#' p = 5
#' rho = 0.1
#' cov_mat <- covMatAR(p = p, rho = rho)
#' data <- mvrnorm(n = n, mu = rep(0,p), Sigma = cov_mat)
#' schottTest(data)
#'
#' # Covariance structure with Compound Symmetry structure
#' cov_mat <- covMatCS(p = p, rho = rho)
#' data <- mvrnorm(n = n, mu = rep(0,p), Sigma = cov_mat)
#' schottTest(data)
#'
#' #' # Covariance structure with Circular structure
#' cov_mat <- covMatC(p = p, rho = rho)
#' data <- mvrnorm(n = n, mu = rep(0,p), Sigma = cov_mat)
#' schottTest(data)
#'
#'
#' @export
#' @importFrom stats cor pnorm
#' @importFrom MASS mvrnorm
schottTest <- function(X, alpha = 0.05) {
  n <- nrow(X)
  p <- ncol(X)

  # Correlation matrix
  A <- stats::cor(X)


  test_statistic <- (sum((A ^ 2)[lower.tri(A)]) - (p * (p - 1) / (2 * n))) / ((p * (p - 1) * (n - 1) / ((n ^ 2) * (n + 2))) ^ (1 / 2))

  # Calculate p-values for both tails
  p_values_upper <- stats::pnorm(test_statistic, mean = 0, sd = 1, lower.tail = FALSE)
  p_values_lower <- stats::pnorm(test_statistic, mean = 0, sd = 1, lower.tail = TRUE)

  # Calculate final p-value using the minimum of probabilities
  p_value <- 2 * min(p_values_upper, p_values_lower)

  result <- data.frame(Test_Statistic = test_statistic,
                         Alpha = alpha,
                         P_Value = p_value,
                         Test_Result = ifelse(p_value <= alpha, "Reject H0", "Fail to Reject H0"))
    return(result)
  }
