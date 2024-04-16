#' Likelihood Ratio Test for Covariance Matrix
#'
#' Performs a likelihood ratio test for the covariance matrix to assess if the covariance matrix is significantly different from an identity matrix.
#'
#' @param X A numeric matrix or data frame containing the variables.
#' @param alpha The significance level for the test. (default is 0.05).
#' @return A data frame containing the test statistic, degrees of freedom, critical value, p-value, and test result.
#' @examples
#'
#' library(MASS)
#'
#' n = 50 # Sample Size
#' p = 5
#' rho = 0.1
#'
#' # Building a Covariance structure with Autoregressive structure
#' cov_mat <- covMatAR(p = p, rho = rho)
#' # Simulated data
#' data <- mvrnorm(n = n, mu = rep(0,p), Sigma = cov_mat)
#' # Performing the test
#' lrTest(data, alpha = 0.01)
#'
#' # Building a Covariance structure with Compound Symmetry structure
#' cov_mat <- covMatCS(p = p, rho = rho)
#' # Simulated data
#' data <- mvrnorm(n = n, mu = rep(0,p), Sigma = cov_mat)
#' # Performing the test
#' lrTest(data)
#'
#' # Building a Covariance structure with Circular structure
#' cov_mat <- covMatC(p = p, rho = rho)
#' # Simulated data
#' data <- mvrnorm(n = n, mu = rep(0,p), Sigma = cov_mat)
#' # Performing the test
#' lrTest(data)
#'
#' @importFrom stats cov qchisq pchisq na.omit
#' @importFrom MASS mvrnorm
#' @export
lrTest <- function(X, alpha = 0.05) {

  if (any(is.na(X))) {

    message("Alert: The data has missing values. The missing values are handled by casewise deletion (and if there are no complete cases, that gives an error)")

    X <- data.frame(na.omit(X))
  }

  n <- nrow(X)
  p <- ncol(X)

  # Maximum likelihood estimator of covariance matrix
  A <- (n - 1) * stats::cov(X)

  # Calculate likelihood ratio test statistic
  LR <- (det(A) / prod(diag(A)))^(n/2)

  # Degrees of freedom
  df <- p * (p + 1) / 2

  # If alpha is provided, calculate critical value for that alpha
  if (!is.null(alpha)) {
    critical_value <- qchisq(1 - alpha, df)
    test_result <- ifelse(LR > critical_value, "Reject H0", "Fail to Reject H0")
    p_value <- pchisq(LR, df, lower.tail = FALSE)
  } else {
    critical_value <- NA
    test_result <- NA
    p_value <- NA
  }

  # Create a data frame to store results
  results <- data.frame(Test_Statistic = LR,
                        Degrees_of_Freedom = df,
                        Critical_Value = critical_value,
                        P_Value = p_value,
                        Test_Result = test_result)

  return(results)
}



