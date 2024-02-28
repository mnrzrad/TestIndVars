#' Generate Simulated Data
#'
#' Generate simulated multivariate normal data.
#'
#' @param n The number of observations.
#' @param p The number of variables.
#' @param mean A vector of means for the variables (default is rep(0, p)).
#' @param Sigma The covariance matrix for the variables (default is diag(p)).
#' @return A matrix of simulated data with dimensions n x p.
#' @examples
#' # Generate simulated data with default parameters
#' # Covariance structure with Compound Symmetry structure
#' n = 100
#' p = 3
#' rho = 0.3
#' cov_mat <- covMatCS(p = p, rho = rho)
#' sim_data <- simData(n = n, p = p, Sigma = cov_mat)
#' head(sim_data)
#' # Generate simulated data with custom mean and covariance matrix
#' custom_mean <- c(1, 2, 3)
#' sim_data_custom <- simData(n = 100, p = 3, mean = custom_mean, Sigma = cov_mat)
#' head(sim_data_custom)
#' @importFrom MASS mvrnorm
#' @export
simData <- function(n, p, mean = rep(0, p), Sigma = diag(p)){
  return(MASS::mvrnorm(n = n, mu = mean, Sigma = Sigma))
}
