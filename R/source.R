library(MASS)
# library(stats)
#
# n <- 199
# NN <- n + 1
# p <- 15
# quantil <- 0.510186028869234
# vec <- c(-0.07, -0.06, -0.05, -0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.1, 0.11, 0.12, 0.13, 0.15, 0.17, 0.2)
# Config <- rep(0, length(vec))
# Config1 <- rep(0, length(vec))
# Config2 <- rep(0, length(vec))
# LRsimfig <- rep(0, length(vec))
# sco <- rep(0, length(vec))
# count <- 0
# ns <- 500
#
# for (gamma in vec) {
#   count <- count + 1
#   Cov <- gamma * matrix(1, nrow = p, ncol = p) + (1 - gamma) * diag(p)
#   mu <- rep(0, p)
#   Con <- rep(0, ns)
#   Con1 <- rep(0, ns)
#   Con2 <- rep(0, ns)
#   Con3 <- rep(0, ns)
#   LRsim <- rep(0, ns)
#
#   for (j in 1:ns) {
#     Data <- MASS::mvrnorm(NN, mu, Cov)
#     vecS <- (NN - 1) * cov(Data)
#     LR <- det(vecS) / prod(diag(vecS))
#     sig <- diag(cov(Data))
#     datan1 <- colSums(Data)
#
#     if (n * var(datan1) / sum(sig) > qchisq(0.975, n) || n * var(datan1) / sum(sig) < qchisq(0.025, n)) {
#       Con1[j] <- 1
#     }
#     if (n * var(datan1) / sum(diag(Cov)) > qchisq(0.975, n) || n * var(datan1) / sum(diag(Cov)) < qchisq(0.025, n)) {
#       Con2[j] <- 1
#     }
#     if (LR < quantil) {
#       LRsim[j] <- 1
#     }
#     vecSS <- cor(Data)
#     stat <- (sum((vecSS ^ 2)[lower.tri(vecSS)]) - (p * (p - 1) / (2 * n))) / ((p * (p - 1) * (n - 1) / ((n ^ 2) * (n + 2))) ^ (1 / 2))
#     if (stat > qnorm(0.975, 0, 1) || stat < qnorm(0.025, 0, 1)) {
#       Con3[j] <- 1
#     }
#   }
#   sco[count] <- sum(Con3) / ns
#   Config1[count] <- sum(Con1) / ns
#   Config2[count] <- sum(Con2) / ns
#   LRsimfig[count] <- sum(LRsim) / ns
# }
#
# plot(vec, LRsimfig, type = "l", col = "blue", xlab = "Values of \u03B3", ylab = "Estimated power", ylim = c(0, 1), xlim = c(min(vec), max(vec)))
# lines(vec, Config2, type = "l", col = "red")
# lines(vec, Config1, type = "l", col = "green")
# lines(vec, sco, type = "l", col = "orange")
# legend("topright", legend = c("LRT", "\u03C3's known", "\u03C3's unknown", "SCH"), col = c("blue", "red", "green", "orange"), lty = 1)
