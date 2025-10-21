# Libraries
library(Rcpp)
library(RcppArmadillo)
library(dplyr)
library(ggplot2)
library(coda)
library(posterior)
library(bayesplot)

load("step1_outputs/R_outputs/step1_preparation_data.Rdata")
sourceCpp('./src/tailored_bayes.cpp')

# train the standard Bayesian logistic regression on the entire training set
w_u = rep(1, length(yTrain))  # set weights to be 1
set.seed(12)
fit_sb_train = tb_horseshoe(yTrain, as.matrix(xTrain), w_u, nIter, nThin, 1)
set.seed(14)
fit_sb_train2 = tb_horseshoe(yTrain, as.matrix(xTrain), w_u, nIter, nThin, 1)


sb_prob_test <- expit(as.matrix(cbind(1, xTest)) %*% t(fit_sb_train$beta[-c(1:nBurn), ]))
sb_prob_test2 <- expit(as.matrix(cbind(1, xTest)) %*% t(fit_sb_train2$beta[-c(1:nBurn), ]))


threshold <- 0.1
sb_NB_test <- vector()
sb_NB_test2 <- vector()
for (i in 1:dim(sb_prob_test)[2]) {
  sb_NB_test[i] <- net_benefit_treated(sb_prob_test[, i], yTest, threshold)$NB
  sb_NB_test2[i] <- net_benefit_treated(sb_prob_test2[, i], yTest, threshold)$NB
}
sb_NB_test_df <- data.frame(sb_NB_test)
sb_NB_test2_df <- data.frame(sb_NB_test2)
colnames(sb_NB_test_df) <- paste0("SB_threshold_", threshold)
colnames(sb_NB_test2_df) <- paste0("SB_threshold_", threshold)

j <- 2
for (threshold in seq(0.15, 0.9, 0.05)) {
  sb_NB_test <- vector()
  sb_NB_test2 <- vector()
  for (i in 1:dim(sb_prob_test)[2]) {
    sb_NB_test[i] <- net_benefit_treated(sb_prob_test[, i], yTest, threshold)$NB
    sb_NB_test2[i] <- net_benefit_treated(sb_prob_test2[, i], yTest, threshold)$NB
  }
  sb_NB_test_df <- cbind(sb_NB_test_df, sb_NB_test)
  sb_NB_test2_df <- cbind(sb_NB_test2_df, sb_NB_test2)
  colnames(sb_NB_test_df)[j] <- paste0("SB_threshold_", threshold)
  colnames(sb_NB_test2_df)[j] <- paste0("SB_threshold_", threshold)
  j <- j + 1
}


SB_chains <- mcmc.list(mcmc(sb_NB_test_df), mcmc(sb_NB_test2_df))

# trace plots
mcmc_trace(SB_chains) +
  labs(title = "MCMC trace of NB per threshold for the SB model",
       x = "iterations",
       y = "NB")


# autocorrelation plots
plot(SB_chains)
autocorr.plot(mcmc(sb_NB_test_df))
autocorr.plot(mcmc(sb_NB_test2_df))


SB_draws <- as_draws(SB_chains)
SB_summ <- summary(SB_draws, 
                   "mean", "sd", ~quantile(.x, probs = c(0.025, 0.5, 0.975)),
                   "mcse_mean", "mcse_sd", ~mcse_quantile(.x, probs = c(0.025, 0.5, 0.975)),
                   "rhat", "ess_bulk", "ess_tail")
SB_summ

save(fit_sb_train, fit_sb_train2, sb_prob_test, sb_prob_test2, sb_NB_test_df, sb_NB_test2_df, SB_chains, SB_draws, SB_summ, file = "SB_results.RData")
