library(coda)
library(posterior)
library(bayesplot)

i <- 1
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".RData")))
tb_NB_test_df <- data.frame(NB_test)
colnames(tb_NB_test_df) <- paste0("TB_threshold_", threshold)
tb_NB_test2_df <- data.frame(NB_test2)
colnames(tb_NB_test2_df) <- paste0("TB_threshold_", threshold)


for (i in 2:17) {
  load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".RData")))
  tb_NB_test_df <- cbind(tb_NB_test_df, NB_test)
  tb_NB_test2_df <- cbind(tb_NB_test2_df, NB_test2)
  colnames(tb_NB_test_df)[i] <- paste0("TB_threshold_", threshold)
  colnames(tb_NB_test2_df)[i] <- paste0("TB_threshold_", threshold)
}


TB_chains <- mcmc.list(mcmc(tb_NB_test_df), mcmc(tb_NB_test2_df))


mcmc_trace(TB_chains) +
  labs(title = "MCMC trace of NB per threshold for the TB model",
       x = "iterations",
       y = "NB")


plot(TB_chains)
autocorr.plot(mcmc(tb_NB_test_df))
autocorr.plot(mcmc(tb_NB_test2_df))


TB_draws <- as_draws(TB_chains)
TB_summ <- summary(TB_draws, 
                   "mean", "sd", ~quantile(.x, probs = c(0.025, 0.5, 0.975)),
                   "mcse_mean", "mcse_sd", ~mcse_quantile(.x, probs = c(0.025, 0.5, 0.975)),
                   "rhat", "ess_bulk", "ess_tail")
TB_summ

save(tb_NB_test_df, tb_NB_test2_df, TB_chains, TB_draws, TB_summ, file = "step6_outputs/TB_convergence_check.RData")
