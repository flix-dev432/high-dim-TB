# posterior densities of top 20 most important variables
library("bayesplot") 
library("ggplot2")
load("step1_outputs/R_outputs/step1_preparation_data.RData")
load("./step8_outputs/variable_importance_results.RData")

# standard bayes
load("step5_outputs/R_outputs/SB_results.RData")
sb_post_full <- fit_sb_train$beta[-c(1:nBurn), ]
colnames(sb_post_full) <- c("intercept", colnames(xTrain))


mcmc_areas(sb_post_full,
           pars = c(PIP_SB_df$parameter[1:20]),
           prob = 0.95,
           point_est = c("median"),
           area_method = "scaled height") +
  ggtitle("SB - Posterior distributions of top 20 most important variables", "with medians and 95% credible intervals") +
  theme_bw() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        plot.subtitle = element_text(size = 8)) +
  scale_x_continuous(breaks = seq(-1, 2, 0.5), limits = c(-1, 2)) 


# tailored bayes 0.1
i <- 1
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
tb_post_full_t0.1 <- fit_develop$beta[-c(1:nBurn), ]
colnames(tb_post_full_t0.1) <- c("intercept", colnames(xDevelop))


mcmc_areas(tb_post_full_t0.1, 
           pars = c(PIP_TB_0.1_df$parameter[1:20]),
           prob = 0.95,
           point_est = c("median"),
           area_method = "scaled height") + 
  ggtitle("TB threshold 0.1 - Posterior distributions of top 20 most important variables", "with medians and 95% credible intervals") +
  theme_bw() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        plot.subtitle = element_text(size = 8)) +
  scale_x_continuous(breaks = seq(-1, 2, 0.5), limits = c(-1, 2)) 



# tailored bayes 0.15
i <- 2
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
tb_post_full_t0.15 <- fit_develop$beta[-c(1:nBurn), ]
colnames(tb_post_full_t0.15) <- c("intercept", colnames(xDevelop))


mcmc_areas(tb_post_full_t0.15,
           pars = c(PIP_TB_0.15_df$parameter[1:20]),
           prob = 0.95,
           point_est = c("median"),
           area_method = "scaled height") + 
  ggtitle("TB threshold 0.15 - Posterior distributions of top 20 most important variables", "with medians and 95% intervals")



# tailored bayes 0.25
i <- 4
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
tb_post_full_t0.25 <- fit_develop$beta[-c(1:nBurn), ]
colnames(tb_post_full_t0.25) <- c("intercept", colnames(xDevelop))


mcmc_areas(tb_post_full_t0.25,
           pars = c(PIP_TB_0.25_df$parameter[1:20]),
           prob = 0.95,
           point_est = c("median"),
           area_method = "scaled height") + 
  ggtitle("TB0.25 - Posterior distributions of top 20 most important variables", "with medians and 95% intervals")


# tailored bayes 0.35
i <- 6
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
tb_post_full_t0.35 <- fit_develop$beta[-c(1:nBurn), ]
colnames(tb_post_full_t0.35) <- c("intercept", colnames(xDevelop))


mcmc_areas(tb_post_full_t0.35,
           pars = c(PIP_TB_0.35_df$parameter[1:20]),
           prob = 0.95,
           point_est = c("median"),
           area_method = "scaled height") + 
  ggtitle("TB threshold 0.35 - Posterior distributions of top 20 most important variables", "with medians and 95% credible intervals") +
  theme_bw() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        plot.subtitle = element_text(size = 8)) +
  scale_x_continuous(breaks = seq(-1, 2, 0.5), limits = c(-1, 2)) 


# tailored bayes 0.4
i <- 7
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
tb_post_full_t0.4 <- fit_develop$beta[-c(1:nBurn), ]
colnames(tb_post_full_t0.4) <- c("intercept", colnames(xDevelop))


mcmc_areas(tb_post_full_t0.4,
           pars = c(PIP_TB_0.4_df$parameter[1:20]),
           prob = 0.95,
           point_est = c("median"),
           area_method = "scaled height") + 
  ggtitle("TB threshold 0.4 - Posterior distributions of top 20 most important variables", "with medians and 95% credible intervals") +
  theme_bw() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        plot.subtitle = element_text(size = 8)) +
  scale_x_continuous(breaks = seq(-1, 2, 0.5), limits = c(-1, 2)) 


# tailored bayes 0.45
i <- 8
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
tb_post_full_t0.45 <- fit_develop$beta[-c(1:nBurn), ]
colnames(tb_post_full_t0.45) <- c("intercept", colnames(xDevelop))


mcmc_areas(tb_post_full_t0.45,
           pars = c(PIP_TB_0.45_df$parameter[1:20]),
           prob = 0.95,
           point_est = c("median"),
           area_method = "scaled height") + 
  ggtitle("TB threshold 0.45 - Posterior distributions of top 20 most important variables", "with medians and 95% credible intervals") +
  theme_bw() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        plot.subtitle = element_text(size = 8)) +
  scale_x_continuous(breaks = seq(-1, 2, 0.5), limits = c(-1, 2)) 



# tailored bayes 0.6
i <- 11
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
tb_post_full_t0.6 <- fit_develop$beta[-c(1:nBurn), ]
colnames(tb_post_full_t0.6) <- c("intercept", colnames(xDevelop))


mcmc_areas(tb_post_full_t0.6,
           pars = c(PIP_TB_0.6_df$parameter[1:20]),
           prob = 0.95,
           point_est = c("median"),
           area_method = "scaled height") + 
  ggtitle("TB threshold 0.6 - Posterior distributions of top 20 most important variables", "with medians and 95% intervals")



# tailored bayes 0.7
i <- 13
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
tb_post_full_t0.7 <- fit_develop$beta[-c(1:nBurn), ]
colnames(tb_post_full_t0.7) <- c("intercept", colnames(xDevelop))


mcmc_areas(tb_post_full_t0.7,
           pars = c(PIP_TB_0.7_df$parameter[1:20]),
           prob = 0.95,
           point_est = c("median"),
           area_method = "scaled height") + 
  ggtitle("TB threshold 0.7 - Posterior distributions of top 20 most important variables", "with medians and 95% intervals")



# tailored bayes 0.75
i <- 14
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
tb_post_full_t0.75 <- fit_develop$beta[-c(1:nBurn), ]
colnames(tb_post_full_t0.75) <- c("intercept", colnames(xDevelop))


mcmc_areas(tb_post_full_t0.75,
           pars = c(PIP_TB_0.75_df$parameter[1:20]),
           prob = 0.95,
           point_est = c("median"),
           area_method = "scaled height") + 
  ggtitle("TB threshold 0.75 - Posterior distributions of top 20 most important variables", "with medians and 95% intervals")



# tailored bayes 0.8
i <- 15
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
tb_post_full_t0.8 <- fit_develop$beta[-c(1:nBurn), ]
colnames(tb_post_full_t0.8) <- c("intercept", colnames(xDevelop))


mcmc_areas(tb_post_full_t0.8,
           pars = c(PIP_TB_0.8_df$parameter[1:20]),
           prob = 0.95,
           point_est = c("median"),
           area_method = "scaled height") + 
  ggtitle("TB0.8 - Posterior distributions of top 20 most important variables", "with medians and 95% intervals")




