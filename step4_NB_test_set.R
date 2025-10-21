# get array element number
task_id_string <- Sys.getenv("SLURM_ARRAY_TASK_ID") 
task_id <- as.numeric(task_id_string)

# Libraries
library(Rcpp)
library(RcppArmadillo)
library(dplyr)
library(ggplot2)


load("step1_outputs/R_outputs/step1_preparation_data.Rdata")
load("./step3_outputs/opt_lambda_selected.RData")
sourceCpp('./src/tailored_bayes.cpp')


tl <- opt_lambda_selected[task_id, ]
threshold <- tl$threshold
lambda <- tl$lambda


opt_w <- exp(-lambda * (unweighted_prob_develop - threshold)^2)

print("run first chain. in progress...")
set.seed(12)
fit_develop <- tb_horseshoe(yDevelop, as.matrix(xDevelop), opt_w, nIter, nThin, 1)
print("first chain. completed!")

print("run second chain. in progress...")
set.seed(17)
fit_develop2 <- tb_horseshoe(yDevelop, as.matrix(xDevelop), opt_w, nIter, nThin, 1)
print("second chain. completed!")


weighted_prob_test <- expit(as.matrix(cbind(1, xTest)) %*% t(fit_develop$beta[-c(1:nBurn), ]))
weighted_prob_test2 <- expit(as.matrix(cbind(1, xTest)) %*% t(fit_develop2$beta[-c(1:nBurn), ]))


NB_test <- vector()
for (i in 1:dim(weighted_prob_test)[2]) {
  NB_test[i] <- net_benefit_treated(weighted_prob_test[, i], yTest, threshold)$NB
}

NB_test2 <- vector()
for (i in 1:dim(weighted_prob_test2)[2]) {
  NB_test2[i] <- net_benefit_treated(weighted_prob_test2[, i], yTest, threshold)$NB
}


NB_test_summary <- data.frame(threshold = threshold, 
                              mean = mean(NB_test), 
                              median = median(NB_test),
                              lower = quantile(NB_test, 0.025), 
                              upper = quantile(NB_test, 0.975))
print(NB_test_summary)


NB_test2_summary <- data.frame(threshold = threshold, 
                               mean = mean(NB_test2), 
                               median = median(NB_test2),
                               lower = quantile(NB_test2, 0.025), 
                               upper = quantile(NB_test2, 0.975))
print(NB_test2_summary)


setwd("/step4_outputs/R_outputs")

output_file <- paste0("output_", task_id, ".RData")
save(tl, threshold, lambda, opt_w, fit_develop, fit_develop2, weighted_prob_test, weighted_prob_test2, NB_test, NB_test2, NB_test_summary, NB_test2_summary, file = output_file)


png_file <- paste0("rplot_", task_id, ".png")
png(png_file, width = 403, height = 386)
ggplot(data = data.frame(NB_test), aes(NB_test)) +
  geom_density() +
  theme_bw() +
  labs(title = paste0("Tailored bayes NB density plot for threshold = ", threshold),
       x = "Net benefit")
dev.off()

print("outputs saved!")




