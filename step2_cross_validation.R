# get array element number
task_id_string <- Sys.getenv("SLURM_ARRAY_TASK_ID") 
task_id <- as.numeric(task_id_string)

# Libraries
library(Rcpp)
library(RcppArmadillo)
library(dplyr)


load("step1_outputs/R_outputs/step1_preparation_data.Rdata")
sourceCpp('./src/tailored_bayes.cpp')


threshold <- rep(seq(0.1, 0.9, 0.05), each = 21)
lambda <- rep(seq(0, 60, 3), 17)

print(paste0(c("threshold = ", threshold[task_id], ", lambda = ", lambda[task_id]), collapse = ""))

w <- exp(-lambda[task_id] * (unweighted_prob_develop - threshold[task_id])^2)

fit_cv <- list()
pred_cv <- list()
NB_cv <- vector()
for (i in 1:K) {
  print(paste0("cross validation: ", i, ". in progress ..."))
  set.seed(12)
  fit_cv[[i]] <- tb_horseshoe(yDevelop[cvIndex != i], as.matrix(xDevelop[cvIndex != i, ]), w[cvIndex != i], nIter, nThin, 1)
  pred_cv[[i]] <- pp_predict(fit_cv[[i]]$beta[-c(1:nBurn), ], cbind(1, xDevelop[cvIndex == i, ]), fun = mean)
  NB_cv[i] <- net_benefit_treated(pred_cv[[i]], yDevelop[cvIndex == i], threshold[task_id])$NB
  print(paste0("cross validation: ", i, ". finished!"))
}
avg_NB <- mean(NB_cv)

threshold_lambda_avgNB_df <- data.frame(threshold = threshold[task_id], lambda = lambda[task_id], NB = avg_NB)
print(threshold_lambda_avgNB_df)


output_file <- paste0("output_", task_id, ".RData")
setwd("/step2_outputs/R_outputs")
save(threshold_lambda_avgNB_df, file = output_file)


print("outputs saved.")















