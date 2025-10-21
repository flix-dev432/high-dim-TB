library(tidyverse)


# SAVS_PIP (Signal Adaptive Variable Selector)
# paper: https://www.tandfonline.com/doi/pdf/10.1080/07350015.2020.1713796?needAccess=true
savs_pip <- function(X, beta.iters){
  # Input: X is nXp design matrix; beta.iters: posterior samples of beta (pXK), K=number of iters
  # Output: Posterior Inclusion Probability (PIP)
  beta.est = double()
  beta = matrix(NA, nrow = dim(beta.iters)[1], ncol = dim(beta.iters)[2])
  
  for(i in 1:(ncol(beta.iters))){ # do not count intercept
    
    beta.in <- beta.iters[,i]
    
    mu = (beta.in) ^ (-2)
    
    p <- length(mu) 
    
    for(j in 1:p){
      xtx = t(X[,j]) %*% X[,j]
      if(mu[j] >= abs(beta.in[j]) * xtx){
        beta.est[j] = 0
      }else{
        beta.est[j] = sign(beta.in[j]) * (abs(beta.in[j]) * xtx - mu[j])/xtx
      }
    }
    beta[,i] = beta.est
  }
  
  help_fun <- function(x) sum(x == 0, na.rm = T)
  
  pep <- apply(beta, MARGIN = 1, FUN = help_fun)/dim(beta.iters)[2] # probability of exclusion
  
  pip <- 1 - pep # probability of inclusion
  
  return(pip)
}

# coefficients are on the rows and iterations are on the columns 
# for Talored Bayes we need to transpose the result
# not consider the intercept (always included)


load("step1_outputs/R_outputs/step1_preparation_data.RData")

# standard bayes
load("step5_outputs/R_outputs/SB_results.RData")

PIP_SB <- savs_pip(as.matrix(xTrain), t(fit_sb_train$beta[-c(1:nBurn), -c(1)]))
PIP_SB_df <- data.frame(parameter = colnames(xTrain), PIP_SB = PIP_SB) %>%
  arrange(desc(PIP_SB))
head(PIP_SB_df, 20)

boxes(fit_sb_train$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for standard bayes")


# tailored bayes
# threshold 0.1
i <- 1
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.1 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.1_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.1 = PIP_TB_0.1) %>%
  arrange(desc(PIP_TB_0.1))
head(PIP_TB_0.1_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.1")


# tailored bayes
# threshold 0.15
i <- 2
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.15 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.15_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.15 = PIP_TB_0.15) %>%
  arrange(desc(PIP_TB_0.15))
head(PIP_TB_0.15_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.15")

# tailored bayes
# threshold 0.2
i <- 3
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.2 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.2_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.2 = PIP_TB_0.2) %>%
  arrange(desc(PIP_TB_0.2))
head(PIP_TB_0.2_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.2")

# tailored bayes
# threshold 0.25
i <- 4
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.25 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.25_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.25 = PIP_TB_0.25) %>%
  arrange(desc(PIP_TB_0.25))
head(PIP_TB_0.25_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.25")


# tailored bayes
# threshold 0.3
i <- 5
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.3 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.3_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.3 = PIP_TB_0.3) %>%
  arrange(desc(PIP_TB_0.3))
head(PIP_TB_0.3_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.3")


# tailored bayes
# threshold 0.35
i <- 6
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.35 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.35_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.35 = PIP_TB_0.35) %>%
  arrange(desc(PIP_TB_0.35))
head(PIP_TB_0.35_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.35")


# tailored bayes
# threshold 0.4
i <- 7
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.4 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.4_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.4 = PIP_TB_0.4) %>%
  arrange(desc(PIP_TB_0.4))
head(PIP_TB_0.4_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.4")


# tailored bayes
# threshold 0.45
i <- 8
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.45 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.45_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.45 = PIP_TB_0.45) %>%
  arrange(desc(PIP_TB_0.45))
head(PIP_TB_0.45_df, 20)
boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.45")


# tailored bayes
# threshold 0.5
i <- 9
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.5 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.5_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.5 = PIP_TB_0.5) %>%
  arrange(desc(PIP_TB_0.5))
head(PIP_TB_0.5_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.5")


# tailored bayes
# threshold 0.55
i <- 10
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.55 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.55_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.55 = PIP_TB_0.55) %>%
  arrange(desc(PIP_TB_0.55))
head(PIP_TB_0.55_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.55")

# tailored bayes
# threshold 0.6
i <- 11
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.6 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.6_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.6 = PIP_TB_0.6) %>%
  arrange(desc(PIP_TB_0.6))
head(PIP_TB_0.6_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.6")


# tailored bayes
# threshold 0.65
i <- 12
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.65 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.65_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.65 = PIP_TB_0.65) %>%
  arrange(desc(PIP_TB_0.65))
head(PIP_TB_0.65_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.65")


# tailored bayes
# threshold 0.7
i <- 13
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.7 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.7_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.7 = PIP_TB_0.7) %>%
  arrange(desc(PIP_TB_0.7))
head(PIP_TB_0.7_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.7")


# tailored bayes
# threshold 0.75
i <- 14
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.75 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.75_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.75 = PIP_TB_0.75) %>%
  arrange(desc(PIP_TB_0.75))
head(PIP_TB_0.75_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.75")

# tailored bayes
# threshold 0.8 
i <- 15
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.8 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.8_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.8 = PIP_TB_0.8) %>%
  arrange(desc(PIP_TB_0.8))
head(PIP_TB_0.8_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.8")

# tailored bayes
# threshold 0.85
i <- 16
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.85 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.85_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.85 = PIP_TB_0.85) %>%
  arrange(desc(PIP_TB_0.85))
head(PIP_TB_0.85_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.85")

# tailored bayes
# threshold 0.9
i <- 17
load(paste0("./step4_outputs/R_outputs/", paste0("output_", i, ".Rdata")))
PIP_TB_0.9 <- savs_pip(as.matrix(xDevelop), t(fit_develop$beta[-c(1:nBurn), -c(1)]))
PIP_TB_0.9_df <- data.frame(parameter = colnames(xDevelop), PIP_TB_0.9 = PIP_TB_0.9) %>%
  arrange(desc(PIP_TB_0.9))
head(PIP_TB_0.9_df, 20)

boxes(fit_develop$beta[-c(1:nBurn), ], outliers = F)
title("Posterior box plots for tailored bayes with threshold = 0.9")



PIP_combined <- cbind(PIP_SB_df, PIP_TB_0.1_df, PIP_TB_0.15_df, PIP_TB_0.2_df, PIP_TB_0.25_df, PIP_TB_0.3_df, PIP_TB_0.35_df,
                      PIP_TB_0.4_df, PIP_TB_0.45_df, PIP_TB_0.5_df, PIP_TB_0.55_df, PIP_TB_0.6_df, PIP_TB_0.65_df, 
                      PIP_TB_0.7_df, PIP_TB_0.75_df, PIP_TB_0.8_df, PIP_TB_0.85_df, PIP_TB_0.9_df)
View(PIP_combined)



# compare with the variables selected by glmnet 
library(glmnet)
set.seed(12)
glmnet_fit <- cv.glmnet(as.matrix(METABRIC_X_pp), METABRIC_y, family = "binomial")
plot(glmnet_fit)

coef(glmnet_fit, s = "lambda.1se")

coef_glmnet_fit <- as.matrix(coef(glmnet_fit, s = "lambda.1se"))
data.frame(coef_glmnet_fit) %>%
  filter(s1 != 0)  %>%
  arrange(desc(abs(s1)))


set.seed(12)
glmnet_fit_auc <- cv.glmnet(as.matrix(METABRIC_X_pp), METABRIC_y, family = "binomial", type.measure = "auc")
plot(glmnet_fit_auc)

coef(glmnet_fit_auc, s = "lambda.1se")

coef_glmnet_fit_auc <- as.matrix(coef(glmnet_fit_auc, s = "lambda.1se"))
data.frame(coef_glmnet_fit_auc) %>%
  filter(s1 != 0)  %>%
  arrange(desc(abs(s1)))



