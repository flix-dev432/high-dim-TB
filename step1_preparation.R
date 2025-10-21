# Libraries
library(Rcpp)
library(RcppArmadillo)
sourceCpp('./src/tailored_bayes.cpp')
source('./R/plot.R')                      # to make posterior boxplots
library(dplyr)
library(caret)


# Data
load("data_METABRIC_ER_pos.Rdata")
METABRIC_X <- df_METABRIC_er_pos[ , c(6, 8:10, 12:1509)]
METABRIC_y <- df_METABRIC_er_pos$y


# Data preprocessing
set.seed(42)
calculatePreProcess <- preProcess(METABRIC_X, method = c("center", "scale", "corr", "nzv"), cutoff = 0.5)
calculatePreProcess
METABRIC_X_pp <- predict(calculatePreProcess, METABRIC_X)


# prediction function (function to summarise posterior predictive distribution)
expit <- function(x){exp(x)/(1 + exp(x))}
pp_predict <- function(param, x, fun) {
  prob1 <- expit(as.matrix(x) %*% t(param))
  prob1[is.nan(prob1)] <- 0      # force not defined values to 0
  predictions_logit <- apply(prob1, MAR = 1, fun)
  predictions_logit
}

# Net_benefit function
# function to calculate the NB of the treated i.e those 
# receiving treatment (risk > risk_threshold)
net_benefit_treated <- function(pred_y, obs_y, risk_threshold){
  # pred_y = (vector) of the predicted probability of outcomes, must be 0 < pred_y < 1
  # obs_y = (vector) of the observed outcome, must be 1 = event and 0 = non-event 
  # risk_threshold = (vector) of the risk thresholds, must be 0 < risk_threshold < 1 
  
  # start checks
  # if (!all(pred_y >= 0) | !all(pred_y <= 1)){
  #    stop("The predicted values must range between 0 and 1")
  # }
  
  if (!all(between(pred_y, 0, 1))){
    stop("The predicted values must range between 0 and 1")
  }
  
  if (!all(obs_y == 0 | obs_y == 1)){
    stop("Outcome must be coded 0 (non event) and 1 (event)")
  }
  
  # if (!all(risk_threshold >= 0) | !all(risk_threshold <= 1)){
  #    stop("The risk threshold must range between 0 and 1")
  # }
  
  if (!all(between(risk_threshold, 0, 1))){
    stop("The risk threshold must range between 0 and 1")
  }
  
  # end checks
  
  # number of observations
  N <- length(obs_y)
  
  # prevalence Pr(obs_y = 1)
  rho = mean(obs_y == 1)
  
  # initialize empty vectors
  tpr <- numeric(length(risk_threshold))
  fpr <- numeric(length(risk_threshold))
  nb <- numeric(length(risk_threshold))
  snb <- numeric(length(risk_threshold))
  U_all <- numeric(length(risk_threshold))
  sU_all <- numeric(length(risk_threshold))
  
  # loop through each risk threshold and calcualte tpr, fpr, nb, snb, U_all, sU_all
  for(i in 1:length(risk_threshold)){
    
    # true positive rate Pr(pred_y > risk_threshold | obs_y = 1)
    tpr[i] <- sum(pred_y >= risk_threshold[i] & obs_y == 1)/sum(obs_y == 1)
    
    # false positive rate Pr(pred_y > risk_threshold | obs_y = 0)
    fpr[i] <- sum(pred_y >= risk_threshold[i] & obs_y == 0)/sum(obs_y == 0) # check again it works ok 
    
    # net benefit
    nb[i] = tpr[i] * rho - (risk_threshold[i]/(1 - risk_threshold[i])) * (1 - rho) * fpr[i]
    
    # standardized net benefit
    snb[i] = nb[i]/rho
    
    # treat all NB = (U_all - U_none)
    U_all[i] = rho - (1 - rho) * (risk_threshold[i]/(1- risk_threshold[i]))
    
    # treat all sNB 
    sU_all[i] = U_all[i]/rho
    
  }
  
  # expected utility treat none
  U_none = 0
  
  output  = data.frame("threshold" = risk_threshold,
                       "TPR" = tpr,
                       "FPR" = fpr,
                       "NB" = nb,
                       "sNB" = snb,  # standardlized by the prevalence of the outcome
                       "rho" = rho,    # empirical prevalence pos_result/total_test
                       "NB_none" = U_none,  # we not classify anyone so anyone is zero, like the threshold be >1
                       "NB_all" = U_all,   # NB if classify everyone as positive, treat everyone
                       "sNB_all" = sU_all)
  return(output)
  
}


# MCMC parameters
nThin = 2    # to reduce the autocorrelation between samples
nBurn = 500  # number of burn-in samples
nMCMC = 1000  # number of post-burn-in samples
nIter = nThin*(nBurn+nMCMC)


# Data splitting and cross-validation setup

# Split the data into 80% training set and 20% test set
set.seed(42)
trainIndex <- createDataPartition(y = METABRIC_y, times = 1, p = 0.8, list = F)  # maintain class balance when splitting the data
yTrain <- METABRIC_y[trainIndex]
yTest <- METABRIC_y[-trainIndex]

xTrain <- METABRIC_X_pp[trainIndex, ]
xTest <- METABRIC_X_pp[-trainIndex, ]

# Split the training set into 80% development set and 20% design set
set.seed(42)
developIndex <- createDataPartition(y = yTrain, times = 1, p = 0.8, list = F)
yDevelop <- yTrain[developIndex]
yDesign <- yTrain[-developIndex]

xDevelop <- xTrain[developIndex, ]
xDesign <- xTrain[-developIndex, ]

# train the unweighted standard Bayesian logistic regression on the design set
w_u = rep(1, length(yDesign))  # set weights to be 1
set.seed(12)
fit_unweighted_design = tb_horseshoe(yDesign, as.matrix(xDesign), w_u, nIter, nThin, 1)
unweighted_prob_develop <- pp_predict(fit_unweighted_design$beta[-c(1:nBurn), ], cbind(1, xDevelop), fun = mean)

K <- 5  # five-fold CV
set.seed(42)
cvIndex <- createFolds(yDevelop, K, list = F)

save.image(file = "/step1_outputs/R_outputs/step1_preparation_data.Rdata")















