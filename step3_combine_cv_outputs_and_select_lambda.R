# combine the cross-validation results
tlNB_combined <- data.frame() 
for (i in 1:357) {
  load(paste0("./step2_outputs/R_outputs/", paste0("output_", i, ".RData")))
  tlNB_combined <- rbind(tlNB_combined, threshold_lambda_avgNB_df)
}

# Net benefit vs lambda plot per threshold
library(ggplot2)
ggplot(tlNB_combined, aes(x = lambda, y = NB)) +
  geom_line() +
  geom_point() +
  facet_wrap( ~ threshold)

# select optimal lambda for each threshold
library(tidyverse)
opt_lambda_selected <- tlNB_combined %>%
  group_by(threshold) %>% 
  slice_max(order_by = NB) %>%
  slice_min(order_by = lambda)
opt_lambda_selected 

# Net benefit vs lambda plot per threshold with optimal lambda highlighted
ggplot(tlNB_combined, aes(x = lambda, y = NB)) +
  geom_line() +
  geom_point() +
  geom_point(data = opt_lambda_selected, aes(x = lambda, y = NB), col = "red") +
  facet_wrap( ~ threshold, labeller = labeller(threshold = function(x) paste("threshold", x))) +
  labs(title = "average 5-fold CV estimate of NB for various lambda values per target threshold")
