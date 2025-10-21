# since the MCMC converges, we may combine the two chains and form a larger chain 
# with 2*1000 = 2000 iterations.
 
library(ggplot2)
library(tidyverse)
library(posterior)

load("./step6_outputs/TB_convergence_check.RData")
load("./step5_outputs/R_outputs/SB_results.RData")


TB_summ %>% 
  as.data.frame() %>% 
  mutate(threshold = as.factor(seq(0.1, 0.9, 0.05))) %>%
  ggplot(aes(threshold, mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0.5) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red") +
  theme_bw() +
  labs(x = "threshold", 
       y = "Net Benefit (TB) (95% CI)",
       title = "Net Benefit of Tailored Bayes (TB) per threshold")

SB_summ %>% 
  as.data.frame() %>% 
  mutate(threshold = as.factor(seq(0.1, 0.9, 0.05))) %>%
  ggplot(aes(threshold, mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0.5) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red") +
  theme_bw() +
  labs(x = "threshold", 
       y = "Net Benefit (SB) (95% CI)",
       title = "Net Benefit of Standard Bayes (SB) per threshold")


NB_diff <- rbind(tb_NB_test_df, tb_NB_test2_df) - rbind(sb_NB_test_df, sb_NB_test2_df)
print(NB_diff %>%
        as_draws() %>%
        summary("mean", ~quantile(.x, probs = c(0.025, 0.975))) %>%
        as.data.frame() %>% 
        mutate(threshold = as.factor(seq(0.1, 0.9, 0.05))) %>%
        ggplot(aes(threshold, mean)) +
        geom_point() +
        geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0.5) +
        geom_hline(yintercept = 0, linetype="dashed", color = "red") +
        theme_bw() +
        labs(x = "threshold", 
             y = "Difference in NB (TB - SB) (95% CI)",
             title = "Difference in NB between TB and SB per threshold"))





