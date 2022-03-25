library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)

selfesteem <- read.csv('C:/Users/Administrator/Ai/R/inferential/FRIEDMAN/friedman.csv')
selfesteem

############################### Summary statistics
selfesteem %>%
  group_by(time) %>%
  get_summary_stats(score, type = "common")

############################### Visualization
ggboxplot(selfesteem, x = "time", y = "score", 
          color = "time", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          add = "jitter")

################################# Computation
res.fried <- selfesteem %>% friedman_test(score ~ time |id)
res.fried

################################# Effect size
selfesteem %>% friedman_effsize(score ~ time |id)

################################# Multiple pairwise-comparisons
# Pairwise comparisons using paired Wilcoxon signed-rank test
pwc <- selfesteem %>%
  wilcox_test(score ~ time, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# Pairwise comparisons using sign test:
pwc2 <- selfesteem %>%
  sign_test(score ~ time, p.adjust.method = "bonferroni")
pwc2

################################# Report
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "time")
ggboxplot(selfesteem, x = "time", y = "score", 
        color = "time", palette = c("#00AFBB", "#E7B800", "#FC4E07"), add = "point") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

