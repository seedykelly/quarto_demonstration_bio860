# rm(list=ls())

## ---- libraries ----
library(Reacnorm)
library(lubridate)
library(scales)
library(purrr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broman)
library(report)
library(forcats)
library(flextable)
library(modelsummary)
library(officer)
library(kableExtra)
library(knitr)
library(officedown)
library(ggh4x)
library(mixtools)
library(mixsmsn)
library(emmeans)
library(ggExtra)
library(rstatix)
library(lme4)
library(janitor)
library(broom.mixed)
library(lmerTest)
library(DataCombine)
library(ftExtra)
library(ggeffects)
library(cmstatr)
library(ggbeeswarm)
library(modelr)
library(MuMIn)
library(performance)
library(cowplot)
library(lattice)
library(patchwork)
library(gridGraphics)
library(lmerTest)
library(vcd)
library(gtools)
library(brms)
library(tidybayes)
library(broom)
library(huxtable)
library(glue)
library(ggside)
## ---- end

## ---- data_prep ----

load(file ="data/mean.sex.data.Rda")

## ---- end

## ---- plot_table ----


# devtime
mean.devtime.brms <- brm(dev_time ~ sex*diet*density + (1 | id_mere),
                         data = mean.sex.data, warmup = 500, iter = 3000, thin=2,
                         control = list(max_treedepth = 15, adapt_delta = 0.99),
                         sample_prior = TRUE,
                         file = "data/processed/mean.devtime.brms.Rds",
                         chains = 4, seed = 12345, cores = 4)
summary(mean.devtime.brms)
conditional_effects(mean.devtime.brms)

mean.devtime.brms_fixed <- broom::tidy(mean.devtime.brms, effects = "fixed", conf.level = 0.95, fix.intercept = FALSE) %>%
  mutate(
    ci_width = abs(conf.low - conf.high)
  )

mean.devtime.brms.em.a <- emmeans(mean.devtime.brms, pairwise ~ density) # 
mean.devtime.brms.em.b <- emmeans(mean.devtime.brms, pairwise ~ sex | density + diet) # Females larger than males at density 1 on poor and good diets;
mean.devtime.brms.em.d <- emmeans(mean.devtime.brms,  ~ density * diet) # Male: density 4 > density 8
mean.devtime.brms.em.contrast  <- contrast(mean.devtime.brms.em.d, interaction = c("pairwise", "consec"))
mean.devtime.brms.em.z <- emmeans(mean.devtime.brms, pairwise ~ density | diet * sex) # Females larger than males at density 1 on poor and good diets;

mean.sex.data.n <- mean.sex.data %>%
  group_by(diet, density, sex) %>%
  summarise(n=n())

library(ggpubr)
# dev time plot
dev.pvalues <- tibble::tribble(
  ~sex, ~diet, ~group1, ~group2, ~p.adj, ~y.position,
  "female", "POOR", "1", "4", "*", 70.5, 
  "female", "POOR", "4", "8", "NS", 70.5,
  "female", "POOR", "1", "8","NS", 72.5,
  "female", "GOOD", "1", "4", "*", 54.5, 
  "female", "GOOD", "4", "8", "*", 54.5,
  "female", "GOOD", "1", "8","*", 56.5,
  "male", "POOR", "1", "4", "NS", 71.5, 
  "male", "POOR", "4", "8", "NS", 71.5,
  "male", "POOR", "1", "8","NS", 73.5,
  "male", "GOOD", "1", "4", "*", 55.5, 
  "male", "GOOD", "4", "8", "NS", 55.5,
  "male", "GOOD", "1", "8","*", 57.5
)

devtime.data <- emmip(mean.devtime.brms, diet ~ density | sex, type="response", CIs=TRUE, CIarg = list(alpha = 1), levels = 0.95, plotit = FALSE) %>%
  left_join(mean.sex.data.n)

devtime.plot <- ggplot(devtime.data, aes(x=density, y=yvar, shape=diet, group=diet)) +
  theme_bw() +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width=0.0) +
  geom_line() +
  xlab("") +
  ylab("Predicted mean (95% CrI)\ndevelopment time (d)") +
  stat_pvalue_manual(dev.pvalues, label = "p.adj", size=2.5, tip.length = 0.01,inherit.aes = FALSE, remove.bracket = FALSE, bracket.shorten = .05) +
  facet_grid(~sex,labeller = labeller(sex = sex.labs)) +
  scale_shape_manual(values=c(15,16), limits=c("POOR", "GOOD"), labels = c("Poor", "Good"), name="Diet") +
  scale_x_discrete(limits=c("1", "4", "8"), labels = c("Low", "Medium", "High")) +
  theme(strip.text.x = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.position = "inside", legend.position.inside = c(0.05, 0.5),
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_rect(fill = "white")) +
  geom_text(data = devtime.data, aes(x=density, y=LCL, label = n), vjust = 1.5, size=2.5) +
  ylim(c(48,74))


## ---- end
