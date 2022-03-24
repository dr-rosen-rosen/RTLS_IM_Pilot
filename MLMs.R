library(lme4)
library(sjstats)
library(sjPlot)
library(sjmisc)
library(lmerTest)
library(lattice)
library(patchwork)

detach("package:lattice", unload=TRUE)
hist(df$Patient.room_perc)
interval_var <- c('rounds')

M00 <- df %>%
  filter(Interval %in% !!interval_var) %>%
  mutate(Patient.room_perc = scale(Patient.room_perc, center = TRUE, scale = TRUE)) %>%
  lm(Patient.room_perc ~ 1, data = .)
summary(M00)

M0a <- df %>%
  filter(Interval %in% !!interval_var) %>%
  mutate(Patient.room_perc = scale(Patient.room_perc, center = TRUE, scale = TRUE)) %>%
  lmer(Patient.room_perc ~ 1 + (1|RTLS_ID), ., REML = FALSE)
summary(M0a)
anova(M0a, M00)
sjPlot::plot_model(M0a)


M0b <- df %>%
  filter(Interval %in% !!interval_var) %>%
  mutate(Patient.room_perc = scale(Patient.room_perc, center = TRUE, scale = TRUE)) %>%
  lmer(Patient.room_perc ~ 1 + (1|RTLS_ID) + (1|Service_grouped), ., REML = FALSE)
summary(M0b)
anova(M0a,M0b)

M1a <- df %>%
  filter(Interval %in% !!interval_var) %>%
  mutate(Patient.room_perc = scale(Patient.room_perc, center = TRUE, scale = TRUE)) %>%
  lmer(Patient.room_perc ~ numDays + (1|RTLS_ID) + (1|Service_grouped), ., REML = FALSE)
summary(M1a)
anova(M0a,M1a)
sjPlot::tab_model(M1a)
lattice::qqmath(M1a)

M1b <- df %>%
  filter(Interval %in% !!interval_var) %>%
  mutate(Patient.room_perc = scale(Patient.room_perc, center = TRUE, scale = TRUE)) %>%
  lmer(Patient.room_perc ~ numDays + Service_numDays + (1|RTLS_ID) + (1|Service_grouped), ., REML = FALSE)
summary(M1b)
anova(M1a,M1b)
sjPlot::tab_model(M1b)
plot(M1b)
sjPlot::plot_model(M1b, sort.est = TRUE)
lattice::qqmath(M1b)

M2a <- df %>%
  filter(Interval %in% !!interval_var) %>%
  mutate(Patient.room_perc = scale(Patient.room_perc, center = TRUE, scale = TRUE)) %>%
  lmer(Patient.room_perc ~ numDays + Service_numDays + DayOfWeek + (1|RTLS_ID) + (1|Service_grouped), ., REML = FALSE)   
anova(M1b,M2a)
summary(M2a)

# library(parameters)
# ci(M2a)
# model_parameters(M2a, ci_method = 'residual')
# library(merTools)
# randomSims <- REsim(M2a, n.sims = 500)
# # and to plot it
# plotREsim(REsim(M2a, n.sims = 500)) +theme_tufte()

p_rounds <- sjPlot::plot_model(M2a, type = 're')[[2]] + 
  theme_tufte() +
  labs(title = 'Rounds',
       y = 'Percent time at the bedside (centered)',
       x = 'Service') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black')
show(p_rounds)

p_all24 <- sjPlot::plot_model(M2a, type = 're')[[2]] + 
  theme_tufte() +
  labs(title = '24 hour period',
       y = 'Percent time at the bedside (centered)',
       x = 'Service') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black')
show(p_all24)

p_all24 / p_rounds + plot_layout(guides = "collect")

sjPlot::tab_model(M2a)
lattice::qqmath(M2a)

sjPlot::tab_model(M00,M0a,M0b,M2a)#, file = here('output','tables','Model_table.html'))
anova(M00)
anova(M0a,M00)
anova(M0a,M0b)
anova(M0b,M2a)


df %>%
  filter(Interval == 'rounds') %>%
  aov(Patient.room_perc ~ Service_grouped, data = .) %>%
  #summary()
  plot()
  #TukeyHSD() %>% plot(las=1)


