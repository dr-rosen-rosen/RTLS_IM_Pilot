library(lme4)
library(sjstats)
library(sjPlot)
library(sjmisc)
library(lmerTest)
library(lattice)

detach("package:lattice", unload=TRUE)
hist(df$Patient.room_perc)
# df <- df %>%
#   filter(Interval == 'all_24')
#df$Patient.room_perc_scaled <- df$Patient.room_perc#scale(df$Patient.room_perc, center = TRUE, scale = TRUE)
#hist(df$Patient.room_perc_scaled)

M0a <- df %>%
  filter(Interval == 'all_24') %>%
  mutate(Patient.room_perc = scale(Patient.room_perc, center = TRUE, scale = TRUE)) %>%
  lmer(Patient.room_perc ~ 1 + (1|RTLS_ID) + (1|Service_grouped), ., REML = FALSE)
summary(M0a)
sjPlot::plot_model(M0a)

M0b <- df %>%
  filter(Interval == 'all_24') %>%
  mutate(Patient.room_perc = scale(Patient.room_perc, center = TRUE, scale = TRUE)) %>%
  lmer(Patient.room_perc ~ 1 + (1|Service_grouped), ., REML = FALSE)
summary(M0b)
anova(M0a,M0b)

M1a <- df %>%
  filter(Interval == 'all_24') %>%
  mutate(Patient.room_perc = scale(Patient.room_perc, center = TRUE, scale = TRUE)) %>%
  lmer(Patient.room_perc ~ numDays + (1|RTLS_ID) + (1|Service_grouped), ., REML = FALSE)
summary(M1a)
anova(M0a,M1a)
sjPlot::tab_model(M1a)
lattice::qqmath(M1a)

M1b <- df %>%
  filter(Interval == 'all_24') %>%
  mutate(Patient.room_perc = scale(Patient.room_perc, center = TRUE, scale = TRUE)) %>%
  lmer(Patient.room_perc ~ numDays + Service_numDays + (1|RTLS_ID) + (1|Service_grouped), ., REML = FALSE)
summary(M1b)
anova(M1a,M1b)
sjPlot::tab_model(M1b)
plot(M1b)
sjPlot::plot_model(M1b, sort.est = TRUE)
lattice::qqmath(M1b)

M2a <- df %>%
  filter(Interval == 'all_24') %>%
  mutate(Patient.room_perc = scale(Patient.room_perc, center = TRUE, scale = TRUE)) %>%
  lmer(Patient.room_perc ~ numDays + Service_numDays + DayOfWeek + (1|RTLS_ID) + (1|Service_grouped), ., REML = FALSE)
anova(M1b,M2a)
summary(M2a)
sjPlot::plot_model(M2a, sort.est = TRUE)
sjPlot::tab_model(M2a)
lattice::qqmath(M2a)

sjPlot::tab_model(M0a,M1a,M1b,M2a, file = here('output','tables','Model_table.html'))
  
  

