library(here)
library(lme4)
library(sjstats)
library(sjPlot)
library(lmerTest)
library(ggplot2)
library(dplyr)

df2 <- read.csv(here('/data/Individual_LONG.csv'))
df2 <- df2[!(df2$Badge %in% shorties),]

hist(df2[df2$Count > 240,]$Percent)
hist(df2[(df2$locs == 'Patient room' & df2$Time_period == 'Total'),]$Percent, breaks = 200)


selected <- c(308807,308808,308771,308774,308776)
df2 <- df2[df2$Time_period == 'Total',]
df2 <- df2[df2$locs == 'Patient room',]
ggplot(data = df2[df2$Badge %in% selected,], aes(x = Num_days, y = Percent, group = Badge)) + 
  geom_line(show.legend = FALSE) + 
  geom_smooth(method='lm') +
  facet_wrap(~ Badge) +
  theme_light()

ggplot(data = df2, aes(x = Num_days, y = Percent, group = Badge)) + 
  geom_line(show.legend = FALSE) + 
  geom_smooth(method='lm') +
  facet_wrap(~ Badge) +
  theme_light()

ggplot(data = df2, aes(x = Num_days, y = Percent, group = Service)) + 
  geom_line(show.legend = FALSE) + 
  geom_smooth(method='lm') +
  facet_wrap(~ Service) +
  theme_light()

M0 <- lmer(Percent ~ 1 + (1|Badge), df2, REML = FALSE)
summary(M0)

M1 <- lmer(Percent ~ Num_days + (1|Badge), df2, REML = FALSE)
anova(M0,M1)
summary(M1)
tab_model(M1)

M2 <- lmer(Percent ~ Num_days + (Num_days|Badge), df2, REML = FALSE, 
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e5)))
anova(M1,M2)
summary(M2)
tab_model(M2)