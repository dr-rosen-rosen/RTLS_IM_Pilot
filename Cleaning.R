library(here)
library(lme4)
library(sjstats)
library(sjPlot)
library(lmerTest)
library(ggplot2)
library(dplyr)

df <- read.csv(here('/data/Flat_ind_level.csv'))
shorties <- c(308419,308762,308775,308784,308786,308792,308796,308797)
df <- df[!(df$Badge %in% shorties),]
df <- df[(df$Total.Total > 0),]
df$Total.Patient.room.perc <- df$Total.Patient.room / df$Total.Total
df$Badge <- factor(df$Badge)
df$Total.Patient.room.perc <- scale(df$Total.Patient.room.perc, center = TRUE, scale = TRUE)
hist(df$Total.Patient.room.perc)

x <- df %>%
  filter(Total.Patient.room != 0) %>%
  group_by(Badge) %>%
  summarise(no_rows = length(Badge))

selected <- c(308807,308808,308771,308774,308776)

ggplot(data = df[df$Badge %in% selected,], aes(x = Num_days, y = Total.Patient.room.perc, group = Badge)) + 
  geom_line(show.legend = FALSE) + 
  geom_smooth(method='lm') +
  facet_wrap(~ Badge) +
  theme_light()

ggplot(data = df, aes(x = Num_days, y = Total.Patient.room.perc, group = Badge)) + 
  geom_line(show.legend = FALSE) + 
  geom_smooth(method='lm') +
  facet_wrap(~ Badge) +
  theme_light()

M0 <- lmer(Total.Patient.room.perc ~ 1 + (1|Badge), df, REML = FALSE)
summary(M0)

M1 <- lmer(Total.Patient.room.perc ~ Num_days + (1|Badge), df, REML = FALSE)
anova(M0,M1)
summary(M1)
tab_model(M1)

M2 <- lmer(Total.Patient.room.perc ~ Num_days + (Num_days|Badge), df, REML = FALSE, 
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e5)))
anova(M1,M2)
summary(M2)
tab_model(M2)