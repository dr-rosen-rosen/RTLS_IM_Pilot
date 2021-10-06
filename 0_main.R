library(here)
library(lme4)
library(sjstats)
library(sjPlot)
library(lmerTest)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(patchwork)
library(lubridate)
library(stargazer)
source('1_funcs.R')

#df <- readr::read_csv('data/IndLongByInterval_03112021.csv') 
df <- read.csv('data/IndLongByInterval_03112021.csv')
df <- do_cleaning(df)


# Intern plots
library(forcats)
df$RTLS_ID <- with(df, reorder(RTLS_ID, Patient.room_perc, median))
i1 <- ggplot(df, aes(x = RTLS_ID, y = Patient.room_perc)) +
  geom_boxplot() +
  labs(x = 'Intern', y = 'Proportion of time spent in Patient Room', title = 'Distribution of time spent at bedside by Intern') +
  coord_flip() +
  theme_tufte()

i2 <- ggplot(data = df, aes(x = Day, y = Patient.room_perc, group = RTLS_ID)) + #Patient.room_perc
  geom_line(show.legend = FALSE) +
  geom_smooth(method = 'lm') +
  facet_wrap(~ RTLS_ID,ncol = 2) +
  labs(x = 'Day', y = 'Proportion of time spent in Patient Room', title = 'Time spent at bedside over time by intern') +
  theme_classic()

(i1 + i2)

# servie plots

s1 <- ggplot(df, aes(x = Service, y = Patient.room_perc)) +
  geom_boxplot() +
  labs(x = 'Service', y = 'Proportion of time spent in Patient Room', title = 'Proportion of time spent in patient room across services') +
  #coord_flip() +Thanks
  geom_boxplot(data = transform(df, Service = 'all')) +
  theme_classic()

s2 <- ggplot(data = df, aes(x = Day, y = Patient.room_perc, group = Service)) + 
  geom_line(show.legend = FALSE) + 
  geom_smooth(method='lm') +
  facet_wrap(~ Service) +
  labs(x = 'Day', y = 'Proportion of time spent in Patient Room', title = 'Changes in the proportion of time spent at the bedside over the year, by service') +
  geom_line(data = transform(df, Service = 'all')) +
  theme_classic()
(s1 / s2)

# Time

ggplot(df, aes(x = Month, y = Patient.room_perc)) +
  geom_boxplot() +
  theme_classic()


get_histograms(df)
make_tables(df)