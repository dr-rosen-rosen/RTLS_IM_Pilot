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
library(forcats)
source('1_funcs.R')

#df <- readr::read_csv('data/IndLongByInterval_03112021.csv') 
df <- read.csv('data/IndLongByInterval_03112021.csv')
df <- do_cleaning(df)

get_histograms(df)
make_tables(df)
get_stacked_bars(df)
# Intern plots

#df$RTLS_ID <- with(df, fct_reorder(RTLS_ID, Patient.room_perc, fun = mean))
i1 <- ggplot(df, aes(x = reorder(RTLS_ID, Patient.room_perc, na.rm = TRUE), y = Patient.room_perc)) +#fct_reorder(RTLS_ID, Patient.room_perc, .fun = median, .desc =TRUE), y = Patient.room_perc)) + #reorder(RTLS_ID, Patient.room_perc, FUN = min), y = Patient.room_perc)) +
  geom_boxplot() +
  labs(x = 'Intern', y = 'Proportion of time spent in Patient Room', title = 'Percent time spent \nat bedside by Intern') +
  coord_flip() +
  theme_tufte() +
  theme(#axis.title.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
        )

i2 <- ggplot(data = df, aes(x = Day, y = Patient.room_perc, group =reorder(RTLS_ID, Patient.room_perc, na.rm = TRUE))) + #Patient.room_perc
  geom_line(show.legend = FALSE) +
  geom_smooth(method = 'lm') +
  facet_wrap(~ RTLS_ID,ncol = 2) +
  labs(x = 'Day', y = 'Proportion of time spent in Patient Room', title = 'Percent time spent at bedside \nover time by intern') +
  theme_tufte() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

(i1 + i2)

# servie plots

s1 <- df %>%
  filter(Interval == 'all_24') %>%
  ggplot(aes(x = Service_grouped, y = Patient.room_perc)) +
  geom_boxplot() +
  labs(x = 'Service', y = 'Percent of time spent in Patient Room', title = 'Percent of time spent in patient room across services') +
  #coord_flip() +Thanks
  geom_boxplot(data = transform(df, Service_grouped = 'all')) +
  theme_tufte() +
  scale_y_continuous(labels = scales::percent_format())

s2 <- df %>%
  filter(Interval == 'all_24') %>%
  ggplot(aes(x = Day, y = Patient.room_perc, group =Service_grouped)) + 
  geom_line(show.legend = FALSE) + 
  facet_wrap(~ Service_grouped) +
  labs(x = 'Day', y = 'Percent of time spent in Patient Room', title = 'Changes in the percent of time spent at the bedside over the year, by service') +
  geom_line(data = transform(df, Service_grouped = 'all')) +
  geom_smooth(method="lm") +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_y_continuous(labels = scales::percent_format())
(s1 / s2)

# Time

ggplot(df, aes(x = Month, y = Patient.room_perc)) +
  geom_boxplot() +
  theme_tufte()
