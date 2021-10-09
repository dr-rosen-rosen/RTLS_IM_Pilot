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
library(viridis)
source('1_funcs.R')

df <- read.csv(here('data','IndLongByInterval_03112021.csv')) %>%
  bind_rows(
    read.csv(here('data','IndLongByInterval_2021-10-08.csv')) # add rounds
  ) %>%
  do_cleaning()

get_histograms(df, interval = 'rounds', variable = 'Patient.room_perc')
make_tables(df)
get_stacked_bars(df)
get_individual_plots(df)
get_service_plots(df)

df %>%
  filter(Interval == 'all_24') %>%
  select(Patient.room) %>%
  hist(.)
# Time

ggplot(df, aes(x = Month, y = Patient.room_perc)) +
  geom_boxplot() +
  theme_tufte()

# Misc
df %>%
  filter(Interval == 'all_24') %>%
  ggplot(aes(x = factor(Service_numDays),y = Patient.room_perc)) +
  geom_violin() +
  theme_tufte()
  

df %>%
  filter(Interval == 'rounds') %>%
  #filter(Service_grouped %in% c('ICU', 'House staff')) %>%
  select(Service_grouped, Patient.room_perc, Ward.Hall_perc) %>%
  tidyr::pivot_longer(cols = !Service_grouped, names_to = "location", values_to = "perc_time") %>%
  tidyr::drop_na() %>%
  ggplot(aes(x = Service_grouped, y = perc_time, fill = location)) +
  geom_boxplot() +
  theme_tufte() +
  scale_y_continuous(labels = scales::percent_format()) #+
  #scale_fill_viridis(discrete = TRUE, option = 'G')







