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
library(skimr)
library(officer)
library(flextable)

source('1_funcs.R')

df <- read.csv(here('data','IndLongByInterval_03112021.csv')) %>%
  bind_rows(
    read.csv(here('data','IndLongByInterval_2021-10-08.csv')) # add rounds
  ) %>%
  do_cleaning()
skimr::skim(df)

get_histograms(df, interval = 'rounds', variable = 'Patient.room_perc')
print_key_stats(df)
make_tables(df)
get_stacked_bars(df)
get_monster_plot(
  ind_plots = get_individual_plots(df),
  serv_plots = get_service_plots(df)
)

#### Table for individuals
table_df <- df %>%
  filter(Interval == 'all_24') %>%
  group_by(RTLS_ID) %>%
  summarize(
    #Min_pr = min(Patient.room),
    N = n(),
    Avg_pr = mean(Patient.room),
    Sd_pr = sd(Patient.room), 
    Median_pr = quantile(Patient.room, .5),
    IQR_pr = quantile(Patient.room, .75) - quantile(Patient.room, .25),
    #Max_pr = max(Patient.room),
    #Min_pr_pct = min(Patient.room_perc),
    Avg_pr_pct = mean(Patient.room_perc),
    Sd_pr_pct = sd(Patient.room_perc), 
    Median_pr_pct = quantile(Patient.room_perc, .5),
    IQR_pr_pct = quantile(Patient.room_perc, .75) - quantile(Patient.room_perc, .25),
    #Max_pr_pct = max(Patient.room_perc),
    sum_hours = sum(Patient.room) / 60) %>%
  ungroup() %>%
  arrange(desc(Avg_pr))
table_df$RTLS_ID <- seq(1:nrow(table_df))

ft <- flextable(data = table_df) %>% 
  theme_alafoli %>% 
  colformat_double(digits = 1) %>%
  set_formatter(
    #Min_pr_pct = function(x) sprintf("%.1f%%", x*100),
    Avg_pr_pct = function(x) sprintf("%.1f%%", x*100),
    Sd_pr_pct = function(x) sprintf("%.1f%%", x*100),
    Median_pr_pct = function(x) sprintf("%.1f%%", x*100),
    IQR_pr_pct = function(x) sprintf("%.1f%%", x*100)#,
    #Max_pr_pct = function(x) sprintf("%.1f%%", x*100)
  ) %>%
  autofit
# See flextable in RStudio viewer
ft
save_as_docx(ft, path = here('output','tables','individual_descriptives.docx'))

min(table_df$Avg_pr_pct)
max(table_df$Avg_pr_pct)
IQR(table_df$Avg_pr_pct)
min(table_df$sum_hours)
max(table_df$sum_hours)
IQR(table_df$sum_hours)
##### table for Services
table_df <- df %>%
  filter(Interval == 'rounds') %>%
  group_by(Service_grouped) %>%
  summarize(
    Avg_pr = mean(Patient.room),
    Sd_pr = sd(Patient.room), 
    Median_pr = quantile(Patient.room, .5),
    IQR_pr = quantile(Patient.room, .75) - quantile(Patient.room, .25),
    Avg_pr_pct = mean(Patient.room_perc),
    Sd_pr_pct = sd(Patient.room_perc), 
    Median_pr_pct = quantile(Patient.room_perc, .5),
    IQR_pr_pct = quantile(Patient.room_perc, .75) - quantile(Patient.room_perc, .25),    
    Avg_wh_pct = mean(Ward.Hall_perc),
    Sd_wh_pct = sd(Ward.Hall_perc), 
    Median_wh_pct = quantile(Ward.Hall_perc, .5),
    IQR_wh_pct = quantile(Ward.Hall_perc, .75) - quantile(Ward.Hall_perc, .25)  
    ) %>%
  ungroup() %>%
  arrange(desc(Avg_pr))

ft <- flextable(data = table_df) %>% 
  theme_alafoli %>% 
  colformat_double(digits = 1) %>%
  set_formatter(
    Avg_pr_pct = function(x) sprintf("%.1f%%", x*100),
    Sd_pr_pct = function(x) sprintf("%.1f%%", x*100),
    Median_pr_pct = function(x) sprintf("%.1f%%", x*100),
    IQR_pr_pct = function(x) sprintf("%.1f%%", x*100),
    Avg_wh_pct = function(x) sprintf("%.1f%%", x*100),
    Sd_wh_pct = function(x) sprintf("%.1f%%", x*100),
    Median_wh_pct = function(x) sprintf("%.1f%%", x*100),
    IQR_wh_pct = function(x) sprintf("%.1f%%", x*100)
  ) %>%
  autofit
# See flextable in RStudio viewer
ft
#save_as_docx(ft, path = here('output','tables','individual_descriptives.docx'))

  








