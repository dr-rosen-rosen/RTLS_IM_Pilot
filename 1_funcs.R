#***********************************************************************
#***********************************************************************
#************  Scripts for RTSL project
#************
#***********************************************************************
#***********************************************************************
do_cleaning <- function(df) {
  df$Day <- as.Date(df$Day, "%Y-%m-%d")
  df$Month <- month(df$Day)
  #formatting vars
  df <- df %>%
    mutate(Service_grouped = recode(Service_grouped, 'Brancati' = 'Hospitalist GIM'))
  df <- df[!(df$Service %in% c("ED")),]
  fac_vars <- c('Service','Interval','RTLS_ID','Month','Service_grouped')
  for (var in fac_vars) {
    df[[var]] <- as.factor(df[[var]])
  }
  df$Interval <- ordered(df$Interval, levels = c('all_24', 'rounds', 'morning', 'afternoon', 'evening', 'night'))
  df$Month <- ordered(df$Month, levels = c(7,8,9,10,11,12,1,2,3,4,5,6))
  to_min_vars <- c('MD.Workroom', 'Supply.and.admin','Patient.room', 'Education', 'staff.admin.area','OTHER.UNKNOWN', 'Family.waiting.space','Transit', 'Ward.Hall', 'Total')
  for (var in to_min_vars) {
    #df[is.na(df[[var]]),df[[var]]] <- 0 # replaces NA's, so this doesn't kill R session
    df[[var]] <- df[[var]] / 60
  }
  to_replace_na_vars <- append(
    to_min_vars,
    c('MD.Workroom_perc', 'Supply.and.admin_perc','Patient.room_perc', 'Education_perc', 'staff.admin.area_perc','OTHER.UNKNOWN_perc', 'Family.waiting.space_perc','Transit_perc', 'Ward.Hall_perc')
  )
  # exclusions
  #df <- df[df$Total >= 240,] # takes all days with at least 4 hours of data
  shorties <- c(308775)
  df <- df[!df$RTLS_ID %in% shorties,]
  big_intervals <- c('all_24', 'morning', 'afternoon', 'evening', 'night')
  df <- df %>%
    filter(
      (Interval %in% big_intervals & Total >= 240) | (Interval == 'rounds' & Total >= 60)
      ) %>%
    mutate(
      across(all_of(to_replace_na_vars), ~tidyr::replace_na(.x, 0))
    )
  return(df)
}

#***********************************************************************
#************  Plotting
#***********************************************************************

get_histograms <- function(df, interval, variable) {
  variable <- ensym(variable)
  p1 <- df %>%
    filter(Interval == interval) %>%
    #filter(Patient.room >= 1) %>%
    #mutate(Patient.room = log(Patient.room)) %>%
    ggplot(aes(x = !! variable)) + 
    geom_histogram(bins = 200) +
    theme_tufte()# + labs(title = 'Total time detected per 24 Hour Day') + ylab('Minutes per day') + xlab('Number of days')
  p_combine <- p1#(p1 + p2) / (p3 + p4) / (p5 + p6) + plot_layout(guides='collect')
  return(p_combine)
}

get_stacked_bars <- function(df){
  locations <- c('Patient.room','MD.Workroom', 'Ward.Hall', 'OTHER.UNKNOWN','staff.admin.area', 'Transit', 'Education', 'Supply.and.admin', 'Family.waiting.space')
  p1 <- df %>% 
    tidyr::pivot_longer(cols = all_of(locations), names_to = "location", values_to = "minutes") %>%
    mutate(location = ordered(location, levels = locations)) %>%
    select(location, minutes, Interval) %>%
    group_by(Interval, location) %>% 
    summarize(
      minutes = mean(minutes, na.rm = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(fill = location, x = Interval, y = minutes)) +
    geom_bar(position = 'fill', stat = 'identity') +
    theme_tufte() +
    scale_fill_viridis(name='Location',discrete = TRUE, direction = -1, option = 'D',labels=c('Patient room','MD Workroom', 'Ward Hall', 'Other / Unknown','Staff area', 'Transit', 'Education', 'Supply and admin', 'Family waiting space')) +
    scale_x_discrete(labels=c('24 hours', 'Rounds', 'Morning', 'Afternoon', 'Evening', 'Night')) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    labs(title = 'All days') +
    ylab('Proportion of time') +
    scale_y_continuous(labels = scales::percent_format()) +
    xlab('Time of day')
  p2 <- df %>% 
    filter(DayOfWeek <= 5) %>%
    tidyr::pivot_longer(cols = all_of(locations), names_to = "location", values_to = "minutes") %>%
    mutate(location = ordered(location, levels = locations)) %>%
    select(location, minutes, Interval) %>%
    group_by(Interval, location) %>% 
    summarize(minutes = mean(minutes, na.rm = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(fill = location, x = Interval, y = minutes)) +
    geom_bar(position = 'fill', stat = 'identity') +
    theme_tufte()+
    scale_fill_viridis(name='Location',discrete = TRUE, direction = -1, option = 'D',labels=c('Patient room','MD Workroom', 'Ward Hall', 'Other / Unknown','Staff area', 'Transit', 'Education', 'Supply and admin', 'Family waiting space')) +
    scale_x_discrete(labels=c('24 hours', 'Rounds', 'Morning', 'Afternoon', 'Evening', 'Night')) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    labs(title = 'Week days') +
    ylab('Proportion of time') +
    scale_y_continuous(labels = scales::percent_format()) +
    xlab('Time of day')
  p3 <- df %>% 
    filter(DayOfWeek > 5) %>%
    tidyr::pivot_longer(cols = all_of(locations), names_to = "location", values_to = "minutes") %>%
    mutate(location = ordered(location, levels = locations)) %>%
    select(location, minutes, Interval) %>%
    group_by(Interval, location) %>% 
    summarize(minutes = mean(minutes, na.rm = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(fill = location, x = Interval, y = minutes)) +
    geom_bar(position = 'fill', stat = 'identity') +
    theme_tufte()+
    scale_fill_viridis(name='Location',discrete = TRUE, direction = -1, option = 'D',labels=c('Patient room','MD Workroom', 'Ward Hall', 'Other / Unknown','Staff area', 'Transit', 'Education', 'Supply and admin', 'Family waiting space')) +
    scale_x_discrete(labels=c('24 hours', 'Rounds', 'Morning', 'Afternoon', 'Evening', 'Night')) +
    #scale_fill_discrete(labels=c('Patient room','MD Workroom', 'Ward Hall', 'Other / Unknow','Staff area', 'Transit', 'Education', 'Supply and admin', 'Family waiting space')) +  
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    labs(title = 'Weekend days') +
    ylab('Proportion of time') +
    scale_y_continuous(labels = scales::percent_format()) +
    xlab('Time of day')
  p = p1 + p2 + p3 +
    plot_annotation(
      #title = 'Percentage of time spent in locations by time of day and days of the week',
      #subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
      caption = 'Time ranges:\nMorning: 6AM to 12PM\nRounds: 830AM to 11AM\nAfternoon: 12PM to 6PM \nEvening: 6PM to 12AM\nNight: 12AM to 6 AM',
      tag_levels = 'A'
    ) +
    plot_layout(guides='collect') & theme(legend.position = 'bottom', text = element_text('serif')) 
  return(p)
}

get_stacked_barsV2 <- function(df){
  locations <- c('Patient.room','MD.Workroom', 'Ward.Hall', 'OTHER.UNKNOWN','staff.admin.area', 'Transit', 'Education', 'Supply.and.admin', 'Family.waiting.space')
  p1 <- df %>% 
    tidyr::pivot_longer(cols = all_of(locations), names_to = "location", values_to = "minutes") %>%
    mutate(location = ordered(location, levels = locations)) %>%
    select(location, minutes, Interval) %>%
    group_by(Interval, location) %>% 
    summarize(minutes = mean(minutes, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(Interval) %>%
    mutate(minutes_perc = minutes / sum(minutes)) %>%
    ungroup() %>%
    ggplot(aes(fill = location, x = Interval, y = minutes_perc)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    theme_tufte() +
    scale_fill_viridis(name='Location',discrete = TRUE, direction = -1, option = 'D',labels=c('Patient room','MD Workroom', 'Ward Hall', 'Other / Unknown','Staff area', 'Transit', 'Education', 'Supply and admin', 'Family waiting space')) +
    scale_x_discrete(labels=c('24 hours', 'Rounds', 'Morning', 'Afternoon', 'Evening', 'Night')) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    labs(title = 'All days') +
    ylab('Proportion of time') +
    scale_y_continuous(labels = scales::percent_format()) +
    xlab('Time of day')
  p2 <- df %>% 
    filter(DayOfWeek <= 5) %>%
    tidyr::pivot_longer(cols = all_of(locations), names_to = "location", values_to = "minutes") %>%
    mutate(location = ordered(location, levels = locations)) %>%
    select(location, minutes, Interval) %>%
    group_by(Interval, location) %>% 
    summarize(minutes = mean(minutes, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(Interval) %>%
    mutate(minutes_perc = minutes / sum(minutes)) %>%
    ungroup() %>%
    ggplot(aes(fill = location, x = Interval, y = minutes_perc)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    theme_tufte()+
    scale_fill_viridis(name='Location',discrete = TRUE, direction = -1, option = 'D',labels=c('Patient room','MD Workroom', 'Ward Hall', 'Other / Unknown','Staff area', 'Transit', 'Education', 'Supply and admin', 'Family waiting space')) +
    scale_x_discrete(labels=c('24 hours', 'Rounds', 'Morning', 'Afternoon', 'Evening', 'Night')) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    labs(title = 'Week days') +
    ylab('Proportion of time') +
    scale_y_continuous(labels = scales::percent_format()) +
    xlab('Time of day')
  p3 <- df %>% 
    filter(DayOfWeek > 5) %>%
    tidyr::pivot_longer(cols = all_of(locations), names_to = "location", values_to = "minutes") %>%
    mutate(location = ordered(location, levels = locations)) %>%
    select(location, minutes, Interval) %>%
    group_by(Interval, location) %>% 
    summarize(minutes = mean(minutes, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(Interval) %>%
    mutate(minutes_perc = minutes / sum(minutes)) %>%
    ungroup() %>%
    ggplot(aes(fill = location, x = Interval, y = minutes_perc)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    theme_tufte()+
    scale_fill_viridis(name='Location',discrete = TRUE, direction = -1, option = 'D',labels=c('Patient room','MD Workroom', 'Ward Hall', 'Other / Unknown','Staff area', 'Transit', 'Education', 'Supply and admin', 'Family waiting space')) +
    scale_x_discrete(labels=c('24 hours', 'Rounds', 'Morning', 'Afternoon', 'Evening', 'Night')) +
    #scale_fill_discrete(labels=c('Patient room','MD Workroom', 'Ward Hall', 'Other / Unknow','Staff area', 'Transit', 'Education', 'Supply and admin', 'Family waiting space')) +  
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    labs(title = 'Weekend days') +
    ylab('Proportion of time') +
    scale_y_continuous(labels = scales::percent_format()) +
    xlab('Time of day')
  p = p1 + p2 + p3 +
    plot_annotation(
      #title = 'Percentage of time spent in locations by time of day and days of the week',
      #subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
      caption = 'Time ranges:\nMorning: 6AM to 12PM\nRounds: 830AM to 11AM\nAfternoon: 12PM to 6PM \nEvening: 6PM to 12AM\nNight: 12AM to 6 AM',
      tag_levels = 'A'
    ) +
    plot_layout(guides='collect') & theme(legend.position = 'bottom', text = element_text('serif')) 
  return(p)
}



get_individual_plots <- function(df) {
  
  i1 <- df %>%
    filter(Interval == 'all_24') %>%
    ggplot(aes(x = reorder(RTLS_ID, Patient.room_perc, na.rm = TRUE), y = Patient.room_perc)) +#fct_reorder(RTLS_ID, Patient.room_perc, .fun = median, .desc =TRUE), y = Patient.room_perc)) + #reorder(RTLS_ID, Patient.room_perc, FUN = min), y = Patient.room_perc)) +
    geom_boxplot() +
    labs(
      x = 'Intern', 
      y = 'Percent of time in Patient Room', 
      #title = 'Percent of time in Patient rooms over 24 hour period by Intern'
      ) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_tufte() +
    geom_hline(aes(yintercept = quantile(Patient.room_perc)[[2]]), linetype="dashed", color = 'red') +
    geom_hline(aes(yintercept = quantile(Patient.room_perc)[[4]]), linetype="dashed", color = 'red') +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  summ_data <- df %>%
    filter(Interval == 'all_24') %>%
    group_by(RTLS_ID) %>%
    summarise(Total.hours = sum(Patient.room) / 60, n = n()) %>%
    ungroup() 
  mean_25 <- summ_data %>%
    filter(Total.hours < quantile(Total.hours,.25)) %>%
    pull(Total.hours) %>%
    mean()
  mean_75 <- summ_data %>%
    filter(Total.hours < quantile(Total.hours,.75)) %>%
    pull(Total.hours) %>%
    mean()
  annotations <- data.frame(
    xpos = c(15),
    ypos =  c(900),
    annotateText = c(paste0("On average, there is a difference of\n",round((mean_75 - mean_25),1)," hours spent in Patient Rooms\nbetweeen the top and bottom \nquartiles of interns")),
    tsize = 8,
    hjustvar = c(0),
    vjustvar = c(1.5)) 
  
  i2 <- summ_data %>%  ggplot(aes(x = reorder(RTLS_ID, Total.hours, na.rm = TRUE), y = Total.hours)) +
    geom_point() +
    labs(x = 'Intern', y = 'Total hours in Patient rooms ', title = 'Total time in Patient rooms over one year\nby Intern') +
    coord_flip() +
    ylim(round(min(summ_data$Total.hours),100),plyr::round_any(max(summ_data$Total.hours),100, f = ceiling)) +
    theme_tufte() +
    geom_hline(aes(yintercept = quantile(Total.hours)[[2]]), linetype="dotted") +
    geom_hline(aes(yintercept = quantile(Total.hours)[[4]]), linetype="dotted") +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.key = element_blank()
    )
  i3 <- df %>%
    filter(Interval == 'all_24') %>%
    ggplot(aes(x = Day, y = Patient.room_perc, group =reorder(RTLS_ID, Patient.room_perc, na.rm = TRUE))) + 
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

  return(list(percent = i1, total_time = i2, over_time = i3))
}

print_key_stats <- function(df){
  print(paste('Total intern hours:',sum(df[which(df$Interval == 'all_24'),'Total'])/60))
}

get_service_plots <- function(df) {
  #creates a dataframe to add the lines for the grand mean for each location to the service plots
  bar_data <- df %>% 
    select(Patient.room_perc, Ward.Hall_perc, MD.Workroom_perc, Interval) %>% 
    filter(Interval %in% c('all_24', 'rounds')) %>%
    group_by(Interval) %>%
    summarise(across(.cols = everything(), mean)) %>%
    ungroup() %>%
    tidyr::pivot_longer(-Interval, names_to = 'location', values_to = 'mean_perc_time') %>%
    mutate(location = fct_relevel(location, "Patient.room_perc", "Ward.Hall_perc", "MD.Workroom_perc"))
  bar_data <- cbind(
    bar_data[rep(seq_len(nrow(bar_data)), 5), ], # This repeats the location and mean times
    data.frame(Service_gruoped = rep(c('Hospitalist GIM', 'House staff', 'ICU', 'Oncology', 'Subspecialty'), each = 6))
  )
  s1 <- df %>%
    filter(Interval %in% c('rounds','all_24')) %>%
    select(Interval,Service_grouped, Patient.room_perc, Ward.Hall_perc, MD.Workroom_perc) %>%
    tidyr::pivot_longer(cols = -c(Service_grouped, Interval), names_to = "location", values_to = "perc_time") %>%
    mutate(location = fct_relevel(location, "Patient.room_perc", "Ward.Hall_perc", "MD.Workroom_perc")) %>%
    tidyr::drop_na() %>%
    ggplot(aes(x = location, y = perc_time, fill = Interval)) +
    geom_boxplot() +
    lemon::facet_rep_wrap(~Service_grouped,repeat.tick.labels = TRUE) +
    theme_tufte() +
    theme(
      legend.position="bottom",
      axis.text.x = element_text(angle = 45, hjust=1)
      ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_discrete(labels = c("Patient room", "Ward hall", "MD workroom")) +
    scale_fill_discrete(labels = c("All day", "Rounds")) +
    labs(
      x = 'Location',
      y = '% time in location',
      fill = 'Interval',
      #title = 'Percent time in Patient rooms, Ward halls and MD workrooms during rounds by service'
      ) +
    geom_crossbar(data=bar_data,aes(x = location, y = mean_perc_time, ymin = mean_perc_time,
                                   ymax = mean_perc_time,fill = Interval),
                  show.legend = FALSE, position = position_dodge(), color="red")
    return(s1)
}

make_tables <- function(df) {
  locations <- c('Patient.room','MD.Workroom', 'Ward.Hall', 'OTHER.UNKNOWN','staff.admin.area', 'Transit', 'Education', 'Supply.and.admin', 'Family.waiting.space','Total')
  df_sub <- df[,append(c('Interval','DayOfWeek'),locations)]
  # Weekdays & Weekends together
  stargazer(df_sub[df_sub$Interval == 'all_24',], type = 'html',
            digits = 1,
            out = here('output','tables',"Overall_OverallTable.doc"))
  stargazer(df_sub[df_sub$Interval == 'morning',], type = 'html',
            digits = 1,
            out = here('output','tables',"Overall_morning_Table.doc"))
  stargazer(df_sub[df_sub$Interval == 'afternoon',], type = 'html',
            digits = 1,
            out = here('output','tables',"Overall_afternoon_Table.doc"))
  stargazer(df_sub[df_sub$Interval == 'evening',], type = 'html',
            digits = 1,
            out = here('output','tables',"Overall_evening_Table.doc"))
  stargazer(df_sub[df_sub$Interval == 'night',], type = 'html',
            digits = 1,
            out = here('output','tables',"Overall_night_Table.doc"))
  stargazer(df_sub[df_sub$Interval == 'rounds',], type = 'html',
            digits = 1,
            out = here('output','tables',"Overall_rounds_Table.doc"))
  
  # Weekdays only
  stargazer(df_sub[(df_sub$Interval == 'all_24') & (df_sub$DayOfWeek < 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekdays_OverallTable.doc"))
  stargazer(df_sub[(df_sub$Interval == 'morning') & (df_sub$DayOfWeek < 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekdays_morning_Table.doc"))
  stargazer(df_sub[(df_sub$Interval == 'afternoon') & (df_sub$DayOfWeek < 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekdays_afternoon_Table.doc"))
  stargazer(df_sub[(df_sub$Interval == 'evening') & (df_sub$DayOfWeek < 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekdays_evening_Table.doc"))
  stargazer(df_sub[(df_sub$Interval == 'night') & (df_sub$DayOfWeek < 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekdays_night_Table.doc"))
  stargazer(df_sub[(df_sub$Interval == 'rounds') & (df_sub$DayOfWeek < 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekdays_rounds_Table.doc"))
  
  # Weekends only
  
  stargazer(df_sub[(df_sub$Interval == 'all_24') & (df_sub$DayOfWeek >= 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekend_OverallTable.doc"))
  stargazer(df_sub[(df_sub$Interval == 'morning') & (df_sub$DayOfWeek >= 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekend_morning_Table.doc"))
  stargazer(df_sub[(df_sub$Interval == 'afternoon') & (df_sub$DayOfWeek >= 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekend_afternoon_Table.doc"))
  stargazer(df_sub[(df_sub$Interval == 'evening') & (df_sub$DayOfWeek >= 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekend_evening_Table.doc"))
  stargazer(df_sub[(df_sub$Interval == 'night') & (df_sub$DayOfWeek >= 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekend_night_Table.doc"))
  stargazer(df_sub[(df_sub$Interval == 'rounds') & (df_sub$DayOfWeek >= 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekend_rounds_Table.doc"))
  NULL
}