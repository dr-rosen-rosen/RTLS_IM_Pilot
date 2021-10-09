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
  df <- df[!(df$Service %in% c("ED")),]
  fac_vars <- c('Service','Interval','RTLS_ID','Month','Service_grouped')
  for (var in fac_vars) {
    df[[var]] <- as.factor(df[[var]])
  }
  df$Interval <- ordered(df$Interval, levels = c('all_24', 'morning', 'afternoon', 'evening', 'night', 'rounds'))
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
  # df <- df[df$Total >= 240,] # takes all days with at least 4 hours of data
  shorties <- c(308419,308762,308775,308779,308784,308779,308784,308785,308786,308795,308796,308797)
  df <- df[!df$RTLS_ID %in% shorties,]
  big_intervals <- c('all_24', 'morning', 'afternoon', 'evening', 'night')
  df <- df %>%
    # filter( 
    #   (Interval %in% big_intervals & Total >= 240) | (Interval == 'rounds' & Total >= 120)
    #   ) %>% 
    mutate(
      across(to_replace_na_vars, ~tidyr::replace_na(.x, 0))
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
    summarize(minutes = mean(minutes, na.rm = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(fill = location, x = Interval, y = minutes)) +
    geom_bar(position = 'fill', stat = 'identity') +
    theme_tufte() +
    scale_fill_viridis(discrete = TRUE, direction = -1, option = 'D') +
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
    scale_fill_viridis(discrete = TRUE, direction = -1, option = 'D') +
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
    scale_fill_viridis(discrete = TRUE, direction = -1, option = 'D') +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    labs(title = 'Weekend days') +
    ylab('Proportion of time') +
    scale_y_continuous(labels = scales::percent_format()) +
    xlab('Time of day')
  p = p1 + p2 + p3 +
    plot_annotation(
      title = 'Percentage of time spent in locations by time of day and days of the week',
      #subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
      caption = 'Time ranges:\nMoring: 6AM to 12PM\nAfternoon: 12PM to 6PM \nEvening: 6PM to 12AM\nNight: 12AM to 6 AM\nRounds: 830AM to 11AM',
      tag_levels = 'A'
    ) +
    plot_layout(guides='collect') & theme(legend.position = 'bottom', text = element_text('serif')) 
  return(p)
}

get_individual_plots<- function(df) {
  
  i1 <- df %>%
    filter(Interval == 'all_24') %>%
    ggplot(aes(x = reorder(RTLS_ID, Patient.room_perc, na.rm = TRUE), y = Patient.room_perc)) +#fct_reorder(RTLS_ID, Patient.room_perc, .fun = median, .desc =TRUE), y = Patient.room_perc)) + #reorder(RTLS_ID, Patient.room_perc, FUN = min), y = Patient.room_perc)) +
    geom_boxplot() +
    labs(x = 'Intern', y = 'Proportion of time spent in Patient Room', title = 'Percent time spent \nat bedside by Intern') +
    coord_flip() +
    theme_tufte() +
    theme(#axis.title.x=element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  
  i2 <- df %>%
    filter(Interval == 'all_24') %>%
    ggplot(aes(x = Day, y = Patient.room_perc, group =reorder(RTLS_ID, Patient.room_perc, na.rm = TRUE))) + #Patient.room_perc
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
  
  i_combined <- (i1 + i2)
  return(i_combined)
}

get_service_plots <- function(df) {
  s1 <- df %>%
    filter(Interval == 'all_24') %>%
    ggplot(aes(x = Service_grouped, y = Patient.room_perc)) +
    geom_boxplot() +
    labs(x = 'Service', y = 'Percent of time spent in Patient Room', title = 'Percent of time spent in patient room across services') +
    #coord_flip() +
    geom_boxplot(data = transform(df[which(df$Interval == 'all_24'),], Service_grouped = 'all')) +
    theme_tufte() +
    scale_y_continuous(labels = scales::percent_format())
  
  s2 <- df %>%
    filter(Interval == 'all_24') %>%
    ggplot(aes(x = Day, y = Patient.room_perc, group = Service_grouped)) + 
    geom_line(show.legend = FALSE) + 
    facet_wrap(~ Service_grouped) +
    labs(x = 'Day', y = 'Percent of time spent in Patient Room', title = 'Changes in the percent of time spent at the bedside over the year, by service') +
    geom_line(data = transform(df[which(df$Interval == 'all_24'),], Service_grouped = 'all')) +
    geom_smooth(method="lm") +
    theme_tufte() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    scale_y_continuous(labels = scales::percent_format())
  s_combined <- (s1 / s2)
  return(s_combined)
}

make_tables <- function(df) {
  # Weekdays & Weekends together
  stargazer(df[df$Interval == 'all_24',], type = 'html',
            digits = 1,
            out = here('output','tables',"Overall_OverallTable.doc"))
  stargazer(df[df$Interval == 'morning',], type = 'html',
            digits = 1,
            out = here('output','tables',"Overall_morning_Table.doc"))
  stargazer(df[df$Interval == 'afternoon',], type = 'html',
            digits = 1,
            out = here('output','tables',"Overall_afternoon_Table.doc"))
  stargazer(df[df$Interval == 'evening',], type = 'html',
            digits = 1,
            out = here('output','tables',"Overall_evening_Table.doc"))
  stargazer(df[df$Interval == 'night',], type = 'html',
            digits = 1,
            out = here('output','tables',"Overall_night_Table.doc"))
  stargazer(df[df$Interval == 'rounds',], type = 'html',
            digits = 1,
            out = here('output','tables',"Overall_rounds_Table.doc"))
  
  # Weekdays only
  stargazer(df[(df$Interval == 'all_24') & (df$DayOfWeek < 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekdays_OverallTable.doc"))
  stargazer(df[(df$Interval == 'morning') & (df$DayOfWeek < 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekdays_morning_Table.doc"))
  stargazer(df[(df$Interval == 'afternoon') & (df$DayOfWeek < 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekdays_afternoon_Table.doc"))
  stargazer(df[(df$Interval == 'evening') & (df$DayOfWeek < 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekdays_evening_Table.doc"))
  stargazer(df[(df$Interval == 'night') & (df$DayOfWeek < 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekdays_night_Table.doc"))
  stargazer(df[(df$Interval == 'rounds') & (df$DayOfWeek < 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekdays_rounds_Table.doc"))
  
  # Weekends only
  
  stargazer(df[(df$Interval == 'all_24') & (df$DayOfWeek >= 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekend_OverallTable.doc"))
  stargazer(df[(df$Interval == 'morning') & (df$DayOfWeek >= 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekend_morning_Table.doc"))
  stargazer(df[(df$Interval == 'afternoon') & (df$DayOfWeek >= 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekend_afternoon_Table.doc"))
  stargazer(df[(df$Interval == 'evening') & (df$DayOfWeek >= 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekend_evening_Table.doc"))
  stargazer(df[(df$Interval == 'night') & (df$DayOfWeek >= 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekend_night_Table.doc"))
  stargazer(df[(df$Interval == 'rounds') & (df$DayOfWeek >= 5),], type = 'html',
            digits = 1,
            out = here('output','tables',"Weekend_rounds_Table.doc"))
  NULL
}