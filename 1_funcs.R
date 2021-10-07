#***********************************************************************
#***********************************************************************
#************  Scripts for RTSL project
#************
#***********************************************************************
#***********************************************************************
do_cleaning <- function(df) {
  df$Day <- as.Date(df$Day, "%Y-%m-%d")
  df$Month <- month(df$Day)
  # formatting vars
  fac_vars <- c('Service','Interval','RTLS_ID','Month','Service_grouped')
  for (var in fac_vars) {
    df[[var]] <- as.factor(df[[var]])
  }
  df$Interval <- ordered(df$Interval, levels = c('all_24', 'morning', 'afternoon', 'evening', 'night'))
  df$Month <- ordered(df$Month, levels = c(7,8,9,10,11,12,1,2,3,4,5,6))
  to_min_vars <- c('MD.Workroom', 'Supply.and.admin','Patient.room', 'Education', 'staff.admin.area','OTHER.UNKNOWN', 'Family.waiting.space','Transit', 'Ward.Hall', 'Total')
  for (var in to_min_vars) {
    #df[is.na(df[[var]]),df[[var]]] <- 0 # replaces NA's, so this doesn't kill R session
    df[[var]] <- df[[var]] / 60
  }
  to_per_vars <-  c('MD.Workroom_perc', 'Supply.and.admin_perc','Patient.room_perc', 'Education_perc', 'staff.admin.area_perc','OTHER.UNKNOWN_perc', 'Family.waiting.space_perc','Transit_perc', 'Ward.Hall_perc')
  for (var in to_per_vars) {
    df[[var]] <- df[[var]] * 100
  }
  # exclusions
  df <- df[df$Total >= 240,] # takes all days with at least 4 hours of data
  df <- df[!(df$Service %in% c("ED")),]
  shorties <- c(308419,308762,308775,308779,308784,308779,308784,308785,308786,308795,308796,308797)
  df <- df[!df$RTLS_ID %in% shorties,]

  return(df)
}

#***********************************************************************
#************  Plotting
#***********************************************************************

get_histograms <- function(df) {
  p1 <- df %>%
    filter(Interval == "all_24") %>%
    ggplot(aes(x = Total)) + 
    geom_histogram(bins = 200) +
    theme_tufte() + labs(title = 'Total time detected per 24 Hour Day') + ylab('Minutes per day') + xlab('Number of days')
  # p2 <- df %>%
  #   filter(Interval == "morning") %>%
  #   ggplot(aes(x = Total)) + 
  #   geom_histogram(bins = 200) +
  #   theme_tufte()+ labs(title = 'Moring: 6 to 12')
  # p3 <- df %>%
  #   filter(Interval == "afternoon") %>%
  #   ggplot(aes(x = Total)) + 
  #   geom_histogram(bins = 200) +
  #   theme_tufte() + labs(title = 'Afternoon: 12 to 6')
  # p4 <- df %>%
  #   filter(Interval == "evening") %>%
  #   ggplot(aes(x = Total)) + 
  #   geom_histogram(bins = 200) +
  #   theme_tufte()+ labs(title = 'Evening: 6 to 12')
  # p5 <- df %>%
  #   filter(Interval == "night") %>%
  #   ggplot(aes(x = Total)) + 
  #   geom_histogram(bins = 200) +
  #   theme_tufte() + labs(title = 'Night: 12 to 6')
  # p6 <- df %>%
  #   filter(Interval == "rounds") %>%
  #   ggplot(aes(x = Total)) + 
  #   geom_histogram(bins = 200) +
  #   theme_tufte()+ labs(title = 'Rounds: ? to ?')
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
    theme_tufte()+
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
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    labs(title = 'Weekend days') +
    ylab('Proportion of time') +
    scale_y_continuous(labels = scales::percent_format()) +
    xlab('Time of day')
  p = p1 + p2 + p3 + plot_layout(guides='collect') & theme(legend.position = 'bottom')
  return(p)
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
  NULL
}