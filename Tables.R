library(stargazer)

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