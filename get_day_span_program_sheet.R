get_day_spanning = function(df,
                            tv_cols,
                            contain_pre_day = T,
                            title = 'Title',
                            desc = 'Description',
                            start = 'Program Start time',
                            dur = 'Duration',
                            end = 'Program End Time',
                            date = 'Date',
                            weekday = 'Weekday',
                            channel = 'Channel') {
  mapping = read.csv("D:/wangxiaoyang/Regular_Work/support_files/channel_mapping.csv")
  df = df %>%
    mutate( Title = !!sym(title),
            Description = !!sym(desc),
            Channel = !!sym(channel),
            Date = as.Date( !!sym(date) ),
            Weekday = !!sym(weekday),
            Start = !!sym(start),
            Dur = !!sym(dur),
            End = !!sym(end)
    ) 

  df_normal = df %>% 
    filter(
      End != "26:00:00",
      Start != "2:00:00"
    ) %>% 
    select(
      Regions,
      Title, 
      Description,
      Channel,
      Date,
      Weekday,
      Start,
      End,
      Dur,
      all_of(tv_cols)
    )
  
  if (contain_pre_day) {
    df_normal = df_normal %>% 
      filter(
        Date != min(Date)
      )
  }
  
  aa = df %>% 
    filter( End == "26:00:00" |
              Start == "2:00:00") 
  
  if (dim(aa)[1] <= 1) {
    return(list(df %>% 
                  select(
                    Regions,
                    Title, 
                    Description,
                    Channel,
                    Date,
                    Weekday,
                    Start,
                    End,
                    Dur,
                    all_of(tv_cols)
                  ))
    )
  }
  
  aa = aa %>% 
    arrange(Channel, Date, hms(Start)) %>%
    mutate(
      indicator = case_when(
        Start == "2:00:00" ~ 2,
        End == "26:00:00" ~ 1,
        .default = 0
      ),
      Date_lead = lead(Date, 1),
      Date_next = Date + 1,
      Title_lead = lead(Title, 1),
      day_span = if_else( is.na(Date_lead),
                          FALSE,
                          (Title == Title_lead & Date_lead == Date_next)
      )
    )
  
  print(aa)

  rows_to_check = c()
  name_list = c()
  for (i in 1:dim(aa)[1]) {
    if (aa$day_span[i]) {
      
      # aa$End[i] = aa$End[i+1]
      # 
      # seconds1 = as.numeric( strptime(aa$Dur[i], format="%H:%M:%S") ) -
      #   as.numeric( strptime("00:00:00", format="%H:%M:%S") )
      # seconds2 = as.numeric( strptime(aa$Dur[i+1], format="%H:%M:%S") ) - 
      #   as.numeric( strptime("00:00:00", format="%H:%M:%S") )
      # total_seconds = seconds1 + seconds2
      # aa$Dur[i] = format(as.POSIXct(total_seconds, 
      #                                     origin="1970-01-01", tz="UTC"),
      #                          "%H:%M:%S")
      rows_to_check = c(rows_to_check, c(i, i+1))
      name_list = c(name_list, rep(paste(aa$Date[i], 
                                         # aa$Title[i], 
                                         aa$Channel[i]), 2))
    }
  }
  
  res_df = aa[rows_to_check, ] %>% 
    left_join(mapping, 
              join_by(Channel == channel))
  
  res = with(res_df %>% 
               mutate(
                 End = if_else(
                   End == "26:00:00",
                   "25:59:59",
                   End
                 )
               ),
             paste(format(as.Date(Date, format="%Y/%m/%d"), "%Y%m%d"),
                   gsub(" ", 0, sprintf("%04s", code)),
                   gsub(" ", 0, gsub(":", "", sprintf("%08s", Start))),
                   gsub(" ", 0, gsub(":", "", sprintf("%08s", End))),
                   name_list
             ) )
  
  df_ds = res_df %>% 
    select(
      Regions,
      Title,  
      Description,
      Channel,
      Date,
      Weekday,
      Start,
      End,
      Dur,
      all_of(tv_cols)
    )
  print(df_ds)
  for (i in 1:dim(df_ds)[1]) {
    if (i %% 2 == 1) {
      df_ds$End[i] = df_ds$End[i+1]
      
      seconds1 = as.numeric( strptime(df_ds$Dur[i], format="%H:%M:%S") ) -
        as.numeric( strptime("00:00:00", format="%H:%M:%S") )
      seconds2 = as.numeric( strptime(df_ds$Dur[i+1], format="%H:%M:%S") ) - 
        as.numeric( strptime("00:00:00", format="%H:%M:%S") )
      total_seconds = seconds1 + seconds2
      df_ds$Dur[i] = format(as.POSIXct(total_seconds, 
                                       origin="1970-01-01", tz="UTC"),
                            "%H:%M:%S")
    }
  }
  print(rows_to_check)
  data_normal = aa[-rows_to_check, ] %>% 
    filter(
      !(Date == min(Date) & Start == '2:00:00')
    ) %>% 
    select(
      Regions,
      Title,  
      Description,
      Channel,
      Date,
      Weekday,
      Start,
      End,
      Dur,
      all_of(tv_cols)
    )
  
  if (contain_pre_day) {
    data_normal = data_normal %>% 
      filter(
        Date != min(Date)
      )
  }
  
  bb <<- data_normal
  # print(data_normal)
  return(
    list(
      res_txt = res,
      data_normal = rbind(df_normal, data_normal),
      data_ds = df_ds[seq(1, dim(df_ds)[1], 2),]
    )
  )
}





