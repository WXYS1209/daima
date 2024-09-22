convert_tz = function(date_str, time_str, hms = TRUE) {
  temp = as.integer(gsub(":", "", time_str))
  if (hms) {
    ss = temp %% 100
    temp = temp %/% 100
    mm = temp %% 100
    temp = temp %/% 100
    extra_days = temp %/% 24
    hh = temp %% 24
    
    datetime_str = paste(date_str, sprintf("%02d:%02d", hh, mm), sep = " ")
    
    input_datetime <- ymd_hm(datetime_str, tz = 'Asia/Shanghai')
  } else {
    mm = temp %% 100
    temp = temp %/% 100
    extra_days = temp %/% 24
    hh = temp %% 24
    
    datetime_str = paste(date_str, sprintf("%02d:%02d", hh, mm), sep = " ")
    
    input_datetime <- ymd_hm(datetime_str, tz = 'Asia/Shanghai')
  }
  
  input_datetime <- input_datetime + ddays(extra_days)
  return(input_datetime)
}

convert_time <- function(x, excel = T) {
  if (excel) {
    total_seconds <- round(as.numeric(x) * 24 * 3600, 0)
  } else {
    total_seconds = as.numeric(x)
  }
  hours <- floor(total_seconds / 3600)
  total_seconds = total_seconds - 3600*hours
  minutes <- floor(total_seconds / 60)
  total_seconds = total_seconds - 60*minutes
  seconds = round(total_seconds, 0)
  
  # for (i in 1:length(seconds)) {
  #   if (seconds[i] >= 60) {
  #     minutes[i] = minutes[i] + 1
  #   }
  # }
  
  x <- sprintf("%02d:%02d:%02d", hours, minutes, seconds)
  return(as.character(x))
}

convert_to_excel_time <- function(time_str) {
  time_parts <- as.numeric(unlist(strsplit(time_str, ":")))
  total_seconds <- time_parts[1] * 3600 + time_parts[2] * 60 + ifelse(length(time_parts) == 3, time_parts[3], 0)
  return(total_seconds / seconds_in_a_day)
}


replace_chn_num = function(text) {
  chn_num = c("一", "二", "三", "四", "五", "六", "七", "八", "九", "十")
  nums = c(1,2,3,4,5,6,7,8,9,10)
  for (i in 1:10) {
    text = gsub(chn_num[i], nums[i], text)
  }
  return (text)
}

nearest_hour_or_halfhour <- function(time_str) {
  time <- unlist(str_split(time_str, ":"))
  
  hour <- as.numeric(time[1])
  minute <- as.numeric(time[2])
  
  if (minute < 15) {
    nearest_time <- paste0(sprintf("%02d", hour), ":00")
  } else if (minute < 45) {
    nearest_time <- paste0(sprintf("%02d", hour), ":30")
  } else {
    nearest_time <- paste0(sprintf("%02d", (hour + 1)), ":00")
  }
  
  return(nearest_time)
}

get_seconds = function(time_str, durr = T) {
  time = hms(time_str)
  ss = hour(time)*60*60 + minute(time)*60 + second(time)
  
  if (!durr) {
    ss[ss < 2*3600] = ss[ss < 2*3600] + 24*3600
  }
  
  return( ss )
}
