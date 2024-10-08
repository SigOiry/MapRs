
# This is a function made to create experiment profiles of the test and the control for ElectricBlue Intertidal chambers

# https://electricblue.eu/intertidal-chamber

# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

require(tidyverse)


Make_Chamber_Profile <- function(max_Air_Temp_control, 
                         min_Air_Temp_control, 
                         max_Air_Temp_test, 
                         min_Air_Temp_test, 
                         max_Water_Temp_control, 
                         min_Water_Temp_control, 
                         max_Water_Temp_test, 
                         min_Water_Temp_test, 
                         daily_increase_Air_Test =0,
                         daily_increase_Water_Test =0,
                         daily_increase_Air_Control =0,
                         daily_increase_Water_Control =0,
                         date_start, 
                         date_end, 
                         low_tide_time,
                         High_Tide_Until = NA,
                         sunrise,
                         sunset,
                         max_light_intensity,
                         export_profile = T,
                         Tide = "Both",
                         night_tide = T,
                         time_step = 6, 
                         Save_Experiment = T, 
                         Experiment_Name = "ThisIsATest"){
  
  
  if(missing(max_Air_Temp_control)) {
    print("max_Air_Temp_control haven't been set !") 
    stop()
  }
  
  if(missing(min_Air_Temp_control)) {
    print("min_Air_Temp_control haven't been set !") 
    stop()
  }
  
  if(missing(max_Air_Temp_test)) {
    print("max_Air_Temp_test haven't been set !") 
    stop()
  }
  
  if(missing(min_Air_Temp_test)) {
    print("min_Air_Temp_test haven't been set !") 
    stop()
  }
  
  if(missing(max_Water_Temp_control)) {
    print("max_Water_Temp_control haven't been set !") 
    stop()
  }
  
  if(missing(min_Water_Temp_control)) {
    print("min_Water_Temp_control haven't been set !") 
    stop()
  }
  
  if(missing(max_Water_Temp_test)) {
    print("max_Water_Temp_test haven't been set !") 
    stop()
  }
  
  if(missing(min_Water_Temp_test)) {
    print("min_Water_Temp_test haven't been set !") 
    stop()
  }
  
  if(missing(date_start)) {
    print("date_start haven't been set !") 
    stop()
  }
  
  if(missing(date_end)) {
    print("date_start haven't been set !") 
    stop()
  }
  
  if(missing(sunrise)) {
    print("sunrise haven't been set !") 
    stop()
  }
  
  if(missing(sunset)) {
    print("sunset haven't been set !") 
    stop()
  }
  
  if(missing(max_light_intensity)) {
    print("max_light_intensity haven't been set !") 
    stop()
  }
  
  calculate_tide <- function(initial_tide_time, start_time, end_time, tide_duration_hours = 6) {
    
    initial_tide_time <- as.POSIXct(initial_tide_time, format = "%Y-%m-%d %H:%M:%S")
    start_time <- as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S")
    end_time <- as.POSIXct(end_time, format = "%Y-%m-%d %H:%M:%S")
    
    tide_times <- data.frame(
      Start_Time = as.POSIXct(character()),
      Mid_Time = as.POSIXct(character()),
      End_Time = as.POSIXct(character()),
      Type = character(),
      stringsAsFactors = FALSE
    )
    
    time <- initial_tide_time
    is_low_tide <- TRUE
    while (time >= start_time) {
      start_time_tide <- time - (tide_duration_hours / 2) * 3600
      end_time_tide <- time + (tide_duration_hours / 2) * 3600
      tide_times <- rbind(tide_times, data.frame(
        Start_Time = start_time_tide,
        Mid_Time = time,
        End_Time = end_time_tide,
        Type = ifelse(is_low_tide, "Low_Tide", "High_Tide")
      ))
      time <- time - tide_duration_hours * 3600 # 12.42 heures en secondes
      is_low_tide <- !is_low_tide
    }
    
    time <- initial_tide_time + tide_duration_hours * 3600
    is_low_tide <- FALSE
    while (time <= end_time) {
      start_time_tide <- time - (tide_duration_hours / 2) * 3600
      end_time_tide <- time + (tide_duration_hours / 2) * 3600
      tide_times <- rbind(tide_times, data.frame(
        Start_Time = start_time_tide,
        Mid_Time = time,
        End_Time = end_time_tide,
        Type = ifelse(is_low_tide, "Low_Tide", "High_Tide")
      ))
      time <- time + tide_duration_hours * 3600
      is_low_tide <- !is_low_tide
    }
    
    tide_times <- tide_times[order(tide_times$Start_Time), ]
    
    return(tide_times)
  }
  
  decimal_to_hhmm <- function(decimal_hour) {
    # Extract the integer part of the hour
    hours <- floor(decimal_hour)
    # Calculate the minutes
    minutes <- round((decimal_hour - hours) * 60)
    # Format hours and minutes to "HH:MM"
    hhmm <- sprintf("%02d:%02d", hours, minutes)
    return(hhmm)
  }
  
  generate_temperature_df <- function(start_date, end_date, time_step, max_T, min_T, daily_increase) {
    # Create a sequence of date-time values from start to end date
    time_seq <- seq(ymd_hms(start_date), ymd_hms(end_date), by = paste(time_step, "mins"))
    
    # Calculate the time in hours from the start date
    time_hours <- as.numeric(difftime(time_seq, ymd_hms(start_date), units = "hours"))
    
    # Calculate the day factor (fraction of days since start date)
    day_factor <- time_hours / 24
    
    # Create the temperature values using a sinusoidal function with gradual daily increase
    id <- (time_hours %% 24) / 24 * 2 * pi  # Normalized time within each day
    
    # Calculate the temperature with a gradual increase throughout each day
    Temp <- (((max_T - min_T) / 2) * sin(id + 4)) + ((max_T + min_T) / 2) + (daily_increase * day_factor)
    # Temp <- (((max_Air - min_Air) / 2) * sin(id + pi + pi / 12)) + ((max_Air + min_Air) / 2) + (daily_increase * day_factor)
    
    
    # Create the data frame
    df_temp <- data.frame(
      Time = time_seq,
      Time_num = time_hours,
      Temp = Temp,
      Hour = sapply(time_hours, decimal_to_hhmm)
    )
    
    return(df_temp)
  }
  
  
  suppressWarnings({
    max_Air_Temp <- c(max_Air_Temp_control,max_Air_Temp_test)
    min_Air_Temp <- c(min_Air_Temp_control,min_Air_Temp_test)
    daily_Air_increase <- c(daily_increase_Air_Control,daily_increase_Air_Test)
    
    
    max_Water_Temp <- c(max_Water_Temp_control,max_Water_Temp_test)
    min_Water_Temp <- c(min_Water_Temp_control,min_Water_Temp_test)
    daily_Water_increase <- c(daily_increase_Water_Control,daily_increase_Water_Test)
    
    Date_Start <- date_start%>% 
      as.POSIXct(format = "%Y-%m-%d %H:%M", tz = "UTC")
    Date_end <- date_end %>% 
      as.POSIXct(format = "%Y-%m-%d %H:%M", tz = "UTC")
    
    low_tide_Time <- low_tide_time %>%
      as.POSIXct(format = "%Y-%m-%d %H:%M",tz = "UTC")
    
    Intensity_Max = max_light_intensity
    
    
    
    for (i in 1:2) {
      
      
      max_Air = max_Air_Temp[i]
      min_Air = min_Air_Temp[i]
      daily_increase = daily_Air_increase[i]
      
      ##### Air Temperature
      # df_Air_temp <- data.frame(Time_num = seq(0,24, by = time_step / 60)) %>%
      #   mutate(id = (Time_num-min(Time_num))/(max(Time_num)-min(Time_num))*2*pi,
      #   Temp = (((max_Air-min_Air)/2)*sin( id+4))+(max_Air+min_Air)/2,
      #   Hour = decimal_to_hhmm(Time_num))
      
      df_Air_temp <- generate_temperature_df(date_start, date_end, time_step,max_Air,min_Air,daily_increase) 
      
      
      #### Water Temperature
      
      max_Water = max_Water_Temp[i]
      min_Water = min_Water_Temp[i]
      daily_increase = daily_Water_increase[i]
      
      ##### Air Temperature
      # df_Air_temp <- data.frame(Time_num = seq(0,24, by = time_step / 60)) %>%
      #   mutate(id = (Time_num-min(Time_num))/(max(Time_num)-min(Time_num))*2*pi,
      #   Temp = (((max_Air-min_Air)/2)*sin( id+4))+(max_Air+min_Air)/2,
      #   Hour = decimal_to_hhmm(Time_num))
      
      df_Water_temp <- generate_temperature_df(date_start, date_end, time_step,max_Water,min_Water,daily_increase) 
      
      
      
      #### Light Intensity 
      
      Duration_of_the_Day = sunset - sunrise
      
      df_light <- data.frame(Time_num = seq(0,23.99, by = time_step / 60)) %>% 
        # mutate(id = (Hour-min(Hour))/(max(Hour)-min(Hour))*pi,
        mutate(light_intensity = Intensity_Max*sin((pi/Duration_of_the_Day)*(Time_num-sunrise)),
               light_intensity = case_when(light_intensity<0 ~ 0,
                                           light_intensity>100 ~ 100,
                                           TRUE ~ light_intensity),
               Hour = decimal_to_hhmm(Time_num))      ### Tide 
      
      tide_table <- calculate_tide(low_tide_Time,Date_Start,Date_end)
      
      tide_table$End_Time[nrow(tide_table)] <- tide_table$End_Time[nrow(tide_table)]+(60*60)*2
      tide_table$Start_Time[1] <- tide_table$Start_Time[1]-(60*60)*2
      
      
      
      ### Profile 
      df_profile <- data.frame(Time = Date_Start)
      while(df_profile[nrow(df_profile),1] != Date_end){
        df_profile[nrow(df_profile)+1,1] <- df_profile[nrow(df_profile),1] + (time_step*60)
      }
      
      
      df_profile <- df_profile %>% 
        mutate(Hour_light = substr(as.character(Time), 12, 16),
               Hour_light = case_when(Hour_light == "" ~ "00:00",
                                      TRUE ~ Hour_light)) %>%
        left_join(df_Air_temp, by = "Time") %>% 
        dplyr::select(-c(Hour,Time_num)) %>% 
        rename(Temp_Air = Temp) %>% 
        left_join(df_Water_temp, by = "Time") %>% 
        dplyr::select(-c(Hour,Time_num)) %>% 
        rename(Temp_Water = Temp) %>% 
        left_join(df_light, by = c("Hour_light" = "Hour")) %>% 
        dplyr::select(-c(Time_num))
      
      
      data <- df_profile %>%
        rowwise() %>%
        mutate(Tide_Status = {
          tide_row <- tide_table %>%
            dplyr::filter(Time >= Start_Time & Time <= End_Time)
          if (nrow(tide_row) > 0) tide_row$Type[1] else NA
        }) %>%
        ungroup() %>% 
        mutate(Tide_Status = case_when(Tide == "Low" | Tide == "low" ~ "Low_Tide",
                                       Tide == "High" | Tide == "high" ~ "High_Tide",
                                       TRUE ~ Tide_Status),
               Temp = case_when(Tide_Status == "High_Tide" ~ Temp_Water,
                                TRUE ~ Temp_Air))
      
      for (iii in 1:(nrow(data)-1)) {
        
        if (data$Tide_Status[iii]!=data$Tide_Status[iii+1]) {
          a <- data[1,] %>% 
            mutate(Time = data$Time[iii]+60,
                   Hour_light = data$Hour_light[iii],
                   Temp_Air = data$Temp_Air[iii],
                   Temp_Water = data$Temp_Water[iii],
                   light_intensity = data$light_intensity[iii],
                   Tide_Status = if(data$Tide_Status[iii] == "High_Tide") "Low_Tide" else "High_Tide",
                   Temp = if(data$Tide_Status[iii] == "High_Tide") data$Temp_Air[iii] else data$Temp_Water[iii]
            )
          data <- data %>% 
            bind_rows(a)
        }
      }
      
      data <- data %>% 
        arrange(Time)
      
      for (ii in 1:nrow(data)) {
        
        if (is.na(data$Tide_Status[ii])) {
          
          status <- data$Tide_Status[ii+1:nrow(data)]
          status <- status[!is.na(status)][1]
          
          data$Tide_Status[ii] <- status
        }
        
      }
      data$tide_ID=NA
      for (ii in 1:nrow(data)) {
        if (ii == 1) {
          count = 1
          data$tide_ID[ii] = count 
        }else{
          if ((data$Tide_Status[ii] != data$Tide_Status[ii-1])) {
            count = count+ 1
          }
          data$tide_ID[ii] = count
        }
        
      }
      
      if (night_tide == F) {
        
        data <- data %>% 
          mutate(Tide_Status = case_when(Hour_num <= sunrise+1 ~ "High_Tide",
                                         Hour_num >= sunset-1 ~ "High_Tide",
                                         TRUE ~ Tide_Status),
                 Temp = case_when(Hour_num <= sunrise+1 ~ Temp_Water,
                                  Hour_num >= sunset-1 ~ Temp_Water,
                                  TRUE ~ Temp))
        
      }
      
      if(!is.na(High_Tide_Until)){
        low_tide_at <- as.POSIXct(High_Tide_Until, format = "%Y-%m-%d %H:%M", tz = "UTC")
        
        Temp1 <- data %>% 
          dplyr::filter(Time == low_tide_at)
        
        temp_diff <- Temp1$Temp_Water - min_Water
        
        data <- data %>% 
          ungroup() %>% 
          mutate(Tide_Status = case_when(Time <= low_tide_at ~ "High_Tide",
                                         TRUE ~ Tide_Status),
                 Temp_Water = Temp_Water - temp_diff,
                 Temp = case_when(Tide_Status == "High_Tide" ~ Temp_Water,
                                  TRUE ~ Temp_Air),
                 Temp = case_when(Time <= low_tide_at ~ min_Water,
                                  TRUE ~ Temp))
        
      }
      
      
      if(daily_increase!=0){
        first_low_tide_max_Temps <- data %>% 
          dplyr::filter(Tide_Status == "Low_Tide") %>% 
          slice(1:7) %>% 
          pull(Temp_Air) %>% 
          max()
        
        Air_Temp_Diff <- first_low_tide_max_Temps-max_Air
        
        data <- data %>% 
          mutate(Temp_Air = Temp_Air - Air_Temp_Diff) %>% 
          mutate(Temp = case_when(Tide_Status == "Low_Tide" ~ Temp_Air,
                                  TRUE ~ Temp))
        
      }
      
      Profil <- data %>% 
        mutate(UNIX = as.numeric(Time) %>% as.character(),
               T_prof = as.character(round(Temp,1)*10), 
               bin_tide = case_when(Tide_Status == "High_Tide" ~ 1, 
                                    TRUE ~ 0),
               light_prof = case_when(nchar(as.character(round(light_intensity))) == 3 ~ as.character(round(light_intensity)),
                                      nchar(as.character(round(light_intensity))) == 1 ~ paste0("00",as.character(round(light_intensity))),
                                      TRUE ~ paste0("0",as.character(round(light_intensity)))),
               prof = paste0(UNIX,"-",T_prof,bin_tide,light_prof,"0")) %>% 
        pull(prof)
      
      
      if(i == 1){
        Control_df = data %>% 
          mutate(Profile = Profil)
        Control_Profil = Profil
      }else{
        Test_df = data %>% 
          mutate(Profile = Profil)
        Test_Profil = Profil
      }
    } ### End For loop
  }) ### SuppressWarings
  
  
  polygon_table <-   Test_df %>% 
    group_by(tide_ID) %>% 
    reframe(Tide_Status = unique(Tide_Status), 
            xmin = min(Time),
            xmax = max(Time),
            T_min = min(Temp_Air,Temp_Water),
            T_max = max(Temp_Air,Temp_Water)) %>% 
    ungroup() %>% 
    mutate( ymin = min(T_min),
            ymax = max(T_max)) %>% 
    rename(Tide = Tide_Status) %>% 
    mutate(Tide = case_when(Tide == "High_Tide" ~ "High",
                            TRUE ~ "Low"))
  
  plot_test <- ggplot()+
    geom_rect(data = polygon_table , aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax, fill = Tide, group = tide_ID),alpha = 0.1) +
    geom_line(data = Test_df,aes(x = Time, y = Temp))+
    ylab("Expermental Temperature (°C)")+
    theme_Bede()+
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.y = element_text(size = 18),
          axis.title.x = element_text(size = 18))
  
  plot_control <- ggplot()+
    geom_rect(data = polygon_table , aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax, fill = Tide, group = tide_ID),alpha = 0.1) +
    geom_line(data = Control_df,aes(x = Time, y = Temp))+
    ylab("Expermental Temperature (°C)")+
    theme_Bede()+
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.y = element_text(size = 18),
          axis.title.x = element_text(size = 18))
  
  if(export_profile == T){
    
    write_delim(as.data.frame(Test_Profil), "Output/Chamber_Profile/Test/Profile.txt", delim = " ")
    write_delim(as.data.frame(Control_Profil), "Output/Chamber_Profile/Control/Profile.txt", delim = " ")
    
  }
  
  if(Save_Experiment == T){
    
    dir.create(paste0("Output/Experiments/", Experiment_Name))
    dir.create(paste0("Output/Experiments/", Experiment_Name, "/Plots"))
    dir.create(paste0("Output/Experiments/", Experiment_Name, "/Profils"))
    dir.create(paste0("Output/Experiments/", Experiment_Name, "/CSV"))
    
    write_delim(as.data.frame(Test_Profil), paste0("Output/Experiments/", Experiment_Name, "/Profils/","Test_Profile.txt"), delim = " ")
    write_delim(as.data.frame(Control_Profil), paste0("Output/Experiments/", Experiment_Name, "/Profils/","Control_Profile.txt"), delim = " ")
    
    write.csv(Control_df, paste0("Output/Experiments/", Experiment_Name, "/CSV/","Control_dataframe.csv"),row.names = F)
    write.csv(Test_df, paste0("Output/Experiments/", Experiment_Name, "/CSV/","Test_dataframe.csv"),row.names = F)
    
    ggsave(paste0("Output/Experiments/", Experiment_Name, "/Plots/","Control_plot.jpg"),plot_control, width = 660*4, height = 457*4,unit = "px")
    ggsave(paste0("Output/Experiments/", Experiment_Name, "/Plots/","Test_plot.jpg"),plot_test, width = 660*4, height = 457*4,unit = "px")
    
  }
  
  
  return(list(Control_df = Control_df,Test_df = Test_df, Control_Profil = Control_Profil, Test_Profil = Test_Profil, plot_test = plot_test, plot_control = plot_control))
} #### End of the function
