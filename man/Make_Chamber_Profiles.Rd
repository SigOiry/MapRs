\name{Make_Chamber_Profiles}
\alias{Intertidal chamber profile maker}
\title{Make_Chamber_Profiles}
\usage{
Make_Chamber_Profiles(max_Air_Temp_control, 
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
                         Experiment_Name = "ThisIsATest")
}
\description{
A function creating plots, dataframes, and files that the intertidal chambers can understand for an experiment involving test and control conditions}
\examples{
test <- Make_Chamber_Profile(min_Air_Temp_control = 18,
                     max_Air_Temp_control = 23,
                    min_Air_Temp_test = 23,
                    max_Air_Temp_test = 35,
                    min_Water_Temp_control= 19, 
                    max_Water_Temp_control = 19,
                    min_Water_Temp_test = 19,
                    max_Water_Temp_test = 19,
                    date_start = "2024-07-22 21:00:00",
                    date_end = "2024-07-26 23:00:00",
                    low_tide_time = "2024-07-23 12:00:00",
                    High_Tide_Until = "2024-07-24 09:00:00",
                    daily_increase_Air_Test = 1,
                    daily_increase_Water_Test = 0.5,
                    daily_increase_Air_Control = 0,
                    daily_increase_Water_Control = 0,
                    sunrise = 6.25,
                    sunset = 22,
                    max_light_intensity = 120,
                    export_profile = T,
                    Tide = "both",
                    night_tide = T,
                    time_step = 60, 
                    Save_Experiment = T, 
                    Experiment_Name = "HW4_24072024_to_26072024")
}