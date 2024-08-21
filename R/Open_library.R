Open_library <- function(dirpath = NA){
  
  if(is.na(dirpath)){
    dirpath = rstudioapi::selectDirectory(caption = "Select the Directory where Spectra are located")
  }
  
  
  
  listfile <- list.files(dirpath, pattern = ".txt",recursive = T , full.names = T) %>% 
    as.data.frame() %>% 
    dplyr::rename(path = ".") %>% 
    dplyr::mutate(Spectra = gsub(".*/","",path) %>% gsub(".asd.txt","",.),
           subdir = gsub(dirpath,"",path) %>% sub("/[^/]*$", "", .))
  
  
  check_metadata <- readr::read_table(listfile$path[1], 
                                      col_names = FALSE, skip = 4)
  
  if(nrow(check_metadata)>748 & ncol(check_metadata)>3){
    meta = T
  }else{
    meta = F
  }
  rm(check_metadata)
  
  if(file.exists(paste0(dirpath ,"/Reflectance_dataframe.csv"))== F){
    
    if(meta){
      for (i in 1:nrow(listfile)) {
        a <- readr::read_table(listfile$path[i], 
                        col_names = FALSE, skip = 4)
        
        b <- readr::read_table(listfile$path[i], skip = 35) %>% 
          magrittr::set_names(c("Wavelength", "Value")) %>% 
          dplyr::mutate(Spectra = listfile$Spectra[i],
                 Date = as.character(a[3,3]),
                 Time = as.character(a[3,5]),
                 IntegrationTime = as.numeric(a[4,4]), 
                 subdir = listfile$subdir[i])
        
        if (i == 1) {
          
          df <- b %>%
            dplyr::mutate(Time_POS = as.POSIXct(paste0(Date," ", substr(Time,1,5)), format = "%m/%d/%Y %H:%M"),
                   ID = paste0(Spectra,subdir,"_",as.numeric(Time_POS)))
          
        }else{
          df <- rbind(df,b %>%
                        dplyr::mutate(Time_POS = as.POSIXct(paste0(Date," ", substr(Time,1,5)), format = "%m/%d/%Y %H:%M"),
                               ID = paste0(Spectra,subdir,"_",as.numeric(Time_POS)))
          ) 
        }
      }
      
      utils::write.csv(df, paste0(dirpath ,"/Reflectance_dataframe.csv"), row.names = F)
      
      df <- df  %>%  
        # dplyr::filter(Wavelength %% 2 == 0) %>% 
        dplyr::group_by(ID) %>% 
        dplyr::reframe(Wavelength = Wavelength,
                Value = Value,
                Value_mean = c(zoo::rollmean(Value, k=9, fill = NA)),
                Date = Date,
                Time = Time,
                Spectra = Spectra,
                IntegrationTime = IntegrationTime,
                Subdir = subdir,
                ID = ID) %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(!is.na(Value)) %>%  
        # !(scenario == "test" & Spectra %in% c("Heatwave100100","Heatwave100108")),
        # !(scenario == "Control" & Spectra %in% c("Heatwave100089")))%>% 
        dplyr::mutate(Time_POS = as.POSIXct(paste0(Date," ", substr(Time,1,5)), format = "%m/%d/%Y %H:%M"))%>% 
        dplyr::filter(Wavelength >= 440 & Wavelength <= 990) %>% 
        dplyr::group_by(Spectra,Subdir) %>% 
        dplyr::mutate(std = (Value_mean-min(Value_mean,na.rm=T))/(max(Value_mean,na.rm=T)-min(Value_mean,na.rm=T))) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(timeDiff = round(as.numeric(Time_POS - min(Time_POS))/3600),0) %>%
        dplyr::select(-`0`)
      
    }else{
      for (i in 1:nrow(listfile)) {
        
        a <-utils::read.delim2(listfile$path[i]) %>%
          dplyr::mutate(Spectra = gsub(".*/","",listfile$path[i]) %>% gsub(".asd.txt","",.), 
                 subdir = listfile$subdir[i])
        
        names(a)<- c("Wavelength","Value","Spectra","Subdir")
        
        if(i == 1){
          df <- a 
        }else{
          df <- rbind(df,a)
        }
      }
      utils::write.csv(df, paste0(dirpath ,"/Reflectance_dataframe.csv"), row.names = F)
    }
  }else{
    if(meta){
      df <- utils::read.csv(paste0(dirpath ,"/Reflectance_dataframe.csv"))%>%  
        # dplyr::filter(Wavelength %% 2 == 0) %>% 
        dplyr::group_by(ID) %>% 
        dplyr::reframe(Wavelength = Wavelength,
                Value = Value,
                Value_mean = c(zoo::rollmean(Value, k=9, fill = NA)),
                Date = Date,
                Time = Time,
                Spectra = Spectra,
                IntegrationTime = IntegrationTime,
                Subdir = subdir,
                ID = ID) %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(!is.na(Value)) %>%  
        # !(scenario == "test" & Spectra %in% c("Heatwave100100","Heatwave100108")),
        # !(scenario == "Control" & Spectra %in% c("Heatwave100089")))%>% 
        dplyr::mutate(Time_POS = as.POSIXct(paste0(Date," ", substr(Time,1,5)), format = "%m/%d/%Y %H:%M"))%>% 
        dplyr::filter(Wavelength >= 440 & Wavelength <= 990) %>% 
        dplyr::group_by(Spectra,Subdir) %>% 
        dplyr::mutate(std = (Value_mean-min(Value_mean,na.rm=T))/(max(Value_mean,na.rm=T)-min(Value_mean,na.rm=T))) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(timeDiff = round(as.numeric(Time_POS - min(Time_POS))/3600),0) %>%
        dplyr::select(-`0`)
    }else{
      df <- utils::read.csv(paste0(dirpath ,"/Reflectance_dataframe.csv"))
    }
    
  }
  return(df)
}

