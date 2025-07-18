# Chla Processing L1 script
# By: Adrienne Breef-Pilz
# Written: 24 Nov. 23
# Edit: 18 Feb. 25 - added an if statement for when we don't have new observations for the year. The function ends
# KKH Updated Jan 25 - rename SNP sites to be consistent with LSPA sites 
# (40 (Hedgehog) > 220 (LSPA code for Hedgehog), 50 (buoy) > 200)
# updated script to handle other possible spec sample ID naming conventions
# 16 Jun 24- add in historical file, add a start and end date filter, add in time of sample
# 24 Sept 24 - round all numeric columns to 4 digits
# 25 Apr Fixed bugs and added warnings
# 

# Things the script does: 
# 1. Read in Maintenance log and read in raw chla file from the spec
#   Put in the right format for processing
# 2. Read in the filtering log and rack map
# 3. Merge everything together
# 4. Maintenance log to flag or remove issues
# 5. Process with a script based on BNN Excel script
# 6. Further QAQC processing
# 7. Save files

#filt_chla_qaqc from L1 script 17 April 2025
#updating code and adding a section to return mismatches as needed
#also adding a section to handle dilutions as needed


sulfate_chlorophyll_qaqc <- function(directory, 
                           rack_map,
                           final_vol_extract = 6, 
                           blank_vol_filt = 500, 
                           outfile,
                           start_date,
                           end_date)
{
  
directory = "Assay Experiment"
rack_map = "Assay Experiment/Round1_chlarackmap.xlsx" #direct to my rack map, include column for volume filtered
final_vol_extract = 6
blank_vol_filt = 500
outfile = "Assay Experiment/Filt_chla_L1.csv" #name of the file and where it will save to
start_date = NULL
end_date = NULL
  
 #packages
  pacman::p_load(tidyverse, gsheet,arsenal, readxl)
  
  #### 1. Read in Maintenance file and the Raw files from the spec 
  ### 1.1 Read in Maintenance file #### 
  
 
  ## check how maintenance log is read in/entered
 # log_read <- read_csv(maintenance_file, col_types = cols(
#    .default = col_character(),
#    Date_processed = col_date("%Y-%m-%d"),
#    Sample_date = col_date("%Y-%m-%d"),
#   flag = col_integer()))
  
#print(("Warning! the following rows from the maintenance log may cause you trouble. Do you have something other than NA or a date in the SampleDate column?"))
#print(problems(log_read))

#  log <- log_read
  
  # filter out observations in log based on start and end date
  # Filter maintenance log based on start and end times
#  if(!is.null(end_date)){
 #   log <- log_read |>
  #    filter(Sample_date<= end_date)
#  }else{
#    log <- log_read
#  }


#  if(!is.null(start_date)){
#    log <- log_read %>%
#      filter(Sample_date >= start_date)
#  }else{
#    log <- log_read
#  }
  
  #### 1.2 Read in files from the spec that are in a folder. 
  
#  print("compiled data frame of absorbances from spec")
  
  # make a function that reads in the file, extract the processing date, label the observation if it was before acid was added or after, get the number of the sample to match with rack map, and indicate if the sample was diluted. 
  
  read_raw_chla_files<-function(FILES){
    
      # print file names to see where errors occur
      print(paste0("Read in ", FILES))
      
      # Get the date the samples were processed on the spec
      sed <- str_extract(FILES, "_\\d+")
      
      # Take out the extra underscore
      Date <- sub("_","",sed)
      
      # Put the date in the proper format
      Date_processed <- as.Date(Date, "%Y%m%d")
      
      # read in the files
      data <- read_csv(FILES)|>
        dplyr::rename("Sample.ID" = `Sample ID`)
      
      # Add the date processed to the files
      data$Date_processed <- as.Date(Date, "%Y%m%d")
      
      
      data2 <- data|>
        # Label the samples either eth_blank, fake samples, reservoir samples 
        # Some samples aren't labeled as eth_blanks in the ID so need to be labeled later in the script as well
        mutate(samp_type = ifelse(grepl("et|blan|buff|blnk|filt", Sample.ID), "eth_blank",
                                  ifelse(grepl("fa", Sample.ID), "fake",
                                         ifelse(grepl("ref", Sample.ID), "ref", "res_samp"))),
          # # samples are labeled "B" for absorbance before adding acid to the sample. "A" is the absorbance after acid has been added. Label the samples so we can pick them out later. 
          #      timing = gsub("_","", gsub("[[:digit:]]", "", Sample.ID)),
          #      timing = ifelse(str_detect(Sample.ID,"b|B")==T,"b", 
          #                      ifelse(str_detect(Sample.ID, "a|A")==T, "a", timing)),
               # remove the _2 for the Num_ID. _2 means the sample was rerun, but for determining
               # the id to link with the field info this number should be ignored. 
               Num_ID = gsub("_2", "", Sample.ID),
               # this ensures there are no letters in the ID so we can use as.numeric below
               Num_ID = gsub("[aA-zZ]", "", Num_ID),
               # indicates samples that were diluted 
               dilution = ifelse(str_detect(Sample.ID,"DIL|dil")==T,"diluted", NA),
               Num_ID = as.numeric(Num_ID)) 
      
      return(data2)
      }
      
    
   # Use the function to make a data frame of raw absorbance from the spec
  # use purr to read in all the files using the function above
  files<-list.files(path= directory,pattern=".txt", full.names=TRUE)
    
    if(length(files)==0){
      
        warning("There are no text files in the directory you listed")
      
    }else{
      out.file <- files|>
        map_df(~ read_raw_chla_files(.x)) # read in files and bind them together
    }
  
  
  ### 2. Get the sample ID number and match with the reservoir and site
  
  ### 2.1 Read in the rack map file ####
  print("Read in rack map and join by Date Processed, Num_ID and dilution")
  
  #CHANGE THIS AS WELL TO BE ABLE TO READ IN RACK MAP 

  rack_map <- read_excel(rack_map)

  
  ###THIS IS WHERE i WILL UPDATE HEADERS 
  rack_map2 <- rack_map %>% 
    mutate(
      Num_ID = as.numeric(Num_ID),
      Vol_filt_mL = as.numeric(Vol_filt_mL),
      Final_vol_extract_mL = final_vol_extract
      # New column based on Flask condition
    )
  #maybe check to see if i need to add flask and dose

  # check if there are any dups in the rack map
 # rack_dup <- rack_map2 |>
#    select(Date_processed, ResSite, Depth, Rep, Sample_date, dil_factor)|>
#    filter(grepl("^20", Sample_date))
  
 # check_rack_dup <- rack_dup[duplicated(rack_dup), ]
  
#  if(nrow(check_rack_dup)>0){
#    warning("There are duplicates in the rack map and the duplicate might not be labeled. See below:")
#    print(check_rack_dup)
#  }
  
  
  
  # Join together by Date_processed and Num_ID
  # perform full_join based on multiple columns on the date processed, the number ID and if there is a dilution factor. 
  df3 <- full_join(out.file, rack_map2, by=c('Date_processed'='Date_processed', 'Num_ID'='Num_ID'))|>
    select(-dilution)|>
    
    #I REMOVED THIS PART FROM NOW BUT MAYBE I WILL NEED IT LATER
  # relabel ethanol blank samples so we can find them later
    mutate(
      samp_type = ifelse(str_detect(Flask, "^blank")==T & !is.na(Flask), "eth_blank", samp_type))
                         #ifelse(str_detect(Flask, "^eth")==T, "eth_blank", samp_type)))
  
  
  # # Get sample dates in the right format
  # res_samp2 <- df3%>%
  #   mutate(
  #     Sample_date2= ifelse(grepl("^20", Sample_date)==T, Sample_date, NA),
  #     Sample_date = as.Date(Sample_date2))|> # put date in format
  #     select(-Sample_date2)

 # Get the minimum sample date from the processed samples 
#  a <- res_samp2|>
#    drop_na(Sample_date)
  
#min_samp_date <- min(a$Sample_date)

  ### 2.2 read in filtering log 
  
  #filtering_log <- gsheet::gsheet2tbl(filtering_log)
  
  # Clean up the data frame
  #filtering_log2 <- filtering_log %>%
  #  mutate(
  #    Sample_date = lubridate::ymd(`Sample Date`),
      #Sample_date = parse_date_time(`Sample Date`, orders = c('dBy')),
  #    Vol_filt_mL = as.numeric(`Volume filtered (mL)`),
  #    Final_vol_extract_mL = final_vol_extract,
  #  ResSite = sub("-", "", ResSite),
  #  samp_type = "res_samp")|>
  #  drop_na(Sample_date)|>
  #  filter(Sample_date >= min_samp_date)|>
  #  select(ResSite, Depth, Sample_date, Rep, Vol_filt_mL, Final_vol_extract_mL, samp_type,Comments)
  
  # check if there are any duplicates in the filtering log
  #filt_dup <- filtering_log2 |>
  #  select(ResSite, Depth, Sample_date, Rep, Vol_filt_mL)
  
#  check_filt_dup <- filt_dup[duplicated(filt_dup), ]
  
#  if(nrow(check_filt_dup)>0){
#    warning("There are unlabeled duplicates in the filtering log. See below:")
#    print(check_filt_dup)
#  }
  
  
  # check if there are any samples after 2022 that are missing a filtered volume in the log
 # if(nrow(filtering_log|>filter(`Sample Date`>"2023-01-01")|>filter(is.na(`Volume filtered (mL)`)))>0){
#    warning("The following samples don't have a filtered volume in the filtering log and will be removed from the log.")
#    print(filtering_log|>filter(`Sample Date`>"2023-01-01")|>filter(is.na(`Volume filtered (mL)`)))
#  }
  
  ### 3. Combine with the filtering log 
#  print("combine samples with the filtering log")
  
#  comb <- left_join(res_samp2, filtering_log2, 
  #                  by=c("Sample_date"="Sample_date", "ResSite"="ResSite", "Rep"="Rep", "Depth"="Depth", "samp_type"="samp_type")) |>
 #   mutate(
   #   ResSite = ifelse(samp_type == 'eth_blank', NA, ResSite),
    #  Sample_date = ifelse(samp_type == 'eth_blank', NA, Sample_date),
     # Sample_date = as.Date(Sample_date, origin="1970-01-01")
    #)
    
#  print("There will be multiple matches and that is ok")
  ## the following reservoir samples don't have a filtered volume. 
  
#  if(nrow(comb|>filter(samp_type == 'res_samp' & is.na(Vol_filt_mL)))>0){
 #   warning("The following Reservoir samples are missing the amount of water filtered. Check the filtering log and check if sample has been mislabeled. If missing, volume filtered should also have been recorded on the frozen filter.")
    
  #  print(comb |>
#            filter(samp_type == 'res_samp' & is.na(Vol_filt_mL))|>
#            select(Date_processed, Sample_ID, ResSite, Depth, Rep, Sample_date, Vol_filt_mL))
#  }
  
  
  # add the vol filt and final vol used for the ethanol samples 
  # We use 500 mL for volume filtered for the ethanol blanks

   comb2 <- df3%>%
    mutate(
      Vol_filt_mL = ifelse(samp_type=="eth_blank", 500, Vol_filt_mL), 
      
      Final_vol_extract_mL = ifelse(samp_type=="eth_blank",final_vol_extract, Final_vol_extract_mL) #Final_vol_extract_mL <- I MAY HAVE MESSED THIS UP I AM A LITTLE CONFUSED I JUST PUT THEM AS THE SAME
    )
  
  # List the Sample.IDs with no observations from the spec
  
  no_obs <- comb2 |> 
    select(c(Sample.ID, WL750.0, Date_processed)) 
    
  if(nrow(no_obs|>filter(is.na(WL750.0)))>0){
    print("The following rows have no observations from the spec and will be removed")
    print(no_obs|>filter(is.na(WL750.0)))
    
    comb2 <- comb2|>
      drop_na(WL750.0)
    
  }
  
  ### 5. Get the Chla concentration from wavelengths from Spec 
  print("Check the samples before calculating chla concentration.")
  
  
  # Label the samples. 
  # samples are labeled "B" for abosorbance before adding acid to the sample. "A" is the absorbance after acid has been added. Label the samples so we can pick them out later. 
  
  raw_df2 <- comb2 %>%
    mutate(
      timing = gsub("_", "", gsub("[[:digit:]]", "", Sample.ID)),
      timing = ifelse(str_detect(Sample.ID, "^b|^B|b$|B$|_b|_B"), "b", 
                      ifelse(str_detect(Sample.ID, "^a|^A|a$|A$|_a|_A"), "a", timing)),
      timing = ifelse(Sample.ID %in% "blnk_a", "a", timing),
      Num_ID = ifelse(Sample.ID %in% c("blnk_b", "blnk_a"), 1, Num_ID), 
      samp_type = ifelse(Sample.ID %in% c("blnk_b", "blnk_a", "1_b", "1_a"), "eth_blank", samp_type)
    )
  
  # Now check for missing Num_ID and drop them
  if (nrow(raw_df2 %>% filter(is.na(Num_ID))) > 0) {
    warning("The following samples don't have a numeric ID and will be dropped")
    
    print(raw_df2 %>% 
            filter(is.na(Num_ID)) %>%
            select(Sample.ID, Date_processed, Num_ID))
    
    raw_df2 <- raw_df2 %>%
      drop_na(Num_ID)
  }

## Check the dups in the file and if there are dups rename them in the maintenance log

check_raw23 <- raw_df2|>
  drop_na(WL750.0)|>
  select(Date_processed, samp_type, timing, Flask, Dose, Vol_filt_mL)|>
  filter(samp_type == "res_samp")


sd_raw <- check_raw23[duplicated(check_raw23),]

# ert <- raw_df2[raw_df2$Sample_date %in% sd_raw$Sample_date &
#                  raw_df2$Depth %in% sd_raw$Depth &  
#                  raw_df2$Sample_ID %in% sd_raw$Sample_ID, ]

# if(nrow(sd_raw)>0){
#   warning("There are duplicates and mistyped Sample.IDs that need to be added to the maintenance log. See above for the duplicated files. Make sure there are only one before and one after acid for each sampling date and site.")
#   
#   print(sd_raw|>
#           select(Sample.ID, ResSite, Depth, Date_processed, Sample_date, timing, WL750.0))
# }


## Check the turbidity absorbance and print out values above 0.005 and save the files

check_turbidity <- raw_df2|>
  drop_na(WL750.0)|>
  filter(WL750.0 >0.005 | WL750.0 < (-0.005)) |> 
  select(c(Sample.ID, WL750.0, Flask, Dose, timing, Date_processed)) |> 
  filter(timing == "b")
####here####

if (nrow(check_turbidity)>0){
  warning("The turbidity in some of the samples was high. They will be flagged. If the duplicate has not yet been run, the dup should be run.")
  print(n = nrow(check_turbidity), check_turbidity)
}


  # The calculations are from BRN Chla processing excel sheet
  
  ### 5.1 Separate the wavelength by before acid and after and then merge together wider 

raw_df2 <- raw_df2|>
  mutate(Flag_Chla_ugL = 0,
         Flag_Pheo_ugL = 0)
  

  before_comb2 <- raw_df2%>%
    filter(timing=="b"|timing=="before")%>%
    dplyr::rename("before_acid_abs_750" = "WL750.0",
                  "before_acid_abs_665" = "WL665.0",
                  "before_acid_abs_664" = "WL664.0",
                  "before_acid_abs_663" = "WL663.0",
                  "before_acid_abs_647" = "WL647.0",
                  "before_acid_abs_630" = "WL630.0",
                  "before_acid_abs_490" = "WL490.0",
                  "before_acid_abs_384" = "WL384.0"
    )|>
    unique() %>% 
    select(Sample.ID, Num_ID, Date_processed, samp_type, Flask, Dose, Vol_filt_mL, Final_vol_extract_mL,
           before_acid_abs_750:before_acid_abs_384, Flag_Chla_ugL, Flag_Pheo_ugL, Notes)
  
  #MAKE SURE ALL THESE LINE UP COLUMN NAMES
  
  # Now do it for after acid readings
  

  after_comb2 <- raw_df2%>%
    filter(Flag_Chla_ugL!=2)%>%
    filter(timing=="a"|timing=="after")%>%
    dplyr::rename("after_acid_abs_750" = "WL750.0",
                  "after_acid_abs_665" = "WL665.0",
                  "after_acid_abs_664" = "WL664.0",
                  "after_acid_abs_663" = "WL663.0",
                  "after_acid_abs_647" = "WL647.0",
                  "after_acid_abs_630" = "WL630.0",
                  "after_acid_abs_490" = "WL490.0",
                  "after_acid_abs_384" = "WL384.0"
    )|>
    unique() %>% 
    select(Sample.ID, Num_ID, Date_processed, samp_type, Flask, Dose,Vol_filt_mL, Final_vol_extract_mL,
           after_acid_abs_750:after_acid_abs_384,Flag_Chla_ugL, Flag_Pheo_ugL, Notes)
  
  
  
  ### Add a check about if there are uneven before and after sample numbers
  check_bef <- before_comb2|>
    select(Date_processed, samp_type,Flask, Dose, Date_processed, Vol_filt_mL)

  check_aft <- after_comb2|>
    select(Date_processed, samp_type,Flask, Dose, Date_processed, Vol_filt_mL)

  check_df <- summary(arsenal::comparedf(check_bef, check_aft, by = c("Date_processed", "samp_type","Flask", "Dose", "Date_processed", "Vol_filt_mL")))

  
  if(nrow(check_df$obs.table)>0){
    warning("There are uneven number of samples in the before or after data frames. Check the output to see where the issue is. There should be the same number of samples in each data frame.")

    print(check_df$obs.table)
  }
  # 
  # Join the two data frames together
  
  comb3 <- full_join(before_comb2, after_comb2, 
                     by=c("Num_ID", "Date_processed", "samp_type", "Flask", "Dose",
                          "Vol_filt_mL", "Final_vol_extract_mL", "Flag_Chla_ugL", "Flag_Pheo_ugL", "Notes")) 
  
  ### 5.2 Calculate the concentration of Chla in ugL 
  
  comb3_calc <- comb3%>%
    mutate(
      # before acidification (turbidity corrected)664-750
      
      before_acid = before_acid_abs_664-before_acid_abs_750,
      
      #after acidification (turbidity corrected) 665-750
      
      after_acid = after_acid_abs_665-after_acid_abs_750,
      
      # difference before and after problems if negative
      
      diff_be_af = before_acid-after_acid)
  
  ### 5.21 Get the average blanks for each processing date 
  
  avg_e_blank <- comb3_calc%>%
    filter(samp_type=="eth_blank")%>%
    group_by(Date_processed)%>%
    summarise(across(c("before_acid", "after_acid", "diff_be_af"), list(mean = mean, sd = sd)))%>%
    ungroup()|>
    dplyr::rename(blank_before_acid_avg = before_acid_mean,
                  blank_after_acid_avg = after_acid_mean,
                  blank_diff_be_af_avg = diff_be_af_mean)%>%
    select(Date_processed, blank_before_acid_avg, blank_after_acid_avg, blank_diff_be_af_avg)
  
  comb4_calc <- left_join(comb3_calc, avg_e_blank, by="Date_processed")
  
  ### 5.22 Now finish the calculations 
  ####calculations####
  comb5_calc <- comb4_calc %>%
    
    mutate(
      # Chlorophyll a in extract (ug/L from Arar) BD if <~65
      
      chla_extract = 1000*28.64*diff_be_af,
      
      # Pheopigment in extract (ug/L from Arar)BD if <~85
      
      pheo_extract = 1000*28.64*((1.72*after_acid)-before_acid),
      
      # Pheopigment in extract (ug/L) with ethanol blank correction
      
      pheo_extract_wblank_corr = 1000*28.64*(1.72*(after_acid-blank_after_acid_avg)-
                                               (before_acid-blank_before_acid_avg)),
      
      # Chlorophyll a Conc of original water sample in ug/L (or mg/m3-same thing- APHA)
      
      chla_in_water = (chla_extract*Final_vol_extract_mL/1000)/((Vol_filt_mL/1000)*1), 
      
      # Pheopigment Conc of original water sample in ug/L (or mg/m3-same thing- APHA)
      
      pheo_in_water = (pheo_extract*Final_vol_extract_mL/1000)/((Vol_filt_mL/1000)*1),
      
      # Ratio before and after (1 = all pheo, 1.72 = all chla)problems if not between 1 and 1.72
      
      ratio_be_af = before_acid/after_acid,
      
      # Ratio ethanol blank corrected
      
      ratio_be_af_eth_corr=(before_acid-blank_before_acid_avg)/
        (after_acid-blank_after_acid_avg)
    )
  
  ### 5.3 Select only the columns of interest and need for EDI and QAQC ###
  # Take out the ethanol blank rows 
  chla_df<- comb5_calc %>%
    filter(samp_type!="eth_blank")%>%
    select(Sample.ID.x,
           Flask,
           Dose,
           before_acid_abs_750,
           before_acid_abs_664,
           chla_extract,
           pheo_extract,
           chla_in_water,
           pheo_in_water,
           ratio_be_af_eth_corr,
           Flag_Chla_ugL,
           Flag_Pheo_ugL)%>%
    dplyr::rename( # rename the columns for below
      Check_Absorb = before_acid_abs_664,
      Chek_Turb_750 = before_acid_abs_750,
      Check_chla = chla_extract,
      Check_pheo = pheo_extract,
      Chla_ugL = chla_in_water,
      Pheo_ugL = pheo_in_water
    )
  
  write.csv(chla_df, "Assay Experiment/csvs/Assay_1_chla.csv")
  
  ####stopping here####
  
  
  ### 6. Further QAQC 
  
  chla_new<-chla_df%>%
    #filter(Sample_ID!="")%>%
    # Get Reservoir and Site
    #separate(.,col = ResSite, into = c("Reservoir", "Site"), sep = 1)%>%
    mutate(Reservoir = substr(ResSite, 1, 1), 
           Site = substring(ResSite, 2)) %>% 
    mutate(Reservoir=ifelse(Reservoir=="B","BVR", Reservoir),
           Reservoir=ifelse(Reservoir=="F","FCR", Reservoir),
           Reservoir=ifelse(Reservoir=="S","SNP", Reservoir),
           Reservoir=ifelse(Reservoir=="C","CCR", Reservoir)) %>%
    # Add flags for low absorbance and pigment below detection
    mutate(Flag_Chla_ugL=ifelse(Check_Absorb<0.03, 1, Flag_Chla_ugL),
           Flag_Pheo_ugL=ifelse(Check_Absorb<0.03, 1, Flag_Pheo_ugL),
           Flag_Chla_ugL=ifelse(Check_chla<34, 4, Flag_Chla_ugL),
           Flag_Pheo_ugL=ifelse(Check_pheo<34, 4, Flag_Pheo_ugL),
           Pheo_ugL=ifelse(Pheo_ugL<0, 0, Pheo_ugL), 
           Flag_Chla_ugL=ifelse(ratio_be_af_eth_corr>1.72,paste0(Flag_Chla_ugL, 6), Flag_Chla_ugL),
           Flag_Pheo_ugL=ifelse(ratio_be_af_eth_corr>1.72,paste0(Flag_Pheo_ugL, 6), Flag_Pheo_ugL))|>
    # flag high turbidity values as anything above 0.005. Maybe change later to 0.01
    mutate(Flag_Chla_ugL = ifelse(Chek_Turb_750>0.005, paste0(Flag_Chla_ugL, 7),Flag_Chla_ugL),
           Flag_Pheo_ugL = ifelse(Chek_Turb_750>0.005, paste0(Flag_Pheo_ugL, 7),Flag_Pheo_ugL))

  
  
  ## Print samples that have more than 2 samples per a sampling date, site, and depth. One probably needs to be added to the maintenance log
  
  check_reps <- chla_new|>
    group_by(Reservoir, Site, Date, Depth_m)%>%
    mutate(count = n())|>
    filter(count>2)
  
  if(nrow(check_reps)>0){
    warning("There are more than 2 samples on a sampling date, site, and depth. One probably needs to be added to the maintenance log. Check the output above. We only collect one duplicate so most likely there is an issue with one of the samples and it needs to be added to the maintenance log.")
    
    print(check_reps)
  }
  
 chla_new2 <- chla_new|>
   # Take out high turbidity in duplicates. If turbidity is below 0.005 then keep both. If one observation is above 0.005 then drop observations with the high turbidity value. If both are high then take the minimum. 
   group_by(ResSite, Depth_m, Date)|>
   mutate(
     flag = case_when(
       all(Chek_Turb_750 > 0.005) ~ 1,  # Both values are over, take the minimum
       any(Chek_Turb_750 > 0.005) ~ 2,  # Remove row if either value exceeds 0.005
       TRUE ~ 0  # Keep the row otherwise
     )
   ) |>
   filter(flag == 0 | (flag >= 1 & Chek_Turb_750 == min(Chek_Turb_750))) %>%  # Keep only appropriate rows
   #select(-flag) %>%
   ungroup()|>
   # since you dropped some high turbidity dups. Count again
   group_by(Reservoir, Site, Date, Depth_m)%>%
   mutate(count = n())|>
 
   # flag samples that had duplicates
   mutate(Flag_Chla_ugL=ifelse(count==2,paste0(Flag_Chla_ugL,5),Flag_Chla_ugL),
          Flag_Pheo_ugL=ifelse(count==2,paste0(Flag_Pheo_ugL,5),Flag_Pheo_ugL),
          # convert to numeric to eliminate leading 0 when pasting flags to each other
          Flag_Chla_ugL = as.numeric(Flag_Chla_ugL),
          Flag_Pheo_ugL = as.numeric(Flag_Pheo_ugL))|>
   
    #mutate(Chla_ugL = mean(Chla_ugL)) %>%
    #mutate(Pheo_ugL = mean(Pheo_ugL))%>%
    ungroup()|>
   # Flag if there are more than 2 samples
    # take out the dups after averaging them together. 
    #distinct(Reservoir, Site, Date, Depth_m, .keep_all = T)%>%
    mutate(Depth_m=as.numeric(Depth_m))%>%
    mutate(Site=as.numeric(Site))%>%
    dplyr::rename(DateTime=Date)%>%
    select(Reservoir,Site,DateTime, Depth_m, Chla_ugL,Pheo_ugL,Flag_Chla_ugL,Flag_Pheo_ugL)%>%
    mutate(Flag_Chla_ugL=ifelse(is.na(Chla_ugL), 2, Flag_Chla_ugL))%>% #Add a 2 flag if an observation is missing
    mutate(Flag_Pheo_ugL=ifelse(is.na(Pheo_ugL),2,Flag_Pheo_ugL))
  


  # If there is no sample time then set it to noon and flag as 1 
  final <- chla_all2%>% #whatever the last file fix this 
    select(Reservoir, Site, DateTime, Depth_m, Chla_ugL, Pheo_ugL,
           Flag_DateTime, Flag_Chla_ugL, Flag_Pheo_ugL)|> #just make sure these are right
    mutate_if(is.numeric, round, digits = 4) # round to 4 digits
  
  # put in order
  final <- final[order(final$DateTime),]
  
  # subset to make the L1 file. Maybe look at moving this earlier. 
  
  if (!is.null(start_date)){
    #force tz check
    start_date <- force_tz(as.POSIXct(start_date), tzone = "America/New_York")
    
    final$DateTime <- as.character(format(final$DateTime)) # convert DateTime to character
    
    final <- final %>%
      filter(DateTime >= start_date)
    
  }
  
  if(!is.null(end_date)){
    #force tz check
    end_date <- force_tz(as.POSIXct(end_date), tzone = "America/New_York")
    
    final$DateTime <- as.character(format(final$DateTime)) # convert DateTime to character
    
    final <- final %>%
      filter(DateTime <= end_date)
    
  }
  
  # Check if there are any files for the L1. If not then end the script
  
  if(nrow(final)==0){
    
    print("No new files for the current year")
    
  }else{
    
    
    ### 7. Save the file. If outfile is NULL then return the file
    
    if(is.null(outfile)){
      
      return(final)
    }else{
      
      write_csv(final, outfile)
    }
    
  } # ends the if statement if there are no new observations
  
} # ends the function
