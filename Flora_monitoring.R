#checking out fluoropobe data to see which reservoir to sample from for sulfate experiment

#flora <- read.csv("Assay Experiment/csvs/fluoroprobe_L1.csv")
flora <- read.csv("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/eed405f279a35c5335a1614e948ddf37a9ef3ad7/Data/DataNotYetUploadedToEDI/FluoroProbe/fluoroprobe_L1.csv")


library(dplyr)
library(lubridate)

df <- flora|>
  mutate(Date = as_date(DateTime))|>
  filter(Date %in% c("2025-06-30"))|> #change for whatever date
  filter(Site == 50)

BVR <- df|>
  filter(Reservoir == "BVR")
  
FCR <- df|>
  filter(Reservoir == "FCR")
