#Maria Popescu Sulfate and Iron concentrations

library(dplyr)
library(lubridate)


#here we go

#change the depth that you want to see
#depths for fcr: 0.1, 5.0, 9.0
#depths for bvr: 0.1, 6.0, 11.0

#library(ggplot2)
#fcr <- filter(dt0, Reservoir == "FCR" & Depth_m == 5.0 & Site == 50)
#bvr <- filter(dt0, Reservoir == "BVR" & Depth_m == 6.0 & Site == 50)
  
#ggplot()+
# geom_point(data = fcr, aes(x = DateTime, y = SO4_mmol_L, color = "blue"))+
# geom_point(data = bvr, aes(x = DateTime, y = SO4_mmol_L, color = "red"))+
#  labs(title = "SO4 at FCR (5m) and BVR (6m)", x = "Month", y = "SO4 mmol/L")+
#  scale_color_manual(values = c("blue", "red"), labels = c("FCR", "BVR"))+  # Legend with colors and labels
#  theme_classic()

#change the depth and year that you want to see
#depths for fcr: epi: 0.1, 1.6, meta: 3.8, 5.0, hypo: 6.2, 8.0, 9.0

library(ggplot2)
library(dplyr)
library(lubridate)

dt0 <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/607/0/86bc3dc8c1eafe36e6935f8a858a7b27")

dt0<- dt0|>
  mutate(FE2_ugL = (FE2_mmol_L * 55845))|> # Conversion factor based on atomic weight of Fe (55.845 g/mol)
  mutate(SO4_ugL = (SO4_mmol_L * 96065))|> # Conversion factor based on molecular weight of SO4 (96.065 g/mol)
  mutate(DateTime = ymd_hms(DateTime))
  
#plotting FCR sulfate  
FCR <- dt0|>
  filter(Reservoir == "FCR")%>%
  mutate(DateTime = ymd_hms(DateTime))%>%
  filter(Site == 50) %>%
  mutate(limnion = case_when(
    Depth_m <= 0.1 ~ "Epilimnion",
    Depth_m > 1.6 & Depth_m < 6 ~ "Metalimnion",
    Depth_m >= 6 ~ "Hypolimnion"
  )) %>%
  mutate(limnion = factor(limnion, levels = c("Epilimnion", "Metalimnion", "Hypolimnion")))


#plotFCR sulfate and iron concentrations
ggplot(data = FCR, aes(x = DateTime, y = SO4_ugL, color = as.factor(Depth_m)))+
  geom_point()+
  facet_grid(rows = vars(limnion), scales = "free_y")+
  theme_bw()+
  labs(x = "Date", y = "SO4_ugL", title = "FCR sulfate")+
  scale_color_discrete(name = "Depth (m)")+ 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),panel.spacing = unit(1, "lines"), 
        strip.text.x = element_text(size = 13),
        strip.text.y = element_text(size = 12))

ggplot(data = FCR, aes(x = DateTime, y = FE2_ugL, color = as.factor(Depth_m)))+
  geom_point()+
  facet_grid(rows = vars(limnion), scales = "free_y")+
  theme_bw()+
  labs(x = "Date", y = "Fe2_ugL", title = "FCR IronII")+
  scale_color_discrete(name = "Depth (m)")+ 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),panel.spacing = unit(1, "lines"), 
        strip.text.x = element_text(size = 13),
        strip.text.y = element_text(size = 12))

#plotBVR sulfate and iron concentrations
#depths for bvr: epi: 0.1, meta: 3.0 hypo: 6.0, 9.0

BVR <- dt0|>
  filter(Reservoir == "BVR")%>%
  mutate(DateTime = ymd_hms(DateTime))%>%
  filter(Site == 50) %>%
  mutate(limnion = case_when(
    Depth_m <= 0.1 ~ "Epilimnion",
    Depth_m > 1.6 & Depth_m < 7 ~ "Metalimnion",
    Depth_m > 6 ~ "Hypolimnion"
  )) %>%
  mutate(limnion = factor(limnion, levels = c("Epilimnion", "Metalimnion", "Hypolimnion")))


#plotFCR sulfate and iron concentrations
ggplot(data = BVR, aes(x = DateTime, y = SO4_ugL, color = as.factor(Depth_m)))+
  geom_point()+
  facet_grid(rows = vars(limnion), scales = "free_y")+
  theme_bw()+
  labs(x = "Date", y = "SO4_ugL", title = "BVR sulfate")+
  scale_color_discrete(name = "Depth (m)")+ 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),panel.spacing = unit(1, "lines"), 
        strip.text.x = element_text(size = 13),
        strip.text.y = element_text(size = 12))

ggplot(data = BVR, aes(x = DateTime, y = FE2_ugL, color = as.factor(Depth_m)))+
  geom_point()+
  facet_grid(rows = vars(limnion), scales = "free_y")+
  theme_bw()+
  labs(x = "Date", y = "Fe2_ugL", title = "BVR IronII")+
  scale_color_discrete(name = "Depth (m)")+ 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),panel.spacing = unit(1, "lines"), 
        strip.text.x = element_text(size = 13),
        strip.text.y = element_text(size = 12))



