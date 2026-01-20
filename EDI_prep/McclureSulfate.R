#to see what to include
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(tidyr)
library(purrr)
library(akima)
library(patchwork)
library(scales)


mcclureso4 <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/607/0/86bc3dc8c1eafe36e6935f8a858a7b27")
print(colnames(mcclureso4))

#reported in mmol_L

# Data flag for site depths (I don't really have these);
# 0 = not suspect
# 1 = sample not taken
# 2 = instrument malfunction
# 3 = sample below detection
# 4 = negative value set to zero
# 5 = demonic intrusion
# 6 = non-standard method
# 7 = sample run multiple times and values averaged

#flags he had 
# Data flag for site depths;
# 0 = not suspect
# 1 = sample not taken
# 2 = instrument malfunction
# 3 = sample below detection
# 4 = negative value set to zero
# 5 = demonic intrusion
# 6 = non-standard method
# 7 = sample run multiple times and values averaged

#for mine I will add a flag:
#8 = container cracked when frozen (when I pulled it out of the freezer it was cracked,
#so as I was thawing it I placed it in another nutrient bottle)