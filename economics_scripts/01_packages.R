### Economic data
### J Collins 
### 2024-04-26
###

## PACKAGES ####

suppressPackageStartupMessages({
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(ggplot2)) install.packages("ggplot2")
  if (!require(ggpubr)) install.packages("ggpubr")
  if (!require(gridExtra)) install.packages("gridExtra")
  if (!require(readxl)) install.packages("readxl")
  if (!require(ggrepel)) install.packages("ggrepel")
  if (!require(plotrix)) install.packages("plotrix")
  if (!require(rstatix)) install.packages("rstatix")
  if (!require(agricolae)) install.packages("agricolae")
  if (!require(ggstats)) install.packages("ggstats")
  if (!require(nlme)) install.packages("nlme")
  if (!require(tidyr)) install.packages("tidyr")
  
  library(dplyr) # for "glimpse" and data manipulation
  library(ggplot2) # general plotting
  library(ggpubr) # custom plotting
  library(gridExtra) # grid plotting
  library(readxl) # read .xlsx files
  library(ggrepel) # Automatically position non-overlapping text labels with 'ggplot2'
  library(plotrix) # standard error
  library(rstatix) # significance bars in plot 
  library(agricolae) # poster theme
  library(ggstats) # calculate proportions and add them to barplots
  library(nlme)
  library(tidyr) # pivot join df's 
})



