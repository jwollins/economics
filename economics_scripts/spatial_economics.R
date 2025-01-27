## HEADER ####
## Who: J Collins
## what: spatial economics
## Last edited: 2022-11-09


## CONTENTS ####
## 00 set working directory
## 01 Plot point data  
## 02 Vario fitting 
## 03 Change CRS
## 04 plot points and grid to estimate 
## 05 Kriging
## 06 Load shp. files for comparison




#******************************************************************** ####
# 00 Setup ####
getwd()




#******************************************************************** ####
# 01 Packages ####
suppressPackageStartupMessages({
  library(dplyr) # for "glimpse" and data manipulation
  library(ggplot2) # general plotting
  library(scales) # for "comma"
  library(magrittr)
  library(ggpubr) 
  library(sp)
  library(gstat)
  library(rgdal)
  library(raster)
  library(gridExtra)
  library(png)
  library(grid)
  library(jpeg)
  library(cowplot)
  library(ggsci)
  library(sjPlot)
  library(sjmisc)
  library(sjlabelled)
  library(viridis)
  library(broom)
  library(sf) # load shp files
  library(reshape2) # cast spatial objects to df
  library(raster)
  library(rasterVis)
  library(automap) # auto variograms
  library(ggspatial) # annotate north arrow
  library(readxl)
})








#******************************************************************** ####
# 02 Data ####



### Read CSV datasets ####
 
dat_beans <- read.csv(file = "data/yield/bean.data.wgs84.csv", 
                   header = TRUE)

dat_beans[is.na(dat_beans)] <- 0


dat_wheat <- read.csv(file = "data/yield/wheat_hh_yield.csv")

# calculate t/ha from g/m2
dat_wheat$t_ha <- (dat_wheat$mass_g / 1000000) * 10000



## Filter to farming system 

# filter to CON system
datCON_beans <- dat_beans %>% filter(Treatment == "Conventional")

# filter to CA system
datCA_beans <- dat_beans %>% filter(Treatment == "Conservation")

# filter to CON system
datCON_wheat <- dat_wheat %>% filter(treatment == "Conventional")

# filter to CA system
datCA_wheat <- dat_wheat %>% filter(treatment == "Conservation")







### spatial data ####

setwd(dir = "~/OneDrive - Harper Adams University/Data/Shapefiles/")

#### Image ####

# read photo in as a raster
image <- raster("Images/WGS84_High_res_field_image.tif")

# check resoloution
res(image)
#aggregate to reduce raster file resoloution
image <- aggregate(image, fact = 20)
# check reduced res
res(image)

#convert the raster to points for plotting
image <- rasterToPoints(image)

#Make the points a dataframe for ggplot
df.image <- data.frame(image)

#Make appropriate column headings
colnames(df.image) <- c("Longitude", "Latitude", 'Values')


## spatial grid load

f_grid <- read.csv("kriging/data/grid_data/wgs_84_smaller_grid_points_clipped.csv")







#******************************************************************** ####
# 01 PLOT SAMPLING POINT DATA ####

setwd(dir = "~/OneDrive - Harper Adams University/Data/agronomy/")

a <- ggplot()+ geom_point(data = df.image, 
                     aes(y = Latitude, 
                         x = Longitude, 
                         color = Values),
                     show.legend = FALSE) +
  scale_colour_gradient(
    low = "white",
    high = "darkgrey",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  geom_point(data = dat_beans,  
             aes(x = x, y = y), 
             color="blue", 
             alpha = 1) +
  # ggtitle("Conservation Agriculture Systems Experiment") +
  xlab(element_blank()) + 
  ylab("Latitude") + 
  xlab("Longitude") + 
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(axis.title.x = element_text(size = 12, face = "bold")) + 
  theme(axis.title.y = element_text(size = 12, face = "bold")) +
  labs(subtitle = "Sampling Points") + 
  theme(legend.position="bottom") +
  labs(fill = "Sampling Zones") +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")) +
  coord_sf(xlim = c(-2.6138, -2.604), 
           ylim = c(52.912, 52.917),
           crs = 4326)

a


ggsave(filename = "plots/sampling_points.png", height = 8, width = 6)


##  03 spatial grid ####

b <- ggplot()+ geom_point(data = df.image, aes(y=Latitude, x=Longitude, color=Values),
                     show.legend = FALSE) +
  scale_colour_gradient(
    low = "white",
    high = "darkgrey",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  geom_point(data = f_grid,  aes(x = x, y = y), 
             color="blue", 
             alpha = 1, size = 0.2) +
  # ggtitle("Conservation Agriculture Systems Experiment") +
  xlab(element_blank()) + 
  ylab("Latitude") + 
  xlab("Longitude") + 
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(axis.title.x = element_text(size = 12, face = "bold")) + 
  theme(axis.title.y = element_text(size = 12, face = "bold")) +
  labs(subtitle = "Interpolation Grid") + 
  theme(legend.position="bottom") +
  labs(fill = "Interpolation Grid") +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")) +
  coord_sf(xlim = c(-2.6138, -2.604), 
           ylim = c(52.912, 52.917),
           crs = 4326)

b

ggsave(filename = "plots/interp_grid.png", height = 8, width = 6)


# #open png for file save and define size and resolution
# png(paste("yield/hand.harvest/plots/", "points.grid.plot", ".png", sep=""),
#     width=1000, height=1000, res=150)
# 
# grid.arrange(plot1points, plot1grid, 
#              ncol = 1)
# 
# dev.off()

b <- b + theme(axis.text.y=element_blank()) 

ggarrange(a,b, ncol = 2, nrow = 1)







### Specify the coordinates ####

coordinates(f_grid) <- ~ x + y 
coordinates(dat_beans) <- ~ x + y 
coordinates(dat_wheat) <- ~ Longitude + Latitude 

# check the class 

glimpse(f_grid)
glimpse(dat_beans)
glimpse(dat_wheat)


### Outlier removal ####
#  
#  # install.packages(form)
#  # 
#  # library(formhub)
#  
#  library(data.table)
#  
#  outlierReplace = function(dataframe, cols, rows, newValue = NA) {
#    if (any(rows)) {
#      set(dataframe, rows, cols, newValue)
#    }
#  }
#  
#  outlierReplace(dat_beans2, "xcoord", which(dat_beans2$xcoord > -2.6077), NA)
#  
#  f2_points <- f2_points + ylim(52.9124,52.9154)
#  
#  f2_points
#  
#  grid.arrange(f1_points, f2_points)
#  
#  










#******************************************************************** ####
# Variogram fitting ####



## Beans ####

class(dat_beans)

vario <-  variogram(t_ha ~ 1, dat_beans)


vario.fit = autofitVariogram(
  t_ha ~ 1,
  dat_beans,
  model = c("Ste", "Sph", "Mat", "Exp", "Gau"),
  #The list of variogrammodels that will be tested.
  kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10),
  # Smoothing parameter of the Matern model. Provide a list if you want to check more than one value.
  fix.values = c(NA, NA, NA),
  #nugget, range and sill respectively NA means that the value is not fixed.
  start_vals = c(NA, NA, NA),
  verbose = T
) # if TRUE the function will give extra feedback on the fitting process

plot(vario.fit)


plot(vario, vario.fit$var_model, main = title_exp)


### Wheat ####

vario <-  variogram(t_ha ~ 1, dat_wheat)


vario.fit = autofitVariogram(
  t_ha ~ 1,
  dat_wheat,
  model = c("Ste", "Sph", "Mat", "Exp", "Gau"),
  #The list of variogrammodels that will be tested.
  kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10),
  # Smoothing parameter of the Matern model. Provide a list if you want to check more than one value.
  fix.values = c(NA, NA, NA),
  #nugget, range and sill respectively NA means that the value is not fixed.
  start_vals = c(NA, NA, NA),
  verbose = T
) # if TRUE the function will give extra feedback on the fitting process

plot(vario.fit)


plot(vario, vario.fit$var_model, main = )

 
 
 


 ## 04 change grid CRS ####
 # 
# # BNG to WGS-84
# 
# library(sf)
# # library(lavaan)
# 
# DT1_sf <- st_as_sf(f_grid, coords = c("x","y"),crs=27700)
# 
# class(DT1_sf)
# 
# DT1_sf_4326 <- DT1_sf %>% st_transform(crs = 4326)  
# 
# f_grid$ycoord <- st_coordinates(DT1_sf_4326)[,2]
# f_grid$xcoord <- st_coordinates(DT1_sf_4326)[,1]











#******************************************************************** ####
# Kriging ####



### Beans ####

#Perform ordinary kriging and store results inside object of type "autoKrige" "list" 
kriging_result = autoKrige(t_ha ~ 1, dat_beans, f_grid)

#open png for file save and define size and resolution
png(paste("plots/", "beans_t_ha_krige_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

plot(kriging_result)

dev.off()

class(kriging_result)
summary(kriging_result)

# Cast the Spatial object to a data.frame


beans_data = as.data.frame(kriging_result$krige_output)

names(beans_data)[names(beans_data) == "var1.pred"] <- "Yield"



### Wheat ####

#Perform ordinary kriging and store results inside object of type "autoKrige" "list" 
kriging_result = autoKrige(t_ha ~ 1, dat_wheat, f_grid)

#open png for file save and define size and resolution
png(paste("plots/", "wheat_t_ha_krige_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

plot(kriging_result)

dev.off()

class(kriging_result)
summary(kriging_result)

# Cast the Spatial object to a data.frame


wheat_data = as.data.frame(kriging_result$krige_output)

names(wheat_data)[names(wheat_data) == "var1.pred"] <- "Yield"









#******************************************************************** ####
# PLOTS ####

## Krige plots ####





#### Beans ####

#open png for file save and define size and resolution
png(paste("plots/", "beans_yield_krige_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

b <- ggplot()+ 
  geom_point(data = df.image, 
             aes(y = Latitude,
                 x = Longitude,
                 color = Values),
                     show.legend = FALSE) +
  scale_colour_gradient(
    low = "white",
    high = "darkgrey",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  geom_raster(data = beans_data, aes(x = x, y = y, fill = Yield)) + 
  scale_fill_gradient(low = 'yellow',
                      high = 'darkgreen') +
  ggtitle("Conservation Agriculture Systems Experiment") +
  xlab(element_blank()) + 
  ylab("Latitude") + 
  xlab("Longitude") + 
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(axis.title.x = element_text(size = 12, face = "bold")) + 
  theme(axis.title.y = element_text(size = 12, face = "bold")) +
  labs(subtitle = "Interpolated Yield. Spring Beans var. Lynx 2022") + 
  theme(legend.position="bottom") +
  labs(fill = expression(Yield~(t~ha^{-1}))) +
  theme(legend.position="bottom",
        legend.key.width= unit(1, 'cm')) +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")) +
  coord_sf(xlim = c(-2.6138, -2.604), 
           ylim = c(52.912, 52.917),
           crs = 4326)

dev.off()




#### Wheat ####

#open png for file save and define size and resolution
png(paste("plots/", "wheat_yield_krige_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

w <- ggplot()+ 
  geom_point(data = df.image, 
             aes(y = Latitude,
                 x = Longitude,
                 color = Values),
             show.legend = FALSE) +
  scale_colour_gradient(
    low = "white",
    high = "darkgrey",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  geom_raster(data = wheat_data, aes(x = x, y = y, fill = Yield)) + 
  scale_fill_gradient(low = 'yellow',
                      high = 'darkgreen') +
  ggtitle("Conservation Agriculture Systems Experiment") +
  xlab(element_blank()) + 
  ylab("Latitude") + 
  xlab("Longitude") + 
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(axis.title.x = element_text(size = 12, face = "bold")) + 
  theme(axis.title.y = element_text(size = 12, face = "bold")) +
  labs(subtitle = "Interpolated Yield. Winter Wheat var. Extase 2023") + 
  theme(legend.position="bottom") +
  labs(fill = expression(Yield~(t~ha^{-1}))) +
  theme(legend.position="bottom",
        legend.key.width= unit(1, 'cm')) +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")) +
  coord_sf(xlim = c(-2.6138, -2.604), 
           ylim = c(52.912, 52.917),
           crs = 4326)

dev.off()


# Remove titles and subtitles for both plots
b <- b + theme(plot.title = element_blank(),    # Remove title
               plot.subtitle = element_blank()) # Remove subtitle

# Assuming 'b' and 'w' are ggplot objects
w <- w + theme(plot.title = element_blank(),    # Remove title
               plot.subtitle = element_blank(), # Remove subtitle
               axis.title.y = element_blank(),  # Remove y-axis title
               axis.text.y = element_blank(),   # Remove y-axis text
               axis.ticks.y = element_blank())  # Remove y-axis ticks


ggarrange(b,w, 
          ncol = 2, 
          nrow = 1, 
          common.legend = TRUE,
          legend = "bottom", 
          align = "hv", 
          labels = c("A","B"),
          widths = c(1, 1))  # Ensure equal widths

ggsave(filename = "plots/joint_yield_map.png", width = 10, height = 5)



### plot the krige SD ####


#open png for file save and define size and resolution
png(paste("kriging/plots/beans_plots/", "stdev_krige_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

ggplot()+ geom_point(data = df.image, 
                     aes(y=Latitude, 
                         x=Longitude, 
                         color=Values),
                     show.legend = FALSE) +
  scale_colour_gradient(
    low = "white",
    high = "darkgrey",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  geom_raster(data = ggplot_data, aes(x = x, y = y, fill = var1.stdev)) + 
  scale_fill_gradient(low = 'yellow', high = muted('orange')) +
  ggtitle("Conservation Agriculture Systems Experiment") +
  xlab(element_blank()) + 
  ylab("Latitude") + 
  xlab("Longitude") + 
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(axis.title.x = element_text(size = 12, face = "bold")) + 
  theme(axis.title.y = element_text(size = 12, face = "bold")) +
  labs(subtitle = "Standard Deviation of Interpolated Yield") + 
  theme(legend.position="bottom") +
  labs(fill = "Standard Deviation") +
  theme(legend.position="bottom",
        legend.key.width= unit(1, 'cm')) +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")) +
  coord_sf(xlim = c(-2.6138, -2.604), 
           ylim = c(52.912, 52.917),
           crs = 4326)

dev.off()




### plot the krige variance ####


#open png for file save and define size and resolution
png(paste("kriging/plots/beans_plots/", "variance_krige_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

ggplot()+ geom_point(data = df.image, 
                     aes(y=Latitude, 
                         x=Longitude, 
                         color=Values),
                     show.legend = FALSE) +
  scale_colour_gradient(
    low = "white",
    high = "darkgrey",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  geom_raster(data = ggplot_data, aes(x = x, y = y, fill = var1.var)) + 
  scale_fill_gradient(low = 'yellow', 
                      high = muted('orange')) +
  ggtitle("Conservation Agriculture Systems Experiment") +
  xlab(element_blank()) + 
  ylab("Latitude") + 
  xlab("Longitude") + 
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(axis.title.x = element_text(size = 12, face = "bold")) + 
  theme(axis.title.y = element_text(size = 12, face = "bold")) +
  labs(subtitle = "Variance of Interpolated Yield") + 
  theme(legend.position="bottom") +
  labs(fill = "Variance") +
  theme(legend.position="bottom",
        legend.key.width= unit(1, 'cm')) +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")) +
  coord_sf(xlim = c(-2.6138, -2.604), 
           ylim = c(52.912, 52.917),
           crs = 4326)

dev.off()












#******************************************************************** ####
# 08 TREATMENT COMPARISON ####

coordinates(datCON_beans) <-
  ~ x + y #specify the coordinates in the dataframe

coordinates(datCON_wheat) <-
  ~ Longitude + Latitude #specify the coordinates in the dataframe


coordinates(datCA_beans) <-
  ~ x + y #specify the coordinates in the dataframe

coordinates(datCA_wheat) <-
  ~ Longitude + Latitude #specify the coordinates in the dataframe




### 08.1 CON ####


#### BEANS ####

#Perform ordinary kriging and store results inside object of type "autoKrige" "list"

con.krig_beans = autoKrige(t_ha ~ 1, datCON_beans, f_grid)

#open png for file save and define size and resolution
png(paste("plots/", "beans_con_krige_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

plot(con.krig_beans)

dev.off()


# Cast the Spatial object to a data.frame

con_data_beans = as.data.frame(con.krig$krige_output)

names(con_data_beans)[names(con_data_beans) == "var1.pred"] <- "Yield"


#### WHEAT ####

#Perform ordinary kriging and store results inside object of type "autoKrige" "list"

con.krig_wheat = autoKrige(t_ha ~ 1, datCON_wheat, f_grid)

#open png for file save and define size and resolution
png(paste("plots/", "wheat_con_krige_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

plot(con.krig)

dev.off()


# Cast the Spatial object to a data.frame

con_data_wheat = as.data.frame(con.krig$krige_output)

names(con_data_wheat)[names(con_data_wheat) == "var1.pred"] <- "Yield"






#### plot CON krige ####

#open png for file save and define size and resolution
png(paste("plots/", "beans_con_yield_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

ggplot()+ geom_point(data = df.image, 
                     aes(y=Latitude, 
                         x=Longitude, 
                         color=Values),
                     show.legend = FALSE) +
  scale_colour_gradient(
    low = "white",
    high = "darkgrey",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  geom_raster(data = con_data_beans,
              aes(x = x, y = y, fill = Yield)) +
  scale_fill_gradient(low = 'yellow', 
                      high = muted('darkgreen'),
                      name = "Yield (t/ha)",
                      breaks = c(4, 5, 6, 7, 8, 9),
                      # labels = c("1","2", "3", "4", ""), 
                      limits = c(4,9)) +
  ggtitle("Conservation Agriculture Systems Experiment") +
  xlab(element_blank()) + 
  ylab("Latitude") + 
  xlab("Longitude") + 
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(axis.title.x = element_text(size = 12, face = "bold")) + 
  theme(axis.title.y = element_text(size = 12, face = "bold")) +
  labs(expression(Yield~(t~ha^{-1})), 
       subtitle = "Yield estimation - Conventional") +
  theme(legend.position="bottom",
        legend.key.width= unit(1, 'cm')) +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")) +
  coord_sf(xlim = c(-2.6138, -2.604), 
           ylim = c(52.912, 52.917),
           crs = 4326)

dev.off()



#### plot CON variance ####

#open png for file save and define size and resolution
png(paste("kriging/plots/beans_plots/", "con_yield_variance_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

ggplot()+ geom_point(data = df.image, 
                     aes(y = Latitude,
                         x = Longitude,
                         color = Values),
                     show.legend = FALSE) +
  scale_colour_gradient(
    low = "white",
    high = "darkgrey",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  geom_raster(data = con_data, aes(x = x, y = y, fill = var1.var)) +
  scale_fill_gradient(low = 'yellow', 
                      high = muted('red'),
                      name = "Yield Variance (t/ha)",
                      breaks = c(1, 2, 3),
                      labels = c("1","2", "3"), 
                      limits = c(0.5,3.5)) +
  ggtitle("Conservation Agriculture Systems Experiment") +
  xlab(element_blank()) + 
  ylab("Latitude") + 
  xlab("Longitude") + 
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(axis.title.x = element_text(size = 12, face = "bold")) + 
  theme(axis.title.y = element_text(size = 12, face = "bold")) +
  labs(size = title_exp, 
       subtitle = "Yield Estimation Variance - Conventional") +
  theme(legend.position="bottom",
        legend.key.width= unit(1, 'cm')) +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")) +
  coord_sf(xlim = c(-2.6138, -2.604), 
           ylim = c(52.912, 52.917),
           crs = 4326)

dev.off()




#### plot CON stdev ####

#open png for file save and define size and resolution
png(paste("kriging/plots/beans_plots/", "con_stdev_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

ggplot()+ geom_point(data = df.image, 
                     aes(y = Latitude,
                         x = Longitude,
                         color = Values),
                     show.legend = FALSE) +
  scale_colour_gradient(
    low = "white",
    high = "darkgrey",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  geom_raster(data = con_data, aes(x = x, y = y, fill = var1.stdev)) +
  scale_fill_gradient(low = 'yellow', 
                      high = muted('red'),
                      name = "Standard deviation (t/ha)",
                      breaks = c(1, 2, 3),
                      labels = c("1","2", "3"), 
                      limits = c(0.5,3.5)) +
  ggtitle("Conservation Agriculture Systems Experiment") +
  xlab(element_blank()) + 
  ylab("Latitude") + 
  xlab("Longitude") + 
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(axis.title.x = element_text(size = 12, face = "bold")) + 
  theme(axis.title.y = element_text(size = 12, face = "bold")) +
  labs(size = title_exp, 
       subtitle = "Standard deviation - Conventional") +
  theme(legend.position="bottom",
        legend.key.width= unit(1, 'cm')) +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")) +
  coord_sf(xlim = c(-2.6138, -2.604), 
           ylim = c(52.912, 52.917),
           crs = 4326)

dev.off()






#open png for file save and define size and resolution
# png(paste("yield/hand.harvest/plots/", "con_krige_yield_plot", ".png", sep=""),
#     width=1000, height=1000, res=150)
# 
# ggarrange(cony,conv, ncol = 1, nrow = 2)
# 
# dev.off()







### 08.2 CA ####

#### BEANS ####

#Perform ordinary kriging and store results inside object of type "autoKrige" "list"

ca.krig_beans = autoKrige(t_ha ~ 1, datCA_beans, f_grid)


#open png for file save and define size and resolution
png(paste("plots/", "beans_ca_krige_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

plot(ca.krig)

dev.off()

# Cast the Spatial object to a data.frame

ca_data_beans = as.data.frame(ca.krig$krige_output)

names(ca_data_beans)[names(ca_data_beans) == "var1.pred"] <- "Yield"



#### WHEAT ####

#Perform ordinary kriging and store results inside object of type "autoKrige" "list"

ca.krig_wheat = autoKrige(t_ha ~ 1, datCA_wheat, f_grid)


#open png for file save and define size and resolution
png(paste("plots/", "wheat_ca_krige_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

plot(ca.krig)

dev.off()

# Cast the Spatial object to a data.frame

ca_data_wheat = as.data.frame(ca.krig$krige_output)

names(ca_data_wheat)[names(ca_data_wheat) == "var1.pred"] <- "Yield"






#open png for file save and define size and resolution
png(paste("plots/", "ca_yield_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

ggplot()+ geom_point(data = df.image, 
                     aes(y = Latitude,
                         x = Longitude,
                         color = Values),
                     show.legend = FALSE) +
  scale_colour_gradient(
    low = "white",
    high = "darkgrey",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  geom_raster(data = ca_data, aes(x = x, y = y, fill = Yield)) +
  scale_fill_gradient(low = 'yellow', 
                      high = muted('darkgreen'),
                      name = "Yield (t/ha)",
                      breaks = c(4, 5, 6, 7, 8, 9),
                      # labels = c("1","2", "3", "4", ""), 
                      limits = c(4,9)) +
  ggtitle("Conservation Agriculture Systems Experiment") +
  xlab(element_blank()) + 
  ylab("Latitude") + 
  xlab("Longitude") + 
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(axis.title.x = element_text(size = 12, face = "bold")) + 
  theme(axis.title.y = element_text(size = 12, face = "bold")) +
  labs(size = title_exp, 
       subtitle = "Yield Estimation (t/ha) - Conservation") +
  theme(legend.position="bottom",
        legend.key.width= unit(1, 'cm')) +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")) +
  coord_sf(xlim = c(-2.6138, -2.604), 
           ylim = c(52.912, 52.917),
           crs = 4326)

dev.off()


#open png for file save and define size and resolution
png(paste("kriging/plots/beans_plots/", "ca_yield_variance_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

ggplot()+ geom_point(data = df.image, aes(y = Latitude,
                                          x = Longitude,
                                          color = Values),
                     show.legend = FALSE) +
  scale_colour_gradient(
    low = "white",
    high = "darkgrey",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  geom_raster(data = ca_data, aes(x = x, y = y, fill = var1.var)) +
  # coord_fixed() +
  scale_fill_gradient(low = 'yellow', 
                      high = muted('red'),
                      name = "Yield Variance (t/ha)", 
                      breaks = c(1, 2, 3),
                      labels = c("1","2", "3"), 
                      limits = c(0.5,3.5)) +
  ggtitle("Conservation Agriculture Systems Experiment") +
  xlab(element_blank()) + 
  ylab("Latitude") + 
  xlab("Longitude") + 
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(axis.title.x = element_text(size = 12, face = "bold")) + 
  theme(axis.title.y = element_text(size = 12, face = "bold")) +
  labs(size = title_exp, 
       subtitle = "Yield Estimation Variance - Conservation") +
  theme(legend.position="bottom",
        legend.key.width= unit(1, 'cm')) +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")) +
  coord_sf(xlim = c(-2.6138, -2.604), 
           ylim = c(52.912, 52.917),
           crs = 4326)

dev.off()


# ggarrange(y,v, ncol = 1, nrow = 2)




## 09 Estimation Comparison ####


### BEANS ####

con.value <- con_data_beans$Yield
ca.value <- ca_data_beans$Yield
diff.yield <- con_data_beans$Yield - con_data_beans$ca.value
con_data_beans$ca <- ca.value
con_data_beans$diff.yield <- con_data_beans$Yield - con_data_beans$ca
con_data_beans$pc.inc <- (con_data_beans$diff.yield / con_data_beans$ca) * 100
con_data_beans$ca.value <- ca.value


### WHEAT ####

con.value <- con_data_wheat$Yield
ca.value <- ca_data_wheat$Yield
diff.yield <- con_data_wheat$Yield - con_data_wheat$ca.value
con_data_wheat$ca <- ca.value
con_data_wheat$diff.yield <- con_data_wheat$Yield - con_data_wheat$ca
con_data_wheat$pc.inc <- (con_data_wheat$diff.yield / con_data_wheat$ca) * 100
con_data_wheat$ca.value <- ca.value







#******************************************************************** ####
# PLOTS ####


## BEANS ####

#open png for file save and define size and resolution
png(paste("plots/", "beans_yield_diff_plot", ".png", sep=""),
    width=1000, height=1000, res=150)

b <- ggplot()+ geom_point(data = df.image, aes(y=Latitude, x=Longitude, color=Values),
                     show.legend = FALSE) +
  scale_colour_gradient(
    low = "white",
    high = "darkgrey",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  geom_raster(data = con_data_beans, aes(x = x, y = y, fill = diff.yield)) +
  # coord_fixed() +
  scale_fill_gradient2(low = 'darkgreen', 
                       mid = "yellow", 
                       high = 'darkred',
                       name = expression(Yield~t~ha^{-1}),
                       breaks = c(-2, -1, 0, 1, 2),
                       labels = c(expression(+2~t~ha^{-1}~Conservation),
                                  expression(+1~t~ha^{-1}~Conservation), 
                                  "Equal Yield", 
                                  expression(+1~t~ha^{-1}~Conventional), 
                                  expression(+2~t~ha^{-1}~Conventional))) +
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold")) + 
  theme(axis.title.x = element_text(size = 12, face = "bold")) + 
  theme(axis.title.y = element_text(size = 12, face = "bold")) +
  labs(size = expression(Yield~t~ha^{-1}), 
       y = "Latitude",
       x = "Longitude",
       subtitle = "Yield difference (t/ha)",
       title = "Conservation Agriculture Systems Experiment") + 
  theme(legend.position="bottom",
        legend.key.width= unit(2, 'cm')) +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")) +
  coord_sf(xlim = c(-2.6138, -2.604), 
           ylim = c(52.912, 52.917),
           crs = 4326)

 dev.off()
 
 
 
 
 
 
 
 ## WHEAT ####
 
 #open png for file save and define size and resolution
 png(paste("plots/", "wheat_yield_diff_plot", ".png", sep=""),
     width=1000, height=1000, res=150)
 
w <- ggplot()+ geom_point(data = df.image, aes(y=Latitude, x=Longitude, color=Values),
                      show.legend = FALSE) +
   scale_colour_gradient(
     low = "white",
     high = "darkgrey",
     space = "Lab",
     na.value = "grey50",
     guide = "colourbar",
     aesthetics = "colour") +
   geom_raster(data = con_data_wheat, 
               aes(x = x, y = y, fill = diff.yield)) +
   # coord_fixed() +
  scale_fill_gradient2(low = 'darkgreen', 
                       mid = "yellow", 
                       high = 'darkred',
                       name = expression(Yield~t~ha^{-1}),
                       breaks = c(-2, -1, 0, 1, 2),
                       labels = c(expression(+2~t~ha^{-1}~Conservation),
                                  expression(+1~t~ha^{-1}~Conservation), 
                                  "Equal Yield", 
                                  expression(+1~t~ha^{-1}~Conventional), 
                                  expression(+2~t~ha^{-1}~Conventional))) +
   xlim(-2.6138, -2.604) +
   ylim(52.912, 52.917) +
   theme(plot.title = element_text(size = 14, face = "bold")) + 
   theme(axis.title.x = element_text(size = 12, face = "bold")) + 
   theme(axis.title.y = element_text(size = 12, face = "bold")) +
   labs(size = expression(Yield~t~ha^{-1}), 
        y = "Latitude",
        x = "Longitude",
        subtitle = "Wheat Yield difference (t/ha)",
        title = "Conservation Agriculture Systems Experiment") + 
   theme(legend.position="bottom",
         legend.key.width= unit(2, 'cm')) +
   annotation_north_arrow(
     location = "tl",
     pad_x = unit(0.5, "in"),
     pad_y = unit(0.25, "in"),
     style = north_arrow_fancy_orienteering) +
   annotation_scale(
     location = "br",
     pad_x = unit(1, "cm"),
     pad_y = unit(1, "cm")) +
   coord_sf(xlim = c(-2.6138, -2.604), 
            ylim = c(52.912, 52.917),
            crs = 4326)
 
 w
 
 dev.off()
 
 
 
 
 
 
 ### COMBINED PLOT ####
 
 # Remove titles and subtitles for both plots
 b <- b + theme(plot.title = element_blank(),    # Remove title
                plot.subtitle = element_blank()) # Remove subtitle
 
 # Assuming 'b' and 'w' are ggplot objects
 w <- w + theme(plot.title = element_blank(),    # Remove title
                plot.subtitle = element_blank(), # Remove subtitle
                axis.title.y = element_blank(),  # Remove y-axis title
                axis.text.y = element_blank(),   # Remove y-axis text
                axis.ticks.y = element_blank())  # Remove y-axis ticks
 
 
 ggarrange(b,w, 
           ncol = 2, 
           nrow = 1, 
           common.legend = TRUE,
           legend = "bottom", 
           align = "hv", 
           labels = c("A","B"),
           widths = c(1, 1))  # Ensure equal widths
 
 ggsave(filename = "plots/joint_yield_difference_maps.png", width = 10, height = 5)
 
 
 
 ggarrange(con.krig_beans, ca.krig_beans)
 
 
 
 
 
 ### plot the percentage difference ####
 
 
 #open png for file save and define size and resolution
 png(paste("kriging/plots/beans_plots/", "beans_yield_pc_diff_plot", ".png", sep=""),
     width=1000, height=1000, res=150)


 ggplot()+ geom_point(data = df.image, aes(y=Latitude, x=Longitude, color=Values),
                      show.legend = FALSE) +
   scale_colour_gradient(
     low = "white",
     high = "darkgrey",
     space = "Lab",
     na.value = "grey50",
     guide = "colourbar",
     aesthetics = "colour") +
  geom_raster(data = con_data, aes(x = x, y = y, fill = pc.inc)) + 
  scale_fill_gradient2(
    low = 'darkgreen',
    mid = "yellow",
    high = 'darkred',
    name = "Yield Difference (%)",
    breaks = c(-20, 0, 20, 40, 60),
    labels = c("+20% Conservation",
               "Equal Yield",
               "+20% Conventional",
               "+40% Conventional",
               "+60% Conventional")) +
   xlim(-2.6138, -2.604) +
   ylim(52.912, 52.917) +
   theme(plot.title = element_text(size = 14, face = "bold")) + 
   theme(axis.title.x = element_text(size = 12, face = "bold")) + 
   theme(axis.title.y = element_text(size = 12, face = "bold")) +
   labs(size = title_exp, 
        y = "Latitude",
        x = "Longitude",
        subtitle = "Yield difference (%)",
        title = "Conservation Agriculture Systems Experiment") + 
  theme(legend.position="bottom",
        legend.key.width= unit(2, 'cm')) +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")) +
  coord_sf(xlim = c(-2.6138, -2.604), 
           ylim = c(52.912, 52.917),
           crs = 4326)
 
 dev.off()
                                

mean(ca_data$Yield)
mean(con_data$Yield)
mean(con_data$GMdiff)

aov()

t.test(x = con_data$GM, y = con_data$ca.value, paired = TRUE)




## barplots for krige yield

con <- data_frame(con_data$Yield)
ca <- data_frame(ca_data$Yield)

ca$treatment <- "Conservation"
con$treatment <- "Conventional"

names(ca)[1] <- "yield"
names(con)[1] <- "yield"

treat.yield <- rbind(con, ca)


# Calculates mean, sd, se and IC - block
my_sum <- treat.yield %>%
  group_by(treatment) %>%
  summarise( 
    n=n(),
    mean=mean(yield),
    sd=sd(yield)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

## 02 Graph ##

title_exp <- expression(Yield~Mg~ha^{-1})  # this is the legend title with correct notation


# Error bars represent standard error of the mean
f <- ggplot(my_sum, aes(x=treatment, y=mean,)) + 
  geom_bar(position=position_dodge(), stat="identity") + labs(title= title_exp,x="Treatment", y = "Yield t/ha") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
f + scale_fill_grey(start = 0.9, end = 0.3) + theme_bw() 

f + geom_text(aes(label = mean), vjust = -0.2)







### krige output plots ####

library(ggplot2)
library(ggpubr)
library(magick)

# Load the PNG files as grobs
con_grob_beans <- magick::image_read("plots/beans_con_krige_plot.png")
ca_grob_beans <- magick::image_read("plots/beans_ca_krige_plot.png")

con_grob_wheat <- magick::image_read("plots/wheat_con_krige_plot.png")
ca_grob_wheat <- magick::image_read("plots/wheat_ca_krige_plot.png")

# Convert the images to ggplot objects
p1 <- ggplot() + annotation_custom(grid::rasterGrob(con_grob_beans)) + theme_void()
p2 <- ggplot() + annotation_custom(grid::rasterGrob(ca_grob_beans)) + theme_void()
p3 <- ggplot() + annotation_custom(grid::rasterGrob(con_grob_wheat)) + theme_void()
p4 <- ggplot() + annotation_custom(grid::rasterGrob(ca_grob_wheat)) + theme_void()

ggarrange(p1, p2, p3, p4, 
          ncol = 2, 
          nrow = 2, 
          labels = c("A - Conventional Bean Yield","B - Conservation Bean Yield",
                     "C - Conventional Wheat Yield","D - Conservation Wheat Yield"), 
          label.x = -0.2)

ggsave(filename = "plots/variogram_yields.png")




