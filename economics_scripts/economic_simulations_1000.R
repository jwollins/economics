### Economic data
### J Collins 
### 2024-04-26
###

# MONTE CARLO SIMULATION ####


# ____________________________####
# ~ PACKAGES ####

setwd(dir = "~/Documents/GitHub/economics/")

source(file = "economics_scripts/01_packages.R")

# Load necessary libraries
library(mc2d)      # For Monte Carlo simulation
library(janitor)

library(ppcor) # for Partial Correlation

library(dplyr)

library(ggridges)

# Load stringr package
library(stringr)

# Load required packages
library(ggplot2)
library(maps)

# install.packages("tidyverse")
# library(tidyverse)






# ____________________________####
# DATA ####

setwd(dir = "~/OneDrive - Harper Adams University/Data/economics/")



# ~ Experiment data ####

dat <- read.csv(file = "data/processed_data/summary_economic_data.csv")

glimpse(dat)

ca_dat <- filter(.data = dat, treatment == "Conservation")
con_dat <- filter(.data = dat, treatment == "Conventional")




# ~ make the pc difference table ####

# Summarize only columns from 6 to the last column
summary_df <- dat %>%
  group_by(treatment, crop.x) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(-c(treatment, crop.x), names_to = "variable", values_to = "mean_value")

glimpse(summary_df)

# Extract baseline (Conventional) values for each crop
baseline <- summary_df %>%
  filter(treatment == "Conventional") %>%
  rename(baseline_value = mean_value) %>%
  dplyr::select(crop.x, variable, baseline_value)

# Join and compute % difference by crop
percentage_diff <- summary_df %>%
  left_join(baseline, by = c("crop.x", "variable")) %>%
  mutate(percent_diff = (mean_value - baseline_value) / baseline_value * 100) %>%
  filter(treatment != "Conventional") %>%  # Remove Conventional rows
  dplyr::select(treatment, crop.x, variable, percent_diff)

# View result
print(percentage_diff)





# Group by both treatment and crop.x, then calculate the mean of yield_t_ha
yield_mean <- summary_df %>%
  filter(variable == "yield_t_ha") %>%
  group_by(treatment, crop.x) %>%
  summarize(mean_yield = mean(mean_value))

# Pivot data into wide format to calculate percentage difference by crop.x
yield_wide <- yield_mean %>%
  spread(key = treatment, value = mean_yield)

# Calculate the percentage difference for each crop
yield_wide <- yield_wide %>%
  mutate(percentage_diff = (Conservation - Conventional) / Conventional * 100)

# View the result
yield_wide











# ~ Su et al Global database ####


# load the data from Su et al.
global_dat <- read.csv(file = "data/Su_et_al_database/Database.csv")

unique(global_dat$Site.country)

# Define a vector of European countries
european_countries <- c("Italy", "Spain", "Sweden", "UK", "Poland", "Belgium", "Germany", 
                        "Croatia", "Switzerland", "France", "Denmark", "Romania", "Hungary", 
                        "Czech Republic", "Greece", "Finland", "Norway", "Serbia")

# Filter the data to include only European countries
europe_dat <- global_dat %>% filter(Site.country %in% european_countries)


# Filter the data to include only European countries
uk_dat <- global_dat %>% filter(Site.country %in% "UK")



global_CA_dat <- filter(.data = global_dat, 
                        Crop.rotation.with.at.least.3.crops.involved.in.NT == "Yes" &
                          global_dat$Soil.cover.in.NT == "Yes")

glimpse(global_CA_dat)


unique(global_CA_dat$Crop)

unique(global_CA_dat$Site.country)



# Assuming global_CA_dat is your data
# Plot the map
world_map <- map_data("world")

ggplot() +
  # Plot the world map
  geom_map(data = world_map, 
           map = world_map, 
           aes(x = long,
               y = lat, 
               map_id = region), 
           fill = "lightgray", 
           color = "white", 
           size = 0.1) +
  # Add points for Latitude and Longitude
  geom_point(data = global_CA_dat, 
             aes(x = Longitude, 
                 y = Latitude), 
             color = "blue", 
             size = 1.5) +
  # Labels and title
  theme_minimal() +
  labs(x = "Longitude", 
       y = "Latitude")

ggsave(filename = "plots/simulation_plots/fig_ca_data_world_map.png", width = 8, height = 4)





## ~~ yield trends ####

unique(global_CA_dat$Crop)

correlation <- cor(global_CA_dat$Years.since.NT.started..yrs., 
                   global_CA_dat$Relative.yield.change, 
                   use = "complete.obs")

print(correlation)  # Check if it's a valid numeric value



# get the rate of change per year 

glimpse(global_CA_dat$Relative.yield.change)

lm_model <- lm(Relative.yield.change ~ Years.since.NT.started..yrs., data = global_CA_dat)
summary(lm_model)

ca_start_yield_reduction <- coef(lm_model)[1]  # Extract intercept
ca_yield_change_rate <- coef(lm_model)[2]      # Extract slope


# Compute summary stats
intercept <- coef(lm_model)[1]
slope <- coef(lm_model)[2]
r_squared <- summary(lm_model)$r.squared
mean_yield_change <- mean(global_CA_dat$Relative.yield.change, na.rm = TRUE)
sd_yield_change <- sd(global_CA_dat$Relative.yield.change, na.rm = TRUE)
n_points <- nrow(global_CA_dat)





# ~ plot the yield change rate ####

# If intercept is negative, format it as -value
intercept_label <- if (intercept < 0) {
  paste("-", abs(round(intercept, 4)))
} else {
  paste("+", round(intercept, 4))
}

# Create the equation string dynamically
equation_label <- paste0(round(slope, 4), " * italic(x) ", intercept_label)

# Create plot
yield_corr_plot <-
  ggplot(data = global_CA_dat, 
         aes(x = Years.since.NT.started..yrs., 
             y = Relative.yield.change)) +
  geom_point(alpha = 0.6) +  # Slight transparency to reduce clutter
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  
  # # Add mean and standard deviation as a reference
  # geom_hline(yintercept = mean_yield_change, linetype = "dashed", color = "black") +
  # 
  # annotate("rect", xmin = -Inf, xmax = Inf, 
  #          ymin = mean_yield_change - sd_yield_change, 
  #          ymax = mean_yield_change + sd_yield_change, 
  #          fill = "gray", alpha = 0.2) +
  
  
  annotate("text", 
           x = 38, 
           y = 1.8, 
           label = paste0("italic(R^2) == ", round(r_squared, 2)), 
           size = 5, color = "black", parse = TRUE) +
  
  # Correlation coefficient
  annotate("text", 
           x = 38, 
           y = 1.4, 
           label = paste0("italic(r) == ", round(correlation, 2)), 
           size = 5, color = "black", parse = TRUE) +
  
  # Sample size
  annotate("text", 
           x = 38, 
           y = 1, 
           label = paste0("italic(n) == ", n_points), 
           size = 5, color = "black", parse = TRUE) +
  
  # Labels and theme
  labs(x = "Years since CA started (yrs)", 
       y = "Relative Yield Change (Conservation / Conventional)") +
  ylim(-1, 2.5) +
  theme_bw()

yield_corr_plot



# dir.create(path = "plots/simulation_plots/")

ggsave(filename = "plots/simulation_plots/rel_yield_change.png", width = 8, height = 4)





# ~~ Europe data ####


# # Define a vector of European countries
# european_countries <- c("Italy", "Spain", "Sweden", "UK", "Poland", "Belgium", "Germany", 
#                         "Croatia", "Switzerland", "France", "Denmark", "Romania", "Hungary", 
#                         "Czech Republic", "Greece", "Finland", "Norway", "Serbia")
# 
# # Filter the data to include only European countries
# europe_dat <- global_dat %>% filter(Site.country %in% european_countries)
# 
# 
# europe_CA_dat <- filter(.data = europe_dat, 
#                         Crop.rotation.with.at.least.3.crops.involved.in.NT == "Yes" &
#                           europe_dat$Soil.cover.in.NT == "Yes")
# 
# 
# 
# unique(europe_CA_dat$Crop)
# 
# correlation <- cor(europe_CA_dat$Years.since.NT.started..yrs., 
#                    europe_CA_dat$Relative.yield.change, 
#                    use = "complete.obs")
# 
# print(correlation)  # Check if it's a valid numeric value
# 
# 
# 
# # get the rate of change per year 
# 
# glimpse(europe_CA_dat$Relative.yield.change)
# 
# lm_model <- lm(Relative.yield.change ~ Years.since.NT.started..yrs., 
#                data = europe_CA_dat)
# summary(lm_model)
# 
# # ca_start_yield_reduction <- coef(lm_model)[1]  # Extract intercept
# # ca_yield_change_rate <- coef(lm_model)[2]      # Extract slope
# 
# 
# # Compute summary stats
# intercept <- coef(lm_model)[1]
# slope <- coef(lm_model)[2]
# r_squared <- summary(lm_model)$r.squared
# mean_yield_change <- mean(europe_CA_dat$Relative.yield.change, na.rm = TRUE)
# sd_yield_change <- sd(europe_CA_dat$Relative.yield.change, na.rm = TRUE)
# n_points <- nrow(europe_CA_dat)
# 
# 
# 
# 
# 
# # ~ plot the yield change rate ####
# 
# # If intercept is negative, format it as -value
# intercept_label <- if (intercept < 0) {
#   paste("-", abs(round(intercept, 4)))
# } else {
#   paste("+", round(intercept, 4))
# }
# 
# # Create the equation string dynamically
# equation_label <- paste0(round(slope, 4), " * italic(x) ", intercept_label)
# 
# # Create plot
# europe_yield_corr_plot <-
#   ggplot(data = europe_CA_dat, 
#          aes(x = Years.since.NT.started..yrs., 
#              y = Relative.yield.change)) +
#   geom_point(alpha = 0.6) +  # Slight transparency to reduce clutter
#   geom_smooth(method = "lm", se = TRUE, color = "blue") +
#   
#   # # Add mean and standard deviation as a reference
#   # geom_hline(yintercept = mean_yield_change, linetype = "dashed", color = "black") +
#   # 
#   # annotate("rect", xmin = -Inf, xmax = Inf, 
#   #          ymin = mean_yield_change - sd_yield_change, 
#   #          ymax = mean_yield_change + sd_yield_change, 
#   #          fill = "gray", alpha = 0.2) +
#   
#   
#   annotate("text", 
#            x = 38, 
#            y = 1.8, 
#            label = paste0("italic(R^2) == ", round(r_squared, 2)), 
#            size = 5, color = "black", parse = TRUE) +
#   
#   # Correlation coefficient
#   annotate("text", 
#            x = 38, 
#            y = 1.4, 
#            label = paste0("italic(r) == ", round(correlation, 2)), 
#            size = 5, color = "black", parse = TRUE) +
#   
#   # Sample size
#   annotate("text", 
#            x = 38, 
#            y = 1, 
#            label = paste0("italic(n) == ", n_points), 
#            size = 5, color = "black", parse = TRUE) +
#   
#   # Labels and theme
#   labs(x = "Years since CA started (yrs)", 
#        y = "Relative Yield Change (Conservation / Conventional)") +
#   ylim(-1, 2.5) +
#   theme_bw()
# 
# europe_yield_corr_plot
# 
# 
# 
# # dir.create(path = "plots/simulation_plots/")
# 
# ggsave(filename = "plots/simulation_plots/rel_yield_change_europe.png", width = 8, height = 4)
# 
# 
# 
















# ~ AHDB Yield data UK ####


# Define crop-specific yield parameters for each treatment (mean and sd for each crop in the rotation)

ahdb_dat <- read_excel(path = "data/processed_data/ahdb_mean_yields_2017_21.xlsx")

# Subtract the values in the top_25_pc row from the values in the bottom_25_pc row
yield_sd <- ahdb_dat %>%
  filter(mean_yield_2017_2021 == "top_25_pc") %>%
  dplyr::select(-mean_yield_2017_2021) %>%
  unlist() - 
  ahdb_dat %>%
  filter(mean_yield_2017_2021 == "bottom_25_pc") %>%
  dplyr::select(-mean_yield_2017_2021) %>%
  unlist()

# Add the new row to the original dataset
ahdb_dat <- rbind(ahdb_dat, c("yield_sd", yield_sd))

# View the updated dataset
glimpse(ahdb_dat)




# Assuming ahdb_dat is your original dataframe

# Transpose the dataframe excluding the first row (mean_yield_2017_2021)
ahdb_dat_transposed <- as.data.frame(t(ahdb_dat[, -1]))  # Remove first column (mean_yield_2017_2021)



# View the transposed dataframe
print(ahdb_dat_transposed)

# Set the first row (the yield categories) as column names
colnames(ahdb_dat_transposed) <- c("top_25_pc", "middle_50_pc", "bottom_25_pc", "yield_sd")




# Capitalize words and remove underscores in row names (crop names)
rownames(ahdb_dat_transposed) <- rownames(ahdb_dat_transposed) %>%
  str_replace_all("_", " ") %>%  # Replace underscores with spaces
  str_to_title()  # Capitalize each word

# View the updated dataframe
print(ahdb_dat_transposed)





# Create a LaTeX table
ahdb_table <- ahdb_dat_transposed %>%
  kbl(format = "latex", 
      booktabs = TRUE, 
      caption = "My Table", 
      label = "MyLabel", 
      digits = 2) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"), # Avoid 'tabu'
    full_width = FALSE,                 # Set to FALSE for `tabular`
    font_size = 15                     # Adjust font size for readability
  ) %>%
  row_spec(0, bold = TRUE)

print(ahdb_table)







yield_middle_50_pc <- filter(.data = ahdb_dat, mean_yield_2017_2021 == "middle_50_pc")
yield_top_25_pc <- filter(.data = ahdb_dat, mean_yield_2017_2021 == "top_25_pc")
yield_bottom_25_pc <- filter(.data = ahdb_dat, mean_yield_2017_2021 == "bottom_25_pc")
yield_sd <- filter(.data = ahdb_dat, mean_yield_2017_2021 == "yield_sd")

# Convert all columns except the first to numeric
yield_sd <- yield_sd %>%
  mutate(across(2:ncol(yield_sd), as.numeric))

# Convert all columns except the first to numeric
yield_top_25_pc <- yield_top_25_pc %>%
  mutate(across(2:ncol(yield_top_25_pc), as.numeric))

# Convert all columns except the first to numeric
yield_bottom_25_pc <- yield_bottom_25_pc %>%
  mutate(across(2:ncol(yield_bottom_25_pc), as.numeric))

# Convert all columns except the first to numeric
yield_middle_50_pc <- yield_middle_50_pc %>%
  mutate(across(2:ncol(yield_middle_50_pc), as.numeric))








# ~ Crop price data  ####


crop_price_dat <- read.csv(file = "data/processed_data/ahdb_commodity_prices.csv", skip = 5, header = TRUE)

names(crop_price_dat)

crop_price_dat <- crop_price_dat %>% clean_names()

names(crop_price_dat)

historic_wheat_price <- mean(x = crop_price_dat$bread_wheat_tonne, na.rm = TRUE)
historic_wheat_sd <- sd(x = crop_price_dat$bread_wheat_tonne, na.rm = TRUE)

historic_barley_price <- mean(x = crop_price_dat$feed_barley_tonne, na.rm = TRUE)
historic_barley_sd <- sd(x = crop_price_dat$feed_barley_tonne, na.rm = TRUE)

historic_wosr_price <- mean(x = crop_price_dat$oilseed_rape_tonne, na.rm = TRUE)
historic_wosr_sd <- sd(x = crop_price_dat$oilseed_rape_tonne, na.rm = TRUE)


# NO DATA 

historic_wbeans_price <- 180
historic_wbeans_sd <- 15

historic_peas_price <- 210
historic_peas_sd <- 15



glimpse(crop_price_dat)

# Load necessary libraries
library(dplyr)
library(psych)

# Select only numeric columns from the dataframe
numeric_data <- crop_price_dat %>%
  select_if(is.numeric)

# Compute summary statistics (mean, SD, SEM, and count)
summary_df <- numeric_data %>%
  summarise_all(list(
    mean = ~mean(. , na.rm = TRUE),
    sd = ~sd(. , na.rm = TRUE),
    sem = ~sd(. , na.rm = TRUE) / sqrt(sum(!is.na(.))),
    n = ~sum(!is.na(.))
  ))

# Print the summary dataframe
print(summary_df)





# Create a LaTeX table
ahdb_table <- crop_price_dat %>%
  kbl(format = "latex", booktabs = TRUE, caption = "My Table", label = "MyLabel", digits = 2) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"), # Avoid 'tabu'
    full_width = FALSE,                 # Set to FALSE for `tabular`
    font_size = 15                     # Adjust font size for readability
  ) %>%
  row_spec(0, bold = TRUE)



# ____________________________####
# DISTRIBUTIONS ####


# ~ Histograms ####

# selected_columns <- dat[, c(6:ncol(dat))]
# 
# # Replace all zeros with NA in the entire dataframe
# # dat[dat == 0] <- NA
# 
# # Function to plot histogram and QQ plot for a single variable
# plot_histogram <- function(var) {
#   p1 <- ggplot(dat, aes_string(x = var)) +
#     geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
#     theme_minimal() +
#     labs(title = var, x = var, y = "Frequency")
#   
#   # p2 <- ggqqplot(dat_y1[[var]], title = paste("QQ Plot of", var)) +
#   #   theme_minimal()
#   
#   return(list(p1))
# }
# 
# # Apply function to all selected variables and store the plots
# plots <- lapply(names(selected_columns), plot_histogram)
# 
# # Flatten the list of plots into a single list
# combined_plots <- do.call(c, plots)
# 
# # Arrange all the plots in a grid layout
# ggarrange(plotlist = combined_plots, ncol = 3, nrow = 3)
# 
# # dir.create(path = "plots/distributions/")
# # 
# # ggsave(filename = "plots/distributions/economic_histograms.png")
# 





























# ____________________________####
#  Model Stats ####




# ~ Set n of simulations ####

# Number of simulations
n_sim <- 10000







# ~ Set Crop rotations ####


rotation_conventional <- c("Winter Wheat", "Winter Barley", "Oilseed Rape")

rotation_conservation <- c("Winter Beans", "Winter Wheat", "Spring Barley", 
                           "Oilseed Rape", "Feed Peas", "Winter Wheat")

# Repeat rotations to match a 12-year period
rotation_conventional_full <- rep(rotation_conventional, length.out = 6)
rotation_conservation_full <- rep(rotation_conservation, length.out = 6)


# Randomly assign starting positions in the 12-year rotation
start_years_conventional <- sample(1:6, n_sim, replace = TRUE)
start_years_conservation <- sample(1:6, n_sim, replace = TRUE)

# Select crops based on the random starting position
crop_sequence_conventional <- rotation_conventional_full[start_years_conventional]
crop_sequence_conservation <- rotation_conservation_full[start_years_conservation]






# ~ Crop Yields ####



## ~~ CON crop yield ####


# Rotation: Wheat / barley / rape

mean_yield_wheat_con <- yield_middle_50_pc$winter_wheat  
sd_yield_wheat_con <- yield_sd$winter_wheat   # Standard deviation for wheat yield

mean_yield_wbarley_con <- yield_middle_50_pc$winter_barley  
sd_yield_wbarley_con <-    yield_sd$winter_barley

mean_yield_wosr_con <- yield_middle_50_pc$winter_osr 
sd_yield_wosr_con <- yield_sd$winter_osr 





## ~~ CON crop prices ####

mean_price_wheat_con <- historic_wheat_price
mean_price_wbarley_con <- historic_barley_price
mean_price_wosr_con <- historic_wosr_price






# ~ Crop Yield ####



## ~~ CA crop yield ####


#Rotation: winter beans / winter wheat / spring Barley / oilseed rape / feed peas / winter wheat

mean_yield_wbeans_ca <- yield_middle_50_pc$winter_beans - ca_start_yield_reduction
sd_yield_wbeans_ca <- yield_sd$winter_beans - ca_start_yield_reduction

mean_yield_wheat_ca <- yield_middle_50_pc$winter_wheat  - ca_start_yield_reduction
sd_yield_wheat_ca <- yield_sd$winter_wheat - ca_start_yield_reduction

mean_yield_sbarley_ca <- yield_middle_50_pc$spring_barley - ca_start_yield_reduction
sd_yield_sbarley_ca <- yield_sd$spring_barley - ca_start_yield_reduction

mean_yield_wosr_ca <- yield_middle_50_pc$winter_osr - ca_start_yield_reduction
sd_yield_wosr_ca <- yield_sd$winter_osr - ca_start_yield_reduction

mean_yield_peas_ca <- yield_middle_50_pc$feed_peas - ca_start_yield_reduction
sd_yield_peas_ca <- yield_sd$feed_peas - ca_start_yield_reduction





## ~~ CA crop prices ####

mean_price_wbeans_ca <- historic_wbeans_price
mean_price_wheat_ca <- historic_wheat_price

mean_price_barley_ca <- historic_barley_price
mean_price_wosr_ca <- historic_wosr_price

mean_price_peas_ca <- historic_peas_price









# ~ Expenditure Data ####



# read the ahdb Full economic cost of production by crop for Farmbench middle 50% (£/ha)
ahdb_expenditure_dat <- read.csv(file = "data/processed_data/ahdb_crop_expenditure_middle_50.csv")

# extract the mean and sd of crop expenditure
wheat_expenditure_mean <- mean(x = ahdb_expenditure_dat$winter_wheat, na.rm = TRUE)
wheat_expenditure_sd <- sd(x = ahdb_expenditure_dat$winter_wheat, na.rm = TRUE)

wbarley_expenditure_mean <- mean(x = ahdb_expenditure_dat$winter_barley, na.rm = TRUE)
wbarley_expenditure_sd <- sd(x = ahdb_expenditure_dat$winter_barley, na.rm = TRUE)

wbeans_expenditure_mean <- mean(x = ahdb_expenditure_dat$winter_beans, na.rm = TRUE)
wbeans_expenditure_sd <- sd(x = ahdb_expenditure_dat$winter_beans, na.rm = TRUE)

wosr_expenditure_mean <- mean(x = ahdb_expenditure_dat$winter_oilseed_rape, na.rm = TRUE)
wosr_expenditure_sd <- sd(x = ahdb_expenditure_dat$winter_oilseed_rape, na.rm = TRUE)

peas_expenditure_mean <- mean(x = ahdb_expenditure_dat$feed_peas, na.rm = TRUE)
peas_expenditure_sd <- sd(x = ahdb_expenditure_dat$feed_peas, na.rm = TRUE)

sbarley_expenditure_mean <- mean(x = ahdb_expenditure_dat$spring_barley, na.rm = TRUE)
sbarley_expenditure_sd <- sd(x = ahdb_expenditure_dat$spring_barley, na.rm = TRUE)









# ____________________________####
# Yield ####




# ~ Functions ####

# Conventional 

simulate_yield_trend_con <- function(initial_yield, 
                                     drift, 
                                     volatility, 
                                     years = 6, 
                                     n_sim = 1000) {
  # Create a dataframe with simulations
  sim_data <- expand.grid(Year = 1:years, run = 1:n_sim) %>%
    mutate(Yield = NA)
  
  # Simulate for each run
  for (i in 1:n_sim) {
    yield_series <- numeric(years)
    
    # Add variability to the first-year yield
    yield_series[1] <- initial_yield * (1 + rnorm(1, mean = 0, sd = volatility))  
    
    # Simulate subsequent years
    for (t in 2:years) {
      yield_series[t] <- yield_series[t-1] * (1 + drift + rnorm(1, mean = 0, sd = volatility))
    }
    
    sim_data$Yield[sim_data$run == i] <- yield_series
  }
  
  return(sim_data)
}



# Conservation

simulate_yield_trend_ca <- function(initial_yield, 
                                    drift, 
                                    volatility, 
                                    years = 6, 
                                    n_sim = 1000) {
  # Create a dataframe with simulations
  sim_data <- expand.grid(Year = 1:years, run = 1:n_sim) %>%
    mutate(Yield = NA)
  
  # Simulate for each run
  for (i in 1:n_sim) {
    yield_series <- numeric(years)
    
    # Add variability to the first-year yield
    yield_series[1] <- initial_yield * (1 + rnorm(1, mean = 0, sd = volatility))  
    
    # Simulate subsequent years
    for (t in 2:years) {
      yield_series[t] <- yield_series[t-1] * (1 + drift + rnorm(1, mean = 0, sd = volatility))
    }
    
    sim_data$Yield[sim_data$run == i] <- yield_series
  }
  
  return(sim_data)
}







# ~ Define the long-term yield trend ####

# Define yield improvement rate (0.5-1.5% per year)
yield_drift_con <- 0  
yield_drift_ca <- ca_yield_change_rate

print(ca_yield_change_rate)

# Define yield variability (random shocks per year)
yield_volatility <- 0.03  









# Apply to each crop



# Conventional 
yield_wheat_trend_con_1 <- simulate_yield_trend_con(initial_yield = mean_yield_wheat_con, 
                                              drift = yield_drift_con, 
                                              volatility = yield_volatility, n_sim = 1000)

yield_wbarley_trend_con_1 <- simulate_yield_trend_con(initial_yield = mean_yield_wbarley_con, 
                                                drift = yield_drift_con, 
                                                volatility = yield_volatility, n_sim = 1000)

yield_wosr_trend_con_1 <- simulate_yield_trend_con(initial_yield = mean_yield_wosr_con, 
                                             drift = yield_drift_con, 
                                             volatility = yield_volatility, n_sim = 1000)

yield_wheat_trend_con_2 <- simulate_yield_trend_con(initial_yield = mean_yield_wheat_con, 
                                                    drift = yield_drift_con, 
                                                    volatility = yield_volatility, n_sim = 1000)

yield_wbarley_trend_con_2 <- simulate_yield_trend_con(initial_yield = mean_yield_wbarley_con, 
                                                      drift = yield_drift_con, 
                                                      volatility = yield_volatility, n_sim = 1000)

yield_wosr_trend_con_2 <- simulate_yield_trend_con(initial_yield = mean_yield_wosr_con, 
                                                   drift = yield_drift_con, 
                                                   volatility = yield_volatility, n_sim = 1000)






# Conservation

# THIS ALREADY ACCOUNTS FOR LOWER INITIAL YIELD 

yield_wheat_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_wheat_ca, 
                                             drift = yield_drift_ca, 
                                             volatility = yield_volatility, n_sim = 1000)

yield_wbarley_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_wbarley_con, 
                                               drift = yield_drift_ca, 
                                               volatility = yield_volatility, n_sim = 1000)

yield_wosr_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_wosr_con, 
                                            drift = yield_drift_ca, 
                                            volatility = yield_volatility, n_sim = 1000)

yield_wbeans_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_wbeans_ca, 
                                              drift = yield_drift_ca, 
                                              volatility = yield_volatility, n_sim = 1000)

yield_sbarley_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_sbarley_ca, 
                                               drift = yield_drift_ca, 
                                               volatility = yield_volatility, n_sim = 1000)

yield_fpeas_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_peas_ca, 
                                             drift = yield_drift_ca, 
                                             volatility = yield_volatility, n_sim = 1000)





# ~ Combine both datasets ####


# Add a "Crop" column to each dataset

# Conventional
yield_wheat_trend_con_1$Crop <- "Winter Wheat 1"
yield_wbarley_trend_con_1$Crop <- "Winter Barley 1"
yield_wosr_trend_con_1$Crop <- "Oilseed Rape 1"
yield_wheat_trend_con_2$Crop <- "Winter Wheat 2"
yield_wbarley_trend_con_2$Crop <- "Winter Barley 2"
yield_wosr_trend_con_2$Crop <- "Oilseed Rape 2"


# Conservation
yield_wheat_trend_ca$Crop <- "Winter Wheat"
yield_wbarley_trend_ca$Crop <- "Winter Barley"
yield_wosr_trend_ca$Crop <- "Oilseed Rape"
yield_wbeans_trend_ca$Crop <- "Winter Beans"
yield_sbarley_trend_ca$Crop <- "Spring Barley"
yield_fpeas_trend_ca$Crop <- "Feed Peas"



# Combine all simulated yield datasets
yield_sim_con <- bind_rows(yield_wheat_trend_con_1,
                           yield_wbarley_trend_con_1,
                           yield_wosr_trend_con_1,
                           yield_wheat_trend_con_2,
                           yield_wbarley_trend_con_2,
                           yield_wosr_trend_con_2)
glimpse(yield_sim_con)


yield_sim_ca <- bind_rows(yield_wheat_trend_ca,
                          yield_wbarley_trend_ca,
                           yield_wosr_trend_ca, 
                          yield_wbeans_trend_ca, 
                          yield_sbarley_trend_ca, 
                          yield_fpeas_trend_ca)
glimpse(yield_sim_ca)



# combine both 

yield_sim_all <- rbind(
  data.frame(yield_sim_con, System = "Conventional"),
  data.frame(yield_sim_ca, System = "Conservation")
)

glimpse(yield_sim_all)






# add this to the main df
sim_all <- yield_sim_all



# add a column for the plotting crop names 

sim_all$Crop_Name <- sim_all$Crop

sim_all$Crop_Name <- if_else(
  condition = sim_all$Crop == "Winter Wheat 1" | sim_all$Crop == "Winter Wheat 2", 
  true = "Winter Wheat", false = sim_all$Crop_Name)

sim_all$Crop_Name <- if_else(
  condition = sim_all$Crop == "Oilseed Rape 1" | sim_all$Crop == "Oilseed Rape 2", 
  true = "Oilseed Rape", false = sim_all$Crop_Name)

sim_all$Crop_Name <- if_else(
  condition = sim_all$Crop == "Winter Barley 1" | sim_all$Crop == "Winter Barley 2", 
  true = "Winter Barley", false = sim_all$Crop_Name)

unique(sim_all$Crop_Name)










# ~ summarise the dataset ####


# # Summarize the mean and confidence intervals
# yield_summary_con <- yield_sim_con %>%
#   group_by(Year, Crop) %>%
#   summarise(
#     mean_yield = mean(Yield),
#     ymin = quantile(Yield, 0.025),  # 2.5% percentile (lower bound)
#     ymax = quantile(Yield, 0.975)   # 97.5% percentile (upper bound)
#   )
# 
# 
# # Summarize the mean and confidence intervals
# yield_summary_ca <- yield_sim_ca %>%
#   group_by(Year, Crop) %>%
#   summarise(
#     mean_yield = mean(Yield),
#     ymin = quantile(Yield, 0.025),  # 2.5% percentile (lower bound)
#     ymax = quantile(Yield, 0.975)   # 97.5% percentile (upper bound)
#   )









summary_table <- sim_all %>%
  group_by(System) %>%
  summarise(
    n = n(),  # Add count of observations
    across(where(is.numeric), 
           list(mean = mean, 
                sd = sd, 
                min = min, 
                max = max), 
           na.rm = TRUE)
  )

print(summary_table)






# ~ Plots ####

# Create the plot using facets instead of separate plots

fig_yield_nsim_plot <-
ggplot(sim_all, 
       aes(x = Year, 
           y = Yield, 
           group = interaction(Crop, as.factor(run)))) +
  geom_line(alpha = 0.05, 
            aes(color = Crop_Name), 
            linewidth = 0.5) +
  labs(
       x = "Year",
       y = expression("Yield (t ha"^{-1}~")"), 
       color = "Crop"  # Change legend title here
       ) +
  facet_wrap(~ System, 
             ncol = 2) +
  theme_bw() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 5))) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12, 
                                    # color = "black", 
                                    face = "bold.italic"))

 fig_yield_nsim_plot


# ggsave(plot = fig_yield_nsim_plot,
#        filename = "plots/simulation_plots/fig_yield_nsim_plot.png",
#        width = 10,
#        height = 5)

 


fig_yield_hist_nsim_plot <-
ggplot(sim_all,
       aes(x = Yield,
           fill = Crop_Name)) +
  geom_histogram(bins = 30,
                 alpha = 0.7) +
  facet_wrap(~ System) +
  labs( x = expression("Yield (t ha"^{-1}~")"), 
        y = "Frequency",
        fill = "Crop"  # Change legend title here
  ) +
  theme_bw() +
  theme(legend.position = "bottom", 
        strip.text.x = element_text(size = 12,
                                    # color = "black",
                                    face = "bold.italic"))

fig_yield_hist_nsim_plot

# ggsave(plot = fig_yield_hist_nsim_plot,
#        filename = "plots/simulation_plots/fig_yield_hist_nsim_plot.png",
#        width = 10, height = 5)


fig_joint_yield_sim <-
ggarrange(fig_yield_nsim_plot,
          fig_yield_hist_nsim_plot,
          ncol = 1,
          nrow = 2, 
          common.legend = TRUE, 
          legend = "bottom", 
          align = "hv", labels = c("A","B"))

fig_joint_yield_sim
 
ggsave(plot = fig_joint_yield_sim,
       filename = "plots/simulation_plots/fig_joint_yield_nsim_plot.png",
       width = 8, height = 6)


















# ____________________________####
# Price & Revenue ####




# ~ Functions ####


# Simulate 12 years of price trends for each crop with 1000 simulations
simulate_price_trend <- function(initial_price, 
                                 drift, 
                                 volatility, 
                                 years = 6, 
                                 n_sim = 1000) {
  # Create a dataframe with simulations
  sim_data <- expand.grid(Year = 1:years, run = 1:n_sim) %>%
    mutate(Price = NA)
  
  # Simulate for each run
  for (i in 1:n_sim) {
    price_series <- numeric(years)
    price_series[1] <- initial_price * (1 + rnorm(1, mean = 0, sd = volatility))
    
    for (t in 2:years) {
      price_series[t] <- price_series[t-1] * (1 + drift + rnorm(1, mean = 0, sd = volatility))
    }
    
    sim_data$Price[sim_data$run == i] <- price_series
  }
  
  return(sim_data)
}




# ~ Define Long-Term Price Trends ####

# Define inflation rate (1-2% per year)
price_drift_ca <- 0.035  
price_drift_con <- 0.035 


# Define price volatility (random shocks per year)
price_volatility_ca <- 0.1  # 5% fluctuation per year
price_volatility_con <- 0.1  # 5% fluctuation per year



# Conventional - Simulating Crop Price Trends
price_wheat_trend_con_1 <- simulate_price_trend(initial_price = mean_price_wheat_con, 
                                              drift = price_drift_con, 
                                              volatility = price_volatility_con, 
                                              n_sim = 1000)

price_wbarley_trend_con_1 <- simulate_price_trend(initial_price = mean_price_wbarley_con, 
                                                drift = price_drift_con, 
                                                volatility = price_volatility_con, 
                                                n_sim = 1000)

price_wosr_trend_con_1 <- simulate_price_trend(initial_price = mean_price_wosr_con, 
                                             drift = price_drift_con, 
                                             volatility = price_volatility_con, 
                                             n_sim = 1000)

price_wheat_trend_con_2 <- simulate_price_trend(initial_price = mean_price_wheat_con, 
                                                drift = price_drift_con, 
                                                volatility = price_volatility_con, 
                                                n_sim = 1000)

price_wbarley_trend_con_2 <- simulate_price_trend(initial_price = mean_price_wbarley_con, 
                                                  drift = price_drift_con, 
                                                  volatility = price_volatility_con, 
                                                  n_sim = 1000)

price_wosr_trend_con_2 <- simulate_price_trend(initial_price = mean_price_wosr_con, 
                                               drift = price_drift_con, 
                                               volatility = price_volatility_con, 
                                               n_sim = 1000)




# Conservation - Simulating Crop Price Trends

price_wheat_trend_ca <- simulate_price_trend(initial_price = mean_price_wheat_ca, 
                                             drift = price_drift_ca, 
                                             volatility = price_volatility_ca, 
                                             n_sim = 1000)

price_wbarley_trend_ca <- simulate_price_trend(initial_price = mean_price_barley_ca, 
                                               drift = price_drift_ca, 
                                               volatility = price_volatility_ca, 
                                               n_sim = 1000)

price_wosr_trend_ca <- simulate_price_trend(initial_price = mean_price_wosr_ca, 
                                            drift = price_drift_ca, 
                                            volatility = price_volatility_ca, 
                                            n_sim = 1000)

price_wbeans_trend_ca <- simulate_price_trend(initial_price = mean_price_wbeans_ca, 
                                              drift = price_drift_ca, 
                                              volatility = price_volatility_ca, 
                                              n_sim = 1000)

price_sbarley_trend_ca <- simulate_price_trend(initial_price = mean_price_barley_ca, 
                                               drift = price_drift_ca, 
                                               volatility = price_volatility_ca, 
                                               n_sim = 1000)

price_fpeas_trend_ca <- simulate_price_trend(initial_price = mean_price_peas_ca, 
                                             drift = price_drift_ca, 
                                             volatility = price_volatility_ca, 
                                             n_sim = 1000)






# Add a "Crop" column to each dataset

# Conventional
price_wheat_trend_con_1$Crop <- "Winter Wheat 1"
price_wbarley_trend_con_1$Crop <- "Winter Barley 1"
price_wosr_trend_con_1$Crop <- "Oilseed Rape 1"
price_wheat_trend_con_2$Crop <- "Winter Wheat 2"
price_wbarley_trend_con_2$Crop <- "Winter Barley 2"
price_wosr_trend_con_2$Crop <- "Oilseed Rape 2"


# Conservation
price_wheat_trend_ca$Crop <- "Winter Wheat"
price_wbarley_trend_ca$Crop <- "Winter Barley"
price_wosr_trend_ca$Crop <- "Oilseed Rape"
price_wbeans_trend_ca$Crop <- "Winter Beans"
price_sbarley_trend_ca$Crop <- "Spring Barley"
price_fpeas_trend_ca$Crop <- "Feed Peas"






# ~ combine the df's ####


# Combine all simulated yield datasets
price_sim_con <- bind_rows(price_wheat_trend_con_1,
                           price_wbarley_trend_con_1,
                           price_wosr_trend_con_1,
                           price_wheat_trend_con_2,
                           price_wbarley_trend_con_2,
                           price_wosr_trend_con_2)

glimpse(price_sim_con)


price_sim_ca <- bind_rows(price_wheat_trend_ca,
                          price_wbarley_trend_ca,
                          price_wosr_trend_ca, 
                          price_wbeans_trend_ca, 
                          price_sbarley_trend_ca, 
                          price_fpeas_trend_ca)
glimpse(price_sim_ca)




# check for duplicates 

sim_all %>%
  count(Year, run, Crop, System) %>%
  filter(n > 1)


# Combine both prcie datasets
price_sim_all <- rbind(
  data.frame(price_sim_con, System = "Conventional"),
  data.frame(price_sim_ca, System = "Conservation")
)

glimpse(price_sim_all)
glimpse(sim_all)






# Join price and yield data to main df
sim_all <- sim_all %>%
  left_join(price_sim_all, by = c("Year", "run", "Crop", "System")) 

# Glimpse at the result
glimpse(sim_all)


# ~ Calculate Revenue ####
sim_all$Revenue <- sim_all$Yield * sim_all$Price





# add a column for the plotting crop names 

sim_all$Crop_Name <- sim_all$Crop

sim_all$Crop_Name <- if_else(
  condition = sim_all$Crop == "Winter Wheat 1" | sim_all$Crop == "Winter Wheat 2", 
  true = "Winter Wheat", false = sim_all$Crop_Name)

sim_all$Crop_Name <- if_else(
  condition = sim_all$Crop == "Oilseed Rape 1" | sim_all$Crop == "Oilseed Rape 2", 
  true = "Oilseed Rape", false = sim_all$Crop_Name)

sim_all$Crop_Name <- if_else(
  condition = sim_all$Crop == "Winter Barley 1" | sim_all$Crop == "Winter Barley 2", 
  true = "Winter Barley", false = sim_all$Crop_Name)

unique(sim_all$Crop_Name)






# ~ Price Plots ####

fig_price_nsim_plot <-
ggplot(sim_all, 
       aes(x = Year, 
           y = Price, 
           group = interaction(Crop, as.factor(run)))) +
  geom_line(alpha = 0.05, 
            aes(color = Crop_Name), 
            linewidth = 0.5) +
  labs(
    x = "Year",
    y = expression("Price (£ t"^{-1}~")"), 
    color = "Crop") +
  facet_wrap(~ System,
             ncol = 2) +
  theme_bw() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 5))) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12, 
                                    # color = "black", 
                                    face = "bold.italic"))


fig_price_nsim_plot


# ggsave(plot = fig_price_nsim_plot,
#        filename = "plots/simulation_plots/fig_price_nsim_plot.png", 
#        width = 10, 
#        height = 5)



fig_price_hist_nsim_plot <-
  ggplot(sim_all,
         aes(x = Price,
             fill = Crop_Name)) +
  geom_histogram(bins = 30,
                 alpha = 0.7) +
  facet_wrap(~ System) +
  labs( x = expression("Price (£ t"^{-1}~")"), 
        y = "Frequency",
        fill = "Crop"  # Change legend title here
  ) +
  theme_bw() +
  theme(legend.position = "bottom", 
        strip.text.x = element_text(size = 12,
                                    # color = "black",
                                    face = "bold.italic"))

fig_price_hist_nsim_plot

# ggsave(plot = fig_yield_hist_nsim_plot,
#        filename = "plots/simulation_plots/fig_yield_hist_nsim_plot.png",
#        width = 10, height = 5)


fig_joint_price_sim <-
  ggarrange(fig_price_nsim_plot,
            fig_price_hist_nsim_plot,
            ncol = 1,
            nrow = 2, 
            common.legend = TRUE, 
            legend = "bottom", 
            align = "hv", 
            labels = c("A","B"))

fig_joint_price_sim

ggsave(plot = fig_joint_price_sim,
       filename = "plots/simulation_plots/fig_joint_price_nsim_plot.png",
       width = 8, height = 6)




# ggplot(data = sim_all, 
#        aes(x = System, 
#            y = Price, 
#            fill = System)) +
#   geom_violin(alpha = 0.4, color = NA) +  # Transparent violin for density
#   geom_boxplot(width = 0.2, outlier.alpha = 0.2) +  # Compact box plot overlay
#   theme_minimal() +
#   labs(title = "Simulated price Distribution by System",
#        x = "Crop",
#        y = "Price (£/t)") +
#   scale_fill_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), 
#         legend.position = "bottom") 







# ~ Revenue Plots ####

fig_Revenue_nsim_plot <-
  ggplot(sim_all, 
         aes(x = Year, 
             y = Revenue, 
             group = interaction(Crop, as.factor(run)))) +
  geom_line(alpha = 0.02, 
            aes(color = Crop_Name), 
            linewidth = 0.5) +
  labs(
    x = "Year",
    y = expression("Revenue (£ t"^{-1}~")"), 
    color = "Crop") +
  facet_wrap(~ System,
             ncol = 2) +
  theme_bw() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 5))) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12, 
                                    # color = "black", 
                                    face = "bold.italic"))


fig_Revenue_nsim_plot


fig_Revenue_hist_nsim_plot <-
  ggplot(sim_all,
         aes(x = Revenue,
             fill = Crop_Name)) +
  geom_histogram(bins = 30,
                 alpha = 0.6) +
  facet_wrap(~ System) +
  labs( x = expression("Revenue (£ t"^{-1}~")"), 
        y = "Frequency",
        fill = "Crop"  # Change legend title here
  ) +
  theme_bw() +
  theme(legend.position = "bottom", 
        strip.text.x = element_text(size = 12,
                                    # color = "black",
                                    face = "bold.italic"))

fig_Revenue_hist_nsim_plot


fig_joint_Revenue_sim <-
  ggarrange(fig_Revenue_nsim_plot,
            fig_Revenue_hist_nsim_plot,
            ncol = 1,
            nrow = 2, 
            common.legend = TRUE, 
            legend = "bottom", 
            align = "hv", 
            labels = c("A","B"))

fig_joint_Revenue_sim

ggsave(plot = fig_joint_Revenue_sim,
       filename = "plots/simulation_plots/fig_joint_Revenue_nsim_plot.png",
       width = 8, height = 6)



# boxplot 

fig_revenue_box_plot <-
  ggplot(data = sim_all, 
         aes(x = System, 
             y = Revenue, 
             fill = System)) +
  geom_violin(alpha = 0.4, color = NA) +  # Transparent violin for density
  geom_boxplot(width = 0.2, outlier.alpha = 0.2) +  # Compact box plot overlay
  theme_minimal() +
  labs(
    x = "Treatment",
    y = "Crop Rotation Revenue (£/ha)") +
  scale_fill_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
  theme(axis.text.x = element_blank(), 
        legend.position = "bottom") 

fig_revenue_box_plot































# ____________________________####
# Expenditure ####




# ~ Define Inflation on Expenditure ####

# Define annual expenditure inflation rate (2-4%)
expenditure_inflation <- 0.035  

# Define random market fluctuation (±2%)
expenditure_volatility <- 0.05  






# ~ Function ####

simulate_expenditure_trend <- function(initial_expenditure, 
                                       expenditure_mean, 
                                       expenditure_sd, 
                                       inflation, 
                                       volatility, 
                                       years = 6, 
                                       n_sim = 1000) {
  # Create a dataframe for storing the simulation results
  sim_data <- expand.grid(Year = 1:years, run = 1:n_sim) %>%
    mutate(Expenditure = NA)
  
  # Simulate for each run
  for (i in 1:n_sim) {
    expenditure_series <- numeric(years)
    expenditure_series[1] <- initial_expenditure * (1 + rnorm(1, mean = 0, sd = volatility))
    
    for (t in 2:years) {
      # Expenditure grows with inflation and is adjusted by volatility
      growth_factor <- (1 + inflation) + rnorm(1, mean = 0, sd = volatility)
      expenditure_series[t] <- expenditure_series[t-1] * growth_factor
    }
    
    sim_data$Expenditure[sim_data$run == i] <- expenditure_series
  }
  
  return(sim_data)
}







# ~ Run the expenditure simulation ####



# Conventional

expen_wheat_trend_con_1 <- simulate_expenditure_trend(
  initial_expenditure = wheat_expenditure_mean, 
  expenditure_mean = wheat_expenditure_mean, 
  expenditure_sd = wheat_expenditure_sd, 
  inflation = expenditure_inflation, 
  volatility = expenditure_volatility)

expen_wbarley_trend_con_1 <- simulate_expenditure_trend(
  initial_expenditure =  wbarley_expenditure_mean, 
  expenditure_mean = wbarley_expenditure_mean, 
  expenditure_sd = wbarley_expenditure_sd, 
  inflation = expenditure_inflation, 
  volatility = expenditure_volatility)

expen_wosr_trend_con_1 <- simulate_expenditure_trend(
  initial_expenditure = wosr_expenditure_mean, 
  expenditure_mean = wosr_expenditure_mean, 
  expenditure_sd = wosr_expenditure_sd, 
  inflation = expenditure_inflation, 
  volatility = expenditure_volatility)

expen_wheat_trend_con_2 <- simulate_expenditure_trend(
  initial_expenditure = wheat_expenditure_mean, 
  expenditure_mean = wheat_expenditure_mean, 
  expenditure_sd = wheat_expenditure_sd, 
  inflation = expenditure_inflation, 
  volatility = expenditure_volatility)

expen_wbarley_trend_con_2 <- simulate_expenditure_trend(
  initial_expenditure =  wbarley_expenditure_mean, 
  expenditure_mean = wbarley_expenditure_mean, 
  expenditure_sd = wbarley_expenditure_sd, 
  inflation = expenditure_inflation, 
  volatility = expenditure_volatility)

expen_wosr_trend_con_2 <- simulate_expenditure_trend(
  initial_expenditure = wosr_expenditure_mean, 
  expenditure_mean = wosr_expenditure_mean, 
  expenditure_sd = wosr_expenditure_sd, 
  inflation = expenditure_inflation, 
  volatility = expenditure_volatility)






# Conservation


# Simulate expenditure trends for CA treatment with 1000 simulations
expen_wheat_trend_ca <- simulate_expenditure_trend(
  initial_expenditure = wheat_expenditure_mean * (1 - 0.2), 
  expenditure_mean = wheat_expenditure_mean * (1 - 0.2), 
  expenditure_sd = wheat_expenditure_sd * (1 - 0.2), 
  inflation = expenditure_inflation, 
  volatility = expenditure_volatility)

expen_wbarley_trend_ca <- simulate_expenditure_trend(
  initial_expenditure = wbarley_expenditure_mean * (1 - 0.2), 
  expenditure_mean = wbarley_expenditure_mean * (1 - 0.2), 
  expenditure_sd = wbarley_expenditure_sd * (1 - 0.2), 
  inflation = expenditure_inflation, 
  volatility = expenditure_volatility)

expen_wosr_trend_ca <- simulate_expenditure_trend(
  initial_expenditure = wosr_expenditure_mean * (1 - 0.2), 
  expenditure_mean = wosr_expenditure_mean * (1 - 0.2), 
  expenditure_sd = wosr_expenditure_sd * (1 - 0.2), 
  inflation = expenditure_inflation, 
  volatility = expenditure_volatility)

expen_wbeans_trend_ca <- simulate_expenditure_trend(
  initial_expenditure = wbeans_expenditure_mean * (1 - 0.2), 
  expenditure_mean = wbeans_expenditure_mean * (1 - 0.2), 
  expenditure_sd = wbeans_expenditure_sd * (1 - 0.2), 
  inflation = expenditure_inflation, 
  volatility = expenditure_volatility)

expen_sbarley_trend_ca <- simulate_expenditure_trend(
  initial_expenditure = sbarley_expenditure_mean * (1 - 0.2), 
  expenditure_mean = sbarley_expenditure_mean * (1 - 0.2), 
  expenditure_sd = sbarley_expenditure_sd * (1 - 0.2), 
  inflation = expenditure_inflation, 
  volatility = expenditure_volatility)

expen_fpeas_trend_ca <- simulate_expenditure_trend(
  initial_expenditure = peas_expenditure_mean * (1 - 0.2), 
  expenditure_mean = peas_expenditure_mean * (1 - 0.2), 
  expenditure_sd = peas_expenditure_sd * (1 - 0.2), 
  inflation = expenditure_inflation, 
  volatility = expenditure_volatility)




# ~ Combine the df'ds ####

# Add a "Crop" column to each dataset

# Conventional
expen_wheat_trend_con_1$Crop <- "Winter Wheat 1"
expen_wbarley_trend_con_1$Crop <- "Winter Barley 1"
expen_wosr_trend_con_1$Crop <- "Oilseed Rape 1"
expen_wheat_trend_con_2$Crop <- "Winter Wheat 2"
expen_wbarley_trend_con_2$Crop <- "Winter Barley 2"
expen_wosr_trend_con_2$Crop <- "Oilseed Rape 2"


# Conservation
expen_wheat_trend_ca$Crop <- "Winter Wheat"
expen_wbarley_trend_ca$Crop <- "Winter Barley"
expen_wosr_trend_ca$Crop <- "Oilseed Rape"
expen_wbeans_trend_ca$Crop <- "Winter Beans"
expen_sbarley_trend_ca$Crop <- "Spring Barley"
expen_fpeas_trend_ca$Crop <- "Feed Peas"


# Combine all simulated yield datasets
expen_sim_con <- bind_rows(expen_wheat_trend_con_1,
                           expen_wbarley_trend_con_1,
                           expen_wosr_trend_con_1,
                           expen_wheat_trend_con_2,
                           expen_wbarley_trend_con_2,
                           expen_wosr_trend_con_2)


expen_sim_ca <- bind_rows(expen_wheat_trend_ca,
                          expen_wbarley_trend_ca,
                          expen_wosr_trend_ca, 
                          expen_wbeans_trend_ca, 
                          expen_sbarley_trend_ca, 
                          expen_fpeas_trend_ca)


# Combine both datasets
expen_sim_all <- rbind(
  data.frame(expen_sim_con, System = "Conventional"),
  data.frame(expen_sim_ca, System = "Conservation")
)

glimpse(expen_sim_all)



# ~ combine to main df ####

# Join price and yield data to main df
sim_all <- sim_all %>%
  left_join(expen_sim_all, by = c("Year", "run", "Crop", "System")) 

# Glimpse at the result
glimpse(sim_all)




# add a column for the plotting crop names 

sim_all$Crop_Name <- sim_all$Crop

sim_all$Crop_Name <- if_else(
  condition = sim_all$Crop == "Winter Wheat 1" | sim_all$Crop == "Winter Wheat 2", 
  true = "Winter Wheat", false = sim_all$Crop_Name)

sim_all$Crop_Name <- if_else(
  condition = sim_all$Crop == "Oilseed Rape 1" | sim_all$Crop == "Oilseed Rape 2", 
  true = "Oilseed Rape", false = sim_all$Crop_Name)

sim_all$Crop_Name <- if_else(
  condition = sim_all$Crop == "Winter Barley 1" | sim_all$Crop == "Winter Barley 2", 
  true = "Winter Barley", false = sim_all$Crop_Name)

unique(sim_all$Crop_Name)










# ~ Plots ####

names(sim_all)

fig_Expenditure_nsim_plot <-
  ggplot(sim_all, 
         aes(x = Year, 
             y = Expenditure, 
             group = interaction(Crop, as.factor(run)))) +
  geom_line(alpha = 0.02, 
            aes(color = Crop_Name), 
            linewidth = 0.5) +
  labs(
    x = "Year",
    y = expression("Expenditure (£ ha"^{-1}~")"), 
    color = "Crop") +
  facet_wrap(~ System,
             ncol = 2) +
  theme_bw() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 5))) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12, 
                                    # color = "black", 
                                    face = "bold.italic"))


fig_Expenditure_nsim_plot


fig_Expenditure_hist_nsim_plot <-
  ggplot(sim_all,
         aes(x = Expenditure,
             fill = Crop_Name)) +
  geom_histogram(bins = 30,
                 alpha = 0.6) +
  facet_wrap(~ System) +
  labs( x = expression("Expenditure (£ ha"^{-1}~")"), 
        y = "Frequency",
        fill = "Crop"  # Change legend title here
  ) +
  theme_bw() +
  theme(legend.position = "bottom", 
        strip.text.x = element_text(size = 12,
                                    # color = "black",
                                    face = "bold.italic"))

fig_Expenditure_hist_nsim_plot


fig_joint_Expenditure_sim <-
  ggarrange(fig_Expenditure_nsim_plot,
            fig_Expenditure_hist_nsim_plot,
            ncol = 1,
            nrow = 2, 
            common.legend = TRUE, 
            legend = "bottom", 
            align = "hv", 
            labels = c("A","B"))

fig_joint_Expenditure_sim

ggsave(plot = fig_joint_Expenditure_sim,
       filename = "plots/simulation_plots/fig_joint_Expenditure_nsim_plot.png",
       width = 8, height = 6)




fig_expenditure_box_plot <-
  ggplot(data = sim_all, 
         aes(x = System, 
             y = Expenditure, 
             fill = System)) +
  geom_violin(alpha = 0.4, color = NA) +  # Transparent violin for density
  geom_boxplot(width = 0.2, outlier.alpha = 0.2) +  # Compact box plot overlay
  theme_minimal() +
  labs(
    x = "Treatment",
    y = "Expenditure") +
  scale_fill_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
  theme(axis.text.x = element_blank(), 
        legend.position = "bottom") 

 fig_expenditure_box_plot

 
 
# ggsave(plot = fig_revenue_box_plot, 
#        filename = "plots/simulation_plots/fig_expenditure_box_plot.png")


# ggarrange(fig_expen_nsim_plot, fig_expenditure_box_plot, 
#           ncol = 2, nrow = 1, labels = c("A", "B"), align = "h")
# 
# ggsave(filename = "plots/simulation_plots/fig_expenditure_sim_joint_plot.png", 
#        width = 8, height = 6)







 # ____________________________####
# Gross Margin ####

 
# ~ Calculate GM ####
 
sim_all$Gross_Margin <- sim_all$Revenue - sim_all$Expenditure

glimpse(sim_all)


# ~ Calculate cumulative GM ####


# Compute cumulative gross margin for each run
sim_all_cumulative <- sim_all %>%
  group_by(run, System, Year) %>%  
  summarise(Yearly_GM = sum(Gross_Margin, na.rm = TRUE), .groups = "drop") %>%  
  arrange(run, System, Year) %>%  
  group_by(run, System) %>%  
  mutate(Cumulative_Gross_Margin = cumsum(Yearly_GM)) %>%  
  ungroup()

# Compute summary statistics (mean and quantiles)
sim_all_cumulative_summary <- sim_all_cumulative %>%
  group_by(System, Year) %>%
  summarise(
    mean_CGM = mean(Cumulative_Gross_Margin, na.rm = TRUE),
    lower_CGM = quantile(Cumulative_Gross_Margin, 0.05, na.rm = TRUE),  # 5th percentile
    upper_CGM = quantile(Cumulative_Gross_Margin, 0.95, na.rm = TRUE),  # 95th percentile
    .groups = "drop"
  )






# ~ Plots ####

names(sim_all)

fig_Gross_Margin_nsim_plot <-
  ggplot(sim_all, 
         aes(x = Year, 
             y = Gross_Margin, 
             group = interaction(Crop, as.factor(run)))) +
  geom_line(alpha = 0.02, 
            aes(color = Crop_Name), 
            linewidth = 0.5) +
  labs(
    x = "Year",
    y = expression("Gross_Margin (£ ha"^{-1}~")"), 
    color = "Crop") +
  facet_wrap(~ System,
             ncol = 2) +
  theme_bw() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 5))) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12, 
                                    # color = "black", 
                                    face = "bold.italic"))


fig_Gross_Margin_nsim_plot


fig_Gross_Margin_hist_nsim_plot <-
  ggplot(sim_all,
         aes(x = Gross_Margin,
             fill = Crop_Name)) +
  geom_histogram(bins = 30,
                 alpha = 0.6) +
  facet_wrap(~ System) +
  labs( x = expression("Gross_Margin (£ ha"^{-1}~")"), 
        y = "Frequency",
        fill = "Crop"  # Change legend title here
  ) +
  theme_bw() +
  theme(legend.position = "bottom", 
        strip.text.x = element_text(size = 12,
                                    # color = "black",
                                    face = "bold.italic"))

fig_Gross_Margin_hist_nsim_plot


fig_joint_Gross_Margin_sim <-
  ggarrange(fig_Gross_Margin_nsim_plot,
            fig_Gross_Margin_hist_nsim_plot,
            ncol = 1,
            nrow = 2, 
            common.legend = TRUE, 
            legend = "bottom", 
            align = "hv", 
            labels = c("A","B"))

fig_joint_Gross_Margin_sim

ggsave(plot = fig_joint_Gross_Margin_sim,
       filename = "plots/simulation_plots/fig_joint_Gross_Margin_nsim_plot.png",
       width = 8, height = 6)








# Plot cumulative gross margin with simulations and summary statistics
fig_gm_cumulative_plot <-
  ggplot() +
  
  # Add individual simulation lines (original data)
  geom_line(data = sim_all_cumulative, 
            aes(x = Year, 
                y = Cumulative_Gross_Margin, 
                group = as.factor(run), 
                color = System),
            alpha = 0.05, 
            linewidth = 0.5) +
  
  # Add uncertainty band (5th–95th percentile)
  geom_ribbon(data = sim_all_cumulative_summary, 
              aes(x = Year, 
                  ymin = lower_CGM, 
                  ymax = upper_CGM), 
              alpha = 0, color = "black", 
              linetype = "dotted") +
  
  # Add mean cumulative gross margin line
  geom_line(data = sim_all_cumulative_summary, 
            aes(x = Year, 
                y = mean_CGM, 
                color = "black"), 
            linewidth = 1, 
            linetype = "dashed") +
  
  # Custom colors
  scale_color_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
  
  # Labels and theme
  labs(
    x = "Year",
    y = "Cumulative Gross Margin (£/ha)") +
  facet_wrap(~ System, ncol = 2) +
  theme_bw() + 
  theme(legend.position = "bottom",  
        strip.text.x = element_text(size = 12, face = "bold.italic"))

fig_gm_cumulative_plot


ggsave(filename = "plots/simulation_plots/fig_GM_cumulative_joint_plot.png", 
       plot = fig_gm_cumulative_plot, 
       width = 8, height = 5)











fig_gm_box_plot <-
  ggplot(data = sim_all, 
         aes(x = System, 
             y = Gross_Margin, 
             fill = System)) +
  geom_violin(alpha = 0.4, color = NA) +  # Transparent violin for density
  geom_boxplot(width = 0.2, outlier.alpha = 0.2) +  # Compact box plot overlay
  theme_minimal() +
  labs(
    x = "Treatment",
    y = "Gross Margin") +
  scale_fill_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
  theme(axis.text.x = element_blank(), 
        legend.position = "bottom") 

 fig_gm_box_plot
# 
# ggsave(plot = fig_gm_box_plot, filename = "plots/simulation_plots/fig_gm_box_plot.png")








# ____________________________####
# Climate-Driven Yield Shocks ####


# ~ Define Climate Shocks ####


# Probability of a climate shock in each year (increasing over time)
climate_shock_prob_con <- seq(0.05, 0.20, length.out = 6)  # 5% in Year 1, 20% in Year 12

climate_shock_prob_ca <- seq(0.05, 0.20, length.out = 6)  # 5% in Year 1, 20% in Year 12

# Define severity of yield reduction (10% to 30%)
climate_shock_severity_con <- function() runif(1, min = 0.1, max = 0.3)  

climate_shock_severity_ca <- function() runif(1, min = 0.05, max = 0.15)  





# ~ Function ####

glimpse(sim_all)


apply_climate_shocks <- function(data, key, shock_prob, shock_severity) {
  Year <- key$Year  # Extract Year from the grouping metadata
  
  data %>%
    mutate(
      Climate_Shock = runif(n()) < shock_prob[Year],  # Use extracted Year
      Shock_Factor = ifelse(Climate_Shock, 1 - shock_severity(), 1),  
      Yield_Shocked = Yield * Shock_Factor,  
      Revenue_Shocked = Yield_Shocked * Price
    )
}






# ~ Apply function ####


# Apply shocks to each crop

set.seed(123)  # For reproducibility

names(sim_all)



# Apply to conventional and conservation systems separately
revenue_sim_all_con <- sim_all %>%
  filter(System == "Conventional") %>%
  group_by(Year, run) %>%
  group_modify(~ apply_climate_shocks(.x, .y, climate_shock_prob_con, climate_shock_severity_con)) %>%
  ungroup()

revenue_sim_all_ca <- sim_all %>%
  filter(System == "Conservation") %>%
  group_by(Year, run) %>%
  group_modify(~ apply_climate_shocks(.x, .y, climate_shock_prob_ca, climate_shock_severity_ca)) %>%
  ungroup()

# Combine datasets
revenue_sim_all_shocked <- bind_rows(revenue_sim_all_con, revenue_sim_all_ca)

glimpse(revenue_sim_all_shocked)
glimpse(sim_all)





# Define the key columns for sorting
key_columns <- c("Year", "run", "Crop", "System")

sim_all <- sim_all %>%
  left_join(revenue_sim_all_shocked %>% 
              dplyr::select(Year, run, Crop, System, Revenue_Shocked, Yield_Shocked, Shock_Factor, Climate_Shock), 
            by = key_columns)







# add a column for the plotting crop names 

unique(sim_all$Crop)

sim_all$Crop_Name <- sim_all$Crop

sim_all$Crop_Name <- if_else(
  condition = sim_all$Crop == "Winter Wheat 1" | sim_all$Crop == "Winter Wheat 2", 
  true = "Winter Wheat", false = sim_all$Crop_Name)

sim_all$Crop_Name <- if_else(
  condition = sim_all$Crop == "Oilseed Rape 1" | sim_all$Crop == "Oilseed Rape 2", 
  true = "Oilseed Rape", false = sim_all$Crop_Name)

sim_all$Crop_Name <- if_else(
  condition = sim_all$Crop == "Winter Barley 1" | sim_all$Crop == "Winter Barley 2", 
  true = "Winter Barley", false = sim_all$Crop_Name)

unique(sim_all$Crop_Name)







# ~ Plots ####

names(sim_all)

fig_Yield_Shocked_nsim_plot <-
  ggplot(sim_all, 
         aes(x = Year, 
             y = Yield_Shocked, 
             group = interaction(Crop, as.factor(run)))) +
  geom_line(alpha = 0.02, 
            aes(color = Crop_Name), 
            linewidth = 0.5) +
  labs(
    x = "Year",
    y = expression("Yield Shocked (t ha"^{-1}~")"), 
    color = "Crop") +
  facet_wrap(~ System,
             ncol = 2) +
  theme_bw() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 5))) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12, 
                                    # color = "black", 
                                    face = "bold.italic"))


fig_Yield_Shocked_nsim_plot


fig_Yield_Shocked_hist_nsim_plot <-
  ggplot(sim_all,
         aes(x = Yield_Shocked,
             fill = Crop_Name)) +
  geom_histogram(bins = 30,
                 alpha = 0.6) +
  facet_wrap(~ System) +
  labs( x = expression("Yield Shocked (t ha"^{-1}~")"), 
        y = "Frequency",
        fill = "Crop"  # Change legend title here
  ) +
  theme_bw() +
  theme(legend.position = "bottom", 
        strip.text.x = element_text(size = 12,
                                    # color = "black",
                                    face = "bold.italic"))

fig_Yield_Shocked_hist_nsim_plot


fig_joint_Yield_Shocked_sim <-
  ggarrange(fig_Yield_Shocked_nsim_plot,
            fig_Yield_Shocked_hist_nsim_plot,
            ncol = 1,
            nrow = 2, 
            common.legend = TRUE, 
            legend = "bottom", 
            align = "hv", 
            labels = c("A","B"))

fig_joint_Yield_Shocked_sim

ggsave(plot = fig_joint_Yield_Shocked_sim,
       filename = "plots/simulation_plots/fig_joint_Yield_Shocked_nsim_plot.png",
       width = 8, height = 6)











# ____________________________####
# Inflation ####




# # ~ Define Inflation on Expenditure ####
# 
# # Define annual expenditure inflation rate (2-4%)
# expenditure_inflation <- 0.02  
# 
# # Define random market fluctuation (±2%)
# expenditure_volatility <- 0.02  
# 
# 
# 
# 
# 
# # ~ Function ####
# 
# # Define the function to apply inflation correctly
# 
# apply_inflation <- function(data, inflation_rate, volatility_rate) {
#   # Get the number of rows in the data
#   n_rows <- nrow(data)
#   
#   # Calculate the inflation factor with a random market fluctuation for each row
#   inflation_factor <- (1 + inflation_rate) * (1 + runif(n_rows, -volatility_rate, volatility_rate))
#   
#   # Apply inflation to the revenue (adjusting by inflation factor)
#   data %>%
#     mutate(
#       Revenue_Adjusted = Revenue_Shocked * inflation_factor,  # Apply inflation to shocked revenue
#       Expenditure_Adjusted = Expenditure * inflation_factor,  # Apply inflation to shocked revenue
#     )
# }
# 
# 
# 
# 
# 
# # ~ Apply inflation function ####
# 
# 
# 
# # Apply to the dataset with climate shocks
# sim_all <- sim_all %>%
#   group_by(Year, run) %>%
#   group_modify(~ apply_inflation(.x, expenditure_inflation, expenditure_volatility)) %>%
#   ungroup()
# 
# 
# # Check the result
# 
# glimpse(sim_all)
# 
# 
# sim_all$GM_inflated <- sim_all$Revenue_Adjusted - sim_all$Expenditure_Adjusted
# 
# glimpse(sim_all)
# 
# 
# 
# 
# 
# # ~ Plots ####
# 
# 
# ## ~~ Revenue + inflation ####
# 
# fig_revenue_inflation_nsim_plot <-
#   ggplot(data = sim_all, 
#          aes(x = Year, 
#              y = Revenue_Adjusted, 
#              group = interaction(Crop, as.factor(run)))) +
#   geom_line(alpha = 0.05, aes(color = Crop), size = 0.5) +
#   labs(
#     x = "Year",
#     y = "Revenue (£/ha)") +
#   facet_wrap(~ System,
#              ncol = 2) +
#   theme_bw() + 
#   guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 5))) + 
#   theme(legend.position = "bottom",
#         strip.text.x = element_text(size = 12, 
#                                     # color = "black", 
#                                     face = "bold.italic"))
# 
# fig_revenue_inflation_nsim_plot
# 
# 
# fig_revenue_inflation_box_plot <-
#   ggplot(data = sim_all, 
#          aes(x = System, 
#              y = Revenue_Adjusted, 
#              fill = System)) +
#   geom_violin(alpha = 0.4, color = NA) +  # Transparent violin for density
#   geom_boxplot(width = 0.2, outlier.alpha = 0.2) +  # Compact box plot overlay
#   theme_minimal() +
#   labs(
#     x = "Treatment",
#     y = "Revenue") +
#   scale_fill_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
#   theme(axis.text.x = element_blank(), 
#         legend.position = "bottom") 
# 
# fig_revenue_inflation_box_plot
# 
# # ggsave(plot = fig_revenue_inflation_box_plot, 
# #        filename = "plots/simulation_plots/fig_revenue_inflation_box_plot.png")
# 
# 
# 
# 
# 
# 
# 
# ## ~~ Expenditure + inflation ####
# 
# 
# 
# 
# 
# 
# 
# 
# ## ~~ GM + inflation ####
# 
# 
# fig_GM_inflation_nsim_plot <-
#   ggplot(data = sim_all, 
#          aes(x = Year, 
#              y = GM_inflated, 
#              group = interaction(Crop, as.factor(run)))) +
#   geom_line(alpha = 0.05, aes(color = Crop), size = 0.5) +
#   labs(
#     x = "Year",
#     y = "GM (£/ha)") +
#   facet_wrap(~ System,
#              ncol = 2) +
#   theme_bw() + 
#   guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 5))) + 
#   theme(legend.position = "bottom",
#         strip.text.x = element_text(size = 12, 
#                                     # color = "black", 
#                                     face = "bold.italic"))
# 
# fig_GM_inflation_nsim_plot
# 
# 
# 
# 
# fig_GM_inflation_box_plot <-
#   ggplot(data = sim_all, 
#          aes(x = System, 
#              y = GM_inflated, 
#              fill = System)) +
#   geom_violin(alpha = 0.4, color = NA) +  # Transparent violin for density
#   geom_boxplot(width = 0.2, outlier.alpha = 0.2) +  # Compact box plot overlay
#   theme_minimal() +
#   labs(
#     x = "Treatment",
#     y = "Revenue") +
#   scale_fill_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
#   theme(axis.text.x = element_blank(), 
#         legend.position = "bottom") 
# 
# fig_GM_inflation_box_plot
# 
# 
# 
# 
# 
# glimpse(sim_all)








# ____________________________####

# Summary table ####




# ~ Cumulative summaries ####

glimpse(sim_all)


# summarise by treatment system and the run

summary_table <- sim_all %>%
  group_by(System, run) %>%  # Group by System and Run (crop rotation unit)
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),  # Sum numeric values over the full rotation
    .groups = "drop"
  ) %>%
  group_by(System) %>%  # Now group by System
  summarise(
    across(where(is.numeric) & !run,  # Exclude `run`
           list(
             mean = ~ mean(., na.rm = TRUE), 
             sd = ~ sd(., na.rm = TRUE),
             sem = ~ sd(., na.rm = TRUE) / sqrt(sum(!is.na(.))),  # Standard Error of the Mean
             min = ~ min(., na.rm = TRUE), 
             max = ~ max(., na.rm = TRUE)
           )),
    .groups = "drop"
  )


print(summary_table)

names(summary_table)

glimpse(sim_all)



# Compute cumulative gross margin for each run

sim_all_cumulative <- sim_all %>%
  group_by(run, System, Year) %>%  
  summarise(Yearly_GM = sum(Gross_Margin, na.rm = TRUE), .groups = "drop") %>%  
  arrange(run, System, Year) %>%  
  group_by(run, System) %>%  
  mutate(Cumulative_Gross_Margin = cumsum(Yearly_GM)) %>%  
  ungroup()

# Compute summary statistics (mean and quantiles)
sim_all_cumulative_summary <- sim_all_cumulative %>%
  group_by(System, Year) %>%
  summarise(
    mean_CGM = mean(Cumulative_Gross_Margin, na.rm = TRUE),
    lower_CGM = quantile(Cumulative_Gross_Margin, 0.05, na.rm = TRUE),  # 5th percentile
    upper_CGM = quantile(Cumulative_Gross_Margin, 0.95, na.rm = TRUE),  # 95th percentile
    .groups = "drop"
  )





# ~ Yield summary ####

names(sim_all)

summary_yield <- sim_all %>%
  group_by(System, Year, Crop_Name) %>%
  summarise(
    mean_yield = mean(Yield, na.rm = TRUE),
    sd_yield = sd(Yield, na.rm = TRUE),  
    se_yield = sd_yield / sqrt(n()),  
    lower_yield = quantile(Yield, 0.05, na.rm = TRUE),
    upper_yield = quantile(Yield, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(System, Crop_Name, Year) %>%  # Ensure correct order before lagging
  group_by(System, Crop_Name) 

total_pct_increase <- summary_yield %>%
  group_by(System, Crop_Name) %>%
  summarise(
    start_yield = first(mean_yield),  # Yield in Year 1
    end_yield = last(mean_yield),  # Yield in Year 6
    total_pct_increase = (end_yield - start_yield) / start_yield * 100,
    .groups = "drop"
  )



# ~ climate-shocked summary ####

names(sim_all)

summary_yield <- 
  sim_all %>%
  group_by(System, Year, Crop_Name) %>%
  summarise(
    mean_yield = mean(Yield, na.rm = TRUE),
    sd_yield = sd(Yield, na.rm = TRUE),  
    se_yield = sd_yield / sqrt(n()),  
    lower_yield = quantile(Yield, 0.05, na.rm = TRUE),
    upper_yield = quantile(Yield, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(System, Year) %>%  # Ensure correct order before lagging
  group_by(System) 

summary_yield_shocked <- 
  sim_all %>%
  group_by(System, Year, Crop_Name) %>%
  summarise(
    mean_yield = mean(Yield_Shocked, na.rm = TRUE),
    sd_yield = sd(Yield_Shocked, na.rm = TRUE),  
    se_yield = sd_yield / sqrt(n()),  
    lower_yield = quantile(Yield_Shocked, 0.05, na.rm = TRUE),
    upper_yield = quantile(Yield_Shocked, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(System, Year) %>%  # Ensure correct order before lagging
  group_by(System) 

glimpse(summary_yield)
glimpse(summary_yield_shocked)



# Merge the two summaries by System and Year
summary_comparison <- merge(
  x = summary_yield, 
  y = summary_yield_shocked, 
  by = c("System", "Year", "Crop_Name"), 
  suffixes = c("_unchanged", "_shocked"))

# Calculate the yield difference
summary_comparison$yield_diff <- summary_comparison$mean_yield_unchanged - summary_comparison$mean_yield_shocked

# Calculate the percentage reduction
summary_comparison$perc_reduction <- (summary_comparison$yield_diff / summary_comparison$mean_yield_unchanged) * 100

# Calculate average reduction per system
avg_reduction <- summary_comparison %>%
  group_by(System, Crop_Name) %>%
  summarise(mean_reduction = mean(yield_diff),
            avg_perc_reduction = mean(perc_reduction))




# ~ Revenue summary ####


names(sim_all)

glimpse(sim_all)

print(mean(sim_all$Revenue_Shocked))

summary_Revenue_Shocked <- sim_all %>%
  group_by(System, Year, Crop_Name) %>%
  summarise(
    mean_revenue = mean(Revenue_Shocked, na.rm = TRUE),
    sd_revenue = sd(Revenue_Shocked, na.rm = TRUE),  
    se_revenue = sd_revenue / sqrt(n()),  
    lower_revenue = quantile(Revenue_Shocked, 0.05, na.rm = TRUE),
    upper_revenue = quantile(Revenue_Shocked, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(System, Crop_Name, Year) %>%  # Ensure correct order before lagging
  group_by(System, Crop_Name, Year) 

glimpse(summary_Revenue_Shocked)

total_pct_increase <- summary_Revenue_Shocked %>%
  group_by(System, Crop_Name) %>%
  summarise(
    start_revenue = first(mean_revenue),  # in Year 1
    end_revenue = last(mean_revenue),  # in Year 6
    total_pct_increase = (end_revenue - start_revenue) / start_revenue * 100,
    .groups = "drop"
  )


print(summary_Revenue_Shocked, n = 54)







# ~ GM summary ####

summary_GM <- sim_all %>%
  group_by(System, Year) %>%
  summarise(
    mean_GM = mean(Gross_Margin, na.rm = TRUE),
    lower_GM = quantile(Gross_Margin, 0.05, na.rm = TRUE),
    upper_GM = quantile(Gross_Margin, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

loss_probability <- sim_all %>%
  group_by(System) %>%
  summarise(probability_loss = mean(Gross_Margin < 0, na.rm = TRUE))








# ~ Plots ####


# Plot cumulative gross margin with simulations and summary statistics
fig_gm_cumulative_plot <-
  ggplot() +
  
  # Add individual simulation lines (original data)
  geom_line(data = sim_all_cumulative, 
            aes(x = Year, 
                y = Cumulative_Gross_Margin, 
                group = as.factor(run), 
                color = System),
            alpha = 0.05, 
            linewidth = 0.5) +
  
  # Add uncertainty band (5th–95th percentile)
  geom_ribbon(data = sim_all_cumulative_summary, 
              aes(x = Year, 
                  ymin = lower_CGM, 
                  ymax = upper_CGM), 
              alpha = 0, color = "black", 
              linetype = "dotted") +
  
  # Add mean cumulative gross margin line
  geom_line(data = sim_all_cumulative_summary, 
            aes(x = Year, 
                y = mean_CGM, 
                color = "black"), 
            linewidth = 1, 
            linetype = "dashed") +
  
  # Custom colors
  scale_color_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
  
  # Labels and theme
  labs(
    x = "Year",
    y = "Inflation-Adjusted 6-year Gross Margin (£/ha)") +
  facet_wrap(~ System, ncol = 2) +
  theme_bw() + 
  theme(legend.position = "bottom",  
        strip.text.x = element_text(size = 12, face = "bold.italic")) +
  
  # Improve legend appearance
  guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 2.5)))

fig_gm_cumulative_plot


ggsave(filename = "plots/simulation_plots/fig_gm_cumulative_plot.png", 
       plot = fig_gm_cumulative_plot, width = 8, height = 5)



names(sim_all_cumulative)


fig_GM_inflation_bar_plot <- 
  ggplot(data = summary_table, 
         aes(x = System, 
             y = Gross_Margin_mean, 
             fill = System)) +
  geom_bar(stat = "identity", 
           position = "dodge", 
           alpha = 1) +  # Bar plot
  geom_errorbar(aes(ymin = Gross_Margin_mean - Gross_Margin_sem,
                    ymax = Gross_Margin_mean + Gross_Margin_sem),
                width = 0.2, 
                color = "black") +  # Error bars
  theme_bw() + 
  labs(
    x = "Treatment System",
    y = "Inflation-Adjusted 6-year Gross Margin (£/ha)"
  ) +
  scale_fill_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
  theme(legend.position = "bottom")

print(fig_GM_inflation_bar_plot)

ggsave(filename = "plots/simulation_plots/fig_gm_inflated_plot.png", width = 6, height = 5)



ggarrange(fig_gm_cumulative_plot, fig_GM_inflation_bar_plot, 
          ncol = 2, nrow = 1, 
          widths = c(2, 1), # Make the first plot span 2 columns
          common.legend = TRUE, 
          legend = "bottom", 
          labels = c("A","B"))  


ggsave(filename = "plots/simulation_plots/fig_rotation_GM_plot.png", width = 9, height = 4.5)







# # ____________________________####
# # Sensitivity analysis ####
# 
# 
# glimpse(sim_all)
# 
# 
# # ~ Correlation Analysis (Simple Sensitivity Check) ####
# 
# # Correlation analysis between selected input and output variables
# cor_results <- sim_all %>%
#   dplyr::select(Price, Expenditure, Yield_Shocked, Revenue_Adjusted, Gross_Margin) %>%
#   cor(use = "complete.obs")
# 
# print(cor_results)
# 
# 
# 
# 
# 
# 
# # ~ Linear Regression Models (Quantifying Relationships) ####
# 
# # Linear regression to analyze the relationship between inputs and output
# lm_model <- lm(formula = Gross_Margin ~ Price + Expenditure + Climate_Shock, 
#                data = sim_all)
# summary(lm_model)




# ~ Partial Correlation (Controlling for Other Variables) ####



# # Partial correlation between Price and Yield_Shocked, controlling for Expenditure
# pcor_results <- pcor.test(sim_all$Price, sim_all$Yield_Shocked, sim_all$Expenditure)
# print(pcor_results)
# 
# # Partial correlation between Price and Yield_Shocked, controlling for Climate_Shock
# pcor_results <- pcor.test(sim_all$Price, sim_all$Yield_Shocked, sim_all$Climate_Shock)
# print(pcor_results)







# # ~  Sobol' Sensitivity Analysis (Variance-based) ####
# 
# library(sensitivity)
# 
# # Define model function
# model_func <- function(X) {
#   with(as.data.frame(X), {
#     Gross_Margin <- Yield * Price - Expenditure
#     return(Gross_Margin)
#   })
# }
# 
# # Generate random parameter sets
# X_sample <- data.frame(
#   Yield = runif(1000, min(sim_all$Yield_Shocked), max(sim_all$Yield_Shocked)),
#   Price = runif(1000, min(sim_all$Price), max(sim_all$Price)),
#   Expenditure = runif(1000, min(sim_all$Expenditure_Adjusted), max(sim_all$Expenditure_Adjusted))
# )
# 
# # Perform Sobol' analysis
# sobol_result <- sobol(model = model_func, 
#                       X1 = X_sample, 
#                       X2 = X_sample, 
#                       nboot = 100)
# print(sobol_result)
# plot(sobol_result)






# # ________________________________________####
# #  Monte Carlo Simulation (For probabilistic risk assessment) ####
# 
# 
# library(triangle)  # For triangular distributions
# 
# glimpse(sim_all)
# 
# 
# # Simulating price variations (triangular distribution)
# sim_all$Price_Shocked <- rtriangle(nrow(sim_all), 
#                                    a = min(sim_all$Price) * 0.8,  # 20% lower bound
#                                    b = max(sim_all$Price) * 1.2,  # 20% upper bound
#                                    c = mean(sim_all$Price))       # Mode (most likely value)
# 
# sim_all$Expenditure_Shocked <- rtriangle(nrow(sim_all), 
#                                          a = min(sim_all$Expenditure) * 0.9,  # 10% lower bound
#                                          b = max(sim_all$Expenditure) * 1.2,  # 20% upper bound
#                                          c = mean(sim_all$Expenditure))       # Mode (most likely value)
# 
# 
# # Recalculate revenue and gross margin
# sim_all$Revenue_Shocked <- sim_all$Yield * sim_all$Price_Shocked
# sim_all$GM_Shocked <- sim_all$Revenue_Shocked - sim_all$Expenditure_Shocked
# 
# # Visualizing risk distribution
# ggplot(sim_all, aes(x = GM_Shocked, fill = System)) +
#   geom_density(alpha = 0.6) +
#   scale_fill_manual(values = c(
#     "Conventional" = "tomato2",
#     "Conservation" = "turquoise3"
#   )) +
#   labs(title = "Simulated Gross Margin Distribution (Risk Analysis)")
# 
# # Visualizing risk distribution
# ggplot(sim_all, aes(x = Expenditure_Adjusted, fill = System)) +
#   geom_density(alpha = 0.6) +
#   scale_fill_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
#   labs(title = "Simulated Gross Expenditure Distribution (Risk Analysis)")
# 
# # Visualizing risk distribution
# ggplot(sim_all, aes(x = Revenue_Adjusted, fill = System)) +
#   geom_density(alpha = 0.6) +
#   scale_fill_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
#   labs(title = "Simulated Gross Revenue Distribution (Risk Analysis)")
# 
# # Visualizing risk distribution
# ggplot(sim_all, aes(x = Yield_Shocked, fill = System)) +
#   geom_density(alpha = 0.6) +
#   scale_fill_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
#   labs(title = "Simulated Gross Revenue Distribution (Risk Analysis)")
# 
# 
# 
# 
# 
# 
# 
# 
# # ~ Extreme Scenario Analysis ####
# 
# # Best-Case Scenario: High prices, Low input costs
# sim_all$Price_Best <- max(sim_all$Price) * 1.1
# sim_all$Expenditure_Best <- min(sim_all$Expenditure_Adjusted) * 0.9
# 
# sim_all$Revenue_Best <- sim_all$Yield * sim_all$Price_Best
# sim_all$GM_Best <- sim_all$Revenue_Best - sim_all$Expenditure_Best
# 
# 
# 
# # Worst-Case Scenario: Low prices, High input costs
# sim_all$Price_Worst <- min(sim_all$Price) * 0.9
# sim_all$Expenditure_Worst <- max(sim_all$Expenditure) * 1.1
# 
# sim_all$Revenue_Worst <- sim_all$Yield * sim_all$Price_Worst
# sim_all$GM_Worst <- sim_all$Revenue_Worst - sim_all$Expenditure_Worst
# 
# 
# 
# 
# #_______________________________________________________
# # scenario 2
# # sim_all$Revenue_Best <- max(sim_all$Revenue_Adjusted) * 1.1
# # sim_all$Expenditure_Best <- min(sim_all$Expenditure_Adjusted) * 0.9
# # sim_all$GM_Best <- sim_all$Revenue_Best - sim_all$Expenditure_Best
# # 
# # sim_all$Revenue_Worst <- min(sim_all$Revenue_Adjusted) * 0.9
# # sim_all$Expenditure_Worst <- max(sim_all$Expenditure_Adjusted) * 1.1
# # sim_all$GM_Worst <- sim_all$Revenue_Worst - sim_all$Expenditure_Worst
# 
# 
# 
# 
# 
# # Melt data for comparison
# library(reshape2)
# 
# sim_all_melted <- melt(sim_all, 
#                        id.vars = "System", 
#                        measure.vars = c("GM_Best", "GM_Worst"))
# 
# 
# unique(sim_all_melted$variable)
# 
# 
# ggplot(sim_all_melted, aes(x = value, fill = System)) +
#   geom_density(alpha = 0.6) +
#   facet_wrap(~variable, scales = "free_x") +
#   labs(title = "Best vs. Worst Case Gross Margin Distribution",
#        x = "Gross Margin (€ per ha)", 
#        y = "Density") +
#   theme_minimal()
# 
# 
# 
# names(sim_all)
# 
# # # Probability of Negative GM under shocked conditions
# # risk_summary <- sim_all %>%
# #   group_by(System) %>%
# #   summarise(
# #     Prob_Negative_GM_Shocked = mean(GM_inflated < 0),
# #     Prob_Negative_GM_Worst = mean(GM_Worst < 0),
# #     Prob_Negative_GM_Best = mean(GM_Best < 0)
# #   )
# # 
# # print(risk_summary)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ~ Variance-Based Sensitivity Analysis (Sensitivity Indices) ####
# 
# 
# # library(sensitivity)
# # 
# # # Define the input matrix (you can select specific columns if necessary)
# # inputs_matrix <- as.matrix(select(sim_all, Price, Expenditure, Climate_Shock))
# # 
# # # Define the model as a function (for example, a linear regression or other model)
# # model_function <- function(X) {
# #   # For demonstration, we're just using Gross_Margin as the output directly
# #   return(sim_all$Gross_Margin)
# # }
# # 
# # # Perform Sobol sensitivity analysis
# # sobol_analysis <- sobol(model = model_function, X1 = inputs_matrix, X2 = inputs_matrix)
# # 
# # # Check the sensitivity results
# # print(sobol_analysis)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ~ Monte Carlo Simulation Sensitivity ####
# 
# 
# # Example of running Monte Carlo simulation for sensitivity, grouped by System
# set.seed(123)
# 
# # Define a simple function that runs a simulation, with an additional grouping variable (System)
# sensitivity_monte_carlo_grouped <- function(input_variable, output_variable, group_variable) {
#   # Initialize an empty list to store results by group
#   group_results <- list()
#   
#   # Loop through each unique system in the group_variable (e.g., experimental treatment "System")
#   for (system in unique(group_variable)) {
#     # Subset the data for the current group
#     group_data <- which(group_variable == system)
#     input_subset <- input_variable[group_data]
#     output_subset <- output_variable[group_data]
#     
#     # Run the simulation for the current group
#     sim_results <- replicate(1000, {
#       # Adjust input variable randomly (for example, apply random shocks)
#       simulated_input <- input_subset + rnorm(length(input_subset), 0, 1)
#       
#       # Recalculate output variable (in your case, this could be gross margin or yield)
#       simulated_output <- lm(output_subset ~ simulated_input)$fitted.values
#       return(simulated_output)
#     })
#     
#     # Store the simulation results for the current group
#     group_results[[system]] <- sim_results
#   }
#   
#   return(group_results)
# }
# 
# # Run the Monte Carlo simulation grouped by "System"
# simulated_results_grouped <- sensitivity_monte_carlo_grouped(sim_all$Price, sim_all$Gross_Margin, sim_all$System)
# 
# # Check the results for a specific system (e.g., "Conservation Agriculture")
# simulated_results_conservation <- simulated_results_grouped[["Conservation Agriculture"]]
# 
# 
# 
# 
# 
# 










