### Economic data
### J Collins 
### 2024-04-26
###

# MONTE CARLO SIMULATION ####


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# ~ PACKAGES ####

setwd(dir = "~/Documents/GitHub/economics/")

source(file = "economics_scripts/01_packages.R")

# Load necessary libraries
library(mc2d)      # For Monte Carlo simulation
library(janitor)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# DATA ####

setwd(dir = "~/OneDrive - Harper Adams University/Data/economics/")


# ~~~~~~~~~~~~####
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


# Extract baseline (Conventional) values for each crop
baseline <- summary_df %>%
  filter(treatment == "Conventional") %>%
  rename(baseline_value = mean_value) %>%
  select(crop.x, variable, baseline_value)

# Join and compute % difference by crop
percentage_diff <- summary_df %>%
  left_join(baseline, by = c("crop.x", "variable")) %>%
  mutate(percent_diff = (mean_value - baseline_value) / baseline_value * 100) %>%
  filter(treatment != "Conventional") %>%  # Remove Conventional rows
  select(treatment, crop.x, variable, percent_diff)

# View result
print(percentage_diff)








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



global_CA_dat <- filter(.data = global_dat, Crop.rotation.with.at.least.3.crops.involved.in.NT == "Yes" &
                          global_dat$Soil.cover.in.NT == "Yes")


unique(global_CA_dat$Crop)
names(global_CA_dat)


### ~~~ yield trends ####

unique(global_CA_dat$Crop)

correlation <- cor(global_CA_dat$Years.since.NT.started..yrs., 
                   global_CA_dat$Relative.yield.change, 
                   use = "complete.obs")

print(correlation)  # Check if it's a valid numeric value


yield_corr_plot <-
  ggplot(global_CA_dat, aes(x = Years.since.NT.started..yrs., y = Relative.yield.change)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Years since CA started (yrs)", 
       y = "Relative Yield Change", 
       subtitle = "All crops") +
   ylim(-1,2.5) +
  annotate("text", 
           x = 40, 
           y = 1.5, 
           label = paste0("italic(r) == ", round(correlation, 2)), 
           size = 5, color = "red", 
           parse = TRUE) +
  theme_minimal()

yield_corr_plot

dir.create(path = "plots/simulation_plots/")

ggsave(filename = "plots/simulation_plots/rel_yield_change.png", width = 8, height = 4)


# get the rate of change per year 

lm_model <- lm(Relative.yield.change ~ Years.since.NT.started..yrs., data = global_CA_dat)
summary(lm_model)

ca_start_yield_reduction <- coef(lm_model)[1]  # Extract intercept
ca_yield_change_rate <- coef(lm_model)[2]      # Extract slope






### ~~~ winter wheat trends ####

unique(global_CA_dat$Crop)

global_CA_dat_wwheat <- filter(.data = global_CA_dat, Crop == "wheat.winter")

correlation <- cor(global_CA_dat_wwheat$Years.since.NT.started..yrs., 
                   global_CA_dat_wwheat$Relative.yield.change, 
                   use = "complete.obs")

print(correlation)  # Check if it's a valid numeric value


wwheat_yield_corr_plot <-
ggplot(global_CA_dat_wwheat, aes(x = Years.since.NT.started..yrs., y = Relative.yield.change)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Years since CA started (yrs)", 
       y = "Relative Yield Change", 
       subtitle = "Winter Wheat") +
  annotate("text", 
           x = max(global_CA_dat_wwheat$Years.since.NT.started..yrs., na.rm = TRUE) * 0.7, 
           y = max(global_CA_dat_wwheat$Relative.yield.change, na.rm = TRUE) * 0.9, 
           label = paste0("italic(r) == ", round(correlation, 2)), 
           size = 5, color = "red", 
           parse = TRUE) +
  theme_minimal()

wwheat_yield_corr_plot

dir.create(path = "plots/simulation_plots/")

ggsave(filename = "plots/simulation_plots/rel_yield_change_wwheat.png", width = 8, height = 4)


# get the rate of change per year 

lm_model <- lm(Relative.yield.change ~ Years.since.NT.started..yrs., data = global_CA_dat_wwheat)
summary(lm_model)

start_yield_reduction <- coef(lm_model)[1]  # Extract intercept






### ~~~ spring barley trends ####

unique(global_CA_dat$Crop)

global_CA_dat_sbarley <- filter(.data = global_CA_dat, Crop == "barley.spring")

cor(global_CA_dat_sbarley$Years.since.NT.started..yrs., 
    global_CA_dat_sbarley$Relative.yield.change, 
    use = "complete.obs")

sbarley_yield_corr_plot <-
  ggplot(data = global_CA_dat_sbarley, 
         aes(x = Years.since.NT.started..yrs., 
             y = Relative.yield.change)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Years since CA started (yrs)", 
       y = "Relative Yield Change", 
       subtitle = "Spring Barley") +
  theme_minimal()

sbarley_yield_corr_plot




### ~~~ winter barley trends ####

unique(global_CA_dat$Crop)

global_CA_dat_wbarley <- filter(.data = global_CA_dat, Crop == "barley.winter")

cor(global_CA_dat_wbarley$Years.since.NT.started..yrs., 
    global_CA_dat_wbarley$Relative.yield.change, 
    use = "complete.obs")

wbarley_yield_corr_plot <-
  ggplot(data = global_CA_dat_wbarley, 
         aes(x = Years.since.NT.started..yrs., 
             y = Relative.yield.change)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Years since CA started (yrs)", 
       y = "Relative Yield Change", 
       subtitle = "Winter Barley") +
  theme_minimal()

wbarley_yield_corr_plot













# ~ AHDB Yield data UK ####


# Define crop-specific yield parameters for each treatment (mean and sd for each crop in the rotation)

ahdb_dat <- read_excel(path = "data/processed_data/ahdb_mean_yields_2017_21.xlsx")

# Subtract the values in the top_25_pc row from the values in the bottom_25_pc row
yield_sd <- ahdb_dat %>%
  filter(mean_yield_2017_2021 == "top_25_pc") %>%
  select(-mean_yield_2017_2021) %>%
  unlist() - 
  ahdb_dat %>%
  filter(mean_yield_2017_2021 == "bottom_25_pc") %>%
  select(-mean_yield_2017_2021) %>%
  unlist()

# Add the new row to the original dataset
ahdb_dat <- rbind(ahdb_dat, c("yield_sd", yield_sd))

# View the updated dataset
glimpse(ahdb_dat)



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





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# DISTRIBUTIONS ####


# ~~~~~~~~~~~~####
# ~ histograms ####

selected_columns <- dat[, c(6:ncol(dat))]

# Replace all zeros with NA in the entire dataframe
# dat[dat == 0] <- NA

# Function to plot histogram and QQ plot for a single variable
plot_histogram <- function(var) {
  p1 <- ggplot(dat, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
    theme_minimal() +
    labs(title = var, x = var, y = "Frequency")
  
  # p2 <- ggqqplot(dat_y1[[var]], title = paste("QQ Plot of", var)) +
  #   theme_minimal()
  
  return(list(p1))
}

# Apply function to all selected variables and store the plots
plots <- lapply(names(selected_columns), plot_histogram)

# Flatten the list of plots into a single list
combined_plots <- do.call(c, plots)

# Arrange all the plots in a grid layout
ggarrange(plotlist = combined_plots, ncol = 3, nrow = 3)

dir.create(path = "plots/distributions/")

ggsave(filename = "plots/distributions/economic_histograms.png")






























# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#  Economic Model ####


# Number of simulations
n_sim <- 10000













# ~~~~~~~~~~~~####
# ~ Set Crop rotations ####


rotation_conventional <- c("Winter Wheat", "Winter Barley", "Oilseed Rape")
rotation_conservation <- c("Winter Beans", "Winter Wheat", "Spring Barley", "Oilseed Rape", "Feed Peas", "Winter Wheat")

# Repeat rotations to match a 12-year period
rotation_conventional_full <- rep(rotation_conventional, length.out = 12)
rotation_conservation_full <- rep(rotation_conservation, length.out = 12)


# Randomly assign starting positions in the 12-year rotation
start_years_conventional <- sample(1:12, n_sim, replace = TRUE)
start_years_conservation <- sample(1:12, n_sim, replace = TRUE)

# Select crops based on the random starting position
crop_sequence_conventional <- rotation_conventional_full[start_years_conventional]
crop_sequence_conservation <- rotation_conservation_full[start_years_conservation]






# ~~~~ ####
# ~ CON crop yield ####


# Rotation: Wheat / barley / rape

mean_yield_wheat_con <- yield_middle_50_pc$winter_wheat  
sd_yield_wheat_con <- yield_sd$winter_wheat   # Standard deviation for wheat yield

mean_yield_wbarley_con <- yield_middle_50_pc$winter_barley  
sd_yield_wbarley_con <-    yield_sd$winter_barley

mean_yield_wosr_con <- yield_middle_50_pc$winter_osr 
sd_yield_wosr_con <- yield_sd$winter_osr 


# Simulate yields for each crop in the rotation for each treatment
yield_wheat_con <- rnorm(n_sim, mean_yield_wheat_con, sd_yield_wheat_con)
yield_wbarley_con <- rnorm(n_sim, mean_yield_wbarley_con, sd_yield_wbarley_con)
yield_wosr_con <- rnorm(n_sim, mean_yield_wosr_con, sd_yield_wosr_con)






# ~ CON crop prices ####

# Optionally: Compute additional metrics (e.g., price, expenditure)
# Define price and expenditure parameters (you can adjust these for each crop too)
mean_price_wheat_con <- historic_wheat_price
mean_price_wbarley_con <- historic_barley_price
mean_price_wosr_con <- historic_wosr_price


# Simulate prices for each crop in the rotation
price_wheat_con <- rnorm(n = n_sim, 
                         mean = historic_wheat_price, 
                         sd = historic_wheat_sd)

price_wbarley_con <- rnorm(n = n_sim, 
                           mean = historic_barley_price, 
                           sd = historic_barley_sd)

price_wosr_con <- rnorm(n = n_sim, 
                        mean = historic_wosr_price, 
                        sd = historic_wosr_sd)






# ~ CON crop revenue ####

# # Calculate revenue for each treatment (revenue = yield * price)
# revenue_conventional <- (
#   (yield_wheat_con * price_wheat_con) + 
#   (yield_wbarley_con * price_wbarley_con) +
#   (yield_wosr_con * price_wosr_con) * 5)


# Assign revenue based on the selected crop
revenue_conventional <- ifelse(
  crop_sequence_conventional == "Winter Wheat", yield_wheat_con * price_wheat_con,
  ifelse(crop_sequence_conventional == "Winter Barley", yield_wbarley_con * price_wbarley_con,
         yield_wosr_con * price_wosr_con)  # Oilseed Rape as default
)







# ~~~~ ####


# ~ CA crop yield ####


#Rotation: winter beans / winter wheat / spring Barley / oilseed rape / feed peas / winter wheat

mean_yield_wbeans_ca <- yield_middle_50_pc$winter_beans
sd_yield_wbeans_ca <- yield_sd$winter_beans

yield_wbeans_ca <- rnorm(n = n_sim, 
                         mean = mean_yield_wbeans_ca, 
                         sd = sd_yield_wbeans_ca)

mean_yield_wheat_ca <- yield_middle_50_pc$winter_wheat  
sd_yield_wheat_ca <- yield_sd$winter_wheat   # Standard deviation for wheat yield
yield_wheat_ca <- rnorm(n_sim, mean_yield_wheat_ca, sd_yield_wheat_ca)

mean_yield_sbarley_ca <- yield_middle_50_pc$spring_barley
sd_yield_sbarley_ca <- yield_sd$spring_barley
yield_sbarley_ca <- rnorm(n_sim, mean_yield_sbarley_ca, sd_yield_sbarley_ca)

mean_yield_wosr_ca <- yield_middle_50_pc$winter_osr 
sd_yield_wosr_ca <- yield_sd$winter_osr 
yield_wosr_ca <- rnorm(n_sim, mean_yield_wosr_ca, sd_yield_wosr_ca)

mean_yield_peas_ca <- yield_middle_50_pc$feed_peas
sd_yield_peas_ca <- yield_sd$feed_peas
yield_peas_ca <- rnorm(n_sim, mean_yield_peas_ca, sd_yield_peas_ca)







# ~ CA crop prices ####


# Simulate prices for each crop in the rotation
price_wheat_ca <- rnorm(
  n = n_sim, 
  mean = historic_wheat_price, 
  sd = historic_wheat_sd)

price_wbeans_ca <- rnorm(
  n = n_sim, 
  mean = 180, 
  sd = 15)

price_peas_ca <- rnorm(
  n = n_sim, 
  mean = 210, 
  sd = 15)

price_sbarley_ca <- rnorm(
  n = n_sim, 
  mean = historic_barley_price, 
  sd = historic_barley_sd)

price_wosr_ca <- rnorm(
  n = n_sim, 
  mean = historic_wosr_price, 
  sd = historic_wosr_sd)



# ~ CA crop revenue ####

# revenue_conservation <- (
#   (yield_wbeans_ca * price_wbeans_ca) + 
#   (yield_wheat_ca * price_wheat_ca) +
#   (yield_sbarley_ca * price_sbarley_ca) +
#   (yield_wosr_ca * price_wosr_ca) +
#   (yield_peas_ca * price_peas_ca) * 3)

revenue_conservation <- ifelse(
  crop_sequence_conservation == "Winter Beans", yield_wbeans_ca * price_wbeans_ca,
  ifelse(crop_sequence_conservation == "Winter Wheat", yield_wheat_ca * price_wheat_ca,
         ifelse(crop_sequence_conservation == "Spring Barley", yield_sbarley_ca * price_sbarley_ca,
                ifelse(crop_sequence_conservation == "Oilseed Rape", yield_wosr_ca * price_wosr_ca,
                       ifelse(crop_sequence_conservation == "Feed Peas", yield_peas_ca * price_peas_ca, 0)))))



# # Combine yields for each treatment (total yield for each rotation)
# # For example, you might want to sum or average the yields for each rotation
# total_yield_conventional <- yield_wheat_con + yield_maize_con  # Sum of yields for Conventional Agriculture
# total_yield_conservation <- yield_legumes_con + yield_barley_con  # Sum of yields for Conservation Agriculture















# ~~~~~~~~~~~~####
# Expenditure ####

## ~ Data ####

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



glimpse(dat)



## ~ CON expenditure simulation ####

# Simulate the expenditure for each crop in the rotation
expen_wheat_con <- rnorm(
  n = n_sim, 
  mean = wheat_expenditure_mean, 
  sd = wheat_expenditure_sd)

expen_wbarley_con <- rnorm(
  n = n_sim, 
  mean = wbarley_expenditure_mean, 
  sd = wbarley_expenditure_sd)

expen_wosr_con <- rnorm(
  n = n_sim, 
  mean = wosr_expenditure_mean, 
  sd = wosr_expenditure_sd)



## ~ CON expenditure sum ####

# Calculate revenue for each treatment (revenue = yield * price)
expenditure_conventional <- ifelse(
  crop_sequence_conventional == "Winter Wheat", expen_wheat_con,
  ifelse(crop_sequence_conventional == "Winter Barley", expen_wbarley_con,
         expen_wosr_con)  # Oilseed Rape as default
)






## ~ CA expenditure simulation ####

# ACCOUNT FOR 17% REDUCTION IN EXPENDITURE BASED ON OUR DATA 

#view the rotation
print(rotation_conservation)

# then view the pc difference in our study
print(percentage_diff)



# Simulate the expenditure for each crop in the rotation
expen_wbeans_ca <- rnorm(
  n = n_sim, 
  mean = wbeans_expenditure_mean * (1 - 0.17), 
  sd = wbeans_expenditure_sd * (1 - 0.17))

expen_wheat_ca <- rnorm(
  n = n_sim, 
  mean = wheat_expenditure_mean * (1 - 0.17), 
  sd = wheat_expenditure_sd * (1 - 0.17))

expen_sbarley_ca <- rnorm(
  n = n_sim, 
  mean = sbarley_expenditure_mean * (1 - 0.17), 
  sd = sbarley_expenditure_sd * (1 - 0.17))

expen_wosr_ca <- rnorm(
  n = n_sim, 
  mean = wosr_expenditure_mean * (1 - 0.17), 
  sd = wosr_expenditure_sd * (1 - 0.17))

expen_peas_ca <- rnorm(
  n = n_sim, 
  mean = peas_expenditure_mean * (1 - 0.17), 
  sd = peas_expenditure_sd * (1 - 0.17))



expenditure_conservation <- ifelse(
  crop_sequence_conservation == "Winter Beans", expen_wbeans_ca,
  ifelse(crop_sequence_conservation == "Winter Wheat", expen_wheat_ca,
         ifelse(crop_sequence_conservation == "Spring Barley", expen_sbarley_ca,
                ifelse(crop_sequence_conservation == "Oilseed Rape", expen_wosr_ca,
                       ifelse(crop_sequence_conservation == "Feed Peas", expen_peas_ca, 0)))))






# # Cost for Conventional Agriculture (fixed or variable)
# mean_expenditure_conventional <- mean(con_dat$total_expenditure_ha)  


# # Cost for Conservation Agriculture (fixed or variable)
# mean_expenditure_conservation <- mean(ca_dat$total_expenditure_ha)



# # Simulate expenditure for each treatment (for simplicity, we keep it constant here)
# expenditure_conventional <- rnorm(n_sim, 
#                                   mean = mean_expenditure_conventional, 
#                                   sd = sd(con_dat$total_expenditure_ha))

# expenditure_conservation <- rnorm(n_sim,
#                                   mean =  mean_expenditure_conservation, 
#                                   sd = sd(ca_dat$total_expenditure_ha))







# ~~~~~~~~~~~~####
# Gross Margin ####



# ~ calculation ####

# Calculate gross margin for each treatment (gross margin = revenue - expenditure)
gross_margin_conventional <- revenue_conventional - expenditure_conventional
gross_margin_conservation <- revenue_conservation - expenditure_conservation



# Store the results in a data frame for easier analysis
results <- data.frame(
  gross_margin_conventional = gross_margin_conventional,
  gross_margin_conservation = gross_margin_conservation
)

# You can now analyze the results, for example:
summary(results)



# Combine the results into a long format data frame for ggplot
results_long <- data.frame(
  gross_margin = c(gross_margin_conventional, gross_margin_conservation),
  treatment = rep(c("Conventional", "Conservation"), each = n_sim)
)




# ~ plots ####

# Plot density plots for both treatments
ggplot(results_long, aes(x = gross_margin, fill = treatment)) +
  geom_density(alpha = 0.5) +
  labs(title = "12 Year Gross Margin Distribution by Treatment ha-1",
       x = "Gross Margin",
       y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "green"))

# Plot boxplots for both treatments
ggplot(results_long, aes(x = treatment, y = gross_margin, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Boxplot of Gross Margin by Treatment",
       x = "Treatment",
       y = "Gross Margin") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "green"))











# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# ~ Yield Long-Term Trends ####


# ~ Function ####

# Simulate 12 years of yield trends for each crop
simulate_yield_trend_con <- function(initial_yield, drift, volatility, years = 12) {
  yield_series <- numeric(years)
  yield_series[1] <- initial_yield  # Start with the historical yield
  
  for (t in 2:years) {
    yield_series[t] <- yield_series[t-1] * (1 + drift + rnorm(1, mean = 0, sd = volatility))
  }
  return(yield_series)
}



simulate_yield_trend_ca <- function(initial_yield, drift, volatility, years = 12) {
  yield_series <- numeric(years)
  yield_series[1] <- initial_yield  # Start with the historical yield
  
  for (t in 2:years) {
    yield_series[t] <- yield_series[t-1] * (1 + drift + rnorm(1, mean = 0, sd = volatility))
  }
  return(yield_series)
}






# ~ Define the long-term yield trend ####

# Define yield improvement rate (0.5-1.5% per year)
yield_drift_con <- 0  
yield_drift_ca <- ca_yield_change_rate

# Define yield variability (random shocks per year)
yield_volatility <- 0.03  










# Apply to each crop



# Conventional 
yield_wheat_trend_con <- simulate_yield_trend_con(initial_yield = mean_yield_wheat_con, 
                                              drift = yield_drift_con, 
                                              volatility = yield_volatility)

yield_wbarley_trend_con <- simulate_yield_trend_con(initial_yield = mean_yield_wbarley_con, 
                                                drift = yield_drift_con, 
                                                volatility = yield_volatility)
yield_wosr_trend_con <- simulate_yield_trend_con(initial_yield = mean_yield_wosr_con, 
                                             drift = yield_drift_con, 
                                             volatility = yield_volatility)






# Conservation

# ACCOUNT FOR LOWER INITIAL YIELD 

# view pc diff of lower yield 
print(percentage_diff)



yield_wheat_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_wheat_ca - ca_start_yield_reduction, 
                                             drift = yield_drift_ca, 
                                             volatility = yield_volatility)

yield_wbarley_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_wbarley_con - ca_start_yield_reduction, 
                                               drift = yield_drift_ca, 
                                               volatility = yield_volatility)

yield_wosr_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_wosr_con - ca_start_yield_reduction, 
                                            drift = yield_drift_ca, 
                                            volatility = yield_volatility)

yield_wbeans_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_wbeans_ca - ca_start_yield_reduction, 
                                              drift = yield_drift_ca, 
                                              volatility = yield_volatility)

yield_sbarley_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_sbarley_ca - ca_start_yield_reduction, 
                                               drift = yield_drift_ca, 
                                               volatility = yield_volatility)

yield_fpeas_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_peas_ca - ca_start_yield_reduction, 
                                             drift = yield_drift_ca, 
                                             volatility = yield_volatility)






## ~~ yield simulation ####

# Conventional
# make the yield df 


yield_sim_con <- data.frame(
  Year = rep(1:12, 3),
  Yield = c(yield_wheat_trend_con, 
            yield_wbarley_trend_con, 
            yield_wosr_trend_con),
  Crop = rep(c("Winter Wheat", 
               "Winter Barley", 
               "Oilseed Rape"), 
             each = 12)
)

# plot the yield simulation

ggplot(data = yield_sim_con, 
       aes(x = Year, 
           y = Yield, 
           color = Crop)) +
  geom_line(size = 1.2) +
  labs(title = "Conventional: Simulated Yield Trends Over 12 Years", 
       y = "Yield (kg/ha)", 
       x = "Year") +
  theme_minimal()




# Conservation
# make the yield df 

# yield_wheat_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_wheat_ca, 
#                                              drift = yield_drift_ca, 
#                                              volatility = yield_volatility)
# 
# yield_wbarley_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_wbarley_con, 
#                                                drift = yield_drift_ca, 
#                                                volatility = yield_volatility)
# 
# yield_wosr_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_wosr_ca, 
#                                             drift = yield_drift_ca, 
#                                             volatility = yield_volatility)
# 
# yield_wbeans_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_wbeans_ca, 
#                                               drift = yield_drift_ca, 
#                                               volatility = yield_volatility)
# 
# yield_sbarley_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_sbarley_ca,
#                                                drift = yield_drift_ca,
#                                                volatility = yield_volatility)
# 
# yield_fpeas_trend_ca <- simulate_yield_trend_ca(initial_yield = mean_yield_peas_ca, 
#                                              drift = yield_drift_ca, 
#                                              volatility = yield_volatility)


yield_sim_ca <- data.frame(
  Year = rep(1:12, 6),
  Yield = c(yield_wheat_trend_ca, 
            yield_wbarley_trend_ca, 
            yield_wosr_trend_ca,
            yield_wbeans_trend_ca,
            yield_sbarley_trend_ca,
            yield_fpeas_trend_ca),
  Crop = rep(c("Winter Wheat", 
               "Winter Barley", 
               "Oilseed Rape",
               "Winter Beans", 
               "Spring Barley",
               "Feed Peas"), 
             each = 12)
)

# plot the yield simulation



ggplot(data = yield_sim_ca, 
       aes(x = Year,
           y = Yield, 
           color = Crop)) +
  geom_line(size = 1.2) +
  labs(title = "Conservation: Simulated Yield Trends Over 12 Years", 
       y = "Yield (t ha-1)", 
       x = "Year") +
  theme_minimal()




## ~~ joint plot ####

# Combine both datasets
yield_sim_all <- rbind(
  data.frame(yield_sim_con, System = "Conventional"),
  data.frame(yield_sim_ca, System = "Conservation")
)

# Create the plot using facets instead of separate plots
joint_yield_sim <- 
  ggplot(data = yield_sim_all, 
         aes(x = Year, 
             y = Yield, 
             color = Crop)) +
  geom_line(size = 1.2) +
  labs(y = "Yield (kg/ha)", 
       x = "Year", 
       title = "Simulated Yield Trends Over 12 Years") +
  theme_minimal() +
  facet_wrap(~ System, 
             ncol = 2) +  # Facet by system
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 15, hjust = 0))

# Display the plot
joint_yield_sim










# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`~~~~~~~~~~~####
# Revenue long-term variability ####


# ~ Functions ####

# Simulate 12 years of price trends for each crop
simulate_price_trend <- function(initial_price, drift, volatility, years = 12) {
  price_series <- numeric(years)
  price_series[1] <- initial_price  # Start with the historical price
  
  for (t in 2:years) {
    price_series[t] <- price_series[t-1] * (1 + drift + rnorm(1, mean = 0, sd = volatility))
  }
  return(price_series)
}



# ~ Define Long-Term Price Trends ####

# Define inflation rate (1-2% per year)
price_drift <- 0.015  

# Define price volatility (random shocks per year)
price_volatility <- 0.05  # 5% fluctuation per year






# Apply to each crop
price_wheat_trend <- simulate_price_trend(historic_wheat_price, price_drift, price_volatility)
price_wbarley_trend <- simulate_price_trend(historic_barley_price, price_drift, price_volatility)
price_wosr_trend <- simulate_price_trend(historic_wosr_price, price_drift, price_volatility)

price_wbeans_trend <- simulate_price_trend(historic_wbeans_price, price_drift, price_volatility)
price_sbarley_trend <- simulate_price_trend(historic_barley_price, price_drift, price_volatility)
price_fpeas_trend <- simulate_price_trend(historic_peas_price, price_drift, price_volatility)






# ~ Integrate Time-Adjusted Prices & Yields into Revenue ####


# Assign revenues based on the simulated year in rotation
revenue_conventional <- ifelse(
  crop_sequence_conventional == "Winter Wheat", 
  yield_wheat_trend_con[start_years_conventional] * price_wheat_trend[start_years_conventional],ifelse(crop_sequence_conventional == "Winter Barley", 
         yield_wbarley_trend_con[start_years_conventional] * price_wbarley_trend[start_years_conventional],
                yield_wosr_trend_con[start_years_conventional] * price_wosr_trend[start_years_conventional])  # Oilseed Rape as default
)

revenue_conservation <- ifelse(
  crop_sequence_conservation == "Winter Beans", yield_wbeans_trend_ca[start_years_conservation] * price_wbeans_trend[start_years_conservation],
  ifelse(crop_sequence_conservation == "Winter Wheat", yield_wheat_trend_ca[start_years_conservation] * price_wheat_trend[start_years_conservation],
         ifelse(crop_sequence_conservation == "Spring Barley", yield_sbarley_trend_ca[start_years_conservation] * price_sbarley_trend[start_years_conservation],
                ifelse(crop_sequence_conservation == "Oilseed Rape", yield_wosr_trend_ca[start_years_conservation] * price_wosr_trend[start_years_conservation],
                       ifelse(crop_sequence_conservation == "Feed Peas", yield_fpeas_trend_ca[start_years_conservation] * price_fpeas_trend[start_years_conservation], 0)))))  # Default: no revenue for cover crops



# ~ plots ####


## ~~ price data simulation ####

# make the plotting dataframe

# Combine price trends for visualization
price_data <- data.frame(
  Year = rep(1:12, 6),
  Price = c(price_wheat_trend, price_wbarley_trend, 
            price_wosr_trend, price_wbeans_trend,
            price_sbarley_trend, price_fpeas_trend),
  Crop = rep(c("Winter Wheat", "Winter Barley", 
               "Oilseed Rape", "Winter Beans",
               "Spring Barley", "Feed Peas"), 
             each = 12)
)


# plot the price simulation 

ggplot(data = price_data, 
       aes(x = Year, 
           y = Price, 
           color = Crop)) +
  geom_line(size = 1.2) +
  labs(title = "Simulated Crop Price Trends Over 12 Years", 
       y = "Price (£ t-1)", 
       x = "Year") +
  theme_minimal()
















# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# Climate-Driven Yield Shocks ####


# ~ Function ####

# Apply climate shocks to yield trends
apply_climate_shocks <- function(yield_trend) {
  for (year in 1:length(yield_trend)) {
    if (runif(1) < climate_shock_prob[year]) {  # Climate event occurs
      yield_trend[year] <- yield_trend[year] * (1 - climate_shock_severity())
    }
  }
  return(yield_trend)
}





# ~ Define Climate Shocks ####

# Probability of a climate shock in each year (increasing over time)
climate_shock_prob_con <- seq(0.05, 0.20, length.out = 12)  # 5% in Year 1, 20% in Year 12

climate_shock_prob_ca <- seq(0.04, 0.16, length.out = 12)  # 5% in Year 1, 20% in Year 12

# Define severity of yield reduction (10% to 30%)
climate_shock_severity_con <- function() runif(1, min = 0.1, max = 0.3)  

climate_shock_severity_ca <- function() runif(1, min = 0.05, max = 0.15)  




# ~ apply function ####

# Apply shocks to each crop

# Conventional 
yield_wheat_trend_con <- apply_climate_shocks(yield_wheat_trend_con)
yield_wbarley_trend_con <- apply_climate_shocks(yield_wbarley_trend_con)
yield_wosr_trend_con <- apply_climate_shocks(yield_wosr_trend_con)


# Conservation
yield_wbeans_trend_ca <- apply_climate_shocks(yield_wbeans_trend_ca)
yield_sbarley_trend_ca <- apply_climate_shocks(yield_sbarley_trend_ca)
yield_fpeas_trend_ca <- apply_climate_shocks(yield_fpeas_trend_ca)
yield_wheat_trend_ca <- apply_climate_shocks(yield_wheat_trend_ca)
yield_wbarley_trend_ca <- apply_climate_shocks(yield_wbarley_trend_ca)
yield_wosr_trend_ca <- apply_climate_shocks(yield_wosr_trend_ca)





# ~ Visualizing Climate Impact ####

# Conventional
climate_yield_data_con <- 
  data.frame(
  Year = rep(1:12, 3),
  Yield = c(yield_wheat_trend_con, yield_wbarley_trend_con, yield_wosr_trend_con),
  Crop = rep(c("Winter Wheat", "Winter Barley", "Oilseed Rape"), each = 12)
)


# Conservation
climate_yield_data_ca <- 
  data.frame(
    Year = rep(1:12, 3),
    Yield = c(yield_wheat_trend_ca, yield_wbarley_trend_ca, yield_wosr_trend_ca,
              yield_wbeans_trend_ca, yield_sbarley_trend_ca, yield_fpeas_trend_ca),
    Crop = rep(c("Winter Wheat", "Winter Barley", "Oilseed Rape",
                 "Winter Beans", "Spring Barley", "Feed Peas"), each = 12)
  )


ggplot(yield_data, aes(x = Year, y = Yield, color = Crop)) +
  geom_line(size = 1.2) +
  labs(title = "Yield Trends with Climate Shocks", y = "Yield (kg/ha)", x = "Year") +
  theme_minimal()



## ~~ joint plot ####

# Combine both datasets
climate_yield_sim_all <- rbind(
  data.frame(climate_yield_data_con, System = "Conventional"),
  data.frame(climate_yield_data_ca, System = "Conservation")
)

# Create the plot using facets instead of separate plots
joint_yield_sim <- 
  ggplot(data = climate_yield_sim_all, 
         aes(x = Year, 
             y = Yield, 
             color = Crop)) +
  geom_line(size = 1.2) +
  labs(y = "Yield (kg/ha)", 
       x = "Year", 
       title = "Simulated Yield Trends Over 12 Years") +
  theme_minimal() +
  facet_wrap(~ System, 
             ncol = 2) +  # Facet by system
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 15, hjust = 0))

# Display the plot
joint_yield_sim






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# Inflation ####


# ~ Function ####

# Simulate 12 years of increasing expenditures
simulate_expenditure_trend <- function(initial_expenditure, inflation, volatility, years = 12) {
  exp_series <- numeric(years)
  exp_series[1] <- initial_expenditure  # Start with the base cost
  
  for (t in 2:years) {
    exp_series[t] <- exp_series[t-1] * (1 + inflation + rnorm(1, mean = 0, sd = volatility))
  }
  return(exp_series)
}




# ~ Define Inflation on Expenditure ####

# Define annual expenditure inflation rate (2-4%)
expenditure_inflation <- 0.02  

# Define random market fluctuation (±2%)
expenditure_volatility <- 0.02  






# ~ Apply inflation function ####

# Conventional 

expen_wheat_trend <- simulate_expenditure_trend(initial_expenditure = wheat_expenditure_mean, 
                                                inflation = expenditure_inflation, 
                                                volatility = expenditure_volatility)

expen_wbarley_trend <- simulate_expenditure_trend(initial_expenditure = wbarley_expenditure_mean, 
                                                  inflation = expenditure_inflation, 
                                                  volatility = expenditure_volatility)

expen_wosr_trend <- simulate_expenditure_trend(initial_expenditure = wosr_expenditure_mean, 
                                               inflation = expenditure_inflation, 
                                               volatility = expenditure_volatility)






expen_wbeans_trend <- simulate_expenditure_trend(initial_expenditure = wbeans_expenditure_mean, 
                                                 inflation = expenditure_inflation, 
                                                 volatility = expenditure_volatility)

expen_sbarley_trend <- simulate_expenditure_trend(initial_expenditure = sbarley_expenditure_mean, 
                                                  inflation = expenditure_inflation, 
                                                  volatility = expenditure_volatility)

expen_fpeas_trend <- simulate_expenditure_trend(initial_expenditure = peas_expenditure_mean, 
                                                inflation = expenditure_inflation, 
                                                volatility = expenditure_volatility)





# ~ plot cost inflation ####

expen_data <- data.frame(
  Year = rep(1:12, 3),
  Expenditure = c(expen_wheat_trend, expen_wbarley_trend, expen_wosr_trend),
  Crop = rep(c("Winter Wheat", "Winter Barley", "Oilseed Rape"), each = 12)
)

ggplot(expen_data, aes(x = Year, y = Expenditure, color = Crop)) +
  geom_line(size = 1.2) +
  labs(title = "Expenditure Trends with Inflation", y = "Expenditure (£/ha)", x = "Year") +
  theme_minimal()






# ~~~~~~~~~~~~~~~~~~~~ ####
# Updating Gross Margin Calculation ####

gross_margin_conventional <- (
  ((yield_wheat_trend_con * price_wheat_trend) - expen_wheat_trend) + 
    ((yield_wbarley_trend * price_wbarley_trend) - expen_wbarley_trend) +
    ((yield_wosr_trend * price_wosr_trend) - expen_wosr_trend)
)

gross_margin_conservation <- (
  ((yield_wbeans_trend * price_wbeans_trend) - expen_wbeans_trend) + 
    ((yield_wheat_trend * price_wheat_trend) - expen_wheat_trend) +
    ((yield_sbarley_trend * price_sbarley_trend) - expen_sbarley_trend) +
    ((yield_wosr_trend * price_wosr_trend) - expen_wosr_trend) +
    ((yield_fpeas_trend * price_fpeas_trend) - expen_fpeas_trend)
)





# ~ plot the updated GM ####

# Store the results in a data frame for easier analysis
results <- data.frame(
  gross_margin_conventional = gross_margin_conventional,
  gross_margin_conservation = gross_margin_conservation
)

# You can now analyze the results, for example:
summary(results)



# Combine the results into a long format data frame for ggplot
results_long <- data.frame(
  gross_margin = c(gross_margin_conventional, gross_margin_conservation),
  treatment = rep(c("Conventional", "Conservation"))
)




# Plot density plots for both treatments
ggplot(data = results_long, 
       aes(x = gross_margin, 
           fill = treatment)) +
  geom_density(alpha = 0.5) +
  labs(title = "12 Year Gross Margin Distribution by Treatment ha-1",
       x = "Gross Margin",
       y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "green"))

# Plot boxplots for both treatments
ggplot(results_long, aes(x = treatment, y = gross_margin, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Boxplot of Gross Margin by Treatment",
       x = "Treatment",
       y = "Gross Margin") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "green"))









# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# Risk Assessment: Worst-Case Scenarios ####


# ~ calcularte scenarios ####

set.seed(123)

# Define severity of worst-case events
worst_yield_drop <- function() runif(1, min = 0.4, max = 0.6)  # 40-60% yield reduction
worst_price_drop <- function() runif(1, min = 0.3, max = 0.5)  # 30-50% price drop
worst_cost_increase <- function() runif(1, min = 0.1, max = 0.2)  # 10-20% cost increase

# Function to apply worst-case events randomly over 12 years
apply_worst_case <- function(value_series, event_prob, adjustment_func) {
  for (year in 1:length(value_series)) {
    if (runif(1) < event_prob) {  # Probability of bad event
      value_series[year] <- value_series[year] * (1 - adjustment_func())
    }
  }
  return(value_series)
}

# Define event probabilities
worst_case_prob <- 0.1  # 10% chance per year

# Apply to yields, prices, and expenditures
yield_wheat_worst <- apply_worst_case(yield_wheat_trend, worst_case_prob, worst_yield_drop)
price_wheat_worst <- apply_worst_case(price_wheat_trend, worst_case_prob, worst_price_drop)
expen_wheat_worst <- apply_worst_case(expen_wheat_trend, worst_case_prob, worst_cost_increase)




# ~ plot the scenarios ####

# Combine normal vs. worst-case revenue trends
data_worst_case <- data.frame(
  Year = rep(1:12, 2),
  Revenue = c(yield_wheat_trend * price_wheat_trend, yield_wheat_worst * price_wheat_worst),
  Scenario = rep(c("Normal", "Worst-Case"), each = 12)
)

ggplot(data_worst_case, aes(x = Year, y = Revenue, color = Scenario)) +
  geom_line(size = 1.2) +
  labs(title = "Revenue Trends Under Normal vs. Worst-Case Scenarios",
       y = "Revenue (£/ha)", x = "Year") +
  theme_minimal()








# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# Long term Profit ####


# ~ Simulating Profitability Trends ####

# Calculate cumulative profits
cumulative_profit_conventional <- cumsum(gross_margin_conventional)
cumulative_profit_conservation <- cumsum(gross_margin_conservation)

# Create a data frame for visualization
profit_data <- data.frame(
  Year = rep(1:12, 2),
  Cumulative_Profit = c(cumulative_profit_conventional, cumulative_profit_conservation),
  Treatment = rep(c("Conventional", "Conservation"), each = 12)
)

ggplot(profit_data, aes(x = Year, y = Cumulative_Profit, color = Treatment)) +
  geom_line(size = 1.2) +
  labs(title = "Cumulative Profitability Over 12 Years",
       y = "Cumulative Profit (£/ha)", x = "Year") +
  theme_minimal()







# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# Sensitivity Analysis ####


# ~ Define Sensitivity Functions ####

set.seed(123)

# Inflation impact on expenditure
apply_inflation <- function(expen_series, rate) {
  for (year in 2:length(expen_series)) {
    expen_series[year] <- expen_series[year-1] * (1 + rate)
  }
  return(expen_series)
}

# Climate impact on yields
apply_climate_shock <- function(yield_series, severity) {
  for (year in 1:length(yield_series)) {
    yield_series[year] <- yield_series[year] * (1 - runif(1, min = severity[1], max = severity[2]))
  }
  return(yield_series)
}

# Market volatility impact on prices
apply_price_volatility <- function(price_series, volatility) {
  for (year in 1:length(price_series)) {
    price_series[year] <- price_series[year] * (1 + runif(1, min = -volatility[2], max = volatility[2]))
  }
  return(price_series)
}






# ~ Apply Sensitivity Analysis to Revenue and Costs ####

# Define Scenarios
inflation_rates <- c(0.01, 0.03, 0.05)  # 1%, 3%, 5%
climate_severities <- list(mild = c(0.05, 0.10), moderate = c(0.10, 0.20), severe = c(0.20, 0.40))
price_volatilities <- list(low = c(0.05, 0.10), medium = c(0.10, 0.20), high = c(0.20, 0.30))

# Run sensitivity analysis for different scenarios
results_list <- list()

for (inflation in inflation_rates) {
  for (climate in names(climate_severities)) {
    for (volatility in names(price_volatilities)) {
      
      # Adjust yields, prices, and expenditures
      yield_wheat_adj <- apply_climate_shock(yield_wheat_trend, climate_severities[[climate]])
      price_wheat_adj <- apply_price_volatility(price_wheat_trend, price_volatilities[[volatility]])
      expen_wheat_adj <- apply_inflation(expen_wheat_trend, inflation)
      
      # Calculate revenue and gross margin
      revenue_wheat_adj <- yield_wheat_adj * price_wheat_adj
      gross_margin_adj <- revenue_wheat_adj - expen_wheat_adj
      
      # Store results
      results_list[[paste(inflation, climate, volatility, sep = "_")]] <- gross_margin_adj
    }
  }
}




# ~ plot the Sensitivity Analysis ####

# Convert results to dataframe
sensitivity_df <- do.call(rbind, lapply(names(results_list), function(x) {
  data.frame(
    Scenario = x,
    Year = 1:12,
    Gross_Margin = results_list[[x]]
  )
}))

# Plot all scenarios
ggplot(sensitivity_df, aes(x = Year, y = Gross_Margin, color = Scenario)) +
  geom_line(alpha = 0.5) +
  labs(title = "Sensitivity Analysis: Gross Margin Under Different Scenarios",
       y = "Gross Margin (£/ha)", x = "Year") +
  theme_minimal()



# ~~~~~~~~~ ####
# ~ compare treatments ####


# ~ Define Sensitivity Functions for Both Systems ####

set.seed(123)

# Inflation impact on expenditure
apply_inflation <- function(expen_series, rate) {
  for (year in 2:length(expen_series)) {
    expen_series[year] <- expen_series[year-1] * (1 + rate)
  }
  return(expen_series)
}

# Climate impact on yields
apply_climate_shock <- function(yield_series, severity) {
  for (year in 1:length(yield_series)) {
    yield_series[year] <- yield_series[year] * (1 - runif(1, min = severity[1], max = severity[2]))
  }
  return(yield_series)
}

# Market volatility impact on prices
apply_price_volatility <- function(price_series, volatility) {
  for (year in 1:length(price_series)) {
    price_series[year] <- price_series[year] * (1 + runif(1, min = -volatility[2], max = volatility[2]))
  }
  return(price_series)
}





# ~ run the sensitivity analysis ####

# Define Scenarios
inflation_rates <- c(0.01, 0.03, 0.05)  # 1%, 3%, 5%
climate_severities <- list(mild = c(0.05, 0.10), moderate = c(0.10, 0.20), severe = c(0.20, 0.40))
price_volatilities <- list(low = c(0.05, 0.10), medium = c(0.10, 0.20), high = c(0.20, 0.30))

# Initialize lists to store results
results_ct <- list()
results_ca <- list()

for (inflation in inflation_rates) {
  for (climate in names(climate_severities)) {
    for (volatility in names(price_volatilities)) {
      
      # Apply risk factors to conventional tillage
      yield_ct_adj <- apply_climate_shock(yield_ct_trend, climate_severities[[climate]])
      price_ct_adj <- apply_price_volatility(price_ct_trend, price_volatilities[[volatility]])
      expen_ct_adj <- apply_inflation(expen_ct_trend, inflation)
      revenue_ct_adj <- yield_ct_adj * price_ct_adj
      gross_margin_ct <- revenue_ct_adj - expen_ct_adj
      
      # Apply risk factors to conservation agriculture
      yield_ca_adj <- apply_climate_shock(yield_ca_trend, climate_severities[[climate]])
      price_ca_adj <- apply_price_volatility(price_ca_trend, price_volatilities[[volatility]])
      expen_ca_adj <- apply_inflation(expen_ca_trend, inflation)
      revenue_ca_adj <- yield_ca_adj * price_ca_adj
      gross_margin_ca <- revenue_ca_adj - expen_ca_adj
      
      # Store results
      results_ct[[paste(inflation, climate, volatility, sep = "_")]] <- gross_margin_ct
      results_ca[[paste(inflation, climate, volatility, sep = "_")]] <- gross_margin_ca
    }
  }
}





# ~ Visualizing CA vs. CT Under Risk Scenarios ####


# Convert results to dataframe
sensitivity_df_ct <- do.call(rbind, lapply(names(results_ct), function(x) {
  data.frame(
    Scenario = x,
    Year = 1:12,
    Gross_Margin = results_ct[[x]],
    Treatment = "Conventional"
  )
}))

sensitivity_df_ca <- do.call(rbind, lapply(names(results_ca), function(x) {
  data.frame(
    Scenario = x,
    Year = 1:12,
    Gross_Margin = results_ca[[x]],
    Treatment = "Conservation"
  )
}))

# Combine both datasets
sensitivity_df <- rbind(sensitivity_df_ct, sensitivity_df_ca)

# Plot comparison
ggplot(sensitivity_df, aes(x = Year, y = Gross_Margin, color = Treatment)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~Scenario, scales = "free_y") +
  labs(title = "CA vs. CT Gross Margin Under Different Risk Scenarios",
       y = "Gross Margin (£/ha)", x = "Year") +
  theme_minimal()







# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####

# Define Monte Carlo Simulation for Gross Margin Comparison ####

# Set simulation parameters
n_sim <- 10000  # Number of simulations

# Simulate prices and yields for both systems under risk scenarios
simulate_system <- function(yield_mean, yield_sd, price_mean, price_sd, expenditure_mean, expenditure_sd, n_sim) {
  
  # Generate random values for yield, price, and expenditure
  yields <- rnorm(n_sim, mean = yield_mean, sd = yield_sd)
  prices <- rnorm(n_sim, mean = price_mean, sd = price_sd)
  expenditures <- rnorm(n_sim, mean = expenditure_mean, sd = expenditure_sd)
  
  # Calculate revenue and gross margin
  revenue <- yields * prices
  gross_margin <- revenue - expenditures
  
  return(gross_margin)
}

# Example of parameters (replace with actual values)
yield_mean_ct <- 8000   # Average yield for CT (kg/ha)
yield_sd_ct <- 1000     # Standard deviation for CT yield (kg/ha)
price_mean_ct <- 200    # Average price for CT crop (€/kg)
price_sd_ct <- 30       # Standard deviation for CT price (€/kg)
expenditure_mean_ct <- 5000  # Average expenditure for CT (€)
expenditure_sd_ct <- 1000    # Standard deviation for CT expenditure (€)

yield_mean_ca <- 7500   # Average yield for CA (kg/ha)
yield_sd_ca <- 800      # Standard deviation for CA yield (kg/ha)
price_mean_ca <- 210    # Average price for CA crop (€/kg)
price_sd_ca <- 25       # Standard deviation for CA price (€/kg)
expenditure_mean_ca <- 4500  # Average expenditure for CA (€)
expenditure_sd_ca <- 800     # Standard deviation for CA expenditure (€)

# Simulate both systems
gross_margin_ct_sim <- simulate_system(yield_mean_ct, yield_sd_ct, price_mean_ct, price_sd_ct, expenditure_mean_ct, expenditure_sd_ct, n_sim)
gross_margin_ca_sim <- simulate_system(yield_mean_ca, yield_sd_ca, price_mean_ca, price_sd_ca, expenditure_mean_ca, expenditure_sd_ca, n_sim)





# compare the treatment GM's ####

# Calculate probability that CA is more profitable than CT
probability_ca_more_profitable <- mean(gross_margin_ca_sim > gross_margin_ct_sim)

# Output the result
cat("Probability that CA is more profitable than CT:", probability_ca_more_profitable, "\n")


# Create a data frame for visualization
results_df <- data.frame(
  Gross_Margin = c(gross_margin_ct_sim, gross_margin_ca_sim),
  Treatment = rep(c("Conventional", "Conservation"), each = n_sim)
)

# Plot the distributions of gross margin for both systems
ggplot(results_df, aes(x = Gross_Margin, fill = Treatment)) +
  geom_density(alpha = 0.6) +
  labs(title = "Comparison of Gross Margin for CA vs. CT", 
       x = "Gross Margin (€)", 
       y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "green"))



# ~ compare worst case scenarios ####
worst_case_ct <- min(gross_margin_ct_sim)
worst_case_ca <- min(gross_margin_ca_sim)
cat("Worst-case gross margin for CT:", worst_case_ct, "\n")
cat("Worst-case gross margin for CA:", worst_case_ca, "\n")




































