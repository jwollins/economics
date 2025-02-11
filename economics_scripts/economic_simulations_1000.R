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
           y = 2, 
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

# start_yield_reduction <- coef(lm_model)[1]  # Extract intercept






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
#  Model Stats ####


# Number of simulations
n_sim <- 10000





# ~~~~~~~~~~~~####
# ~ Set Crop rotations ####


rotation_conventional <- c("Winter Wheat", "Winter Barley", "Oilseed Rape")

rotation_conservation <- c("Winter Beans", "Winter Wheat", "Spring Barley", 
                           "Oilseed Rape", "Feed Peas", "Winter Wheat")

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



# ~ CON crop prices ####

mean_price_wheat_con <- historic_wheat_price
mean_price_wbarley_con <- historic_barley_price
mean_price_wosr_con <- historic_wosr_price






# ~~~~ ####


# ~ CA crop yield ####


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



# ~ CA crop prices ####

mean_price_wbeans_ca <- historic_wbeans_price
mean_price_wheat_ca <- historic_wheat_price

mean_price_barley_ca <- historic_barley_price
mean_price_wosr_ca <- historic_wosr_price

mean_price_peas_ca <- historic_peas_price









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









# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# ~ Yield Long-Term Trends ####


# Functions ####

# Conventional 

simulate_yield_trend_con <- function(initial_yield, 
                                     drift, 
                                     volatility, 
                                     years = 12, 
                                     n_sim = 1000) {
  # Create a dataframe with simulations
  sim_data <- expand.grid(Year = 1:years, run = 1:n_sim) %>%
    mutate(Yield = NA)

  # Simulate for each run
  for (i in 1:n_sim) {
    yield_series <- numeric(years)
    yield_series[1] <- initial_yield

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
                                    years = 12, 
                                    n_sim = 1000) {
  # Create a dataframe with simulations
  sim_data <- expand.grid(Year = 1:years, run = 1:n_sim) %>%
    mutate(Yield = NA)

  # Simulate for each run
  for (i in 1:n_sim) {
    yield_series <- numeric(years)
    yield_series[1] <- initial_yield

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

# Define yield variability (random shocks per year)
yield_volatility <- 0.03  










# Apply to each crop



# Conventional 
yield_wheat_trend_con <- simulate_yield_trend_con(initial_yield = mean_yield_wheat_con, 
                                              drift = yield_drift_con, 
                                              volatility = yield_volatility, n_sim = 1000)

yield_wbarley_trend_con <- simulate_yield_trend_con(initial_yield = mean_yield_wbarley_con, 
                                                drift = yield_drift_con, 
                                                volatility = yield_volatility, n_sim = 1000)

yield_wosr_trend_con <- simulate_yield_trend_con(initial_yield = mean_yield_wosr_con, 
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





####### test #####

# Add a "Crop" column to each dataset

# Conventional
yield_wheat_trend_con$Crop <- "Winter Wheat"
yield_wbarley_trend_con$Crop <- "Winter Barley"
yield_wosr_trend_con$Crop <- "Oilseed Rape"


# Conservation
yield_wheat_trend_ca$Crop <- "Winter Wheat"
yield_wbarley_trend_ca$Crop <- "Winter Barley"
yield_wosr_trend_ca$Crop <- "Oilseed Rape"
yield_wbeans_trend_ca$Crop <- "Winter Beans"
yield_sbarley_trend_ca$Crop <- "Spring Barley"
yield_fpeas_trend_ca$Crop <- "Feed Peas"


# Combine all simulated yield datasets
yield_sim_con <- bind_rows(yield_wheat_trend_con,
                           yield_wbarley_trend_con,
                           yield_wosr_trend_con)

length(yield_sim_con)


yield_sim_ca <- bind_rows(yield_wheat_trend_ca,
                          yield_wbarley_trend_ca,
                           yield_wosr_trend_ca, 
                          yield_wbeans_trend_ca, 
                          yield_sbarley_trend_ca, 
                          yield_fpeas_trend_ca)


# Combine both datasets
yield_sim_all <- rbind(
  data.frame(yield_sim_con, System = "Conventional"),
  data.frame(yield_sim_ca, System = "Conservation")
)



# ~ summarise the dataset ####


# Summarize the mean and confidence intervals
yield_summary_con <- yield_sim_con %>%
  group_by(Year, Crop) %>%
  summarise(
    mean_yield = mean(Yield),
    ymin = quantile(Yield, 0.025),  # 2.5% percentile (lower bound)
    ymax = quantile(Yield, 0.975)   # 97.5% percentile (upper bound)
  )


# Summarize the mean and confidence intervals
yield_summary_ca <- yield_sim_ca %>%
  group_by(Year, Crop) %>%
  summarise(
    mean_yield = mean(Yield),
    ymin = quantile(Yield, 0.025),  # 2.5% percentile (lower bound)
    ymax = quantile(Yield, 0.975)   # 97.5% percentile (upper bound)
  )




# Combine both datasets
yield_sim_summary_all <- rbind(
  data.frame(yield_summary_con, System = "Conventional"),
  data.frame(yield_summary_ca, System = "Conservation")
)





names(yield_sim_summary_all)

# Create the plot using facets instead of separate plots

  ggplot(data = yield_sim_summary_all, 
         aes(x = Year, 
             y = mean_yield, 
             color = Crop)) +
  geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = Crop), alpha = 0.2) +
    labs(title = "Simulated Yield Trends",
         x = "Year",
         y = "Yield (t/ha)") +
  theme_minimal() +
  facet_wrap(~ System, 
             ncol = 2) +  # Facet by system
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 15, hjust = 0))





names(yield_sim_all)

fig_yield_nsim_plot <-
ggplot(yield_sim_all, 
       aes(x = Year, 
           y = Yield, 
           group = interaction(Crop, as.factor(run)))) +
  geom_line(alpha = 0.05, aes(color = Crop), size = 0.5) +
  labs(
       x = "Year",
       y = "Yield (t/ha)") +
  facet_wrap(~ System, 
             ncol = 2) +
  theme_bw() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 5))) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12, 
                                    # color = "black", 
                                    face = "bold.italic"))

ggsave(plot = fig_yield_nsim_plot,
       filename = "plots/simulation_plots/fig_yield_nsim_plot.png", 
       width = 10, 
       height = 5)


fig_yield_hist_nsim_plot <-
ggplot(yield_sim_all, 
       aes(x = Yield, 
           fill = Crop)) +
  geom_histogram(bins = 30, 
                 alpha = 0.7) +
  facet_wrap(~ System) +
  labs( x = "Yield (t/ha)", y = "Frequency") +
  theme_bw() + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12, 
                                    # color = "black", 
                                    face = "bold.italic"))

ggsave(plot = fig_yield_hist_nsim_plot,
       filename = "plots/simulation_plots/fig_yield_hist_nsim_plot.png", 
       width = 10, height = 5)


fig_joint_yield_sim <-
ggarrange(fig_yield_nsim_plot, 
          fig_yield_hist_nsim_plot, 
          ncol = 1, 
          nrow = 2, common.legend = TRUE, legend = "bottom")

ggsave(plot = fig_joint_yield_sim,
       filename = "plots/simulation_plots/fig_joint_yield_nsim_plot.png", 
       width = 10, height = 8)


ggplot(data = yield_sim_all, 
       aes(x = System, 
           y = Yield, 
           fill = System)) +
  geom_violin(alpha = 0.4, color = NA) +  # Transparent violin for density
  geom_boxplot(width = 0.2, outlier.alpha = 0.2) +  # Compact box plot overlay
  theme_minimal() +
  labs(title = "Simulated Revenue Distribution by Crop and System",
       x = "Crop",
       y = "yield") +
  scale_fill_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom") 






















# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`~~~~~~~~~~~####
# Price long-term variability ####


# ~ Functions ####


# Simulate 12 years of price trends for each crop with 1000 simulations
simulate_price_trend <- function(initial_price, 
                                 drift, 
                                 volatility, 
                                 years = 12, 
                                 n_sim = 1000) {
  # Create a dataframe with simulations
  sim_data <- expand.grid(Year = 1:years, run = 1:n_sim) %>%
    mutate(Price = NA)
  
  # Simulate for each run
  for (i in 1:n_sim) {
    price_series <- numeric(years)
    price_series[1] <- initial_price
    
    for (t in 2:years) {
      price_series[t] <- price_series[t-1] * (1 + drift + rnorm(1, mean = 0, sd = volatility))
    }
    
    sim_data$Price[sim_data$run == i] <- price_series
  }
  
  return(sim_data)
}




# ~ Define Long-Term Price Trends ####

# Define inflation rate (1-2% per year)
price_drift_ca <- 0.015  
price_drift_con <- 0.015 


# Define price volatility (random shocks per year)
price_volatility_ca <- 0.05  # 5% fluctuation per year
price_volatility_con <- 0.05  # 5% fluctuation per year



# Conventional - Simulating Crop Price Trends
price_wheat_trend_con <- simulate_price_trend(initial_price = mean_price_wheat_con, 
                                              drift = price_drift_con, 
                                              volatility = price_volatility, 
                                              n_sim = 1000)

price_wbarley_trend_con <- simulate_price_trend(initial_price = mean_price_wbarley_con, 
                                                drift = price_drift_con, 
                                                volatility = price_volatility, 
                                                n_sim = 1000)

price_wosr_trend_con <- simulate_price_trend(initial_price = mean_price_wosr_con, 
                                             drift = price_drift_con, 
                                             volatility = price_volatility, 
                                             n_sim = 1000)




# Conservation - Simulating Crop Price Trends

price_wheat_trend_ca <- simulate_price_trend(initial_price = mean_price_wheat_ca, 
                                             drift = price_drift_ca, 
                                             volatility = price_volatility, 
                                             n_sim = 1000)

price_wbarley_trend_ca <- simulate_price_trend(initial_price = mean_price_barley_ca, 
                                               drift = price_drift_ca, 
                                               volatility = price_volatility, 
                                               n_sim = 1000)

price_wosr_trend_ca <- simulate_price_trend(initial_price = mean_price_wosr_ca, 
                                            drift = price_drift_ca, 
                                            volatility = price_volatility, 
                                            n_sim = 1000)

price_wbeans_trend_ca <- simulate_price_trend(initial_price = mean_price_wbeans_ca, 
                                              drift = price_drift_ca, 
                                              volatility = price_volatility, 
                                              n_sim = 1000)

price_sbarley_trend_ca <- simulate_price_trend(initial_price = mean_price_barley_ca, 
                                               drift = price_drift_ca, 
                                               volatility = price_volatility, 
                                               n_sim = 1000)

price_fpeas_trend_ca <- simulate_price_trend(initial_price = mean_price_peas_ca, 
                                             drift = price_drift_ca, 
                                             volatility = price_volatility, 
                                             n_sim = 1000)






# Add a "Crop" column to each dataset

# Conventional
price_wheat_trend_con$Crop <- "Winter Wheat"
price_wbarley_trend_con$Crop <- "Winter Barley"
price_wosr_trend_con$Crop <- "Oilseed Rape"


# Conservation
price_wheat_trend_ca$Crop <- "Winter Wheat"
price_wbarley_trend_ca$Crop <- "Winter Barley"
price_wosr_trend_ca$Crop <- "Oilseed Rape"
price_wbeans_trend_ca$Crop <- "Winter Beans"
price_sbarley_trend_ca$Crop <- "Spring Barley"
price_fpeas_trend_ca$Crop <- "Feed Peas"


# Combine all simulated yield datasets
price_sim_con <- bind_rows(price_wheat_trend_con,
                           price_wbarley_trend_con,
                           price_wosr_trend_con)


price_sim_ca <- bind_rows(price_wheat_trend_ca,
                          price_wbarley_trend_ca,
                          price_wosr_trend_ca, 
                          price_wbeans_trend_ca, 
                          price_sbarley_trend_ca, 
                          price_fpeas_trend_ca)


# Combine both datasets
price_sim_all <- rbind(
  data.frame(price_sim_con, System = "Conventional"),
  data.frame(price_sim_ca, System = "Conservation")
)

names(price_sim_all)



# ~ plots ####

fig_price_nsim_plot <-
ggplot(price_sim_all, 
       aes(x = Year, 
           y = Price, 
           group = interaction(Crop, as.factor(run)))) +
  geom_line(alpha = 0.05, aes(color = Crop), size = 0.5) +
  labs(
    x = "Year",
    y = "Price (£/ha)") +
  facet_wrap(~ System,
             ncol = 2) +
  theme_bw() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 5))) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12, 
                                    # color = "black", 
                                    face = "bold.italic"))





ggsave(plot = fig_price_nsim_plot,
       filename = "plots/simulation_plots/fig_price_nsim_plot.png", 
       width = 10, 
       height = 5)


fig_price_hist_nsim_plot <-
  ggplot(price_sim_all, 
         aes(x = Price, 
             fill = Crop)) +
  geom_histogram(bins = 30, 
                 alpha = 0.7) +
  facet_wrap(~ System) +
  labs( x = "Price (£/ha)", y = "Frequency") +
  theme_bw() + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12, 
                                    # color = "black", 
                                    face = "bold.italic"))

ggsave(plot = fig_price_hist_nsim_plot,
       filename = "plots/simulation_plots/fig_price_hist_nsim_plot.png", 
       width = 10, height = 5)


fig_joint_price_sim <-
  ggarrange(fig_price_nsim_plot, 
            fig_price_hist_nsim_plot, 
            ncol = 2, 
            nrow = 1, common.legend = TRUE, legend = "bottom")

ggsave(plot = fig_joint_price_sim,
       filename = "plots/simulation_plots/fig_joint_price_nsim_plot.png", 
       width = 10, height = 4)



ggplot(data = price_sim_all, 
       aes(x = System, 
           y = Price, 
           fill = System)) +
  geom_violin(alpha = 0.4, color = NA) +  # Transparent violin for density
  geom_boxplot(width = 0.2, outlier.alpha = 0.2) +  # Compact box plot overlay
  theme_minimal() +
  labs(title = "Simulated price Distribution by System",
       x = "Crop",
       y = "Price") +
  scale_fill_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom") 







glimpse(price_sim_all)

glimpse(yield_sim_all)







# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# Revenue  ####


# Join price and yield data
revenue_sim_all <- yield_sim_all %>%
  inner_join(price_sim_all, by = c("Year", "run", "Crop", "System")) %>%
  mutate(Revenue = Yield * Price) %>%
  select(Year, run, Crop, System, Yield, Price, Revenue)  # Arrange columns neatly

# Glimpse at the result
glimpse(revenue_sim_all)

names(revenue_sim_all)





# ~ Plots ####

fig_revenue_nsim_plot <-
  ggplot(revenue_sim_all, 
         aes(x = Year, 
             y = Revenue, 
             group = interaction(Crop, as.factor(run)))) +
  geom_line(alpha = 0.05, aes(color = Crop), size = 0.5) +
  labs(
    x = "Year",
    y = "Revenue (£/ha)") +
  facet_wrap(~ System,
             ncol = 2) +
  theme_bw() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 5))) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12, 
                                    # color = "black", 
                                    face = "bold.italic"))


fig_revenue_nsim_plot


fig_revenue_box_plot <-
ggplot(data = revenue_sim_all, 
       aes(x = System, 
           y = Revenue, 
           fill = System)) +
  geom_violin(alpha = 0.4, color = NA) +  # Transparent violin for density
  geom_boxplot(width = 0.2, outlier.alpha = 0.2) +  # Compact box plot overlay
  theme_minimal() +
  labs(
       x = "Treatment",
       y = "Revenue") +
  scale_fill_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
  theme(axis.text.x = element_blank(), 
        legend.position = "bottom") 

fig_revenue_box_plot

ggsave(plot = fig_revenue_box_plot, filename = "plots/simulation_plots/price.png")


fig_joint_revenue_sim <-
  ggarrange(fig_revenue_nsim_plot, 
            fig_revenue_box_plot, 
            ncol = 2, 
            nrow = 1, 
            common.legend = FALSE, 
            legend = "bottom", 
            align = "hv", axis = "tblr")

fig_joint_revenue_sim

ggsave(plot = fig_joint_revenue_sim, 
       filename = "plots/simulation_plots/fig_joint_revenue_sim.png", 
       width = 10,
       height = 4)







glimpse(revenue_sim_all)








# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# Expenditure ####


# ~ Define Inflation on Expenditure ####

# Define annual expenditure inflation rate (2-4%)
expenditure_inflation <- 0.02  

# Define random market fluctuation (±2%)
expenditure_volatility <- 0.02  


# Function ####

simulate_expenditure_trend <- function(initial_expenditure, 
                                       expenditure_mean, 
                                       expenditure_sd, 
                                       inflation, 
                                       volatility, 
                                       years = 12, 
                                       n_sim = 1000) {
  # Create a dataframe for storing the simulation results
  sim_data <- expand.grid(Year = 1:years, run = 1:n_sim) %>%
    mutate(Expenditure = NA)
  
  # Simulate for each run
  for (i in 1:n_sim) {
    expenditure_series <- numeric(years)
    expenditure_series[1] <- initial_expenditure
    
    for (t in 2:years) {
      # Expenditure grows with inflation and is adjusted by volatility
      growth_factor <- (1 + inflation) + rnorm(1, mean = 0, sd = volatility)
      expenditure_series[t] <- expenditure_series[t-1] * growth_factor
    }
    
    sim_data$Expenditure[sim_data$run == i] <- expenditure_series
  }
  
  return(sim_data)
}







# Run the expenditure simulation ####



# Conventional

expen_wheat_trend_con <- simulate_expenditure_trend(
  initial_expenditure = wheat_expenditure_mean, 
  expenditure_mean = wheat_expenditure_mean, 
  expenditure_sd = wheat_expenditure_sd, 
  inflation = expenditure_inflation, 
  volatility = expenditure_volatility)

expen_wbarley_trend_con <- simulate_expenditure_trend(
  initial_expenditure =  wbarley_expenditure_mean, 
  expenditure_mean = wbarley_expenditure_mean, 
  expenditure_sd = wbarley_expenditure_sd, 
  inflation = expenditure_inflation, 
  volatility = expenditure_volatility)

expen_wosr_trend_con <- simulate_expenditure_trend(
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




# Combine the df'ds ####

# Add a "Crop" column to each dataset

# Conventional
expen_wheat_trend_con$Crop <- "Winter Wheat"
expen_wbarley_trend_con$Crop <- "Winter Barley"
expen_wosr_trend_con$Crop <- "Oilseed Rape"


# Conservation
expen_wheat_trend_ca$Crop <- "Winter Wheat"
expen_wbarley_trend_ca$Crop <- "Winter Barley"
expen_wosr_trend_ca$Crop <- "Oilseed Rape"
expen_wbeans_trend_ca$Crop <- "Winter Beans"
expen_sbarley_trend_ca$Crop <- "Spring Barley"
expen_fpeas_trend_ca$Crop <- "Feed Peas"


# Combine all simulated yield datasets
expen_sim_con <- bind_rows(expen_wheat_trend_con,
                           expen_wbarley_trend_con,
                           expen_wosr_trend_con)


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

names(expen_sim_all)




glimpse(expen_sim_all)


# ~ plots ####

fig_expen_nsim_plot <-
  ggplot(expen_sim_all, 
         aes(x = Year, 
             y = Expenditure, 
             group = interaction(Crop, as.factor(run)))) +
  geom_line(alpha = 0.05, aes(color = Crop), size = 0.5) +
  labs(
    x = "Year",
    y = "Expenditure (£/ha)") +
  facet_wrap(~ System,
             ncol = 2) +
  theme_bw() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 5))) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12, 
                                    # color = "black", 
                                    face = "bold.italic"))


fig_expen_nsim_plot


fig_expenditure_box_plot <-
  ggplot(data = expen_sim_all, 
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

ggsave(plot = fig_revenue_box_plot, 
       filename = "plots/simulation_plots/fig_expenditure_box_plot.png")







# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` ####

# Gross Margin ####

names(revenue_sim_all)

# Join price and yield data
sim_all <- revenue_sim_all %>%
  inner_join(expen_sim_all, by = c("Year", "run", "Crop", "System")) %>%
  mutate(Gross_Margin = Revenue - Expenditure) %>%
  select(Year, run, Crop, System, Yield, Price, Revenue, Expenditure, Gross_Margin)  # Arrange columns neatly`


# ~ plots ####

fig_gm_nsim_plot <-
  ggplot(sim_all, 
         aes(x = Year, 
             y = Gross_Margin, 
             group = interaction(Crop, as.factor(run)))) +
  geom_line(alpha = 0.05, aes(color = Crop), size = 0.5) +
  labs(
    x = "Year",
    y = "Gross Margin (£/ha)") +
  facet_wrap(~ System,
             ncol = 2) +
  theme_bw() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 5))) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12, 
                                    # color = "black", 
                                    face = "bold.italic"))

fig_gm_nsim_plot



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

ggsave(plot = fig_gm_box_plot, filename = "plots/simulation_plots/fig_gm_box_plot.png")






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# Climate-Driven Yield Shocks ####


# # Apply climate shocks to yield trends
# 
# apply_climate_shocks_con <- function(yield_trend) {
#   for (year in 1:length(yield_trend)) {
#     if (runif(1) < climate_shock_prob_con[year]) {  # Climate event occurs
#       yield_trend[year] <- yield_trend[year] * (1 - climate_shock_severity_con())
#     }
#   }
#   return(yield_trend)
# }
# 
# 
# apply_climate_shocks_ca <- function(yield_trend) {
#   for (year in 1:length(yield_trend)) {
#     if (runif(1) < climate_shock_prob_ca[year]) {  # Climate event occurs
#       yield_trend[year] <- yield_trend[year] * (1 - climate_shock_severity_ca())
#     }
#   }
#   return(yield_trend)
# }








# ~ Define Climate Shocks ####


# Probability of a climate shock in each year (increasing over time)
climate_shock_prob_con <- seq(0.05, 0.20, length.out = 12)  # 5% in Year 1, 20% in Year 12

climate_shock_prob_ca <- seq(0.04, 0.16, length.out = 12)  # 5% in Year 1, 20% in Year 12

# Define severity of yield reduction (10% to 30%)
climate_shock_severity_con <- function() runif(1, min = 0.1, max = 0.3)  

climate_shock_severity_ca <- function() runif(1, min = 0.05, max = 0.15)  







# ~ apply function ####


# Apply shocks to each crop

set.seed(123)  # For reproducibility

names(revenue_sim_all)

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

# Apply to conventional and conservation systems separately
revenue_sim_all_con <- revenue_sim_all %>%
  filter(System == "Conventional") %>%
  group_by(Year, run) %>%
  group_modify(~ apply_climate_shocks(.x, .y, climate_shock_prob_con, climate_shock_severity_con)) %>%
  ungroup()

revenue_sim_all_ca <- revenue_sim_all %>%
  filter(System == "Conservation") %>%
  group_by(Year, run) %>%
  group_modify(~ apply_climate_shocks(.x, .y, climate_shock_prob_ca, climate_shock_severity_ca)) %>%
  ungroup()

# Combine datasets
revenue_sim_all_shocked <- bind_rows(revenue_sim_all_con, revenue_sim_all_ca)






# ~ plots ####


fig_climate_rev_nsim_plot <-
  ggplot(data = revenue_sim_all_shocked, 
         aes(x = Year, 
             y = Revenue_Shocked, 
             group = interaction(Crop, as.factor(run)))) +
  geom_line(alpha = 0.05, aes(color = Crop), size = 0.5) +
  labs(
    x = "Year",
    y = "Revenue (£/ha)") +
  facet_wrap(~ System,
             ncol = 2) +
  theme_bw() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 5))) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12, 
                                    # color = "black", 
                                    face = "bold.italic"))

fig_climate_rev_nsim_plot



fig_climate_shock_box_plot <-
  ggplot(data = revenue_sim_all_shocked, 
         aes(x = System, 
             y = Revenue_Shocked, 
             fill = System)) +
  geom_violin(alpha = 0.4, color = NA) +  # Transparent violin for density
  geom_boxplot(width = 0.2, outlier.alpha = 0.2) +  # Compact box plot overlay
  theme_minimal() +
  labs(
    x = "Treatment",
    y = "Revenue") +
  scale_fill_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
  theme(axis.text.x = element_blank(), 
        legend.position = "bottom") 

fig_climate_shock_box_plot

ggsave(plot = fig_climate_shock_box_plot, 
       filename = "plots/simulation_plots/climate_shock_box_plot.png", 
       width = 5, height = 4)




glimpse(revenue_sim_all_shocked)





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# Inflation ####


# ~ Function ####

# # Simulate 12 years of increasing expenditures
# simulate_expenditure_trend <- function(initial_expenditure, inflation, volatility, years = 12) {
#   exp_series <- numeric(years)
#   exp_series[1] <- initial_expenditure  # Start with the base cost
#   
#   for (t in 2:years) {
#     exp_series[t] <- exp_series[t-1] * (1 + inflation + rnorm(1, mean = 0, sd = volatility))
#   }
#   return(exp_series)
# }




# ~ Define Inflation on Expenditure ####

# Define annual expenditure inflation rate (2-4%)
expenditure_inflation <- 0.02  

# Define random market fluctuation (±2%)
expenditure_volatility <- 0.02  




# ~ Apply inflation function ####

# Define the function to apply inflation correctly
apply_inflation <- function(data, inflation_rate, volatility_rate) {
  # Get the number of rows in the data
  n_rows <- nrow(data)
  
  # Calculate the inflation factor with a random market fluctuation for each row
  inflation_factor <- (1 + inflation_rate) * (1 + runif(n_rows, -volatility_rate, volatility_rate))
  
  # Apply inflation to the revenue (adjusting by inflation factor)
  data %>%
    mutate(
      Revenue_Adjusted = Revenue_Shocked * inflation_factor  # Apply inflation to shocked revenue
    )
}

# Apply to the dataset with climate shocks
revenue_sim_all_inflated <- revenue_sim_all_shocked %>%
  group_by(Year, run) %>%
  group_modify(~ apply_inflation(.x, expenditure_inflation, expenditure_volatility)) %>%
  ungroup()

# Check the result
glimpse(revenue_sim_all_inflated)





# Plots ####



fig_revenue_inflation_nsim_plot <-
  ggplot(data = revenue_sim_all_inflated, 
         aes(x = Year, 
             y = Revenue_Adjusted, 
             group = interaction(Crop, as.factor(run)))) +
  geom_line(alpha = 0.05, aes(color = Crop), size = 0.5) +
  labs(
    x = "Year",
    y = "Revenue (£/ha)") +
  facet_wrap(~ System,
             ncol = 2) +
  theme_bw() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 5))) + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12, 
                                    # color = "black", 
                                    face = "bold.italic"))

fig_revenue_inflation_nsim_plot


fig_revenue_inflation_box_plot <-
  ggplot(data = revenue_sim_all_inflated, 
         aes(x = System, 
             y = Revenue_Adjusted, 
             fill = System)) +
  geom_violin(alpha = 0.4, color = NA) +  # Transparent violin for density
  geom_boxplot(width = 0.2, outlier.alpha = 0.2) +  # Compact box plot overlay
  theme_minimal() +
  labs(
    x = "Treatment",
    y = "Revenue") +
  scale_fill_manual(values = c("Conventional" = "tomato2", "Conservation" = "turquoise3")) +
  theme(axis.text.x = element_blank(), 
        legend.position = "bottom") 

fig_revenue_inflation_box_plot

ggsave(plot = fig_revenue_inflation_box_plot, 
       filename = "plots/simulation_plots/fig_revenue_inflation_box_plot.png")



















# # Conventional 
# 
# expen_wheat_trend_con <- simulate_expenditure_trend(initial_expenditure = wheat_expenditure_mean, 
#                                                 inflation = expenditure_inflation, 
#                                                 volatility = expenditure_volatility)
# 
# expen_wbarley_trend_con <- simulate_expenditure_trend(initial_expenditure = wbarley_expenditure_mean, 
#                                                   inflation = expenditure_inflation, 
#                                                   volatility = expenditure_volatility)
# 
# expen_wosr_trend_con <- simulate_expenditure_trend(initial_expenditure = wosr_expenditure_mean, 
#                                                inflation = expenditure_inflation, 
#                                                volatility = expenditure_volatility)




# Conservation





expen_wheat_trend_ca <- simulate_expenditure_trend(initial_expenditure = wheat_expenditure_mean * (1 - 0.2), 
                                                    inflation = expenditure_inflation, 
                                                    volatility = expenditure_volatility)

expen_wbarley_trend_ca <- simulate_expenditure_trend(initial_expenditure = wbarley_expenditure_mean * (1 - 0.2), 
                                                      inflation = expenditure_inflation, 
                                                      volatility = expenditure_volatility)

expen_wosr_trend_ca <- simulate_expenditure_trend(initial_expenditure = wosr_expenditure_mean * (1 - 0.2), 
                                                   inflation = expenditure_inflation, 
                                                   volatility = expenditure_volatility)

expen_wbeans_trend_ca <- simulate_expenditure_trend(initial_expenditure = wbeans_expenditure_mean * (1 - 0.2), 
                                                 inflation = expenditure_inflation, 
                                                 volatility = expenditure_volatility)

expen_sbarley_trend_ca <- simulate_expenditure_trend(initial_expenditure = sbarley_expenditure_mean * (1 - 0.2), 
                                                  inflation = expenditure_inflation, 
                                                  volatility = expenditure_volatility)

expen_fpeas_trend_ca <- simulate_expenditure_trend(initial_expenditure = peas_expenditure_mean* (1 - 0.2), 
                                                inflation = expenditure_inflation, 
                                                volatility = expenditure_volatility)





# ~ merge df's ####

expen_data_con <- data.frame(
  Year = rep(1:12, 3),
  Expenditure = c(expen_wheat_trend, expen_wbarley_trend, expen_wosr_trend),
  Crop = rep(c("Winter Wheat", "Winter Barley", "Oilseed Rape"), each = 12)
)

expen_data_ca <- data.frame(
  Year = rep(1:12, 2),
  Expenditure = c(expen_wheat_trend_ca, expen_wbarley_trend_ca, expen_wosr_trend_ca, 
                  expen_sbarley_trend_ca, expen_wbeans_trend_ca, expen_fpeas_trend_ca),
  Crop = rep(c("Winter Wheat", "Winter Barley", "Oilseed Rape", 
               "Spring Barley", "Winter Beans", "Feed Peas"), each = 12)
)



# Combine both datasets
expenditure_sim_all <- rbind(
  data.frame(expen_data_con, System = "Conventional"),
  data.frame(expen_data_ca, System = "Conservation")
)




## ~~ joint plot ####


# Create the plot using facets instead of separate plots
joint_expenditure_sim_plot <- 
  ggplot(data = expenditure_sim_all, 
         aes(x = Year, 
             y = Expenditure, 
             color = Crop)) +
  geom_line(size = 1.2) +
  labs(y = "Expenditure (£/ha)", 
       x = "Year", 
       title = "Simulated Expenditure Trends Over 12 Years") +
  theme_minimal() +
  facet_wrap(~ System, 
             ncol = 2) +  # Facet by system
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 15, hjust = 0))

# Display the plot
joint_expenditure_sim_plot






















# ~~~~~~~~~~~~~~~~~~~~ ####
# Updating Gross Margin Calculation ####

# Combine normal vs. worst-case revenue trends
gross_margin_conventional <- 
  data.frame(
    Year = rep(1:12, 3),  # 12 years for each crop
    GM = c(((yield_wheat_trend_con * price_wheat_trend) - expen_wheat_trend_con), 
           ((yield_wbarley_trend_con * price_wbarley_trend) - expen_wbarley_trend_con),
           ((yield_wosr_trend_con * price_wosr_trend) - expen_wosr_trend_con)),
    Crop = rep(c("Winter Wheat", "Winter Barley", "Oilseed Rape"), each = 12)
  )


gross_margin_conservation <- 
  data.frame(
    Year = rep(1:12, 3),  # 12 years for each crop
    GM = c(((yield_wheat_trend_ca * price_wheat_trend) - expen_wheat_trend_ca), 
           ((yield_wbarley_trend_ca * price_wbarley_trend) - expen_wbarley_trend_ca),
           ((yield_wosr_trend_ca * price_wosr_trend) - expen_wosr_trend_ca)),
    Crop = rep(c("Winter Wheat", "Winter Barley", "Oilseed Rape"), each = 12)
  )


data_worst_case_ca <- 
  data.frame(
    Year = rep(1:12, 2),
    Revenue = c(yield_wheat_trend_ca * price_wheat_trend, yield_wheat_worst_ca * price_wheat_worst_ca),
    Scenario = rep(c("Normal", "Worst-Case"), each = 12)
  )

gross_margin_conventional <- (
  ((yield_wheat_trend_con * price_wheat_trend) - expen_wheat_trend_con) + 
    ((yield_wbarley_trend_con * price_wbarley_trend) - expen_wbarley_trend_con) +
    ((yield_wosr_trend_con * price_wosr_trend) - expen_wosr_trend_con)
)

gross_margin_conservation <- (
  ((yield_wbeans_trend_ca * price_wbeans_trend) - expen_wbeans_trend_ca) + 
    ((yield_wheat_trend_ca * price_wheat_trend) - expen_wheat_trend_ca) +
    ((yield_sbarley_trend_ca * price_sbarley_trend) - expen_sbarley_trend_ca) +
    ((yield_wosr_trend_ca * price_wosr_trend) - expen_wosr_trend_ca) +
    ((yield_fpeas_trend_ca * price_fpeas_trend) - expen_fpeas_trend_ca)
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

set.seed(123)


# ~ calcularte scenarios ####

# Define severity of worst-case events
worst_yield_drop <- function() runif(1, min = 0.4, max = 0.6)  # 40-60% yield reduction
worst_price_drop <- function() runif(1, min = 0.3, max = 0.5)  # 30-50% price drop
worst_cost_increase <- function() runif(1, min = 0.1, max = 0.2)  # 10-20% cost increase

# Define event probabilities
worst_case_prob <- 0.1  # 10% chance per year


# ~ Function ####

# Function to apply worst-case events randomly over 12 years
apply_worst_case <- function(value_series, event_prob, adjustment_func) {
  for (year in 1:length(value_series)) {
    if (runif(1) < event_prob) {  # Probability of bad event
      value_series[year] <- value_series[year] * (1 - adjustment_func())
    }
  }
  return(value_series)
}





# ~ Apply to yields, prices, and expenditures ####

# Conventional

yield_wheat_worst_con <- apply_worst_case(yield_wheat_trend_con, worst_case_prob, worst_yield_drop)
price_wheat_worst_con <- apply_worst_case(price_wheat_trend, worst_case_prob, worst_price_drop)
expen_wheat_worst_con <- apply_worst_case(expen_wheat_trend_con, worst_case_prob, worst_cost_increase)


# Conservation 

yield_wheat_worst_ca <- apply_worst_case(yield_wheat_trend_ca, worst_case_prob, worst_yield_drop)
price_wheat_worst_ca <- apply_worst_case(price_wheat_trend, worst_case_prob, worst_price_drop)
expen_wheat_worst_ca <- apply_worst_case(expen_wheat_trend_ca, worst_case_prob, worst_cost_increase)




# ~ combine the df's ####

# Combine normal vs. worst-case revenue trends
data_worst_case_con <- 
  data.frame(
  Year = rep(1:12, 2),
  Revenue = c(yield_wheat_trend_con * price_wheat_trend, yield_wheat_worst_con * price_wheat_worst_con),
  Scenario = rep(c("Normal", "Worst-Case"), each = 12)
)

data_worst_case_ca <- 
  data.frame(
    Year = rep(1:12, 2),
    Revenue = c(yield_wheat_trend_ca * price_wheat_trend, yield_wheat_worst_ca * price_wheat_worst_ca),
    Scenario = rep(c("Normal", "Worst-Case"), each = 12)
  )


# Combine both datasets
worst_case_sim_all <- rbind(
  data.frame(data_worst_case_con, System = "Conventional"),
  data.frame(data_worst_case_ca, System = "Conservation")
)



# ~ plot the scenarios ####


ggplot(data = worst_case_sim_all, 
       aes(x = Year, 
           y = Revenue, 
           color = System, linetype = Scenario)) +
  geom_line(size = 1.2) +
  labs(title = "Revenue Trends Under Normal vs. Worst-Case Scenarios",
       y = "Revenue (£/ha)", x = "Year") +
  theme_minimal()










# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# Long term Profit ####


# ~ Simulating Profitability Trends ####
print(gross_margin_conventional)
print(gross_margin_conservation)

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




































