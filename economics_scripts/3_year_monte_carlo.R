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





library(dplyr)
library(fitdistrplus)

# Split data by treatment and crop
dat_grouped <- dat %>%
  group_by(treatment, crop.x) 

# Fit distributions for each treatment-crop combination
fits <- dat_grouped %>%
  summarise(
    yield_mean = mean(yield_t_ha),
    yield_sd = sd(yield_t_ha),
    price_mean = mean(price_per_t),
    price_sd = sd(price_per_t),
    cost_mean = mean(total_expenditure_ha),
    cost_sd = sd(total_expenditure_ha),
    .groups = "drop"
  )

print(fits)





library(mc2d)

simulate_economics <- 
  function(yield_mean, yield_sd, price_mean, price_sd, cost_mean, cost_sd, n = 10000) {
  sim_yield <- rnorm(n, yield_mean, yield_sd)
  sim_price <- rnorm(n, price_mean, price_sd)
  sim_cost <- rnorm(n, cost_mean, cost_sd)
  
  sim_revenue <- sim_yield * sim_price
  sim_gm <- sim_revenue - sim_cost
  
  return(data.frame(sim_gm))
}

# Apply simulation for each treatment and crop
sim_results <- fits %>%
  rowwise() %>%
  mutate(
    sims = list(simulate_economics(yield_mean, yield_sd, price_mean, price_sd, cost_mean, cost_sd))
  ) %>%
  unnest(cols = sims)

# Visualize the results
library(ggplot2)

ggplot(data = sim_results, 
       aes(x = sim_gm, 
           fill = treatment)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ crop.x) +
  theme_minimal()





sim_means <- sim_results %>%
  group_by(treatment, crop.x) %>%
  summarise(sim_gm_mean = mean(sim_gm))

observed_means <- dat %>%
  group_by(treatment, crop.x) %>%
  summarise(obs_gm_mean = mean(total_gm_ha))

comparison <- left_join(x = observed_means, 
                        y = sim_means, 
                        by = c("treatment", "crop.x"))
print(comparison)




library(dplyr)
library(tidyr)
library(ggplot2)

# Reshape to long format
comparison_long <- comparison %>%
  pivot_longer(cols = c(obs_gm_mean, sim_gm_mean), 
               names_to = "type", 
               values_to = "gm_value") %>%
  mutate(type = ifelse(type == "obs_gm_mean", "Observed", "Simulated"))

# Print first rows
head(comparison_long)


ggplot(data = comparison_long, 
       aes(x = crop.x, 
           y = gm_value, 
           fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ treatment) +
  labs(title = "Observed vs. Simulated Gross Margin",
       y = "Gross Margin (GBP/ha)", x = "Crop") +
  scale_fill_manual(values = c("Observed" = "blue", "Simulated" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####

# Extending the Monte Carlo Simulation for Risk Analysis ####

glimpse(sim_results)

library(dplyr)

risk_summary <- sim_results %>%
  group_by(treatment, crop.x) %>%
  summarise(
    prob_loss = mean(sim_gm < 0),  # Probability of Gross Margin < 0
    mean_gm = mean(sim_gm),
    gm_sd = sd(sim_gm),
    gm_p5 = quantile(sim_gm, 0.05), # 5% worst-case
    gm_p50 = quantile(sim_gm, 0.50), # Median
    gm_p95 = quantile(sim_gm, 0.95)  # 95% best-case
  )

print(risk_summary)


library(ggplot2)

ggplot(sim_results, aes(x = sim_gm, fill = treatment)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ crop.x) +
  labs(title = "Gross Margin Distribution by Crop and Treatment",
       x = "Simulated Gross Margin (GBP/ha)",
       y = "Density") +
  theme_minimal()


expected_shortfall <- sim_results %>%
  group_by(treatment, crop.x) %>%
  summarise(
    ES_10 = mean(sim_gm[sim_gm <= quantile(sim_gm, 0.10)]) # Expected Shortfall (worst 10%)
  )

print(expected_shortfall)





















