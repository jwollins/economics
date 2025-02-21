### Economic data
### J Collins 
### 2024-04-26
###

## 05 STATS ####

#_____________________________________####
# Packages ####

source(file = "economics_scripts/01_packages.R")


#__________________________________####
# Functions ####


source(file = "~/Documents/GitHub/phd_tools/fun_glm_diagnostic_plots.R")
source(file = "~/Documents/GitHub/phd_tools/fun_distribution_plots.R")



#__________________________________####
# Data ####

setwd(dir = "~/OneDrive - Harper Adams University/Data/economics/")

# expenditure data 

dat <- read.csv(file = "data/processed_data/summary_economic_data.csv")

names(dat)

dat$treatment <- factor(x = dat$treatment, levels = c("Conventional", "Conservation"))




#_____________________________________####
# MODELS ####


# ~ GM ####

# Shift the data to ensure positivity
dat$total_gm_ha_shifted <- dat$total_gm_ha - min(dat$total_gm_ha) + 1

# Apply the square transformation
dat$total_gm_ha_transformed <- dat$total_gm_ha_shifted^2

# Plot the transformed distribution
distribution_plots(data = dat, variable = dat$total_gm_ha_transformed, colour = dat$treatment)


lmm_model <- lmer(total_gm_ha_transformed ~ treatment + (1 | block) + (1 | year) + (1 | crop.x), 
                   data = dat)

# View summary
summary(lmm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(lmm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = lmm_model)






# ~ Expenditure ####

# Shift the data to ensure positivity
dat$total_expenditure_ha_shifted <- dat$total_expenditure_ha - min(dat$total_expenditure_ha) + 1

# Apply the square transformation
dat$total_expenditure_ha_transformed <- dat$total_expenditure_ha_shifted^2

# Plot the transformed distribution
distribution_plots(data = dat, variable = dat$total_expenditure_ha_transformed, colour = dat$treatment)


lmm_model <- lmer(total_expenditure_ha_transformed ~ treatment + (1 | year) + (1 | crop.x), 
                  data = dat)

# View summary
summary(lmm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(lmm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = lmm_model)





# ~ Revenue ####

# Shift the data to ensure positivity
dat$total_revenue_ha_shifted <- dat$total_revenue_ha - min(dat$total_revenue_ha) + 1

# Apply the square transformation
dat$total_revenue_ha_transformed <- dat$total_revenue_ha_shifted^2

# Plot the transformed distribution
distribution_plots(data = dat, 
                   variable = dat$total_revenue_ha_transformed, 
                   colour = dat$treatment)


lmm_model <- lmer(total_revenue_ha_transformed ~ treatment + (1 | block) + (1 | year) + (1 | crop.x), 
                  data = dat)

# View summary
summary(lmm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(lmm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = lmm_model)



































