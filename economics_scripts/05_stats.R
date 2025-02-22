### Economic data
### J Collins 
### 2024-04-26
###

## 05 STATS ####

#_____________________________________####
# Packages ####

setwd(dir = "~/Documents/GitHub/economics/")

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

mach_dat <- read.csv(file = "data/processed_data/machinery_operation_summary.csv")



# ~ set factors ####

dat$treatment <- factor(x = dat$treatment, levels = c("Conventional", "Conservation"))

mach_dat$treatment <- factor(x = mach_dat$treatment, levels = c("Conventional", "Conservation"))







#_____________________________________####
# MODELS ####

# Perfect fit simulation 

# Simulate dataset in R
set.seed(42)
n <- 30
x <- seq(1, 10, length.out = n)
mu <- exp(2.5 + 0.7 * x)  # Perfect log-linear relationship
y <- rgamma(n, shape = 50, scale = mu / 50)  # Minimal noise

# Data frame
df <- data.frame(x = x, y = y)

# Fit Gamma GLM
model <- glm(y ~ x, family = Gamma(link = "log"), data = df)

# Generate diagnostic plots
diagnostic_plots_glm(model)






# ~ GM ####

# Shift the data to ensure positivity
dat$total_gm_ha_shifted <- dat$total_gm_ha - min(dat$total_gm_ha) + 1

# Plot the transformed distribution
distribution_plots(data = dat, 
                   variable = dat$total_gm_ha_shifted, 
                   colour = dat$total_gm_ha_ha_shifted)

ggsave(filename = "plots/distributions/dist_GM.png", width = 10, height = 2.25)


glm_model <- glmer(formula = total_gm_ha_shifted ~ treatment + (1 | year) + (1 | crop.x), 
                   data = dat, 
                   family = Gamma(link = "inverse"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)


diagnostic_plots_glm(model = glm_model)

ggsave(filename = "plots/mode_diagnostics/model_diag_gross_margin_glmer.png", width = 10, height = 3.5)


# get the contrast in non-inverse scale 

# Regrid emmeans to response scale
em_response <- regrid(emmeans(glm_model, ~ treatment, type = "response"))

# Now calculate contrasts on the response scale
contrast_response <- contrast(em_response, method = "pairwise")

# Display result
summary(contrast_response)







# ~ Expenditure ####

# Plot the transformed distribution
distribution_plots(data = dat, 
                   variable = dat$total_expenditure_ha, 
                   colour = dat$total_expenditure_ha)

ggsave(filename = "plots/distributions/dist_total_expenditure.png", width = 10, height = 2.25)


glm_model <- glmer(formula = total_expenditure_ha ~ treatment + (1 | year) + (1 | crop.x), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = glm_model)

ggsave(filename = "plots/mode_diagnostics/model_diag_total_expenditure_glmer.png", width = 10, height = 3.5)




## ~~ Crop Expenditure ####


# Plot the transformed distribution
distribution_plots(data = dat, 
                   variable = dat$expenditure_Application, 
                   colour = dat$expenditure_Application)

ggsave(filename = "plots/distributions/dist_application_expenditure.png", width = 10, height = 2.25)


glm_model <- glmer(formula = expenditure_Application ~ treatment + (1 | year) + (1 | crop.x), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = glm_model)

ggsave(filename = "plots/mode_diagnostics/model_diag_app_expenditure_glmer.png", width = 10, height = 3.5)





## ~~ Operations Expenditure ####


# Plot the transformed distribution
distribution_plots(data = dat, 
                   variable = dat$expenditure_Operation, 
                   colour = dat$expenditure_Operation)

ggsave(filename = "plots/distributions/dist_operational_expenditure.png", width = 10, height = 2.25)


glm_model <- glmer(formula = expenditure_Operation ~ treatment + (1 | year) + (1 | crop.x), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = glm_model)

ggsave(filename = "plots/mode_diagnostics/model_diag_op_expenditure_glmer.png", width = 10, height = 3.5)





## ~~ Grain Expenditure ####


# Plot the transformed distribution
distribution_plots(data = dat, 
                   variable = dat$grain_expenditure_total_ha, 
                   colour = dat$grain_expenditure_total_ha)

ggsave(filename = "plots/distributions/dist_operational_expenditure.png", width = 10, height = 2.25)


glm_model <- glmer(formula = grain_expenditure_total_ha ~ treatment + (1 | year) + (1 | crop.x), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = glm_model)

# ggsave(filename = "plots/mode_diagnostics/model_diag_grain_expenditure_glmer.png", width = 10, height = 4)






# ~ Revenue ####

# Plot the transformed distribution
distribution_plots(data = dat, 
                   variable = dat$total_revenue_ha, 
                   colour = dat$total_revenue_ha)

ggsave(filename = "plots/distributions/dist_total_revenue.png", width = 10, height = 2.25)


glm_model <- glmer(formula = total_revenue_ha ~ treatment + (1 | year) + (1 | crop.x), 
                   data = dat, 
                   family = Gamma(link = "inverse"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = glm_model)

ggsave(filename = "plots/mode_diagnostics/model_diag_total_revenue_glmer.png", width = 10, height = 3.5)



# get the contrast in non-inverse scale 

# Regrid emmeans to response scale
em_response <- regrid(emmeans(glm_model, ~ treatment, type = "response"))

# Now calculate contrasts on the response scale
contrast_response <- contrast(em_response, method = "pairwise")

# Display result
summary(contrast_response)





# ~ n operations ####

# Plot the transformed distribution
distribution_plots(data = dat, 
                   variable = dat$n_Operation, 
                   colour = dat$n_Operation)

ggsave(filename = "plots/distributions/dist_n_Operation.png", width = 10, height = 2.25)


glm_model <- glmer(formula = n_Operation ~ treatment + (1 | year) + (1 | crop.x), 
                   data = dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = glm_model)

ggsave(filename = "plots/mode_diagnostics/model_diag_n_operations_glmer.png", width = 10, height = 3.5)








# ~ machinery operation ####

glimpse(mach_dat)

# Plot the transformed distribution
distribution_plots(data = mach_dat, 
                   variable = mach_dat$time_per_ha_sum, 
                   colour = mach_dat$time_per_ha_sum)

ggsave(filename = "plots/distributions/dist_operation_time.png", width = 10, height = 2.25)


glm_model <- glmer(formula = time_per_ha_sum ~ treatment + (1 | year) + (1 | crop), 
                   data = mach_dat, 
                   family = Gamma(link = "log"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = glm_model)

ggsave(filename = "plots/mode_diagnostics/model_diag_operation_time_glmer.png", width = 10, height = 3.5)




# ~ diesel consumption ####

glimpse(mach_dat)

# Plot the transformed distribution
distribution_plots(data = mach_dat, 
                   variable = mach_dat$diesel_consum_l_ha_sum, 
                   colour = mach_dat$diesel_consum_l_ha_sum)

ggsave(filename = "plots/distributions/dist_diesel_consum.png", width = 10, height = 2.25)


glm_model <- glmer(formula = diesel_consum_l_ha_sum ~ treatment + (1 | year) + (1 | crop), 
                   data = mach_dat, 
                   family = Gamma(link = "inverse"))

# View summary
summary(glm_model)

# Run pairwise comparisons for the 'Treatment' factor
pairwise_comparisons <- emmeans(glm_model, pairwise ~ treatment)

# View the results of pairwise comparisons
summary(pairwise_comparisons)

diagnostic_plots_glm(model = glm_model)

ggsave(filename = "plots/mode_diagnostics/model_diag_diesel_consum_glmer.png", width = 10, height = 3.5)













