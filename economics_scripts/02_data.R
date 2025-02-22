### Economic data
### J Collins 
### 2024-04-26
###


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# DATA ####




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# PACKAGES ####

setwd(dir = "~/Documents/GitHub/economics/")
source(file = "economics_scripts/01_packages.R")






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# LOAD RAW DATA ####


setwd(dir = "~/OneDrive - Harper Adams University/Data/economics/")

# expenditure data 

dat <- read_xlsx(path = "data/raw_data/application_operations_data.xlsx", 
                 sheet = 1, 
                 col_names = TRUE)

# convert date info in format 'mm/dd/yyyy'
dat$Date <- as.Date(dat$Date)

# Order dataframe by date
dat <- dat[order(dat$Date), ]



## ~ add year ####

unique(dat$crop)

dat$year <- NA

dat$year <- if_else(condition = dat$crop == "Spring beans", 
                    true = 2022, false = dat$year)

dat$year <- if_else(condition = dat$crop == "Winter wheat", 
                    true = 2023, false = dat$year)

dat$year <- if_else(condition = dat$crop == "Oilseed Rape" | dat$crop == "Spring Barley", 
                    true = 2024, false = dat$year)






## ~ filter the raw data ####

app_dat <- subset(dat, dat$Type == "Application")

op_dat <- subset(dat, dat$Type == "Operation")


glimpse(app_dat)




## ~ set factors ####

dat$Treatment <- factor(dat$Treatment, 
                        levels = c("Conventional", "Conservation"))

dat$year <- factor(x = dat$year, levels = c(2022,2023,2024))





# summary_dat$treatment <- factor(summary_dat$treatment, 
#                                 levels = c("Conventional", "Conservation"))
# 
# summary_dat$block <- factor(summary_dat$block, 
#                             levels = c("1", "2", "3", "4", "5"))
# 
# summary_dat$plot <- factor(summary_dat$plot, 
#                            levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
# 
# summary_dat$year <- factor(x = summary_dat$year, levels = c(2022,2023,2024))
# 
# ex_proportions$treatment <- factor(ex_proportions$treatment, 
#                                    levels = c("Conventional", "Conservation"))
# 
# app_dat$Treatment <- factor(app_dat$Treatment, 
#                             levels = c("Conventional", "Conservation"))
# 
# op_dat$Treatment <- factor(op_dat$Treatment, 
#                            levels = c("Conventional", "Conservation"))
# 
# ex_proportions$category <- factor(ex_proportions$category, 
#                                   levels = c("Crop Expenditure",
#                                              "Operational Expenditure", 
#                                              "Grain Expenditure", 
#                                              "Straw Profit Margin",
#                                              "Grain Profit Margin"))







# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# DATA PROCESS ####













## ~ set factors ####

















# ~~~~~~~~~~ ####
# ~ APPLICATIONS ####


app_dat$price_per_unit <- replace(x = app_dat$pack_price / app_dat$pack_size, 
                                  list =  app_dat$pack_size == 0, 
                                  values =  0)

app_dat$units_per_ha <- app_dat$Area_ha * app_dat$Rate_ha

app_dat$cost_per_ha <- app_dat$Rate_ha * app_dat$price_per_unit


# Calculate the accumulated sum based on the 'group' column
app_dat <- app_dat %>%
  group_by(Treatment) %>%
  mutate(accumulated_cost_ha = cumsum(cost_per_ha))


glimpse(app_dat)






# ~~~~~~~~~~ ####
# ~ OPERATIONS ####

op_dat$cost_per_ha <- op_dat$`gross_£`

# Calculate the accumulated sum based on the 'group' column
op_dat <- op_dat %>%
  group_by(Treatment) %>%
  mutate(accumulated_cost_ha = cumsum(cost_per_ha))










# ~~~~~~~~~~ ####
# ~ TOTAL EXPENDITURE ####

dat$price_per_unit <- replace(x = dat$pack_price / dat$pack_size, 
                                  list =  dat$pack_size == 0, 
                                  values =  0)

dat$units_per_ha <- dat$Area_ha * dat$Rate_ha



dat$cost_per_ha <- if_else(condition = dat$Type == "Operation", 
                           true = dat$`gross_£`, 
                           false = NA)

dat$cost_per_ha <- if_else(condition = dat$Type == "Application", 
                           true = dat$Rate_ha * dat$price_per_unit, 
                           false = dat$cost_per_ha)


# Calculate the accumulated sum based on the 'group' column
dat <- dat %>%
  group_by(Treatment) %>%
  mutate(accumulated_cost_ha = cumsum(cost_per_ha))











# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# SUMMARY DATA PROCESSING ####

names(app_dat)


# make the basic summary table of the raw dataframe...
op_app_sum <- dat %>%
  group_by(year, Treatment, Type) %>%
  summarise( 
    n = n(),
    expenditure = sum(cost_per_ha)
  ) %>%
  pivot_wider(
    names_from = Type,  # Create columns based on "Type"
    values_from = c(n, expenditure),  # Fill values for "n" and "expenditure"
    names_sep = "_"  # Add a separator for the new column names
  )


op_app_sum$year <- factor(x = op_app_sum$year, levels = c(2022,2023,2024))
op_app_sum$Treatment <- factor(x = op_app_sum$Treatment, levels = c("Conventional", "Conservation"))


# load the supplementary data frames (crop yield and grain costs per tonne)

yield_dat <- read.csv(file = "data/raw_data/crop_yield.csv")

yield_dat$year <- factor(x = yield_dat$year, levels = c(2022,2023,2024))
yield_dat$treatment <- factor(x = yield_dat$treatment, levels = c("Conventional", "Conservation"))


grain_ex_dat <- read.csv(file = "data/raw_data/grain_costs.csv")

grain_ex_dat$year <- factor(x = grain_ex_dat$year, levels = c(2022,2023,2024))
grain_ex_dat$treatment <- factor(x = grain_ex_dat$treatment, levels = c("Conventional", "Conservation"))

glimpse(op_app_sum)
glimpse(yield_dat)
glimpse(grain_ex_dat)



# join the summary table and the supplementary df's

# Perform the join
yield_dat_joined <- yield_dat %>%
  left_join(
    op_app_sum,
    by = c("year", "treatment" = "Treatment")
  )


# Perform the join
yield_dat_joined <- yield_dat_joined %>%
  left_join(
    grain_ex_dat,
    by = c("year", "treatment")
  )




# make the calculations of variable to plot 

# grain expenditure total per ha
yield_dat_joined$grain_expenditure_total_ha <- 
  yield_dat_joined$yield_t_ha * ( yield_dat_joined$grain_drying_GBP_t + yield_dat_joined$grain_handling_GBP_t )

# grain revenue per ha
yield_dat_joined$grain_rev_ha <- 
  yield_dat_joined$yield_t_ha * yield_dat_joined$price_per_t

# total expensiture per ha
yield_dat_joined$total_expenditure_ha <- 
  yield_dat_joined$expenditure_Application + yield_dat_joined$expenditure_Operation + yield_dat_joined$grain_expenditure_total_ha

# grain gross margin per ha
yield_dat_joined$grain_gm <- 
  yield_dat_joined$grain_rev_ha - yield_dat_joined$total_expenditure_ha

# straw revenue per ha 
yield_dat_joined$straw_rev_ha <- 
  yield_dat_joined$straw_t_ha * yield_dat_joined$straw_price_t

# total revenue per ha
yield_dat_joined$total_revenue_ha <- yield_dat_joined$grain_rev_ha + yield_dat_joined$straw_rev_ha

# gross margin per ha
yield_dat_joined$total_gm_ha <- yield_dat_joined$total_revenue_ha - yield_dat_joined$total_expenditure_ha

# net profit margin
yield_dat_joined$net_profit_margin <- 
  ((yield_dat_joined$total_revenue_ha - yield_dat_joined$total_expenditure_ha) / yield_dat_joined$total_revenue_ha) * 100


summary_dat <- yield_dat_joined

summary_dat <- na.omit(summary_dat)



# Remove a column (e.g., "column_to_remove")
summary_dat <- summary_dat %>%
  select(- "crop.y")


summary_dat[,7:ncol(summary_dat)] <- round(summary_dat[,7:ncol(summary_dat)], digits = 2)

write.csv(x = summary_dat, 
          file = "data/processed_data/summary_economic_data.csv", 
          row.names = FALSE)



glimpse(summary_dat)






# net profit margin tests

summary_dat <- summary_dat %>%
  mutate(net_profit_margin_winsorized = case_when(
    net_profit_margin < quantile(net_profit_margin, 0.05, na.rm = TRUE) ~ quantile(net_profit_margin, 0.05, na.rm = TRUE),
    net_profit_margin > quantile(net_profit_margin, 0.95, na.rm = TRUE) ~ quantile(net_profit_margin, 0.95, na.rm = TRUE),
    TRUE ~ net_profit_margin
  ))








# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# SUMMARY STATS ####


# ~~ GM ####

# Calculates mean, sd, se and IC - block
gm_sum_no_year <- 
  summary_dat %>%
  group_by(treatment) %>%
  summarise( 
    n = n(),
    mean = mean(total_gm_ha),
    sd = sd(total_gm_ha)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) 

gm_sum_no_year

# Calculates mean, sd, se and IC - block
gm_sum <- 
  summary_dat %>%
  group_by(year, treatment) %>%
  summarise( 
    n = n(),
    mean = mean(total_gm_ha),
    sd = sd(total_gm_ha)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>%
 arrange(year)  # Order by year

gm_sum

gm_sum[,3:ncol(gm_sum)] <- round(gm_sum[,3:ncol(gm_sum)], digits = 2)




# Create a LaTeX table
latex_table <- gm_sum %>%
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

print(latex_table)


# gm_sum_block <- summary_dat %>%
#   group_by(treatment, year) %>%
#   summarise( 
#     n=n(),
#     mean=mean(gross_margin),
#     sd=sd(gross_margin)
#   ) %>%
#   mutate( se=sd/sqrt(n))  %>%
#   mutate( ic=se * qt((1-0.05)/2 + .5, n-1))




# ~~ NO STRAW ####

# Calculates mean, sd, se and IC - block
no_straw_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(grain_gm),
    sd=sd(grain_gm)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))



# Calculates mean, sd, se and IC - block
no_straw_sum_no_year <- 
  summary_dat %>%
  group_by(treatment) %>%
  summarise( 
    n=n(),
    mean=mean(grain_gm),
    sd=sd(grain_gm)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

no_straw_sum_no_year


# ~~ Revenue summary ####

# Calculates mean, sd, se and IC - block
rev_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(total_revenue_ha),
    sd=sd(total_revenue_ha)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  arrange(year) 

rev_sum

names(summary_dat)

# Calculates mean, sd, se and IC - block
straw_rev_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(straw_rev_ha),
    sd=sd(straw_rev_ha)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  arrange(year) 

straw_rev_sum


# Calculates mean, sd, se and IC - block
grain_rev_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(grain_rev_ha),
    sd=sd(grain_rev_ha)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  arrange(year) 

grain_rev_sum






# ~~ Net profit margin summary ####

# Calculates mean, sd, se and IC - block
npm_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(net_profit_margin_winsorized),
    sd=sd(net_profit_margin_winsorized)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


# Calculates mean, sd, se and IC - block
npm_sum_no_year <- 
  summary_dat %>%
  group_by(treatment) %>%
  summarise( 
    n=n(),
    mean=mean(net_profit_margin_winsorized),
    sd=sd(net_profit_margin_winsorized)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

npm_sum_no_year




# ~~ YIELD ####

# Calculates mean, sd, se and IC - block
yield_sum <- summary_dat %>%
  group_by(treatment, crop.x) %>%
  summarise( 
    n=n(),
    mean=mean(yield_t_ha),
    sd=sd(yield_t_ha)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

unique(yield_sum$crop.x)

yield_sum$crop.x <- factor(yield_sum$crop.x, levels = c("Spring Beans", "Winter Wheat", "Oilseed Rape", "Spring Barley"))



# ~~ Total EXPENDITURE ####

# Calculates mean, sd, se and IC - block
expenditure_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(total_expenditure_ha),
    sd=sd(total_expenditure_ha)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  arrange(year) 

expenditure_sum














# ~~ OP EXPENDITURE ####

# Calculates mean, sd, se and IC - block
op_expenditure_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n = n(),
    mean = mean(expenditure_Operation),
    sd = sd(expenditure_Operation)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>%
  arrange(year) %>%  # Order by year
  pivot_wider(names_from = treatment, values_from = mean) %>%  # Wide format to compare treatments
  mutate(
    percentage_difference = ((`Conventional` - `Conservation`) / `Conservation`) * 100
  ) %>%
  pivot_longer(cols = c(`Conservation`, `Conventional`), names_to = "treatment", values_to = "mean") %>%
  arrange(year)  # <- This orders the output by year

View(op_expenditure_sum)
print(op_expenditure_sum)




# ~~ AP EXPENDITURE ####

# Calculate mean, sd, se, IC, and percentage difference between treatments
crop_expenditure_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise(
    n = n(),
    mean = mean(expenditure_Application, na.rm = TRUE),
    sd = sd(expenditure_Application, na.rm = TRUE)
  ) %>%
  mutate(
    se = sd / sqrt(n),
    ic = se * qt((1 - 0.05) / 2 + 0.5, n - 1)
  ) %>%
  arrange(year) %>%  # Order by year
  pivot_wider(names_from = treatment, values_from = mean) %>%  # Wide format to compare treatments
  mutate(
    percentage_difference = ((`Conventional` - `Conservation`) / `Conservation`) * 100
  ) %>%
  pivot_longer(cols = c(`Conservation`, `Conventional`), names_to = "treatment", values_to = "mean") %>%
  arrange(year)  # <- This orders the output by year

# Display result
crop_expenditure_sum









# ~~ GRAIN EXPENDITURE ####

names(summary_dat)

# Calculates mean, sd, se and IC - block
grain_expenditure_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n = n(),
    mean = mean(grain_expenditure_total_ha),
    sd = sd(grain_expenditure_total_ha)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1)) %>%
  arrange(year)  # <- This orders the output by year

grain_expenditure_sum


# ~~ Expenditure propotions ####

# Calculates mean, sd, se and IC - block
crop_expenditure_sum <- app_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n = n(),
    mean = mean(expenditure_Application),
    sd = sd(expenditure_Application)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1))




# ~~~~~~~~~~ ####
# ~ PROPORTIONS ####

app_proportions <- 
  app_dat %>%
  group_by(Treatment, year, Category, Type) %>%
  summarise(total_cost_per_ha = sum(cost_per_ha, na.rm = TRUE), .groups = "drop") %>%
  group_by(Treatment, year) %>%
  mutate(proportion = total_cost_per_ha / sum(total_cost_per_ha)) %>%
  arrange(Treatment, year, desc(proportion))


op_proportions <- 
  op_dat %>%
  group_by(Treatment, year, Category, Type) %>%
  summarise(total_cost_per_ha = sum(cost_per_ha, na.rm = TRUE), .groups = "drop") %>%
  group_by(Treatment, year) %>%
  mutate(proportion = total_cost_per_ha / sum(total_cost_per_ha)) %>%
  arrange(Treatment, year, desc(proportion))


combined_proportions <- rbind(app_proportions[,1:5], op_proportions[,1:5])

glimpse(combined_proportions)
glimpse(gm_sum)


# Add the revenue data as rows with Category set to "Profit"
gm_sum_long <- gm_sum %>%
  select(treatment, year, mean) %>%
  rename(Treatment = treatment) %>%
  mutate(Category = "Profit", 
         Type = "Profit", 
         total_cost_per_ha = mean) %>%
  select(Treatment, year, Category, Type, total_cost_per_ha)

# Ensure the year is correctly populated
gm_sum_long$year <- as.numeric(as.character(gm_sum_long$year))  # Convert year to numeric

# Bind the rows of combined_proportions and rev_sum_long
combined_proportions <- bind_rows(combined_proportions, gm_sum_long)

glimpse(combined_proportions)


# Calculate the proportion of total_cost_per_ha within each group
combined_proportions <- combined_proportions %>%
  group_by(Treatment, year, Type) %>%
  mutate(total_group_cost = sum(total_cost_per_ha)) %>%  # Calculate the total cost for each group
  ungroup() %>%
  mutate(proportion = total_cost_per_ha / total_group_cost)  # Calculate the proportion

# Check the results
glimpse(combined_proportions)

write.csv(x = combined_proportions, file = "data/processed_data/expen_and_revenue_proportions.csv")


# Calculate the proportion of total_cost_per_ha within each group
total_proportion <- combined_proportions %>%
  group_by(Treatment, year) %>%
  mutate(total_group_cost = sum(total_cost_per_ha)) %>%  # Calculate the total cost for each group
  ungroup() %>%
  mutate(proportion = total_cost_per_ha / total_group_cost)  # Calculate the proportion

write.csv(x = total_proportion, file = "data/processed_data/total_proportion.csv")





