### Economic data
### J Collins 
### 2024-04-26
###


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# DATA ####




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# PACKAGES ####

source(file = "economics_scripts/01_packages.R")






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# LOAD RAW DATA ####


setwd(dir = "~/OneDrive - Harper Adams University/Data/economics/")

# expenditure data 

dat <- read_xlsx(path = "data/04_all/economic_data.xlsx", 
                 sheet = 1, 
                 col_names = TRUE)

# convert date info in format 'mm/dd/yyyy'
dat$Date <- as.Date(dat$Date)

# Order dataframe by date
dat <- dat[order(dat$Date), ]


# expenditure proportions 

ex_proportions <- read_xlsx(path = "data/04_all/expenditure_proportions.xlsx", 
                            sheet = 1, 
                            col_names = TRUE)



# summary data

summary_dat <- read_xlsx(path = "data/04_all/economic_summary.xlsx", 
                         sheet = 1, 
                         col_names = TRUE)


# wheat yield data 

wheat_yield_dat <- read.csv(file = "data/02_wheat/2023.yield.wheat.csv")








# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# DATA PROCESS ####


# add year 

unique(dat$crop)

dat$year <- NA

dat$year <- if_else(condition = dat$crop == "Spring beans", 
                    true = 2022, false = dat$year)

dat$year <- if_else(condition = dat$crop == "Winter wheat", 
                    true = 2023, false = dat$year)

dat$year <- if_else(condition = dat$crop == "Oilseed Rape" | dat$crop == "Spring Barley", 
                    true = 2024, false = dat$year)


## ~ FILTER DATA ####

# Filter the data based on a criteria of a certain column
bean_dat <- subset(dat, dat$crop == "Spring beans")

wheat_dat <- subset(dat, dat$crop == "Winter wheat")

y3_dat <- subset(x = dat, subset = dat$year == 2024)




app_dat <- subset(dat, dat$Type == "Application")

op_dat <- subset(dat, dat$Type == "Operation")







## ~ set factors ####


summary_dat$treatment <- factor(summary_dat$treatment, 
                                levels = c("Conventional", "Conservation"))

summary_dat$block <- factor(summary_dat$block, 
                            levels = c("1", "2", "3", "4", "5"))

summary_dat$plot <- factor(summary_dat$plot, 
                           levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

summary_dat$year <- factor(x = summary_dat$year, levels = c(2022,2023,2024))

ex_proportions$treatment <- factor(ex_proportions$treatment, 
                                levels = c("Conventional", "Conservation"))

app_dat$Treatment <- factor(app_dat$Treatment, 
                                   levels = c("Conventional", "Conservation"))

op_dat$Treatment <- factor(op_dat$Treatment, 
                            levels = c("Conventional", "Conservation"))

ex_proportions$category <- factor(ex_proportions$category, 
                                   levels = c("Crop Expenditure",
                                              "Operational Expenditure", 
                                              "Grain Expenditure", 
                                              "Straw Profit Margin",
                                              "Grain Profit Margin"))














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











# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# SUMMARY STATS ####

# Calculates mean, sd, se and IC - block
gm_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(gross_margin),
    sd=sd(gross_margin)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

gm_sum$year <- factor(x = gm_sum$year, levels = c(2022,2023,2024))



gm_sum_block <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(gross_margin),
    sd=sd(gross_margin)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))




# ~~ NO STRAW ####

# Calculates mean, sd, se and IC - block
no_straw_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(grain_gross_margin),
    sd=sd(grain_gross_margin)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))



# ~~ Revenue summary ####

# Calculates mean, sd, se and IC - block
rev_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(total_revenue),
    sd=sd(total_revenue)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))



# ~~ Net profit margin summary ####

# Calculates mean, sd, se and IC - block
npm_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(net_profit_margin),
    sd=sd(net_profit_margin)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))



# ~~ YIELD ####

# Calculates mean, sd, se and IC - block
yield_sum <- summary_dat %>%
  group_by(treatment, crop) %>%
  summarise( 
    n=n(),
    mean=mean(yield_t_ha),
    sd=sd(yield_t_ha)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

unique(yield_sum$crop)

yield_sum$crop <- factor(yield_sum$crop, levels = c("Spring Beans", "Winter Wheat", "Oilseed Rape", "Spring Barley"))



# ~~ EXPENDITURE ####

# Calculates mean, sd, se and IC - block
expenditure_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n=n(),
    mean=mean(expenditure),
    sd=sd(expenditure)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))






# ~~ OP EXPENDITURE ####

# Calculates mean, sd, se and IC - block
op_expenditure_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n = n(),
    mean = mean(operational_expenditure),
    sd = sd(operational_expenditure)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1))



# ~~ CROP EXPENDITURE ####

# Calculates mean, sd, se and IC - block
crop_expenditure_sum <- summary_dat %>%
  group_by(treatment, year) %>%
  summarise( 
    n = n(),
    mean = mean(crop_expenditure),
    sd = sd(crop_expenditure)
  ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1))








# ~~ application by year and crop ####

names(app_dat)


op_app_sum <- dat %>%
  group_by(Type, Treatment, year) %>%
  summarise( 
    n = n(),
    expenditure = sum(cost_per_ha)
  )

ex_sum <- dat %>%
  group_by(year, Treatment) %>%
  summarise( 
    n = n(),
    expenditure = sum(cost_per_ha)
  )



