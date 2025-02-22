### Economic data
### J Collins 
### 2024-04-26
###

# MACHINERY OPERATIONS ####


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# ~ PACKAGES ####

setwd(dir = "~/Documents/GitHub/economics/")

source(file = "economics_scripts/01_packages.R")




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# ~ DATA ####

setwd(dir = "~/OneDrive - Harper Adams University/Data/economics/")

dat <- read_excel(path = "data/raw_data/04_all/operations/operations_data.xlsx")




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# ~ CALCULATIONS ####


calculate_TFC <- function(width_m, speed_kmh) {
  TFC <- width_m * speed_kmh / 10
  return(TFC)
}

calculate_time_per_ha <- function(width_m, speed_kmh) {
  TFC <- width_m * speed_kmh / 10  # Theoretical Field Capacity in ha/hr
  time_per_ha <- 1 / TFC           # Time needed per hectare
  return(time_per_ha)
}

calculate_total_time <- function(treatment_area, width_m, speed_kmh) {
  time_per_ha <- calculate_time_per_ha(width_m, speed_kmh)
  total_time <- treatment_area * time_per_ha  # Total time for treatment
  return(total_time)
}


dat$tfc <- calculate_TFC(dat$width_m, dat$speed_km_h)

dat$time_per_ha <- calculate_time_per_ha(dat$width_m, dat$speed_km_h)

dat$total_working_time <- calculate_total_time(dat$area_ha, dat$width_m, dat$speed_km_h)

dat$diesel_consum_l_ha <- dat$diesel_consum_kg_ha / 0.835



dat$power_kW = (dat$diesel_consum_l_ha * 10) / 0.60  # Using 40% efficiency

dat$power_HP = dat$power_kW * 1.341  # Convert kW to HP











# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# ~ SUMMARY STATS ####


names(dat)

glimpse(dat)
head(dat)

# Calculates mean, sd, se and IC - block
TFC_sum <- dat %>%
  group_by(treatment, year, crop) %>%
  summarise( 
    n = n(),
    tfc_sum = sum(tfc, na.rm = TRUE),
    tfc_mean = mean(tfc, na.rm = TRUE) / n,
    time_per_ha_sum = sum(time_per_ha, na.rm = TRUE),
    total_working_time_sum = sum(total_working_time, na.rm = TRUE),
    diesel_consum_l_ha_sum = sum(diesel_consum_kg_ha, na.rm = TRUE)
  ) %>% 
  arrange(year)

TFC_sum

write.csv(x = TFC_sum, file = "data/processed_data/machinery_operation_summary.csv")



hp_comparison_per_ha <- dat %>%
  group_by(treatment) %>%
  summarise(
    total_hp_hours = sum(horse_power * total_working_time, na.rm = TRUE),
    total_area = sum(area_ha, na.rm = TRUE),
    hp_hours_per_ha = total_hp_hours / total_area  # Normalized per ha
  )

print(hp_comparison_per_ha)









# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# ~ PLOTS ####


## ~~ application number ####
opp_n_plot <- 
  ggplot(data = TFC_sum, 
         aes(x = treatment, 
             y = n, 
             fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  scale_fill_manual(values=c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  labs(
    x = "Year",
    y = expression("Operational Passes " ~ italic("(n)")),
    subtitle = expression("Operational Passes " ~ italic("(n)"))
  ) +
  facet_wrap(~ year, ncol = 3) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12, 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.title.x = element_blank(), axis.text.x = element_blank())


opp_n_plot




## ~~ work time per ha ####

time_per_ha_plot <- 
  ggplot(data = TFC_sum, 
         aes(x = treatment, 
             y = time_per_ha_sum, 
             fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  scale_fill_manual(values=c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  labs(
    x = "Year",
    y = expression("Theoretical time (Hrs ha"^{-1}*")"),
    subtitle = expression("Theoretical machinery operation (Hrs ha"^{-1}*")")
  ) +
  facet_wrap(~ year, ncol = 3) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12, 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank())

time_per_ha_plot



## ~~ diesel consumption ####

diesel_l_ha_plot <- 
  ggplot(data = TFC_sum, 
         aes(x = treatment, 
             y = diesel_consum_l_ha_sum, 
             fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  scale_fill_manual(values=c("turquoise3", "tomato2"), 
                    name = "Treatment") +
  labs(
    x = "Year",
    y = expression("Diesel consumption (l ha"^{-1}*")"),
    subtitle = expression("Theoretical diesel consumption (l ha"^{-1}*")")
  ) +
  facet_wrap(~ year, ncol = 3) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12, 
                                    face = "bold.italic"), 
        legend.position = "bottom", 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank())

diesel_l_ha_plot




## ~~ joint plot ####

ggarrange(opp_n_plot, time_per_ha_plot, diesel_l_ha_plot, 
          ncol = 3, 
          common.legend = TRUE, 
          legend = "bottom", 
          labels = c("A","B","C"))

ggsave(filename = "plots/fig_operations.png", width = 10.5, height = 3.5)








