# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# ~ Su et al Global database ####


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


global_CA_dat$Relative.yield.change.perc <- global_CA_dat$Relative.yield.change * 100





library(ggplot2)

ggplot(global_CA_dat, aes(x = Relative.yield.change)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
  xlim(-1,3) +
  labs(title = "Distribution of Relative Yield Change",
       x = "Relative Yield Change",
       y = "Count") +
  geom_vline(aes(xintercept = mean(Relative.yield.change, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(Relative.yield.change, na.rm = TRUE)), 
             color = "green", linetype = "dotted", size = 1) +
  theme_minimal()


ggplot(global_CA_dat, aes(x = Relative.yield.change.perc)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  xlim(-100, 100) + 
  labs(title = "Relative Yield Change (%)",
       x = "Relative Yield Change (%)",
       y = "Count") +
  geom_vline(aes(xintercept = mean(Relative.yield.change.perc, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(Relative.yield.change.perc, na.rm = TRUE)), 
             color = "green", linetype = "dotted", size = 1) +
  theme_minimal()



summary_stats <- global_CA_dat %>%
  summarise(
    Mean_Rel_Change = mean(Relative.yield.change, na.rm = TRUE),
    Median_Rel_Change = median(Relative.yield.change, na.rm = TRUE),
    Percent_Positive = mean(Relative.yield.change > 0, na.rm = TRUE) * 100,
    Percent_Negative = mean(Relative.yield.change < 0, na.rm = TRUE) * 100
  )

print(summary_stats)


ggplot(global_CA_dat, aes(x = Relative.yield.change)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
  geom_vline(aes(xintercept = mean(Relative.yield.change, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(Relative.yield.change, na.rm = TRUE)), 
             color = "green", linetype = "dotted", size = 1) +
  labs(title = "Histogram of Relative Yield Change",
       x = "Relative Yield Change",
       y = "Count") +
  theme_minimal()






