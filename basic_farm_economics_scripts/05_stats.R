### Economic data
### J Collins 
### 2024-04-26
###

## 05 STATS ####

## 05.1 DATA SUBSET ####

wheat_dat <- subset(summary_dat, summary_dat$crop == "Winter Wheat")

bean_dat <- subset(summary_dat, summary_dat$crop == "Spring Beans")




## 05.2 MODELS ####

#### Yield ####

# run glm with 
glm <- glm(yield_t_ha ~ treatment,
           data = wheat_dat)

summary(glm) 


glm <- glm(yield_t_ha ~ treatment,
           data = bean_dat)

summary(glm) 




### Grain gross Margin ####

glm <- glm(grain_gross_margin ~ treatment,
           data = bean_dat)

summary(glm)


glm <- glm(grain_gross_margin ~ treatment,
           data = wheat_dat)

summary(glm) 




### Grain gross Margin ####

glm <- glm(gross_margin ~ treatment,
           data = bean_dat)

summary(glm)


glm <- glm(gross_margin ~ treatment,
           data = wheat_dat)

summary(glm) 





### Gross margin ####

glm <- glm(gross_profit_margin ~ treatment,
           data = bean_dat)

summary(glm)


glm <- glm(gross_profit_margin ~ treatment,
           data = wheat_dat)

summary(glm) 





### Revenue ####

glm <- glm(total_revenue ~ treatment,
           data = bean_dat)

summary(glm)


glm <- glm(total_revenue ~ treatment,
           data = wheat_dat)

summary(glm) 






## 05.3 STATS ####


# 1. Mixed effects model
# (1|block) identifies bird as the random effects variable

m1 <- lme(fixed = gross_margin ~ factor(treatment),
          random = ~1|block,
          data = wheat_dat)
anova(m1)
